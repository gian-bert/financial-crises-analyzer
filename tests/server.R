# Server-logic tests for the Financial Crises Analyzer.
#
# Run from the app directory with:   shiny::runTests()
# (runTests() sources every .R file in tests/ with tests/ as the working
# directory.) Uses only shiny's built-in testServer() — no extra packages.

library(shiny)

APP_DIR <- normalizePath("..")

# Sources app.R into a fresh environment, from `dir`, in hosted or local mode.
# Each call gives a separate copy of the app's globals, like a new R process.
load_app <- function(hosted, dir = APP_DIR) {
  env    <- new.env()
  old_wd <- setwd(dir)
  on.exit(setwd(old_wd))
  Sys.setenv(FCA_HOSTED = if (hosted) "true" else "false")
  on.exit(Sys.unsetenv("FCA_HOSTED"), add = TRUE)
  # parse(encoding=) rather than source(): works in non-UTF-8 locales too
  eval(parse("app.R", encoding = "UTF-8", keep.source = FALSE), env)
  env
}

# A small valid single-index CSV (60 business days, strictly positive).
write_test_csv <- function() {
  path  <- tempfile(fileext = ".csv")
  dates <- seq(as.Date("2020-01-01"), by = "day", length.out = 90)
  dates <- dates[!format(dates, "%u") %in% c("6", "7")][1:60]
  write.csv(data.frame(date = format(dates), value = 100 + seq_along(dates)),
            path, row.names = FALSE)
  path
}
upload <- function(path, name = "test_daily.csv") {
  data.frame(name = name, size = file.size(path), type = "text/csv",
             datapath = path, stringsAsFactors = FALSE)
}

# TRUE if evaluating `expr` fails with a req()-style silent error.
is_silent_error <- function(expr) {
  tryCatch({ force(expr); FALSE },
           shiny.silent.error = function(e) TRUE,
           error = function(e) FALSE)
}

EVIL_NAME <- 'Evil <img src=x onerror="alert(1)">'
csv_path  <- write_test_csv()

# ── Pure helpers ──────────────────────────────────────────────────────────────
app <- load_app(hosted = TRUE)
stopifnot("FCA_HOSTED=true forces hosted mode" = isTRUE(app$IS_HOSTED))

local({
  # Weekend gap between rows 5 and 6: the start date must be the trading day
  # N rows earlier, not the end date minus N calendar days.
  df <- data.frame(date  = as.Date("2024-01-01") + c(0:4, 7:11),
                   value = 100 + 0:9)
  r5 <- app$compute_returns_for_index(df)[["5"]]
  stopifnot(
    "start is NA until N prior prices exist" = all(is.na(r5$start[1:5])),
    "start is the date N rows earlier"       = identical(r5$start[6:10], df$date[1:5]),
    "return is measured from that start"     = isTRUE(all.equal(r5$ret[6], 105 / 100 - 1))
  )

  ep <- app$compute_episodes(r5, k = 1)
  worst <- ep[ep$Type == "Worst", ]
  stopifnot("episode start comes from the data" =
              identical(worst$StartDate, r5$start[which.min(r5$ret)]))
})

# ── Default render: every output works for the built-in data ─────────────────
testServer(app$server, {
  session$setInputs(ticker = "SPX", n_val = "20", limit_active = FALSE,
                    limit_years = 10, obs_years = 10, rw_mode = "full",
                    rank_mode = "one", rank_k = 1, ts_tickers = c("SPX", "EURCHF"),
                    hist_yscale = "linear", def_n = 4, main_tab = "analysis")
  for (id in c("data_info", "series_length_badge", "price_chart", "stats_table",
               "hist_chart", "episodes_table", "rw_avg_badges", "rw_chart",
               "rw_table", "ts_range_list", "ts_chart", "ts_table",
               "coverage_diagram", "def_example_table")) {
    tryCatch(output[[id]], error = function(e)
      stop(sprintf("output$%s failed: %s", id, conditionMessage(e)), call. = FALSE))
  }

  # Cleared numeric inputs arrive as NA; outputs should wait, not error out
  session$setInputs(obs_years = NA)
  stopifnot("blank Observation Period waits" = is_silent_error(output$rw_chart))
  session$setInputs(obs_years = 10, rank_k = NA)
  stopifnot("blank Rank waits" = is_silent_error(output$ts_table))
  session$setInputs(rank_k = 1, def_n = NA)
  stopifnot("blank example N waits" = is_silent_error(output$def_example_table))

  # Sorting by Rank, then leaving "Ranks 1 to N" mode, must not empty the table
  session$setInputs(def_n = 4, rank_mode = "range", rank_k = 3, ts_sort_click = "Rank")
  stopifnot(grepl("<tbody>\\s*<tr", output$ts_table$html))
  session$setInputs(rank_mode = "one")
  stopifnot("table keeps its rows after the sort column disappears" =
              grepl("<tbody>\\s*<tr", output$ts_table$html))
})

# ── Hosted mode: one visitor's imports never reach another ───────────────────
builtin_spx_rows <- nrow(app$raw_data$SPX)

testServer(app$server, {
  session$setInputs(ticker = "SPX", n_val = "20", limit_active = FALSE,
                    rank_mode = "one", rank_k = 1, ts_tickers = "SPX")

  # Add a new index whose name contains HTML
  session$setInputs(import_file = upload(csv_path), import_mode = "new",
                    import_name = EVIL_NAME)
  session$setInputs(import_confirm = 1)
  # testServer() evaluates this block against a snapshot of the server's
  # environment; session$env is the live one the import updated.
  added <- setdiff(names(session$env$INDEX_CFG), app$BUILTIN_TICKERS)
  stopifnot("this session sees its import" = length(added) == 1)

  # The user-supplied name must be escaped where tables render raw HTML
  session$setInputs(ts_tickers = c("SPX", added))
  html <- output$ts_table$html
  stopifnot("label is HTML-escaped" = grepl("&lt;img", html, fixed = TRUE),
            "no raw tag from the label" = !grepl("<img", html, fixed = TRUE))

  # Replace a built-in's data
  session$setInputs(import_mode = "replace", import_replace_target = "SPX")
  session$setInputs(import_confirm = 2)
  stopifnot("this session sees its replacement" = nrow(session$env$raw_data$SPX) == 60)
})

stopifnot(
  "globals keep only the built-ins"   = identical(names(app$INDEX_CFG), app$BUILTIN_TICKERS),
  "global SPX data is untouched"      = nrow(app$raw_data$SPX) == builtin_spx_rows,
  "hosted mode writes nothing to disk" =
    identical(sort(list.files(file.path(APP_DIR, "data"))),
              sort(paste0(app$BUILTIN_TICKERS, "_daily.csv")))
)

testServer(app$server, {
  stopifnot("a second session sees only the built-ins" =
              identical(names(session$env$INDEX_CFG), app$BUILTIN_TICKERS),
            "a second session sees the original SPX" =
              nrow(session$env$raw_data$SPX) == builtin_spx_rows)
})

# ── Local mode: imports persist to disk and survive a restart ────────────────
local({
  dir <- tempfile("fca-local-")
  dir.create(file.path(dir, "data"), recursive = TRUE)
  file.copy(file.path(APP_DIR, "app.R"), dir)
  file.copy(list.files(file.path(APP_DIR, "data"), full.names = TRUE,
                       pattern = "_daily\\.csv$"), file.path(dir, "data"))
  on.exit(unlink(dir, recursive = TRUE))

  local_app <- load_app(hosted = FALSE, dir = dir)
  stopifnot(!isTRUE(local_app$IS_HOSTED))
  old_wd <- setwd(dir)   # the running app resolves data/ against its own directory
  on.exit(setwd(old_wd), add = TRUE, after = FALSE)
  testServer(local_app$server, {
    session$setInputs(ticker = "SPX", ts_tickers = "SPX",
                      import_file = upload(csv_path), import_mode = "new",
                      import_name = "Local Test")
    session$setInputs(import_confirm = 1)
  })
  stopifnot("local import updates the shared globals" = "LOCALTEST" %in% names(local_app$INDEX_CFG),
            "local import writes its data file" = file.exists(file.path(dir, "data", "LOCALTEST_daily.csv")))

  restarted <- load_app(hosted = FALSE, dir = dir)
  stopifnot("local import survives a restart" = "LOCALTEST" %in% names(restarted$INDEX_CFG))
})

cat("All server tests passed.\n")
