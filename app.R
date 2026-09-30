# ─────────────────────────────────────────────────────────────────────────────
# Financial Crises Analyzer — R Shiny Edition
#
# Tabs: Analysis | Rolling Windows | Term Structure | Definitions
#
# ── PACKAGE REQUIREMENTS ───────────────────────────────────────────────────
# Only ONE package needs to be separately installed:
#   - shiny   (the app framework)
#
# 'bslib' is loaded too (it provides the modern layout used everywhere:
# page_sidebar, card, navset_tab, layout_columns, etc.) but it does NOT need
# a separate install step — it is a hard dependency of shiny itself
# (shiny's Imports: bslib >= 0.3.0), so install.packages("shiny") already
# brings it along.
#
# Deliberately NOT used (kept minimal for environments where adding
# packages is painful):
#   - plotly  : every chart (price history, histogram, rolling windows,
#               term structure, the Definitions coverage diagram) is drawn
#               with base-R graphics (renderPlot + plot/lines/rect/legend/
#               axis.Date/...) instead. This trades away a few interactive
#               features — hover tooltips, click-and-drag zoom/pan, and
#               legend click-to-toggle — for a self-contained chart engine
#               that ships with every R installation. Every chart can still
#               be enlarged via each card's full-screen expand icon.
#   - DT      : every data table is a small hand-built HTML table (see the
#               html_table() helper below) instead of a DataTables widget.
#               This keeps row/cell colour styling (Worst/Best/Full/Partial)
#               and the one scrollable table (instead of pagination), but
#               drops DT's built-in multi-column sort UI — the Term
#               Structure table instead has simple click-a-header sorting
#               implemented directly (~15 lines, see ts_sort below).
#   - shinyjs : the small amount of "gray out inactive controls" UI logic
#               is done with a ~10-line vanilla-JS message handler instead
#               (see TOGGLE_JS below).
#   - dplyr   : the only thing this app would have used dplyr for is
#               lag(); a 3-line base-R helper (lag_vec, below) replaces it.
#   - No server/hosting packages (rmarkdown, shinyapps.io helpers, etc.)
#     are required — this app is designed to be run locally with
#     shiny::runApp().
#
# To run:
#   setwd("path/to/this/folder")
#   shiny::runApp()          # or press Run App in RStudio
# ─────────────────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(shiny)   # app framework
  library(bslib)   # modern layout (page_sidebar/card/...) — bundled with shiny, no extra install
})

# Default is 5MB; a daily time series of even 100 years (~36,500 rows) is
# well under 1MB as plain CSV, but a little headroom costs nothing.
options(shiny.maxRequestSize = 15 * 1024^2)

# ══════════════════════════════════════════════════════════════════════════════
# 1. CONFIGURATION
# ══════════════════════════════════════════════════════════════════════════════

N_VALUES <- c(1L, 2L, 3L, 5L, 10L, 15L, 20L, 30L, 90L)

# Where imported indices' data files live, and the small registry CSV that
# remembers their display name/colour/etc. so they survive an app restart
# (see Section 2b and Section 3c below).
DATA_DIR        <- "data"     # built-in CSVs always live here (read is always fine)
MIN_IMPORT_ROWS <- 30L        # below this, reject the file as too short to be useful

# Local vs. hosted mode.
#   Local  (shiny::runApp() on your own machine): imports are written to
#          DATA_DIR and shared by every browser tab, so they survive restarts.
#   Hosted (Posit Connect Cloud / Connect / shinyapps.io): one R process
#          serves many visitors, so imports stay in memory for the browser
#          session that made them and nothing is written to disk. Otherwise
#          one visitor could add, replace or delete indices for everyone else.
# Hosted mode is detected when the app bundle is read-only (Connect Cloud,
# shinyapps.io) or when R_CONFIG_ACTIVE carries the value Connect or
# shinyapps.io set. FCA_HOSTED=true/false overrides the detection either way.
DATA_DIR_WRITABLE <- tryCatch({
  probe <- file.path(DATA_DIR, ".write_probe")
  writeLines("ok", probe)
  unlink(probe)
  TRUE
}, error = function(e) FALSE)
IS_HOSTED <- local({
  forced <- as.logical(Sys.getenv("FCA_HOSTED", NA))
  if (!is.na(forced)) return(forced)
  !DATA_DIR_WRITABLE || Sys.getenv("R_CONFIG_ACTIVE") %in% c("rsconnect", "shinyapps")
})

REGISTRY_PATH <- file.path(DATA_DIR, "imported_index_registry.csv")

INDEX_CFG <- list(
  SPX    = list(label="S&P 500",  color="#F59E0B", obs_years=10, default_n=20L),
  DJI    = list(label="Dow Jones",color="#0D9488", obs_years=10, default_n=20L),
  EURCHF = list(label="EUR/CHF",  color="#22C55E", obs_years=10, default_n=20L),
  VIX    = list(label="VIX",      color="#A78BFA", obs_years=5,  default_n=20L),
  USDCHF = list(label="USD/CHF",  color="#60A5FA", obs_years=10, default_n=20L),
  GBPCHF = list(label="GBP/CHF",  color="#F472B6", obs_years=10, default_n=20L),
  JPYCHF = list(label="JPY/CHF",  color="#FB923C", obs_years=10, default_n=20L)
)

# Snapshot of the 7 built-in ticker names, taken before any import can grow
# INDEX_CFG. Used to protect the built-ins from deletion (their data files
# are part of the app itself) — they can still be *replaced* (refreshed with
# new data), just not removed entirely.
BUILTIN_TICKERS <- names(INDEX_CFG)

# Extra colours assigned to imported indices, in order, skipping any already
# in use (by a built-in or an earlier import) so a new series never collides
# visually with an existing one on the multi-index Term Structure chart.
IMPORT_PALETTE <- c("#14B8A6", "#EAB308", "#8B5CF6", "#EC4899", "#06B6D4",
                     "#84CC16", "#F97316", "#6366F1", "#0EA5E9", "#D946EF")
# `cfg` is the INDEX_CFG in effect for the caller (a session-local copy in
# hosted mode — see the top of the server function).
next_color <- function(cfg) {
  used <- vapply(cfg, function(x) x$color, character(1))
  free <- setdiff(IMPORT_PALETTE, used)
  if (length(free) > 0) return(free[1])
  # Palette exhausted (10+ imports) — fall back to a random HSL hue rather
  # than erroring; a desktop tool with this many indices is an edge case.
  hsv_col <- hsv(h=runif(1), s=0.55, v=0.80)
  hsv_col
}

# Base-R replacement for dplyr::lag(x, n): shifts a vector forward by n
# positions, padding the front with NA. Used during the rolling-return
# pre-computation below — this is the only thing this app would otherwise
# need the dplyr package for.
lag_vec <- function(x, n) {
  if (n <= 0) return(x)
  if (n >= length(x)) return(rep(NA_real_, length(x)))  # window longer than series -> no valid returns
  c(rep(NA_real_, n), x[seq_len(length(x) - n)])
}

# Pre-computes the all_returns[[ticker]] structure for ONE index: a named
# list, keyed by N (as a string), of data.frame(date, start, value, ret).
# `date` is the END of each N-day window (the day the return is measured on)
# and `start` is the trading day N rows earlier whose price the return is
# measured from, so window start dates are exact rather than estimated.
# Used both for the 7 built-in indices at startup and for any index imported
# later in Section 3c — kept as one function so both paths can never drift apart.
compute_returns_for_index <- function(df) {
  v <- df$value
  lapply(setNames(as.list(N_VALUES), as.character(N_VALUES)), function(n) {
    ret   <- v / lag_vec(v, n) - 1
    s_idx <- seq_along(v) - n
    start <- df$date[ifelse(s_idx >= 1, s_idx, NA)]
    data.frame(date=df$date, start=start, value=v, ret=ret, stringsAsFactors=FALSE)
  })
}

# ══════════════════════════════════════════════════════════════════════════════
# 2. DATA LOADING — all indices pre-loaded at startup
# ══════════════════════════════════════════════════════════════════════════════

message("Loading index data...")
raw_data <- lapply(names(INDEX_CFG), function(tk) {
  path <- file.path(DATA_DIR, paste0(tk, "_daily.csv"))
  df   <- read.csv(path, stringsAsFactors=FALSE)
  df$date  <- as.Date(df$date)
  df$value <- as.numeric(df$value)
  df <- df[order(df$date), ]
  df
})
names(raw_data) <- names(INDEX_CFG)

# Pre-compute rolling returns for every index × every N at startup
# Structure: all_returns[[ticker]][[as.character(N)]] -> data.frame(date, value, ret)
message("Pre-computing rolling returns...")
all_returns <- lapply(names(INDEX_CFG), function(tk) compute_returns_for_index(raw_data[[tk]]))
names(all_returns) <- names(INDEX_CFG)

# ── 2b. Re-load any indices imported in a previous run ──────────────────────
# In local mode, the "Import Index" sidebar control (Section 3c / server) writes
# each new index's data to DATA_DIR and appends one row to REGISTRY_PATH. On startup,
# replay that registry so imported indices survive an app restart — exactly
# like the 7 built-ins above, just driven by a small CSV instead of code.
# Any row that fails to load (missing file, corrupted registry edit, etc.)
# is skipped with a warning rather than blocking the whole app from starting.
if (file.exists(REGISTRY_PATH)) {
  message("Loading previously-imported indices...")
  registry <- read.csv(REGISTRY_PATH, stringsAsFactors=FALSE)
  for (i in seq_len(nrow(registry))) {
    row <- registry[i, ]
    tryCatch({
      path <- file.path(DATA_DIR, row$filename)
      df   <- read.csv(path, stringsAsFactors=FALSE)
      df$date  <- as.Date(df$date)
      df$value <- as.numeric(df$value)
      df <- df[!is.na(df$date) & !is.na(df$value), ]
      df <- df[order(df$date), ]

      INDEX_CFG[[row$ticker]] <- list(
        label     = as.character(row$label),
        color     = as.character(row$color),
        obs_years = as.numeric(row$obs_years),
        default_n = as.integer(row$default_n))
      raw_data[[row$ticker]]    <- df
      all_returns[[row$ticker]] <- compute_returns_for_index(df)
    }, error=function(e) {
      message(sprintf("  Skipped imported index \"%s\" (%s): %s",
                       row$ticker, row$filename, conditionMessage(e)))
    })
  }
}
message("Ready.")

# ══════════════════════════════════════════════════════════════════════════════
# 3. HELPER FUNCTIONS
# ══════════════════════════════════════════════════════════════════════════════

fmt_pct  <- function(x, digits=2) {
  ifelse(is.na(x), "\u2014", sprintf(paste0("%+.", digits, "f%%"), x * 100))
}
fmt_date <- function(x) format(as.Date(x), "%Y-%m-%d")

# Series length in years
series_years <- function(df) {
  if (nrow(df) < 2) return(0)
  as.numeric(difftime(max(df$date), min(df$date), units="days")) / 365.25
}

# Percentile / VaR statistics
compute_stats <- function(rets) {
  r <- rets[!is.na(rets)]
  n <- length(r)
  if (n == 0) return(NULL)

  # 0.1% and 0.05% tail quantiles are only computed when enough observations
  # are available to produce a stable estimate. Thresholds follow the simple
  # rule "need at least 1/p observations": 0.1% (p=0.001) → ≥1000 obs;
  # 0.05% (p=0.0005) → ≥2000 obs. In practice the quantile estimate will
  # still be noisy at exactly the threshold, but the user can judge relevance
  # from the observation count shown at the top of the table.
  add <- function(rows, label, value) c(rows, list(data.frame(Statistic=label, Value=value, stringsAsFactors=FALSE)))

  rows <- list(data.frame(Statistic="Observations (M-N)", Value=as.numeric(n), stringsAsFactors=FALSE))
  if (n >= 2000) rows <- add(rows, "Worst 0.05%", quantile(r, 0.0005, names=FALSE))
  if (n >= 1000) rows <- add(rows, "Worst 0.1%",  quantile(r, 0.001,  names=FALSE))
  rows <- add(rows, "Worst 1%",  quantile(r, 0.01, names=FALSE))
  rows <- add(rows, "Worst 5%",  quantile(r, 0.05, names=FALSE))
  rows <- add(rows, "Median",    median(r))
  rows <- add(rows, "Mean",      mean(r))
  rows <- add(rows, "Std Dev",   sd(r))
  rows <- add(rows, "Best 5%",   quantile(r, 0.95, names=FALSE))
  rows <- add(rows, "Best 1%",   quantile(r, 0.99, names=FALSE))
  if (n >= 1000) rows <- add(rows, "Best 0.1%",  quantile(r, 0.999,  names=FALSE))
  if (n >= 2000) rows <- add(rows, "Best 0.05%", quantile(r, 0.9995, names=FALSE))
  rows <- add(rows, "Best (Max)", max(r))
  do.call(rbind, rows)
}

# Top-k worst and best episodes
compute_episodes <- function(ret_df, k=5) {
  r <- ret_df[!is.na(ret_df$ret), ]
  if (nrow(r) == 0) return(NULL)

  make_ep <- function(rows, type) {
    if (nrow(rows) == 0) return(NULL)
    data.frame(
      Rank   = seq_len(nrow(rows)),
      Type   = type,
      Return = rows$ret,
      Start  = fmt_date(rows$start),
      End    = fmt_date(rows$date),
      StartDate = as.Date(rows$start),
      EndDate   = as.Date(rows$date),
      stringsAsFactors = FALSE
    )
  }
  rbind(
    make_ep(head(r[order(r$ret),  ], k), "Worst"),
    make_ep(head(r[order(-r$ret), ], k), "Best")
  )
}

# Anchored rolling-window worst/best.
# Each anchor's "Coverage" = effective years of data actually available for
# its lookback window (may be < obs_years if the index history doesn't
# extend far enough back). "Complete" = TRUE when Coverage >= obs_years
# (within a small tolerance for date-arithmetic rounding).
#
# For each anchor, the START and END of the specific N-day window that
# produced the worst/best return are read straight from that return's row
# (its `start` and `date` columns — see compute_returns_for_index).
compute_rolling_windows <- function(ret_df, obs_years, n_anchors=40,
                                     coverage_tol=0.98) {
  r <- ret_df[!is.na(ret_df$ret), ]
  if (nrow(r) < 5) return(NULL)
  last_date <- max(r$date)
  min_date  <- min(r$date)

  anchors   <- seq.Date(from=last_date, by="-1 year", length.out=n_anchors)
  span_days <- round(obs_years * 365.25)

  results <- lapply(anchors, function(anchor) {
    win_start    <- anchor - span_days
    actual_start <- max(win_start, min_date)
    sub <- r[r$date >= win_start & r$date <= anchor, ]   # full sub-dataframe
    if (nrow(sub) < 2) return(NULL)
    coverage <- as.numeric(difftime(anchor, actual_start, units="days")) / 365.25

    wi <- which.min(sub$ret)   # row index of worst return within this window
    bi <- which.max(sub$ret)   # row index of best  return within this window

    data.frame(
      Anchor     = anchor,
      Worst      = sub$ret[wi],
      WorstStart = as.Date(sub$start[wi]),   # first day of that N-day window
      WorstEnd   = as.Date(sub$date[wi]),    # last day of that window (= return date)
      Best       = sub$ret[bi],
      BestStart  = as.Date(sub$start[bi]),
      BestEnd    = as.Date(sub$date[bi]),
      Coverage   = coverage,
      Complete   = (coverage >= obs_years * coverage_tol),
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, Filter(Negate(is.null), results))
}

# Rank-th worst/best across all N values (term structure) for ONE ticker.
# `tk_returns` is that ticker's all_returns[[tk]] entry.
# If min_date is given, only returns whose END date >= min_date are considered
# (used by the "Limit to most recent years" data-window filter).
compute_term_structure <- function(tk, tk_returns, ranks=1L, min_date=NULL) {
  # `ranks` may be a scalar or integer vector (e.g. 1:5 for "top 5 ranks").
  # The output always contains a `Rank` column so chart/table code can
  # distinguish series when multiple ranks are requested.
  rows <- lapply(N_VALUES, function(n) {
    ret_df <- tk_returns[[as.character(n)]]
    r      <- ret_df[!is.na(ret_df$ret), ]
    if (!is.null(min_date)) r <- r[r$date >= min_date, ]
    if (nrow(r) < 1L) return(NULL)

    asc  <- r[order(r$ret), ]
    desc <- r[order(-r$ret), ]

    lapply(ranks, function(rk) {
      if (nrow(r) < rk) return(NULL)
      data.frame(
        Ticker        = tk,
        Rank          = rk,
        `N (days)`    = n,
        Worst         = asc$ret[rk],
        `Worst Start` = fmt_date(asc$start[rk]),
        `Worst End`   = fmt_date(asc$date[rk]),
        Best          = desc$ret[rk],
        `Best End`    = fmt_date(desc$date[rk]),
        stringsAsFactors = FALSE, check.names = FALSE
      )
    })
  })
  do.call(rbind, Filter(Negate(is.null), unlist(rows, recursive=FALSE)))
}

# Cutoff Date for the "limit to most recent X years" data-window filter,
# relative to the most recent of `dates` (one ticker's raw_data[[tk]]$date).
# Returns NULL if the filter is inactive / invalid (i.e., use the full history).
years_cutoff <- function(dates, limit_active, limit_years) {
  if (!isTRUE(limit_active)) return(NULL)
  if (is.null(limit_years) || is.na(limit_years) || limit_years < 1) return(NULL)
  max(dates) - round(limit_years * 365.25)
}

# Term structure for MULTIPLE tickers, combined into one long data.frame.
# `data` and `returns` are the raw_data / all_returns in effect for the caller.
# Each ticker's "last `limit_years` years" cutoff is relative to ITS OWN most
# recent date (so e.g. DJI, which ends in 2023, and SPX, which ends in 2026,
# each get their own most-recent N-year slice).
compute_term_structure_multi <- function(tickers, data, returns, ranks=1L,
                                         limit_active=FALSE, limit_years=NULL) {
  dfs <- lapply(tickers, function(tk) {
    cd <- years_cutoff(data[[tk]]$date, limit_active, limit_years)
    compute_term_structure(tk, returns[[tk]], ranks=ranks, min_date=cd)
  })
  do.call(rbind, Filter(Negate(is.null), dfs))
}

# Build a small, styled HTML table — replaces DT::datatable for this app.
#
#   df            : data.frame of already-formatted display strings. Cells
#                    may contain raw HTML (e.g. "<span style=...>Worst</span>")
#                    — it is rendered as-is, so callers must escape any text
#                    a user supplied (imported index names come from a
#                    text box or CSV headers) with htmltools::htmlEscape().
#   numeric_cols  : column names to right-align in monospace/bold.
#   right_cols    : column names to right-align only (no mono/bold) — e.g.
#                    a "10.0 / 10 yr" coverage column.
#   row_style     : optional function(i) -> named list of CSS declarations
#                    for <tr> i, e.g. list(`background-color`="#FEF2F2").
#   height        : if set (e.g. "180px"), wraps the table in a scrollable
#                    div with a sticky header (replaces DT's scrollY).
#   hover/striped : zebra/hover row styling.
#   sort_input_id : if set, column headers become clickable buttons that call
#                    Shiny.setInputValue(sort_input_id, <column name>) —
#                    used for the one sortable table (Term Structure).
#   sort_col/dir  : currently-active sort column/direction, for the arrow
#                    indicator on the active header.
html_table <- function(df, numeric_cols=character(0), right_cols=character(0),
                        row_style=NULL, height=NULL, hover=TRUE, striped=FALSE,
                        sort_input_id=NULL, sort_col=NULL, sort_dir=NULL) {
  cols <- names(df)

  cell_class <- function(col) {
    if (col %in% numeric_cols) "num" else if (col %in% right_cols) "right" else NULL
  }

  header_cells <- lapply(cols, function(col) {
    cls <- cell_class(col)
    if (!is.null(sort_input_id)) {
      arrow <- if (!is.null(sort_col) && identical(sort_col, col)) {
        if (identical(sort_dir, "asc")) " \u25B2" else " \u25BC"
      } else ""
      tags$th(class=cls,
        tags$button(class="r-sort-btn", type="button",
          onclick=sprintf("Shiny.setInputValue('%s', '%s', {priority:'event'})",
                          sort_input_id, col),
          paste0(col, arrow))
      )
    } else {
      tags$th(class=cls, col)
    }
  })

  body_rows <- lapply(seq_len(nrow(df)), function(i) {
    style <- if (!is.null(row_style)) row_style(i) else NULL
    style_str <- if (!is.null(style) && length(style) > 0)
      paste(sprintf("%s:%s", names(style), unlist(style)), collapse=";")
    else NULL
    cells <- lapply(cols, function(col) {
      tags$td(class=cell_class(col), HTML(as.character(df[[col]][i])))
    })
    tags$tr(style=style_str, cells)
  })

  table_cls <- paste(c("r-table",
                        if (hover) "r-table-hover",
                        if (striped) "r-table-striped"), collapse=" ")
  tbl <- tags$table(class=table_cls,
    tags$thead(tags$tr(header_cells)),
    tags$tbody(body_rows)
  )

  if (!is.null(height)) {
    div(class="r-table-scroll", style=sprintf("max-height:%s", height), tbl)
  } else {
    tbl
  }
}

# ══════════════════════════════════════════════════════════════════════════════
# 3b. IMPORTING ADDITIONAL INDICES — helpers for the "Import Index" sidebar
#     control (server-side logic is in Section 5). These are pure functions
#     (no reactivity), so they're easy to test/reuse:
#     • slugify_ticker()         — filename/label → short uppercase ticker ID
#     • derive_default_label()   — filename → readable display name
#     • validate_csv_for_import()— checks CSV structure (single or multi-index
#                                  format) and returns a clear reason if it fails
# ══════════════════════════════════════════════════════════════════════════════

# Turns free text into a short, uppercase, alnum-only identifier suitable as
# a ticker-like key in INDEX_CFG (e.g. "FTSE 100 Index" -> "FTSE100INDEX",
# truncated to 12 chars). If the result collides with an existing name (a
# built-in or an earlier import), a numeric suffix is appended until unique.
slugify_ticker <- function(text, existing) {
  id <- toupper(gsub("[^A-Za-z0-9]", "", text))
  if (id == "") id <- "INDEX"
  id <- substr(id, 1, 12)
  if (!(id %in% existing)) return(id)
  for (n in 2:99) {
    candidate <- paste0(substr(id, 1, 12 - nchar(as.character(n))), n)
    if (!(candidate %in% existing)) return(candidate)
  }
  paste0(id, sample.int(9999, 1))   # extremely unlikely fallback
}

# Turns a filename into a readable default display name, e.g.
# "ftse_100_daily.csv" -> "ftse 100" (the user edits this in a text input
# before confirming, so it only needs to be a reasonable starting point).
derive_default_label <- function(filename) {
  name <- tools::file_path_sans_ext(filename)
  name <- gsub("(?i)[_-]?daily$", "", name, perl=TRUE)
  name <- gsub("[_\\.-]+", " ", name)
  name <- trimws(gsub("\\s+", " ", name))
  # Guard against filenames that were entirely extension (e.g. ".csv" -> "")
  # or that collapse to only punctuation (e.g. "---" -> "")
  if (nchar(name) == 0 || !grepl("[A-Za-z0-9]", name)) name <- "Imported Index"
  name
}

# ── CSV validation helpers ────────────────────────────────────────────────────
#
# validate_csv_for_import() is the single entry-point for all CSV uploads.
# It automatically detects which of two file shapes was provided:
#
#   SINGLE-INDEX (original format):
#     date,value           ← exactly one value column (any recognised alias)
#     2024-01-02,100.5       OR exactly two columns total (permissive fallback)
#
#   MULTI-INDEX (new format):
#     date,FTSE 100,Nikkei 225,SMI    ← date col + 2+ price columns;
#     2024-01-02,7500.5,38000.1,      column headers = display labels;
#     2024-01-03,,38100.2,5600.0      blank cell = missing value for that
#     2024-01-07,7510.3,38050.8,5605  index on that date (silently dropped
#                                     from that index's series only).
#
# Missing observation DAYS (weekends, holidays) are handled naturally — they
# are simply absent from the CSV and need no special treatment; each index's
# series is built only from the rows where that column has a valid value.
#
# Return values:
#   list(ok=FALSE, message=<specific reason>)
#   list(ok=TRUE, mode="single",  df=data.frame(date,value), n=<rows>)
#   list(ok=TRUE, mode="multi",   indices=list(list(name,df,n,warn)), skipped=list(...))
#
validate_csv_for_import <- function(path) {
  raw <- tryCatch(read.csv(path, stringsAsFactors=FALSE, check.names=FALSE,
                            na.strings=c("","NA","N/A","#N/A","null","NULL")),
                   error=function(e) NULL)
  if (is.null(raw) || ncol(raw) < 2) {
    return(list(ok=FALSE, message=
      "Couldn't read this as a CSV with at least two columns. Make sure it's a plain comma-separated file."))
  }

  # ── Find the date column ────────────────────────────────────────────────────
  lower <- tolower(trimws(names(raw)))
  date_aliases <- c("date", "dates", "time", "timestamp")
  date_col_pos <- match(TRUE, lower %in% date_aliases)
  if (is.na(date_col_pos)) {
    return(list(ok=FALSE, message=
      "No date column found. The first (or only) column named \"date\" (or \"dates\"/\"time\"/\"timestamp\") is required."))
  }
  date_col  <- names(raw)[date_col_pos]
  value_cols <- names(raw)[-date_col_pos]   # everything else is a potential price column

  # Parse all dates once (shared across all indices)
  dates <- as.Date(raw[[date_col]],
                    tryFormats=c("%Y-%m-%d", "%m/%d/%Y", "%d/%m/%Y", "%d.%m.%Y",
                                 "%d-%m-%Y", "%Y/%m/%d", "%b %d, %Y"))
  valid_date_mask <- !is.na(dates)
  if (sum(valid_date_mask) < MIN_IMPORT_ROWS) {
    return(list(ok=FALSE, message=sprintf(
      "Only %d row(s) had a parseable date (need at least %d). Accepted formats: YYYY-MM-DD, DD/MM/YYYY, MM/DD/YYYY, DD.MM.YYYY.",
      sum(valid_date_mask), MIN_IMPORT_ROWS)))
  }

  # ── Determine mode: single vs multi ────────────────────────────────────────
  value_aliases <- c("value", "price", "close", "level", "px", "adj close", "adjclose",
                     "close*", "adj. close", "last", "settle")
  lower_vcols <- tolower(trimws(value_cols))
  known_alias_hits <- sum(lower_vcols %in% value_aliases)

  is_single <- (length(value_cols) == 1) ||
               (length(value_cols) == 2 && known_alias_hits == 1) ||
               (known_alias_hits == 1 && length(value_cols) >= 2)

  # 2-column file with no recognisable alias is also treated as single
  if (length(value_cols) == 1) is_single <- TRUE

  # ── Single-index path ───────────────────────────────────────────────────────
  if (is_single) {
    vcol <- if (known_alias_hits >= 1) {
      value_cols[match(TRUE, lower_vcols %in% value_aliases)]
    } else {
      value_cols[1]
    }
    values <- suppressWarnings(as.numeric(gsub(",", "", raw[[vcol]])))
    keep <- valid_date_mask & !is.na(values) & is.finite(values)
    if (sum(keep) < MIN_IMPORT_ROWS) {
      return(list(ok=FALSE, message=sprintf(
        "Only %d row(s) had both a valid date and a valid number (need at least %d). Check the date format and that the value column is numeric.",
        sum(keep), MIN_IMPORT_ROWS)))
    }
    if (any(values[keep] <= 0)) {
      return(list(ok=FALSE, message=
        "Found zero or negative values. Price/level series must be strictly positive (the chart uses a log scale)."))
    }
    df <- data.frame(date=dates[keep], value=values[keep])
    df <- df[order(df$date), ]
    df <- df[!duplicated(df$date, fromLast=TRUE), ]
    return(list(ok=TRUE, mode="single", df=df, n=nrow(df)))
  }

  # ── Multi-index path ────────────────────────────────────────────────────────
  # Parse every value column independently; each uses only the rows where
  # BOTH the date AND its own value are valid. Missing values (blank cells,
  # NA) and missing days (absent rows) are simply excluded from that index's
  # series — no imputation, no error.
  indices <- list()
  skipped <- list()

  for (vcol in value_cols) {
    raw_vals <- raw[[vcol]]   # may be character if some cells are blank
    values   <- suppressWarnings(as.numeric(gsub(",", "", as.character(raw_vals))))
    keep     <- valid_date_mask & !is.na(values) & is.finite(values)
    n_valid  <- sum(keep)

    if (n_valid < MIN_IMPORT_ROWS) {
      skipped[[length(skipped)+1]] <- list(
        name=vcol,
        reason=sprintf("only %d valid rows (need \u2265%d)", n_valid, MIN_IMPORT_ROWS))
      next
    }

    neg_count <- sum(values[keep] <= 0)
    if (neg_count > 0) {
      skipped[[length(skipped)+1]] <- list(
        name=vcol,
        reason=sprintf("%d zero/negative value(s) — price series must be strictly positive (log-scale chart)", neg_count))
      next
    }

    df <- data.frame(date=dates[keep], value=values[keep])
    df <- df[order(df$date), ]
    df <- df[!duplicated(df$date, fromLast=TRUE), ]   # last observation wins for same-day dupes

    # Count how many rows had a date but a blank value (informational only)
    n_date_no_val <- sum(valid_date_mask & (is.na(values) | !is.finite(values)))

    indices[[length(indices)+1]] <- list(
      name    = vcol,           # original column header = default display label
      df      = df,
      n       = nrow(df),
      n_gaps  = n_date_no_val  # dates present in the file but no value for this column
    )
  }

  if (length(indices) == 0) {
    skip_summary <- if (length(skipped) > 0) {
      paste(sapply(skipped, function(s) sprintf("\u201c%s\u201d: %s", s$name, s$reason)), collapse="; ")
    } else ""
    return(list(ok=FALSE, message=sprintf(
      "No usable price columns found (each needs \u2265%d valid rows with positive values)%s",
      MIN_IMPORT_ROWS, if (nchar(skip_summary) > 0) paste0(": ", skip_summary) else ".")))
  }

  list(ok=TRUE, mode="multi", indices=indices, skipped=skipped)
}

# Keep a thin alias so any future caller that still uses the old name works.
validate_index_csv <- function(path) validate_csv_for_import(path)


# Small helpers around REGISTRY_PATH, shared by the Add/Replace/Delete Index
# server logic. The registry only ever tracks *imported* indices — built-ins
# are loaded directly from their hardcoded data/TICKER_daily.csv path (see
# Section 2), so replacing a built-in's data just overwrites that file in
# place and needs no registry entry at all.
read_registry <- function() {
  if (!file.exists(REGISTRY_PATH)) {
    return(data.frame(ticker=character(0), label=character(0), color=character(0),
                       obs_years=numeric(0), default_n=integer(0), filename=character(0),
                       stringsAsFactors=FALSE))
  }
  read.csv(REGISTRY_PATH, stringsAsFactors=FALSE)
}
write_registry <- function(df) {
  if (nrow(df) == 0 && file.exists(REGISTRY_PATH)) {
    file.remove(REGISTRY_PATH)
  } else if (nrow(df) > 0) {
    write.csv(df, REGISTRY_PATH, row.names=FALSE)
  }
}

# ══════════════════════════════════════════════════════════════════════════════
# 3c. STATIC ILLUSTRATIVE DIAGRAM (Definitions tab — Rolling-Window Coverage)
#     Drawn with base graphics; shown via plotOutput("coverage_diagram").
#     Based on the real EUR/CHF case: data starts ~1999-01-19, Observation
#     Period = 10 years, anchors at 2002-05-29 (partial) and 2020-05-29 (full).
# ══════════════════════════════════════════════════════════════════════════════

# Each row = one horizontal bar (date range) with a caption drawn above it.
COVERAGE_ROWS <- list(
  list(y=3, x0="1999-01-19", x1="2026-06-01", col="#60A5FA", alpha=0.45, lty=1,
       label="EUR/CHF data available (1999 \u2192 today)"),
  list(y=2, x0="1992-05-29", x1="2002-05-29", col="#94A3B8", alpha=0.10, lty=2,
       label="Anchor 2002-05-29 \u2014 requested window (10.0 yr)"),
  list(y=1, x0="1999-01-19", x1="2002-05-29", col="#DC2626", alpha=0.45, lty=1,
       label="Anchor 2002-05-29 \u2014 actual window used (\u22483.4 yr) \u2014 PARTIAL"),
  list(y=0, x0="2010-05-30", x1="2020-05-29", col="#16A34A", alpha=0.45, lty=1,
       label="Anchor 2020-05-29 \u2014 requested = actual (10.0 yr) \u2014 FULL")
)
# Vertical guide lines: data start + the two example anchors.
# "Data starts" has no text label (it would collide with "Anchor A" only
# 3 years later) — the line itself plus the top bar's caption already
# convey when EUR/CHF data begins.
COVERAGE_GUIDES <- list(
  list(x="1999-01-19", col="#94A3B8", label=NULL,             adj=c(0,1)),
  list(x="2002-05-29", col="#475569", label="Anchor A (2002)", adj=c(0.5,1)),
  list(x="2020-05-29", col="#475569", label="Anchor B (2020)", adj=c(0.5,1))
)

# Draws the diagram into the current graphics device (called from renderPlot)
draw_coverage_diagram <- function() {
  xr <- as.numeric(as.Date(c("1990-01-01","2027-06-01")))

  par(mar=c(2.2, 0.5, 0.5, 0.5), family="sans")
  plot(NA, xlim=xr, ylim=c(-0.7, 4.7), axes=FALSE, xlab="", ylab="")

  for (r in COVERAGE_ROWS) {
    rect(as.numeric(as.Date(r$x0)), r$y-0.32, as.numeric(as.Date(r$x1)), r$y+0.32,
         col=adjustcolor(r$col, alpha.f=r$alpha), border=r$col,
         lty=r$lty, lwd=if (r$lty==2) 1.5 else 1)
    text(xr[1], r$y+0.40, r$label, adj=c(0,0), cex=0.8, col=r$col, xpd=TRUE)
  }

  axis.Date(1, at=seq(as.Date("1990-01-01"), as.Date("2027-01-01"), by="5 years"),
            format="%Y", col="#CBD5E1", col.axis="#64748B", cex.axis=0.85)

  for (g in COVERAGE_GUIDES) {
    abline(v=as.numeric(as.Date(g$x)), col=g$col, lty=3)
    if (!is.null(g$label))
      text(as.numeric(as.Date(g$x)), 4.6, g$label, col=g$col, cex=0.8, adj=g$adj, xpd=TRUE)
  }
}


# ══════════════════════════════════════════════════════════════════════════════
# 4. UI
# ══════════════════════════════════════════════════════════════════════════════

APP_THEME <- bs_theme(
  version        = 5,
  bg             = "#F8FAFC",
  fg             = "#1E293B",
  primary        = "#1F4E78",
  secondary      = "#64748B",
  success        = "#16A34A",
  danger         = "#DC2626",
  "font-size-base" = "0.92rem",
  "border-radius"  = "0.5rem"
)

# Builds the {ticker: "Label  (TICKER)"} choices vector used by both index
# selectors (sidebar "Select Index" and Term Structure "Indices to Compare").
# A function rather than a one-off value so it can be recomputed after a
# new index is imported (see Section 3c) and pushed to both selectors via
# updateSelectInput()/updateSelectizeInput().
current_index_choices <- function(cfg) {
  setNames(
    names(cfg),
    sapply(names(cfg), function(k)
      paste0(cfg[[k]]$label, "  (", k, ")")
    )
  )
}

n_choices <- setNames(
  N_VALUES,
  paste0(N_VALUES, " day", ifelse(N_VALUES == 1L, "", "s"))
)

# CSS — sidebar control states, table styling, import preview, compactness
APP_CSS <- "
.ctrl-disabled {
  opacity: 0.38;
  pointer-events: none;
  transition: opacity 0.15s ease;
}
.ctrl-disabled .form-control, .ctrl-disabled .selectize-input {
  background-color: #F1F5F9 !important;
}
.ctrl-note {
  font-size: 0.72rem;
  color: #94A3B8;
  font-style: italic;
  margin-top: -4px;
  margin-bottom: 6px;
}
/* Import/Manage Indices disclosure: match the look of the other sidebar
   section labels, with a default-state marker the browser draws for free */
.sidebar details summary { outline: none; }
.sidebar details summary::-webkit-details-marker { color: #94A3B8; }
.sidebar details[open] summary { margin-bottom: 4px; }
/* bslib's default selectize dropdown max-height is 200px (~7 items).
   With 9 window-length options at ~29px each, this clips the last two.
   280px fits all 9 cleanly with a small margin. */
.selectize-dropdown-content { max-height: 280px !important; }
/* bslib's default flex gap between sidebar elements (~7px, repeated across
   ~18 gaps) adds up to real space on a control-dense sidebar; tightened
   without touching the hr margins, which already carry visual separation */
.sidebar-content.bslib-gap-spacing { gap: 0.25rem !important; }
/* The sidebar only ever needs to scroll vertically. Forcing overflow-x:
   hidden and touch-action: pan-y removes any ambiguity that a trackpad's
   two-finger scroll (which rarely tracks perfectly vertical) could be
   read by the browser as a horizontal pan or pinch-zoom gesture instead
   of a plain scroll — which is what makes a panel look like it's being
   'resized' rather than scrolled.
   Desktop/tablet only (bslib's sidebar breakpoint): on phones the sidebar
   is part of the page, and `overscroll-behavior: contain` there can stop a
   swipe that starts on the menu from scrolling the page (see Phones below). */
@media (min-width: 576px) {
  .sidebar {
    overflow-x: hidden !important;
    overflow-y: auto !important;
    touch-action: pan-y !important;
    overscroll-behavior: contain;
  }
}
/* Phones (below bslib's 576px breakpoint): bslib stacks the menu and the
   cards into one long page. Fixed-height cards and tables with their own
   scroll areas would catch the finger mid-swipe and scroll themselves
   instead of the page, so every card grows to fit its content and only the
   page scrolls vertically. Wide tables still scroll sideways in their card. */
@media (max-width: 575.98px) {
  .bslib-card { height: auto !important; max-height: none !important; }
  .bslib-card .card-body {
    max-height: none !important;
    overflow-x: auto !important;
    overflow-y: visible !important;
  }
  .plot-card .shiny-plot-output { height: 300px !important; }
  .r-table-scroll { max-height: none !important; overflow-y: visible !important; }
}
.plot-card .html-widget {
  height: 100% !important;
}
.plot-card, .plot-card .card-body {
  transition: none !important;
}
.legend-pill {
  display:inline-block; padding:1px 8px; border-radius:999px;
  font-size:.72rem; font-weight:600; color:#fff; margin-right:6px;
}
/* Per-index date-range list (Term Structure 'Indices to Compare' card) */
.idx-range-row {
  display:flex; flex-wrap:wrap; gap:4px 14px; margin-top:6px;
}
.idx-range-item { display:flex; align-items:center; gap:5px; font-size:.74rem; color:#475569; }
.idx-range-dot { width:8px; height:8px; border-radius:50%; flex-shrink:0; }

/* Multi-index import preview list */
.multi-import-list {
  margin:6px 0 4px; border:1px solid #E2E8F0; border-radius:6px; overflow:hidden;
}
.multi-import-row {
  padding:4px 8px; border-bottom:1px solid #F1F5F9; font-size:.76rem; color:#334155;
}
.multi-import-row:last-child { border-bottom:none; }
.multi-import-name { font-weight:600; }
.multi-import-ticker { color:#64748B; font-size:.70rem; }
.multi-import-info { color:#94A3B8; font-size:.70rem; }

/* ── Compactness pass: tighter chrome/spacing without shrinking text ──────── */
.navbar { padding-top: 0.2rem !important; padding-bottom: 0.2rem !important; min-height: unset !important; }
.navbar-brand { font-size: 1.05rem !important; padding: 0 !important; }
.nav-tabs { margin-bottom: 0.4rem !important; }
.nav-tabs .nav-link {
  padding: 0.25rem 0.75rem !important;
}
.card { margin-bottom: 0 !important; }
.card-header {
  padding: 0.3rem 0.7rem !important;
  font-size: 0.85rem !important;
  font-weight: 600;
}
.card-body { padding: 0.5rem; }
hr.my-3 { margin-top: 0.35rem !important; margin-bottom: 0.35rem !important; }
.shiny-input-container { margin-bottom: 0.3rem; }
.bslib-gap-spacing, .html-fill-container.bslib-gap-spacing {
  gap: 0.45rem !important;
}
/* Custom HTML tables (replacing DT) — compact rows, readable text */
.r-table { width:100%; border-collapse:collapse; font-size:0.82rem; }
.r-table th, .r-table td { padding:0.22rem 0.5rem; text-align:left;
  border-bottom:1px solid #E2E8F0; white-space:nowrap; }
.r-table thead th { font-weight:600; border-bottom:2px solid #CBD5E1;
  background:#FFFFFF; }
.r-table td.num, .r-table th.num { text-align:right; font-family:monospace;
  font-weight:600; }
.r-table td.right, .r-table th.right { text-align:right; }
.r-table-hover tbody tr:hover { background:#F8FAFC; }
.r-table-striped tbody tr:nth-child(odd) td { background:#F8FAFC; }
.r-table-scroll { overflow-y:auto; }
.r-table-scroll thead th { position:sticky; top:0; z-index:1; }
.r-sort-btn {
  background:none; border:none; padding:0; margin:0; font:inherit;
  font-weight:600; cursor:pointer; color:inherit;
}
.r-sort-btn:hover { color:#2563EB; }
/* Manage Indices: small delete-row list under Import Index */
.idx-manage-row {
  display:flex; justify-content:space-between; align-items:center;
  padding:3px 0; font-size:.78rem;
}
.idx-del-btn {
  background:none; border:none; padding:0 4px; margin:0; cursor:pointer;
  color:#94A3B8; font-size:.85rem;
}
.idx-del-btn:hover { color:#DC2626; }
"

# Tiny vanilla-JS replacement for shinyjs::disable()/enable()/addClass()/
# removeClass(). A single custom message handler that either toggles a CSS
# class on an element (used to gray out whole control groups via
# .ctrl-disabled), or toggles its `disabled` property (used for the
# "Limit to most recent years" numeric input). Triggered from the server via
# session$sendCustomMessage("uiToggle", list(id=..., cls=..., state=...)).
TOGGLE_JS <- "
Shiny.addCustomMessageHandler('uiToggle', function(msg) {
  var el = document.getElementById(msg.id);
  if (!el) return;
  if (msg.cls)                  el.classList.toggle(msg.cls, msg.state);
  if (msg.attr === 'disabled')  el.disabled = msg.state;
});

// Chrome/Firefox both let a FOCUSED <input type=number> capture the mouse
// wheel to bump its value up/down instead of letting the scroll reach the
// sidebar underneath it. With several numeric fields stacked in a scrolling
// sidebar (Data Window years, Observation Period, Rank), this makes the
// sidebar feel 'stuck' the moment the wheel happens to pass over whichever
// field last had focus. Blurring the field the instant a wheel event starts
// hands the scroll straight back to the page, so number inputs only change
// via click+type or the spinner arrows, never via an incidental scroll.
document.addEventListener('wheel', function(e) {
  var el = document.activeElement;
  if (el && el.tagName === 'INPUT' && el.type === 'number') el.blur();
}, { passive: true, capture: true });
"

ui <- function(request) {
page_sidebar(
  title        = strong("Financial Crises Analyzer"),
  theme        = APP_THEME,
  window_title = "Financial Crises Analyzer",

  tags$script(HTML(TOGGLE_JS)),
  tags$head(tags$style(HTML(APP_CSS))),

  # ── Sidebar ────────────────────────────────────────────────────────────────
  sidebar = sidebar(
    width = 290,
    bg    = "#FFFFFF",

    # -- Select Index (used by: Analysis, Rolling Windows) -------------------
    div(id = "ctrl_ticker",
      tags$p(class="text-uppercase fw-semibold text-muted mb-1",
             style="font-size:.7rem; letter-spacing:.06em", "Select Index"),
      selectizeInput("ticker", NULL, choices=current_index_choices(INDEX_CFG),
                    selected="SPX", width="100%",
                    options=list(dropdownParent="body")),
      uiOutput("index_pill")
    ),
    div(class="ctrl-note", id="note_ticker",
        "Used by: Analysis, Rolling Windows"),

    hr(class="my-2"),

    # -- Data Window (applies globally, to every tab) -------------------------
    div(id = "ctrl_window",
      tags$p(class="text-uppercase fw-semibold text-muted mb-1",
             style="font-size:.7rem; letter-spacing:.06em", "Data Window"),
      checkboxInput("limit_active", "Limit to most recent years", value=FALSE, width="100%"),
      numericInput("limit_years", NULL, value=10, min=1, max=150, step=1, width="100%")
    ),
    div(class="ctrl-note",
        "Applies to all tabs. On Term Structure, each index's window is relative to its own most recent date."),

    hr(class="my-2"),

    # -- Window Length N (used by: Analysis, Rolling Windows) -----------------
    div(id = "ctrl_n",
      tags$p(class="text-uppercase fw-semibold text-muted mb-1",
             style="font-size:.7rem; letter-spacing:.06em", "Window Length (N)"),
      selectizeInput("n_val", NULL, choices=n_choices, selected=20L, width="100%",
                     options=list(dropdownParent="body"))
    ),
    div(class="ctrl-note", id="note_n",
        "Used by: Analysis, Rolling Windows"),

    hr(class="my-2"),

    # -- Observation Period (used by: Rolling Windows only) -------------------
    div(id = "ctrl_obs",
      tags$p(class="text-uppercase fw-semibold text-muted mb-1",
             style="font-size:.7rem; letter-spacing:.06em", "Rolling Windows"),
      numericInput("obs_years", "Observation Period (yrs)",
                   value=10, min=1, max=50, step=1, width="100%"),
      radioButtons("rw_mode", "Anchor Windows",
                   choices = c("Full windows only" = "full",
                                "Show all (flag partial)" = "all"),
                   selected = "full", width="100%")
    ),
    div(class="ctrl-note", id="note_obs",
        "Used by: Rolling Windows only"),

    hr(class="my-2"),

    # -- Rank (used by: Term Structure only) -----------------------------------
    div(id = "ctrl_rank",
      tags$p(class="text-uppercase fw-semibold text-muted mb-1",
             style="font-size:.7rem; letter-spacing:.06em", "Term Structure"),
      radioButtons("rank_mode", NULL,
                   choices = c("Single rank" = "one", "Ranks 1 to N" = "range"),
                   selected = "one", inline = TRUE, width = "100%"),
      numericInput("rank_k", "N  (1 = most extreme)",
                   value=1, min=1, max=20, step=1, width="100%")
    ),
    div(class="ctrl-note", id="note_rank",
        "Used by: Term Structure only"),

    hr(class="my-2"),
    uiOutput("data_info"),

    hr(class="my-2"),

    # -- Import / Manage Indices (collapsed by default — a less-frequent
    #    action than the controls above, and keeping it closed by default
    #    is what keeps the rest of the sidebar visible without scrolling).
    #    Native <details>/<summary> needs no JS: keyboard/screen-reader
    #    accessible out of the box, and click-to-toggle for free.
    tags$details(
      tags$summary(class="text-uppercase fw-semibold text-muted",
             style="font-size:.7rem; letter-spacing:.06em; cursor:pointer",
             "Import / Manage Indices"),
      div(style="margin-top:8px",
        # On hosted platforms (Connect Cloud / shinyapps.io) imports live only
        # in this browser session's memory (see IS_HOSTED in Section 1).
        if (IS_HOSTED)
          div(class="alert alert-warning p-2 mb-2",
              style="font-size:.72rem; line-height:1.35",
              tags$b("\u26A0\uFE0F Hosted mode:"),
              " imports are private to this browser session \u2014 other visitors",
              " never see them. They are not saved, and are lost when the page is",
              " refreshed or the session times out.",
              " The 7 built-in indices are always available.")
        else NULL,
        fileInput("import_file", NULL, accept=".csv",
                  buttonLabel="Browse...", placeholder="No file selected", width="100%"),
        uiOutput("import_feedback"),
        div(class="ctrl-note",
            tags$b("Single index:"), " date + one value column (e.g. \u201cdate,value\u201d). ",
            tags$b("Multiple indices:"), " date column first, then one column per index \u2014 column headers become the index names; blank cells = missing value for that index on that date (silently dropped). At least ", MIN_IMPORT_ROWS, " valid rows per column required."),
        uiOutput("manage_indices")
      )
    )
  ),

  # ── Main panels ───────────────────────────────────────────────────────────
  navset_tab(
    id = "main_tab",

    # ── Analysis ─────────────────────────────────────────────────────────────
    nav_panel(
      title = tagList(icon("chart-line"), " Analysis"),
      value = "analysis",

      layout_columns(
        col_widths = c(8, 4),
        heights_equal = "row",

        card(
          full_screen = TRUE, height = "340px", class = "plot-card",
          card_header(
            "Price History",
            uiOutput("series_length_badge", inline=TRUE)
          ),
          card_body(
            class = "p-1",
            plotOutput("price_chart", height="100%")
          )
        ),

        card(
          height = "340px",
          card_header("Statistics"),
          card_body(
            class = "p-1", style="overflow-y:auto",
            uiOutput("stats_table")
          )
        )
      ),

      layout_columns(
        col_widths = c(5, 7),
        heights_equal = "row",

        card(
          full_screen = TRUE, height = "340px", class = "plot-card",
          card_header(
            "Return Distribution",
            div(style="float:right; display:flex; align-items:center; gap:12px",
              uiOutput("hist_total_badge", inline=TRUE),
              radioButtons("hist_yscale", NULL,
                           choices=c("Linear"="linear","Log"="log"),
                           selected="linear", inline=TRUE)
            )
          ),
          card_body(
            class = "p-1",
            plotOutput("hist_chart", height="100%")
          )
        ),

        card(
          height = "340px",
          card_header("Top 5 Worst & Best Episodes",
                       uiOutput("episodes_context", inline=TRUE)),
          card_body(
            class = "p-1", style="overflow-y:auto",
            uiOutput("episodes_table")
          )
        )
      )
    ),

    # ── Rolling Windows ───────────────────────────────────────────────────────
    nav_panel(
      title = tagList(icon("arrows-rotate"), " Rolling Windows"),
      value = "rolling",

      card(
        full_screen = TRUE, height = "380px", class = "plot-card",
        card_header(
          div(style="display:flex; justify-content:space-between; align-items:baseline; flex-wrap:wrap; gap:6px",
            span("Worst & Best Selected-N Return by Anchor Date"),
            uiOutput("rw_avg_badges", inline=TRUE)
          ),
          div(style="display:flex; justify-content:space-between; align-items:baseline; flex-wrap:wrap",
            div(class="text-muted", style="font-size:.78rem; font-weight:400; margin-top:2px",
                "Each point = most extreme N-day return in the trailing Observation Period ending on that date"),
            uiOutput("series_length_badge_rolling", inline=TRUE)
          )
        ),
        card_body(
          class = "p-1",
          plotOutput("rw_chart", height="100%")
        )
      ),

      card(
        height = "235px",
        card_header("Data Table"),
        card_body(style="padding:4px 8px", uiOutput("rw_table"))
      )
    ),

    # ── Term Structure ────────────────────────────────────────────────────────
    nav_panel(
      title = tagList(icon("chart-bar"), " Term Structure"),
      value = "term",

      card(
        card_body(
          style = "padding:4px 12px",
          div(style="display:flex; justify-content:space-between; align-items:baseline",
            tags$label(class="text-uppercase fw-semibold text-muted mb-1",
                       style="font-size:.7rem; letter-spacing:.06em",
                       "Indices to Compare"),
            tags$div(class="text-muted", style="font-size:.74rem",
              "Rank is set in the sidebar \u2192")
          ),
          selectizeInput("ts_tickers", NULL, choices=current_index_choices(INDEX_CFG),
                        selected="SPX", multiple=TRUE,
                        options=list(plugins=list("remove_button"),
                                     dropdownParent="body"),
                        width="100%"),
          uiOutput("ts_range_list")
        )
      ),

      card(
        full_screen = TRUE, height = "380px", class = "plot-card",
        card_header(
          "Rank-th Worst / Best Return vs. Window Length N",
          span(class="text-muted ms-2", style="font-size:.8rem; font-weight:400",
               "(solid = Worst, dashed = Best; color = index)")
        ),
        card_body(
          class = "p-1",
          plotOutput("ts_chart", height="100%")
        )
      ),

      card(
        height = "235px",
        card_header("Data Table"),
        card_body(style="padding:4px 8px", uiOutput("ts_table"))
      )
    ),

    # ── Definitions ───────────────────────────────────────────────────────────
    nav_panel(
      title = tagList(icon("book-open"), " Definitions"),
      value = "defs",

      fluidRow(
        column(
          width = 8, offset = 1,

          h4(class="mt-3 mb-4", "Definitions & Worked Examples"),

          # ── 1. N-Day Rolling Return ──────────────────────────────────────────
          card(class="mb-4",
            card_header("1.  N-Day Rolling Return"),
            card_body(
              p("An N-day return compares the price on one day to the price N trading days earlier. ",
                "A ", tags$b("rolling"), " N-day return repeats this for every day in the series — ",
                "sliding the window forward one day at a time:"),
              tags$pre(class="bg-light p-3 rounded", style="font-size:.80rem; line-height:1.7",
                "Return(t) = Price(t) / Price(t\u2212N) \u2212 1    \u2190 percentage change"),
              # Window-sliding visual diagram
              HTML('
<div style="margin:14px 0 8px; font-family:monospace; font-size:.78rem; color:#334155">
  <div style="display:flex; align-items:flex-end; gap:0; margin-bottom:2px">
    <span style="width:28px;text-align:center;color:#94A3B8">t0</span>
    <span style="width:28px;text-align:center;color:#94A3B8">t1</span>
    <span style="width:28px;text-align:center;color:#94A3B8">t2</span>
    <span style="width:28px;text-align:center;color:#94A3B8">t3</span>
    <span style="width:28px;text-align:center;color:#94A3B8">t4</span>
    <span style="width:28px;text-align:center;color:#94A3B8">t5</span>
    <span style="width:28px;text-align:center;color:#94A3B8">t6</span>
    <span style="width:28px;text-align:center;color:#94A3B8">t7</span>
    <span style="width:28px;text-align:center;color:#94A3B8">t8</span>
  </div>
  <div style="display:flex; gap:0; margin-bottom:4px">
    <span style="width:28px;height:18px;background:#E2E8F0;border:1px solid #CBD5E1;display:inline-block"></span>
    <span style="width:28px;height:18px;background:#BFDBFE;border:1px solid #93C5FD;display:inline-block"></span>
    <span style="width:28px;height:18px;background:#BFDBFE;border:1px solid #93C5FD;display:inline-block"></span>
    <span style="width:28px;height:18px;background:#BFDBFE;border:1px solid #93C5FD;display:inline-block"></span>
    <span style="width:28px;height:18px;background:#BFDBFE;border:1px solid #93C5FD;display:inline-block"></span>
    <span style="width:28px;height:18px;background:#DBEAFE;border:2px solid #3B82F6;display:inline-block"></span>
    <span style="width:28px;height:18px;background:#E2E8F0;border:1px solid #CBD5E1;display:inline-block"></span>
    <span style="width:28px;height:18px;background:#E2E8F0;border:1px solid #CBD5E1;display:inline-block"></span>
    <span style="width:28px;height:18px;background:#E2E8F0;border:1px solid #CBD5E1;display:inline-block"></span>
  </div>
  <div style="color:#1D4ED8; font-size:.74rem; margin-bottom:8px">
    &nbsp;&nbsp;&nbsp;&nbsp;<b>N = 4 days</b>&nbsp;&nbsp;return&nbsp;=&nbsp;price(t5)&nbsp;/&nbsp;price(t1)&nbsp;&minus;&nbsp;1 &nbsp;&nbsp;&rarr;&nbsp; window slides forward 1 day
  </div>
  <div style="display:flex; gap:0; margin-bottom:2px">
    <span style="width:28px;height:18px;background:#E2E8F0;border:1px solid #CBD5E1;display:inline-block"></span>
    <span style="width:28px;height:18px;background:#E2E8F0;border:1px solid #CBD5E1;display:inline-block"></span>
    <span style="width:28px;height:18px;background:#BFDBFE;border:1px solid #93C5FD;display:inline-block"></span>
    <span style="width:28px;height:18px;background:#BFDBFE;border:1px solid #93C5FD;display:inline-block"></span>
    <span style="width:28px;height:18px;background:#BFDBFE;border:1px solid #93C5FD;display:inline-block"></span>
    <span style="width:28px;height:18px;background:#BFDBFE;border:1px solid #93C5FD;display:inline-block"></span>
    <span style="width:28px;height:18px;background:#DBEAFE;border:2px solid #3B82F6;display:inline-block"></span>
    <span style="width:28px;height:18px;background:#E2E8F0;border:1px solid #CBD5E1;display:inline-block"></span>
    <span style="width:28px;height:18px;background:#E2E8F0;border:1px solid #CBD5E1;display:inline-block"></span>
  </div>
  <div style="color:#64748B; font-size:.74rem">&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;return = price(t6) / price(t2) &minus; 1 &nbsp; (next window)</div>
</div>'),
              p("Change N below \u2014 the Return column recalculates live."),
              fluidRow(
                column(3,
                  numericInput("def_n", tags$b("N:"),
                               value=4, min=1, max=10, step=1, width="100%")
                ),
                column(9,
                  tags$small(class="text-muted",
                    "\u2190 With N=1 every row has a return; with N=4 the first four rows are blank (no prior price N days back).")
                )
              ),
              br(),
              uiOutput("def_example_table")
            )
          ),

          # ── 2. Observation Count ─────────────────────────────────────────────
          card(class="mb-4",
            card_header("2.  Observation Count"),
            card_body(
              p("With M daily prices and window length N there are ", tags$b("M \u2212 N"),
                " overlapping N-day observations (each window needs N prior prices)."),
              # Overlap diagram
              HTML('
<div style="margin:10px 0 14px; font-family:monospace; font-size:.78rem; color:#334155">
  <div style="margin-bottom:4px; color:#64748B"><b>M = 8 days, N = 3 &rarr; M &minus; N = 5 overlapping windows = 5 observations</b></div>
  <div style="display:flex; gap:2px; flex-direction:column">
    <div style="display:flex; align-items:center; gap:2px">
      <span style="width:24px;height:16px;background:#FCA5A5;border:1px solid #F87171;border-radius:3px"></span>
      <span style="width:24px;height:16px;background:#FCA5A5;border:1px solid #F87171;border-radius:3px"></span>
      <span style="width:24px;height:16px;background:#FCA5A5;border:1px solid #F87171;border-radius:3px"></span>
      <span style="width:24px;height:16px;background:#FCA5A5;border:2px solid #DC2626;border-radius:3px"></span>
      <span style="width:24px;height:16px;background:#F1F5F9;border:1px solid #CBD5E1;border-radius:3px"></span>
      <span style="width:24px;height:16px;background:#F1F5F9;border:1px solid #CBD5E1;border-radius:3px"></span>
      <span style="width:24px;height:16px;background:#F1F5F9;border:1px solid #CBD5E1;border-radius:3px"></span>
      <span style="width:24px;height:16px;background:#F1F5F9;border:1px solid #CBD5E1;border-radius:3px"></span>
      <span style="color:#94A3B8; margin-left:6px">&larr; window 1</span>
    </div>
    <div style="display:flex; align-items:center; gap:2px">
      <span style="width:24px;height:16px;background:#F1F5F9;border:1px solid #CBD5E1;border-radius:3px"></span>
      <span style="width:24px;height:16px;background:#BFDBFE;border:1px solid #93C5FD;border-radius:3px"></span>
      <span style="width:24px;height:16px;background:#BFDBFE;border:1px solid #93C5FD;border-radius:3px"></span>
      <span style="width:24px;height:16px;background:#BFDBFE;border:1px solid #93C5FD;border-radius:3px"></span>
      <span style="width:24px;height:16px;background:#BFDBFE;border:2px solid #3B82F6;border-radius:3px"></span>
      <span style="width:24px;height:16px;background:#F1F5F9;border:1px solid #CBD5E1;border-radius:3px"></span>
      <span style="width:24px;height:16px;background:#F1F5F9;border:1px solid #CBD5E1;border-radius:3px"></span>
      <span style="width:24px;height:16px;background:#F1F5F9;border:1px solid #CBD5E1;border-radius:3px"></span>
      <span style="color:#94A3B8; margin-left:6px">&larr; window 2</span>
    </div>
    <div style="display:flex; align-items:center; gap:2px">
      <span style="width:24px;height:16px;background:#F1F5F9;border:1px solid #CBD5E1;border-radius:3px"></span>
      <span style="width:24px;height:16px;background:#F1F5F9;border:1px solid #CBD5E1;border-radius:3px"></span>
      <span style="width:24px;height:16px;background:#D1FAE5;border:1px solid #6EE7B7;border-radius:3px"></span>
      <span style="width:24px;height:16px;background:#D1FAE5;border:1px solid #6EE7B7;border-radius:3px"></span>
      <span style="width:24px;height:16px;background:#D1FAE5;border:1px solid #6EE7B7;border-radius:3px"></span>
      <span style="width:24px;height:16px;background:#D1FAE5;border:2px solid #16A34A;border-radius:3px"></span>
      <span style="width:24px;height:16px;background:#F1F5F9;border:1px solid #CBD5E1;border-radius:3px"></span>
      <span style="width:24px;height:16px;background:#F1F5F9;border:1px solid #CBD5E1;border-radius:3px"></span>
      <span style="color:#94A3B8; margin-left:6px">&larr; window 3 &hellip;</span>
    </div>
  </div>
  <div style="color:#64748B; font-size:.72rem; margin-top:6px">shaded = lookback prices &nbsp; | &nbsp; <b>bold border</b> = end date (return is computed here)</div>
</div>'),
              tags$table(class="table table-sm table-bordered w-auto",
                tags$thead(tags$tr(tags$th("M"), tags$th("N"), tags$th("Observations (M\u2212N)"))),
                tags$tbody(
                  tags$tr(tags$td("11 (worked example above)"), tags$td("4"), tags$td("7")),
                  tags$tr(tags$td("6,873 (EUR/CHF)"),           tags$td("20"),tags$td("6,853")),
                  tags$tr(tags$td("24,729 (S&P 500)"),          tags$td("20"),tags$td("24,709"))
                )
              )
            )
          ),

          # ── 3. Statistics ────────────────────────────────────────────────────
          card(class="mb-4",
            card_header("3.  Percentile (VaR-Style) Statistics"),
            card_body(
              p("All M\u2212N observations are sorted from most negative to most positive. ",
                "The Statistics panel reads off specific points in that sorted list:"),
              # Sorted-distribution visual
              HTML('
<div style="margin:10px 0 14px">
  <div style="display:flex; height:28px; width:100%; max-width:480px; border-radius:4px; overflow:hidden; border:1px solid #CBD5E1">
    <div style="width:1%;  background:#7F1D1D" title="Worst 0.05%"></div>
    <div style="width:1%;  background:#B91C1C" title="Worst 0.1%"></div>
    <div style="width:3%;  background:#DC2626" title="Worst 1%"></div>
    <div style="width:5%;  background:#F87171" title="Worst 5%"></div>
    <div style="width:40%; background:#E2E8F0" title="Middle 40%"></div>
    <div style="width:1%;  background:#94A3B8" title="Median" style="border-left:2px dashed #475569"></div>
    <div style="width:39%; background:#E2E8F0" title="Middle 39%"></div>
    <div style="width:5%;  background:#86EFAC" title="Best 5%"></div>
    <div style="width:3%;  background:#16A34A" title="Best 1%"></div>
    <div style="width:1%;  background:#14532D" title="Best 0.1%"></div>
    <div style="width:1%;  background:#052e16" title="Best 0.05%"></div>
  </div>
  <div style="display:flex; justify-content:space-between; font-size:.70rem; color:#64748B; max-width:480px; margin-top:3px">
    <span style="color:#DC2626">&#9664; more negative (losses)</span>
    <span>&#9474; median</span>
    <span style="color:#16A34A">more positive (gains) &#9654;</span>
  </div>
</div>'),
              tags$table(class="table table-sm table-striped",
                tags$thead(tags$tr(
                  tags$th("Label"), tags$th("R formula"), tags$th("Meaning")
                )),
                tags$tbody(
                  tags$tr(tags$td(tags$span(style="color:#7F1D1D; font-weight:600","Worst 0.05%")),
                           tags$td(tags$code("quantile(r, 0.0005)")),
                           tags$td("Only 0.05% of obs were worse. Shown only when \u22652,000 obs.")),
                  tags$tr(tags$td(tags$span(style="color:#B91C1C; font-weight:600","Worst 0.1%")),
                           tags$td(tags$code("quantile(r, 0.001)")),
                           tags$td("Only 0.1% of obs were worse. Shown only when \u22651,000 obs.")),
                  tags$tr(tags$td(tags$span(style="color:#DC2626; font-weight:600","Worst 1%")),
                           tags$td(tags$code("quantile(r, 0.01)")),
                           tags$td("1% of observations were worse \u2014 common VaR threshold.")),
                  tags$tr(tags$td(tags$span(style="color:#F87171; font-weight:600","Worst 5%")),
                           tags$td(tags$code("quantile(r, 0.05)")),
                           tags$td("5% of observations were worse than this.")),
                  tags$tr(tags$td("Median"),
                           tags$td(tags$code("median(r)")),
                           tags$td("Half of all observations better, half worse.")),
                  tags$tr(tags$td("Mean"),
                           tags$td(tags$code("mean(r)")),
                           tags$td("Arithmetic average of all observations.")),
                  tags$tr(tags$td("Std Dev"),
                           tags$td(tags$code("sd(r)")),
                           tags$td("Typical dispersion / volatility measure.")),
                  tags$tr(tags$td(tags$span(style="color:#86EFAC; font-weight:600","Best 5%")),
                           tags$td(tags$code("quantile(r, 0.95)")),
                           tags$td("Only 5% of observations were better than this.")),
                  tags$tr(tags$td(tags$span(style="color:#16A34A; font-weight:600","Best 1%")),
                           tags$td(tags$code("quantile(r, 0.99)")),
                           tags$td("Only 1% of observations were better than this.")),
                  tags$tr(tags$td(tags$span(style="color:#14532D; font-weight:600","Best 0.1%")),
                           tags$td(tags$code("quantile(r, 0.999)")),
                           tags$td("Only 0.1% better. Shown only when \u22651,000 obs.")),
                  tags$tr(tags$td(tags$span(style="color:#052e16; font-weight:600","Best 0.05%")),
                           tags$td(tags$code("quantile(r, 0.9995)")),
                           tags$td("Only 0.05% better. Shown only when \u22652,000 obs.")),
                  tags$tr(tags$td(tags$span(style="color:#16A34A; font-weight:600","Best (Max)")),
                           tags$td(tags$code("max(r)")),
                           tags$td("The single best observation in the period."))
                )
              )
            )
          ),

          # ── 4. Episodes ──────────────────────────────────────────────────────
          card(class="mb-4",
            card_header("4.  Worst / Best Episodes (Top-5 Tables & Price Chart Badges)"),
            card_body(
              p("The Top-5 tables and the numbered badges on the price chart both rank individual N-day windows:"),
              tags$ul(
                tags$li(tags$b("Rank 1 Worst"), " = the single most negative N-day return ever observed. ",
                        "Badge \u201c1\u201d on the price chart marks its exact start and end date, with a red ",
                        "shaded band spanning the N-day window and an annotation showing the magnitude."),
                tags$li(tags$b("Rank 2, 3, 4, 5"), " = second-, third-... most negative, shown with successively ",
                        "lighter annotations. When several worst episodes fall close together (e.g. multiple days ",
                        "of the same crash), their badges stack vertically so all five labels remain readable."),
                tags$li("The ", tags$b("End Date"), " is the day at which the return is computed; the ",
                        tags$b("Start Date"), " is the trading day N observations earlier, whose price the return ",
                        "is measured from (the same convention used in the Rolling Windows and Term Structure tables)."),
                tags$li("Best episodes work identically, with green shading and positive return values.")
              ),
              p(class="text-muted", style="font-size:.85rem",
                "Note: because windows overlap, the same underlying market event can appear in multiple ranked ",
                "episodes (e.g. ranks 1\u20135 during 1929 all overlap by 3\u20134 days). The start/end dates let you ",
                "verify which specific span each rank covers.")
            )
          ),

          # ── 5. Rolling Windows ────────────────────────────────────────────────
          card(class="mb-4",
            card_header("5.  Rolling Windows Tab: Anchored Lookback Periods"),
            card_body(
              p("The Rolling Windows tab repeats the \u00a71\u20133 analysis at multiple ", tags$b("anchor dates"),
                ", stepping back roughly one year at a time from the most recent date in the data. ",
                "For each anchor, the chart shows the single worst and best N-day move that occurred ",
                "within the trailing Observation Period ending at that anchor."),
              # Anchor-stepping visual diagram
              HTML('
<div style="margin:10px 0 14px; font-size:.78rem; color:#334155">
  <div style="font-weight:600; color:#64748B; margin-bottom:6px">Each bracket = one Observation Period lookback &nbsp;|&nbsp; dot &#9679; = anchor (window end)</div>
  <div style="position:relative; height:88px; background:#F8FAFC; border:1px solid #E2E8F0; border-radius:6px; overflow:hidden; padding:6px">
    <div style="position:absolute; left:5%;  width:35%; height:18px; top:8px;  background:rgba(220,38,38,.12);  border:1px solid #DC2626; border-radius:3px"></div>
    <div style="position:absolute; left:5%;  width:0;   height:18px; top:8px;  border-left:2px dashed #DC2626"></div>
    <div style="position:absolute; left:40%; width:0;   height:18px; top:8px;  border-left:2px solid #DC2626"></div>
    <div style="position:absolute; left:40%; top:4px;  font-size:10px; color:#DC2626">&bull;</div>
    <div style="position:absolute; left:41%; top:6px; font-size:.68rem; color:#DC2626">2015</div>
    <div style="position:absolute; left:20%; width:35%; height:18px; top:34px; background:rgba(30,64,175,.10);  border:1px solid #3B82F6; border-radius:3px"></div>
    <div style="position:absolute; left:55%; top:30px; font-size:10px; color:#3B82F6">&bull;</div>
    <div style="position:absolute; left:56%; top:28px; font-size:.68rem; color:#3B82F6">2016</div>
    <div style="position:absolute; left:35%; width:35%; height:18px; top:60px; background:rgba(22,163,74,.10);  border:1px solid #16A34A; border-radius:3px"></div>
    <div style="position:absolute; left:70%; top:56px; font-size:10px; color:#16A34A">&bull;</div>
    <div style="position:absolute; left:71%; top:54px; font-size:.68rem; color:#16A34A">2017</div>
    <div style="position:absolute; left:3%; top:72px; font-size:.68rem; color:#94A3B8">time &rarr;</div>
  </div>
  <div style="color:#64748B; font-size:.72rem; margin-top:4px">Brackets shift right ~1 year each step. A point on the chart = worst/best N-day move inside that bracket.</div>
</div>'),
              p("The chart reveals how worst/best outcomes ", tags$b("evolved through history"),
                " \u2014 e.g. the 2008 crisis dominates all anchors from 2009 back to the start; ",
                "recent anchors only capture post-2008 markets."),
              p("Dashed horizontal lines mark the ", tags$b("average"), " Worst and Best across all ",
                tags$b("full-coverage"), " anchors \u2014 a quick read on the 'typical' extreme for ",
                "that index and Observation Period. ",
                "The table below the chart also shows ", tags$b("Worst Start / Worst End"),
                " and ", tags$b("Best Start / Best End"),
                " dates, identifying exactly which N-day window produced the extreme for each anchor.")
            )
          ),

          # ── 6. Term Structure ─────────────────────────────────────────────────
          card(class="mb-4",
            card_header("6.  Term Structure Tab: Rank-th Worst/Best vs. Window Length"),
            card_body(
              p("For each window length N (1, 2, 3, 5, 10, 15, 20, 30, 90 days), the app sorts all ",
                "observations from most negative to most positive and picks the ", tags$b("Rank-th"),
                " value from each end."),
              # Rank illustration
              HTML('
<div style="margin:10px 0 14px; font-size:.78rem">
  <div style="display:flex; gap:4px; align-items:center; flex-wrap:wrap">
    <div style="display:flex; flex-direction:column; gap:2px">
      <div style="display:flex; gap:2px">
        <span style="width:20px;height:18px;background:#7F1D1D;border-radius:2px;display:flex;align-items:center;justify-content:center;color:#fff;font-size:.64rem;font-weight:700">1</span>
        <span style="width:20px;height:18px;background:#B91C1C;border-radius:2px;display:flex;align-items:center;justify-content:center;color:#fff;font-size:.64rem;font-weight:700">2</span>
        <span style="width:20px;height:18px;background:#DC2626;border-radius:2px;display:flex;align-items:center;justify-content:center;color:#fff;font-size:.64rem;font-weight:700">3</span>
        <span style="width:80px;height:18px;background:#E2E8F0;border-radius:2px;display:flex;align-items:center;justify-content:center;color:#94A3B8;font-size:.64rem">&ctdot; middle obs &ctdot;</span>
        <span style="width:20px;height:18px;background:#16A34A;border-radius:2px;display:flex;align-items:center;justify-content:center;color:#fff;font-size:.64rem;font-weight:700">3</span>
        <span style="width:20px;height:18px;background:#166534;border-radius:2px;display:flex;align-items:center;justify-content:center;color:#fff;font-size:.64rem;font-weight:700">2</span>
        <span style="width:20px;height:18px;background:#052e16;border-radius:2px;display:flex;align-items:center;justify-content:center;color:#fff;font-size:.64rem;font-weight:700">1</span>
      </div>
      <div style="display:flex; gap:2px; font-size:.64rem; color:#64748B; margin-top:1px">
        <span style="width:20px;text-align:center">&#9660;</span>
        <span style="width:20px;text-align:center">&#9660;</span>
        <span style="width:20px;text-align:center">&#9660;</span>
        <span style="width:80px"></span>
        <span style="width:20px;text-align:center">&#9660;</span>
        <span style="width:20px;text-align:center">&#9660;</span>
        <span style="width:20px;text-align:center">&#9660;</span>
      </div>
    </div>
  </div>
  <div style="display:flex; gap:18px; margin-top:4px; font-size:.72rem; color:#64748B">
    <span><span style="color:#7F1D1D; font-weight:700">Rank 1 Worst</span> = most extreme loss</span>
    <span><span style="color:#B91C1C; font-weight:700">Rank 2 Worst</span> = 2nd most extreme</span>
    <span style="color:#64748B">&hellip;</span>
    <span><span style="color:#052e16; font-weight:700">Rank 1 Best</span> = largest gain</span>
  </div>
</div>'),
              p("The chart shows how each rank's magnitude ", tags$b("scales with window length N"),
                " \u2014 e.g. for S&P 500: Rank-1 1-day \u2248 -20% (Black Monday 1987); ",
                "Rank-1 90-day \u2248 -45% (Great Depression). Longer windows accumulate losses."),
              p("Use ", tags$b("Ranks 1 to N"), " mode to overlay ranks 1 through N simultaneously ",
                "(darker colour = more extreme rank). Use ", tags$b("multiple indices"),
                " via the 'Indices to Compare' selector to compare how different markets' extremes scale with horizon.")
            )
          ),

          # ── 7. Units & Sign Conventions ──────────────────────────────────────
          card(class="mb-4",
            card_header("7.  Units & Sign Conventions"),
            card_body(
              tags$table(class="table table-sm table-striped",
                tags$thead(tags$tr(
                  tags$th("Index type"), tags$th("Return unit"),
                  tags$th(tags$span(style="color:#DC2626","\u201cWorst\u201d means")),
                  tags$th(tags$span(style="color:#16A34A","\u201cBest\u201d means"))
                )),
                tags$tbody(
                  tags$tr(
                    tags$td("Equity (S&P 500, Dow Jones, DAX, FTSE, Nikkei)"),
                    tags$td("% change"),
                    tags$td(tags$span(style="color:#DC2626","Largest price drop (loss)")),
                    tags$td(tags$span(style="color:#16A34A","Largest price rise (gain)"))
                  ),
                  tags$tr(
                    tags$td("Gov. Bond Yields (US 10Y, US 30Y)"),
                    tags$td("% change"),
                    tags$td(tags$span(style="color:#DC2626","Largest yield drop (bull bond move)")),
                    tags$td(tags$span(style="color:#16A34A","Largest yield spike (bear bond move)"))
                  ),
                  tags$tr(
                    tags$td("Volatility (VIX)"),
                    tags$td("% change"),
                    tags$td(tags$span(style="color:#DC2626","Largest vol spike (fear surge)")),
                    tags$td(tags$span(style="color:#16A34A","Largest vol crush (fear collapse)"))
                  ),
                  tags$tr(
                    tags$td("FX vs CHF (USD/CHF, EUR/CHF, GBP/CHF, JPY/CHF)"),
                    tags$td("% change"),
                    tags$td(tags$span(style="color:#DC2626","Largest drop of foreign ccy vs CHF")),
                    tags$td(tags$span(style="color:#16A34A","Largest rise of foreign ccy vs CHF"))
                  )
                )
              ),
              p(class="text-muted", style="font-size:.85rem",
                "For FX pairs vs CHF, a \u201closs\u201d for one side is always a \u201cgain\u201d for the other. ",
                "The app always shows both Worst and Best side-by-side so the full picture is visible.")
            )
          ),

          # ── 8. Rolling-window coverage ─────────────────────────────────────────
          card(class="mb-4",
            card_header("8.  Rolling-Window Data Coverage: Full vs. Partial Anchors"),
            card_body(
              p(paste0("Each point on the Rolling Windows chart is an "), tags$b("anchor"), paste0(" date. ",
                "For an Observation Period of X years, the app wants to scan every N-day return whose end ",
                "date falls in [anchor \u2212 X years, anchor]. But if the index's price history doesn't go ",
                "back that far, that window gets clipped at the start of the available data \u2014 so an early ",
                "anchor might only have, say, 3.4 years of data instead of the requested 10.")),
              p(tags$b("\u201cCoverage\u201d"), " is the number of years of data actually used for an anchor's window. ",
                tags$b("\u201cFull\u201d"), " means Coverage \u2248 Observation Period; ", tags$b("\u201cPartial\u201d"),
                " means it's shorter."),

              h6(class="mt-3", "Worked example: EUR/CHF, Observation Period = 10 years"),
              p(class="text-muted", style="font-size:.85rem",
                "EUR/CHF price history begins 1999-01-19 (for 20-day returns). Two of the 28 candidate anchors:"),
              tags$table(class="table table-sm table-bordered",
                tags$thead(tags$tr(
                  tags$th("Anchor"), tags$th("Requested window"), tags$th("Actual window used"),
                  tags$th("Coverage"), tags$th("Status")
                )),
                tags$tbody(
                  tags$tr(
                    tags$td(tags$b("A: 2002-05-29")),
                    tags$td("1992-05-29 \u2192 2002-05-29  (10.0 yr)"),
                    tags$td("1999-01-19 \u2192 2002-05-29  (\u22483.4 yr)"),
                    tags$td(tags$code("3.4 / 10.0 yr")),
                    tags$td(tags$span(style="color:#DC2626; font-weight:600", "Partial"))
                  ),
                  tags$tr(
                    tags$td(tags$b("B: 2020-05-29")),
                    tags$td("2010-05-30 \u2192 2020-05-29  (10.0 yr)"),
                    tags$td("2010-05-30 \u2192 2020-05-29  (10.0 yr)"),
                    tags$td(tags$code("10.0 / 10.0 yr")),
                    tags$td(tags$span(style="color:#16A34A; font-weight:600", "Full"))
                  )
                )
              ),

              h6(class="mt-3", "Diagram"),
              p(class="text-muted", style="font-size:.85rem",
                "Blue = EUR/CHF data available. The dashed outline is Anchor A's ", tags$em("requested"),
                " 10-year window \u2014 most of it (1992\u20131999) falls before any data exists, so the ",
                tags$em("actual window used"), " (solid red) is only ~3.4 years. Anchor B's window ",
                "(2010\u20132020, solid green) is fully inside the available data."),
              plotOutput("coverage_diagram", height="280px"),

              p(class="mt-3", "Why it matters: a partial window had fewer days in which to ", tags$em("find"),
                " an extreme move, so its Worst/Best tend to look ", tags$b("less extreme"),
                " purely because there was less data \u2014 not because markets were calmer. Comparing a ",
                "partial-window anchor to a full-window anchor can give a misleading impression of how ",
                "risk has evolved over time."),

              p("The ", tags$b("\u201cAnchor Windows\u201d"), " control in the sidebar (Rolling Windows tab) lets you choose:"),
              tags$ul(
                tags$li(tags$b("Full windows only"), " (default) \u2014 only anchors with the complete requested ",
                        "lookback are shown. Rigorous answer to \u201cwhat was the worst N-day move in a true X-year ",
                        "window?\u201d, at the cost of fewer points for shorter-history series."),
                tags$li(tags$b("Show all (flag partial)"), " \u2014 every anchor is shown. Partial-coverage points are ",
                        "drawn with open-circle markers, and the affected date range is shaded gray. The Data Table's ",
                        "\u201cData Coverage\u201d column shows the exact years used for every anchor.")
              ),
              p(class="text-muted", style="font-size:.85rem",
                "In both modes, the Avg Worst / Avg Best badges and dashed reference lines are computed from ",
                tags$b("full-coverage anchors only"), ", so the headline averages stay consistent regardless of which ",
                "mode is selected.")
            )
          ),

          # ── 9. Data Window ────────────────────────────────────────────────────
          card(class="mb-4",
            card_header("9.  Data Window: Limiting to Recent History"),
            card_body(
              p("The ", tags$b("\u201cData Window\u201d"), " control at the top of the sidebar lets you ask: ",
                tags$em("\u201cwhat would these statistics look like if I only had the last X years of data?\u201d"),
                " When enabled, every computation on every tab \u2014 price history, percentile statistics, the ",
                "return-distribution histogram, the Top-5 episode tables, Rolling Windows, and Term Structure \u2014 ",
                "is restricted to N-day returns whose ", tags$b("end date"), " falls within the most recent X years ",
                "of that index's history."),
              p("The N-day return itself is still computed correctly using the full underlying price series ",
                "(so a 20-day return ending 3 days into the window still reflects 20 real trading days) \u2014 only ",
                "the set of ", tags$em("end dates"), " considered is restricted. This is the same end-date-based ",
                "filtering used by the Rolling Windows anchors (\u00a78)."),

              h6(class="mt-3", "Worked example"),
              p(class="text-muted", style="font-size:.85rem",
                "S&P 500 full history: 98.5 years (1927\u20132026), 24,709 20-day observations, Worst 1% = \u221215.37%. ",
                "With Data Window = 15 years: 15.0 years (2011\u20132026), 3,773 observations, Worst 1% = \u221210.76% ",
                "\u2014 the 1929/1987/2008 crashes are excluded, so the tail looks less severe; the 2020 COVID crash ",
                "(\u221230.94% over 20 days) becomes the new #1 worst episode."),

              h6(class="mt-3", "Interactions with other tabs"),
              tags$ul(
                tags$li(tags$b("Rolling Windows (\u00a78):"), " if Data Window < Observation Period, anchors can only ",
                        "ever be \u201cPartial\u201d (there isn't enough data for even one full-coverage window). ",
                        "Typically exactly one anchor \u2014 the most recent \u2014 ends up with Coverage = Data Window ",
                        "years, right at the edge of \u201cFull\u201d; all earlier anchors are partial."),
                tags$li(tags$b("Term Structure:"), " each selected index's \u201clast X years\u201d is measured from ",
                        tags$em("that index's own"), " most recent date \u2014 so for example with Data Window = 10 ",
                        "years, Dow Jones (data ends 2023) uses 2013\u20132023 while S&P 500 (data ends 2026) uses ",
                        "2016\u20132026. This keeps each index's slice meaningful even though their histories end on ",
                        "different dates."),
                tags$li(tags$b("If X exceeds the index's full history"), " (e.g., a 50-year window on EUR/CHF, which ",
                        "only goes back to 1999), the filter has no effect \u2014 all available data is used, and the ",
                        "sidebar notes \u201cLimit exceeds full history.\u201d")
              ),
              p(class="text-muted", style="font-size:.85rem",
                "The sidebar, the Analysis and Rolling Windows card headers, and the Term Structure \u201cIndices to ",
                "Compare\u201d card (one line per selected index) always show the resulting date range, row count, ",
                "and years of data, so it's clear at a glance whether \u2014 and how much \u2014 the window is ",
                "currently limiting the data.")
            )
          )
        )
      )
    )   # end Definitions nav_panel
  )     # end navset_tab
)       # end page_sidebar
}         # end ui function (per-session, so newly-imported indices show up
          # in a fresh page load/new session without needing an app restart)

# ══════════════════════════════════════════════════════════════════════════════
# 5. SERVER
# ══════════════════════════════════════════════════════════════════════════════

# Which sidebar controls apply to which tab (used for graying-out)
RELEVANCE <- list(
  analysis = c("ticker", "n_val"),
  rolling  = c("ticker", "n_val", "obs_years"),
  term     = c("rank_k", "rank_mode"),
  defs     = c()
)
CTRL_DIV  <- c(ticker="ctrl_ticker", n_val="ctrl_n", obs_years="ctrl_obs",
               rank_k="ctrl_rank", rank_mode="ctrl_rank")
ALL_CTRLS <- names(CTRL_DIV)

server <- function(input, output, session) {

  # ── Per-session index state (hosted mode) ──────────────────────────────────
  # In hosted mode each browser session works on its own copies of the three
  # index structures. Every read below then sees this session's copy, and the
  # `<<-` assignments in the Import/Replace/Delete logic update it instead of
  # the process-wide globals, so one visitor's imports never reach another.
  # R copies on modify, so this costs nothing until a session imports data.
  # In local mode there are no local copies: `<<-` updates the globals, and
  # imports are shared across tabs and persisted to DATA_DIR as before.
  if (IS_HOSTED) {
    INDEX_CFG   <- INDEX_CFG
    raw_data    <- raw_data
    all_returns <- all_returns
  }

  # ── UI-toggle helpers (replace shinyjs::addClass/removeClass/enable/disable) ─
  # gray_out(div_id, on): toggle the ".ctrl-disabled" class on a sidebar
  #   control-group <div> (dims it and disables pointer events via CSS).
  # set_disabled(input_id, on): toggle the `disabled` property on a single
  #   plain <input> element (used for the "limit_years" numeric input).
  # Both send a tiny message handled by TOGGLE_JS (see UI section above).
  gray_out <- function(div_id, on) {
    session$sendCustomMessage("uiToggle", list(id=div_id, cls="ctrl-disabled", state=on))
  }
  set_disabled <- function(input_id, on) {
    session$sendCustomMessage("uiToggle", list(id=input_id, attr="disabled", state=on))
  }

  # TRUE when a numericInput holds a usable positive number. A cleared field
  # arrives as NA; outputs that need the value req() this so they simply wait
  # for a valid entry instead of showing a raw R error.
  is_pos_num <- function(x) is.numeric(x) && length(x) == 1 && !is.na(x) && x > 0

  # ── Core reactives ─────────────────────────────────────────────────────────
  # Bumped by the Import/Replace/Delete Index logic below whenever INDEX_CFG,
  # raw_data, or all_returns is mutated. Plain global-list mutations (the
  # `<<-` assignments used there) are invisible to Shiny's reactivity system
  # on their own — reactives only re-run when something they call (an input,
  # a reactive, or a reactiveVal) actually changes. Without this, *adding* a
  # new index works fine (it can't already be selected), but *replacing* the
  # data of the currently-selected index wouldn't refresh the charts, since
  # input$ticker itself doesn't change. Calling data_version() inside a
  # reactive (even without using the value) registers that dependency.
  data_version <- reactiveVal(0)

  tk      <- reactive(input$ticker)
  n_days  <- reactive(as.integer(input$n_val))
  cfg     <- reactive({ data_version(); INDEX_CFG[[tk()]] })

  # "Data Window" cutoff for the currently selected index (NULL = full history)
  cutoff_date <- reactive({
    data_version()
    years_cutoff(raw_data[[tk()]]$date, input$limit_active, input$limit_years)
  })

  # raw(): price history, optionally truncated to the most recent N years
  raw <- reactive({
    data_version()
    df <- raw_data[[tk()]]
    cd <- cutoff_date()
    if (!is.null(cd)) df <- df[df$date >= cd, , drop=FALSE]
    df
  })

  # ret_df(): N-day returns, filtered by their END date >= cutoff. Returns are
  # still computed from the full history (so the N-day lookback stays valid),
  # only the set of END dates considered is restricted.
  ret_df <- reactive({
    data_version()
    df <- all_returns[[tk()]][[as.character(n_days())]]
    cd <- cutoff_date()
    if (!is.null(cd)) df <- df[df$date >= cd, , drop=FALSE]
    df
  })

  # Enable/disable the years input alongside the "Limit to most recent years" checkbox
  observeEvent(input$limit_active, {
    set_disabled("limit_years", !isTRUE(input$limit_active))
  }, ignoreInit = FALSE)

  # Update obs_years default when index switches
  observeEvent(tk(), {
    updateNumericInput(session, "obs_years", value=cfg()$obs_years)
  }, ignoreInit=TRUE)

  # Default the Term-Structure multi-select to the sidebar index, once
  observeEvent(input$main_tab, {
    if (identical(input$main_tab, "term")) {
      current <- input$ts_tickers
      if (is.null(current) || length(current) == 0) {
        updateSelectizeInput(session, "ts_tickers", selected = input$ticker)
      }
    }
  }, ignoreInit = TRUE)

  # ══════════════════════════════════════════════════════════════════════════
  # Import / Replace / Delete Index
  #
  # Add:     upload a CSV -> validate_index_csv() checks structure -> editable
  #          name + live "will appear as" preview -> writes data + a registry
  #          row to disk, grows INDEX_CFG/raw_data/all_returns in memory.
  # Replace: same upload + validation, but instead of a name you pick an
  #          EXISTING index (built-in or imported) whose data file gets
  #          overwritten in place; label/color/identity are unchanged.
  # Delete:  imported indices only (built-ins are protected) — a small list
  #          with one click-to-delete icon per row, a modalDialog confirms,
  #          then its data file + registry row are removed.
  #
  # All three refresh both index selectors via update*Input() and bump
  # data_version() so any reactive currently showing the affected ticker
  # recomputes immediately, with no app restart needed.
  # ══════════════════════════════════════════════════════════════════════════

  # Re-validates whenever a new file is chosen. NULL while no file is chosen.
  import_validation <- reactive({
    f <- input$import_file
    if (is.null(f)) return(NULL)
    validate_csv_for_import(f$datapath)
  })

  # Tracks the outcome of the most recent Add/Replace click for the file
  # currently selected: list(path=<datapath>, ok=<TRUE/FALSE>, message=<text>).
  # Re-rendering the feedback panel for the SAME upload (e.g. after the
  # click triggers other reactives) then shows that outcome instead of
  # re-offering the same import a second time.
  import_result <- reactiveVal(NULL)

  output$import_feedback <- renderUI({
    f <- input$import_file
    if (is.null(f)) return(NULL)

    # If the most recent Add/Replace/ImportAll click has a result for this
    # exact file, show that outcome instead of re-offering the import panel.
    res <- import_result()
    if (!is.null(res) && identical(f$datapath, res$path)) {
      cls <- if (res$ok) "text-success" else "text-danger"
      ic  <- if (res$ok) icon("check-circle") else icon("triangle-exclamation")
      return(div(class=cls, style="font-size:.78rem; margin-top:6px", ic, " ", res$message))
    }

    v <- import_validation()
    if (is.null(v)) return(NULL)
    if (!isTRUE(v$ok)) {
      return(div(class="text-danger", style="font-size:.78rem; margin-top:6px", v$message))
    }

    # ── Single-index mode: original flow unchanged ──────────────────────────
    if (identical(v$mode, "single")) {
      tagList(
        div(class="text-success", style="font-size:.78rem; margin-top:6px",
            icon("check-circle"), sprintf(" Looks good \u2014 %s rows, %s \u2192 %s",
                  format(v$n, big.mark=","), fmt_date(min(v$df$date)), fmt_date(max(v$df$date)))),
        radioButtons("import_mode", NULL, inline=TRUE,
                     choices=c("Add as new"="new", "Replace existing"="replace")),
        uiOutput("import_mode_ui")
      )
    } else {
      # ── Multi-index mode: show a compact preview list ──────────────────────
      # One row per detected index. No Replace in multi-mode (would be ambiguous).
      n_idx <- length(v$indices)
      tagList(
        div(class="text-success", style="font-size:.78rem; margin-top:6px",
            icon("check-circle"), sprintf(" %d ind%s detected", n_idx,
                                          if (n_idx == 1) "ex" else "ices")),
        div(class="multi-import-list",
          lapply(v$indices, function(idx) {
            # Derive ticker against the CURRENT INDEX_CFG (not yet mutated)
            tk_preview <- slugify_ticker(idx$name, names(INDEX_CFG))
            gap_note <- if (idx$n_gaps > 0) {
              span(class="text-muted", style="font-size:.68rem",
                   sprintf(" (%d missing values handled)", idx$n_gaps))
            } else NULL
            div(class="multi-import-row",
              span(class="multi-import-name", idx$name),
              span(class="multi-import-ticker", paste0(" \u2192 ", tk_preview)),
              div(class="multi-import-info",
                  sprintf("%s rows \u2022 %s \u2013 %s",
                          format(idx$n, big.mark=","),
                          fmt_date(min(idx$df$date)), fmt_date(max(idx$df$date)))),
              gap_note
            )
          })
        ),
        if (length(v$skipped) > 0) {
          div(class="text-warning", style="font-size:.72rem; margin-top:4px",
              icon("triangle-exclamation"),
              sprintf(" %d column%s skipped: ",
                      length(v$skipped), if (length(v$skipped) == 1) "" else "s"),
              paste(sapply(v$skipped, function(s) sprintf("\u201c%s\u201d (%s)", s$name, s$reason)),
                    collapse="; "))
        },
        actionButton("import_confirm",
                     sprintf("Import %d %s", n_idx, if (n_idx == 1) "Index" else "Indices"),
                     class="btn-sm btn-primary mt-1")
      )
    }
  })

  # The part of the single-index panel that differs between Add and Replace.
  # Only rendered for single-index mode (multi always adds, never replaces).
  output$import_mode_ui <- renderUI({
    if (identical(input$import_mode, "replace")) {
      tagList(
        selectizeInput("import_replace_target", "Replace data for",
                    choices=c("Choose an index..."="", current_index_choices(INDEX_CFG)),
                    width="100%", options=list(dropdownParent="body")),
        div(class="text-muted", style="font-size:.72rem",
            "Overwrites that index's data. Its name and colour stay the same."),
        actionButton("import_confirm", "Replace Index", class="btn-sm btn-warning mt-1")
      )
    } else {
      f <- input$import_file
      default_label <- if (!is.null(f)) derive_default_label(f$name) else ""
      tagList(
        textInput("import_name", "Index name", value=default_label, width="100%"),
        uiOutput("import_id_preview"),
        actionButton("import_confirm", "Add Index", class="btn-sm btn-primary mt-1")
      )
    }
  })

  # Live "will appear as Name (ID)" preview, recomputed as the user edits
  # the name field — separate from import_mode_ui above so editing the
  # name doesn't reset/rebuild the whole panel (and lose focus) on every keystroke.
  output$import_id_preview <- renderUI({
    nm <- input$import_name
    if (is.null(nm) || trimws(nm) == "") {
      return(div(class="text-danger", style="font-size:.72rem", "Name required."))
    }
    id <- slugify_ticker(nm, names(INDEX_CFG))
    div(class="text-muted", style="font-size:.72rem",
        sprintf("Will appear as: %s  (%s)", nm, id))
  })

  # Writes an index's data file into DATA_DIR. Local mode only: in hosted
  # mode imports stay in this session's memory and nothing touches the disk.
  save_index_file <- function(ticker, df) {
    if (IS_HOSTED) return(invisible(NULL))
    out <- data.frame(date=format(df$date, "%Y-%m-%d"), value=df$value)
    write.csv(out, file.path(DATA_DIR, paste0(ticker, "_daily.csv")), row.names=FALSE)
  }

  observeEvent(input$import_confirm, {
    f <- input$import_file
    if (is.null(f)) return(invisible(NULL))
    v <- validate_csv_for_import(f$datapath)   # re-validate defensively at confirm-time
    if (!isTRUE(v$ok)) return(invisible(NULL))

    # ── Multi-index: add every detected column as its own index ───────────────
    if (identical(v$mode, "multi")) {
      added   <- character(0)
      failed  <- character(0)
      for (idx_info in v$indices) {
        local({                         # local() gives each iteration its own
          info <- idx_info              # snapshot of loop variables, preventing
          tryCatch({                    # the classic R for-loop closure trap
            nm     <- info$name
            df     <- info$df
            ticker <- slugify_ticker(nm, names(INDEX_CFG))
            color  <- next_color(INDEX_CFG)
            filename <- paste0(ticker, "_daily.csv")

            save_index_file(ticker, df)

            reg_row <- data.frame(ticker=ticker, label=nm, color=color,
                                   obs_years=min(10, max(1, floor(series_years(df) / 2))),
                                   default_n=20L, filename=filename, stringsAsFactors=FALSE)
            if (!IS_HOSTED) write_registry(rbind(read_registry(), reg_row))

            INDEX_CFG[[ticker]]   <<- list(label=nm, color=color,
                                            obs_years=reg_row$obs_years, default_n=20L)
            raw_data[[ticker]]    <<- df
            all_returns[[ticker]] <<- compute_returns_for_index(df)

            added   <<- c(added,  sprintf("%s (%s)", nm, ticker))
          }, error=function(e) {
            failed  <<- c(failed, info$name)
          })
        })
      }

      data_version(data_version() + 1)
      choices <- current_index_choices(INDEX_CFG)
      updateSelectizeInput(session, "ticker",    choices=choices, selected=input$ticker)
      updateSelectizeInput(session, "ts_tickers",choices=choices, selected=input$ts_tickers)

      ok_msg <- if (length(added) > 0) {
        paste0("Added ", length(added), " ind",
               if (length(added)==1) "ex" else "ices", ": ",
               paste(added, collapse=", "))
      } else ""
      err_msg <- if (length(failed) > 0) {
        paste0(if (nchar(ok_msg) > 0) "; ", "Failed: ", paste(failed, collapse=", "))
      } else ""
      import_result(list(path=f$datapath, ok=(length(failed)==0),
                          message=paste0(ok_msg, err_msg)))
      return(invisible(NULL))
    }

    # ── Single-index: Replace or Add (original logic) ─────────────────────────
    df <- v$df

    if (identical(input$import_mode, "replace")) {
      ticker <- input$import_replace_target
      if (is.null(ticker) || ticker == "") return(invisible(NULL))

      tryCatch({
        save_index_file(ticker, df)

        new_obs_years <- min(10, max(1, floor(series_years(df) / 2)))
        INDEX_CFG[[ticker]]$obs_years <<- new_obs_years
        raw_data[[ticker]]    <<- df
        all_returns[[ticker]] <<- compute_returns_for_index(df)

        if (!IS_HOSTED) {
          registry <- read_registry()
          if (ticker %in% registry$ticker) {
            registry$obs_years[registry$ticker == ticker] <- new_obs_years
            write_registry(registry)
          }
        }

        data_version(data_version() + 1)
        import_result(list(path=f$datapath, ok=TRUE, message=sprintf(
          "Replaced \u201c%s (%s)\u201d data \u2014 %s rows, %s \u2192 %s",
          INDEX_CFG[[ticker]]$label, ticker,
          format(nrow(df), big.mark=","), fmt_date(min(df$date)), fmt_date(max(df$date)))))
      }, error=function(e) {
        import_result(list(path=f$datapath, ok=FALSE,
                            message=paste("Replace failed:", conditionMessage(e))))
      })

    } else {
      nm <- trimws(input$import_name)
      if (is.null(nm) || nm == "") return(invisible(NULL))

      tryCatch({
        ticker <- slugify_ticker(nm, names(INDEX_CFG))
        color  <- next_color(INDEX_CFG)
        filename <- paste0(ticker, "_daily.csv")

        save_index_file(ticker, df)

        reg_row <- data.frame(ticker=ticker, label=nm, color=color,
                               obs_years=min(10, max(1, floor(series_years(df) / 2))),
                               default_n=20L, filename=filename, stringsAsFactors=FALSE)
        if (!IS_HOSTED) write_registry(rbind(read_registry(), reg_row))

        INDEX_CFG[[ticker]]   <<- list(label=nm, color=color,
                                        obs_years=reg_row$obs_years, default_n=20L)
        raw_data[[ticker]]    <<- df
        all_returns[[ticker]] <<- compute_returns_for_index(df)
        data_version(data_version() + 1)

        choices <- current_index_choices(INDEX_CFG)
        updateSelectizeInput(session, "ticker",    choices=choices, selected=input$ticker)
        updateSelectizeInput(session, "ts_tickers",choices=choices, selected=input$ts_tickers)

        import_result(list(path=f$datapath, ok=TRUE, message=sprintf(
          "Added \u201c%s (%s)\u201d \u2014 %s rows, %s \u2192 %s",
          nm, ticker, format(nrow(df), big.mark=","),
          fmt_date(min(df$date)), fmt_date(max(df$date)))))
      }, error=function(e) {
        import_result(list(path=f$datapath, ok=FALSE,
                            message=paste("Import failed:", conditionMessage(e))))
      })
    }
  })

  # ── Manage Indices: delete an imported index ────────────────────────────
  # Built-ins are never listed here (they can be Replaced above, but not
  # deleted — their data ships with the app). Each row's delete icon is a
  # plain HTML button (not an actionButton) so any number of rows can be
  # rendered dynamically without pre-registering an input id per row; it
  # calls Shiny.setInputValue() directly, the same trick used by the
  # click-to-sort table headers in html_table() above.
  output$manage_indices <- renderUI({
    data_version()   # re-list after any add/delete
    imported <- setdiff(names(INDEX_CFG), BUILTIN_TICKERS)
    if (length(imported) == 0) return(NULL)

    tagList(
      tags$p(class="text-uppercase fw-semibold text-muted mb-1 mt-2",
             style="font-size:.7rem; letter-spacing:.06em", "Manage Imported Indices"),
      lapply(imported, function(tk) {
        div(class="idx-manage-row",
          span(sprintf("%s  (%s)", INDEX_CFG[[tk]]$label, tk)),
          tags$button(class="idx-del-btn", type="button", title="Delete this index",
            onclick=sprintf("Shiny.setInputValue('delete_request', '%s', {priority:'event'})", tk),
            icon("trash-can"))
        )
      })
    )
  })

  observeEvent(input$delete_request, {
    tk_del <- input$delete_request
    if (is.null(tk_del) || !(tk_del %in% names(INDEX_CFG)) || tk_del %in% BUILTIN_TICKERS) {
      return(invisible(NULL))
    }
    showModal(modalDialog(
      title = "Delete index?",
      sprintf("This removes \u201c%s (%s)\u201d%s. This can't be undone.",
              INDEX_CFG[[tk_del]]$label, tk_del,
              if (IS_HOSTED) " from this session" else " and its data file"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_delete", "Delete", class="btn-danger")
      )
    ))
  })

  observeEvent(input$confirm_delete, {
    tk_del <- input$delete_request
    removeModal()
    if (is.null(tk_del) || !(tk_del %in% names(INDEX_CFG)) || tk_del %in% BUILTIN_TICKERS) {
      return(invisible(NULL))
    }

    tryCatch({
      if (!IS_HOSTED) {
        path <- file.path(DATA_DIR, paste0(tk_del, "_daily.csv"))
        if (file.exists(path)) file.remove(path)

        registry <- read_registry()
        write_registry(registry[registry$ticker != tk_del, , drop=FALSE])
      }

      INDEX_CFG[[tk_del]]   <<- NULL
      raw_data[[tk_del]]    <<- NULL
      all_returns[[tk_del]] <<- NULL
      data_version(data_version() + 1)

      choices <- current_index_choices(INDEX_CFG)
      # If the deleted ticker was selected anywhere, fall back sensibly
      # rather than leaving the selector pointing at a now-missing choice.
      new_ticker_sel <- if (identical(input$ticker, tk_del)) unname(choices[1]) else input$ticker
      new_ts_sel     <- setdiff(input$ts_tickers, tk_del)
      if (length(new_ts_sel) == 0 && length(choices) > 0) new_ts_sel <- unname(choices[1])

      updateSelectizeInput(session, "ticker", choices=choices, selected=new_ticker_sel)
      updateSelectizeInput(session, "ts_tickers", choices=choices, selected=new_ts_sel)
    }, error=function(e) {
      showNotification(paste("Delete failed:", conditionMessage(e)), type="error")
    })
  })

  # ── Gray out sidebar controls that don't apply to the active tab ───────────
  # Each control's wrapper <div> gets/loses ".ctrl-disabled" (CSS dims it and
  # sets pointer-events:none — see APP_CSS), which also visually covers
  # "rw_mode" since it lives inside the "ctrl_obs" wrapper div.
  observeEvent(input$main_tab, {
    active <- RELEVANCE[[input$main_tab]]
    if (is.null(active)) active <- character(0)
    for (ctrl in ALL_CTRLS) {
      gray_out(CTRL_DIV[[ctrl]], !(ctrl %in% active))
    }
  }, ignoreInit = FALSE)

  # ── Sidebar info ─────────────────────────────────────────────────────────────
  output$index_pill <- renderUI({
    clr <- cfg()$color
    tags$span(class="legend-pill", style=paste0("background:", clr), cfg()$label)
  })

  output$data_info <- renderUI({
    df       <- raw()
    yrs      <- series_years(df)
    full_yrs <- series_years(raw_data[[tk()]])
    limited  <- !is.null(cutoff_date())

    tags$div(class="text-muted", style="font-size:.78rem; line-height:1.7",
      tags$b(format(nrow(df), big.mark=","), " trading days"), br(),
      fmt_date(min(df$date)), " \u2192 ", fmt_date(max(df$date)), br(),
      sprintf("\u2248 %.1f years of data", yrs),
      if (limited) tagList(br(),
        tags$span(style="color:#D97706; font-weight:600",
          if (yrs < full_yrs - 0.05)
            sprintf("Limited to last %s yr (full history: %.1f yr)", format(input$limit_years), full_yrs)
          else
            "Limit exceeds full history \u2014 showing all data"
        ))
    )
  })

  # ── Series-length badge: which index, how much data, what date range ───────
  # Shown in BOTH the Analysis and Rolling Windows card headers (they're the
  # two tabs driven by the single sidebar "Select Index" control) so it's
  # visible without having to look back at the sidebar. Built once here and
  # rendered into two separate outputs since a single Shiny output can only
  # bind to one place in the UI.
  series_length_badge_content <- function() {
    df       <- raw()
    yrs      <- series_years(df)
    full_yrs <- series_years(raw_data[[tk()]])
    limited  <- !is.null(cutoff_date()) && yrs < full_yrs - 0.05

    span(class="text-muted ms-2", style="font-size:.78rem; font-weight:400",
         sprintf("%s  \u2022  %s rows  \u2022  %.1f years of data (%s \u2192 %s)%s",
                 cfg()$label, format(nrow(df), big.mark=","), yrs,
                 fmt_date(min(df$date)), fmt_date(max(df$date)),
                 if (limited)
                   sprintf("  \u2014 limited from %.1f yr full history", full_yrs)
                 else ""))
  }
  output$series_length_badge         <- renderUI(series_length_badge_content())
  output$series_length_badge_rolling <- renderUI(series_length_badge_content())

  # ── Price history (with worst/best episode annotations) ─────────────────────
  # Format a price-level axis value with a "k" suffix above 1000 (mirrors
  # the look of plotly's default log-axis tick labels, e.g. "10k"). Below
  # 1000 the number of decimals is chosen per-value so FX-rate-scale ticks
  # (e.g. 0.8, 1.2, 1.6) don't all round down to the same integer.
  fmt_level <- function(x) {
    is_close <- function(a, b) abs(a - b) < 1e-9 * max(1, abs(b))
    vapply(x, function(v) {
      if (v >= 1000) {
        d <- if (is_close(v %% 1000, 0)) 0 else 1
        paste0(formatC(v/1000, format="f", digits=d), "k")
      } else if (is_close(round(v), v)) {
        formatC(v, format="f", digits=0, big.mark=",")
      } else if (is_close(round(v, 1), v)) {
        formatC(v, format="f", digits=1)
      } else {
        formatC(v, format="f", digits=2)
      }
    }, character(1))
  }

  output$price_chart <- renderPlot({
    df  <- raw()
    clr <- cfg()$color
    ep  <- compute_episodes(ret_df(), k=1)

    dates  <- as.Date(df$date)
    values <- df$value
    ylim   <- range(values, na.rm=TRUE)
    xlim   <- range(dates)

    par(mar=c(3, 4.2, 0.7, 1), mgp=c(2.2, 0.6, 0), family="sans")
    plot(NA, xlim=xlim, ylim=ylim, log="y", axes=FALSE, xlab="", ylab="")

    # Shaded vertical bands marking the single worst/best N-day episode
    # (drawn before the price line so the line stays on top)
    if (!is.null(ep)) {
      for (i in seq_len(nrow(ep))) {
        row      <- ep[i, ]
        is_worst <- row$Type == "Worst"
        shade  <- adjustcolor(if (is_worst) "#DC2626" else "#16A34A", alpha.f=0.12)
        line_c <- adjustcolor(if (is_worst) "#DC2626" else "#16A34A", alpha.f=0.5)
        rect(row$StartDate, ylim[1], row$EndDate, ylim[2], col=shade, border=line_c)
      }
    }

    lines(dates, values, col=clr, lwd=1.4)

    yticks <- axTicks(2)
    axis(2, at=yticks, labels=fmt_level(yticks), las=1,
         col="#CBD5E1", col.axis="#475569", cex.axis=0.85)
    axis.Date(1, x=dates, col="#CBD5E1", col.axis="#475569", cex.axis=0.85)
    box(col="#E2E8F0")
    mtext("Level (log scale)", side=2, line=3.2, cex=0.88, col="#334155")

    # Callout labels for the worst/best episode: a short arrow from a text
    # box (placed near the top for Worst, near the bottom for Best) pointing
    # at the episode's end date — mirrors the plotly annotation arrows.
    # Alignment flips automatically when the episode is too close to the
    # left/right edge of the chart, so the label text never runs off-plot.
    if (!is.null(ep)) {
      x_pad   <- diff(as.numeric(xlim)) * 0.045
      x_min   <- as.numeric(xlim[1])
      x_max   <- as.numeric(xlim[2])
      for (i in seq_len(nrow(ep))) {
        row      <- ep[i, ]
        is_worst <- row$Type == "Worst"
        col_i    <- if (is_worst) "#DC2626" else "#16A34A"
        anchor_x <- as.numeric(row$EndDate)
        anchor_y <- if (is_worst) ylim[2] / 1.06 else ylim[1] * 1.06
        label <- sprintf("%s %dd: %s\n%s \u2192 %s",
                          row$Type, n_days(), fmt_pct(row$Return), row$Start, row$End)

        # Preferred side: left for Worst, right for Best (matches the
        # original plotly arrow directions) — but flip if there isn't
        # enough room on that side, so text always grows inward.
        prefer_left <- is_worst
        room_left   <- anchor_x - x_min
        room_right  <- x_max - anchor_x
        put_left    <- if (prefer_left) room_left > x_pad * 3 else room_right < x_pad * 3

        if (put_left) {
          text_x <- anchor_x - x_pad
          adj    <- c(1, 0.5)
        } else {
          text_x <- anchor_x + x_pad
          adj    <- c(0, 0.5)
        }
        arrows(text_x, anchor_y, anchor_x, anchor_y, length=0.06, col=col_i, lwd=1.1)
        text(text_x, anchor_y, label, adj=adj, cex=0.82, col=col_i, xpd=TRUE)
      }
    }
  }, bg="transparent")

  # ── Stats table ────────────────────────────────────────────────────────────
  output$stats_table <- renderUI({
    st <- compute_stats(ret_df()$ret)
    if (is.null(st)) return(NULL)

    st$Display <- ifelse(
      st$Statistic == "Observations (M-N)",
      format(as.integer(st$Value), big.mark=","),
      fmt_pct(st$Value)
    )
    df_show <- data.frame(Statistic=st$Statistic, Value=st$Display,
                          stringsAsFactors=FALSE)

    highlight <- c("Worst 0.05%","Worst 0.1%","Worst 1%","Worst 5%",
                    "Best 5%","Best 1%","Best 0.1%","Best 0.05%","Best (Max)")
    highlight_bg <- c(rep("#FEF2F2", 4), rep("#F0FDF4", 5))

    html_table(df_show, numeric_cols="Value", row_style=function(i) {
      idx <- match(df_show$Statistic[i], highlight)
      if (!is.na(idx)) list(`background-color`=highlight_bg[idx]) else NULL
    })
  })

  # ── Return distribution histogram (linear/log y-axis toggle) ────────────────
  output$hist_total_badge <- renderUI({
    total_n <- sum(!is.na(ret_df()$ret))
    span(class="text-muted", style="font-size:.78rem; font-weight:400; white-space:nowrap",
         sprintf("Total observations: %s", format(total_n, big.mark=",")))
  })

  output$hist_chart <- renderPlot({
    r <- ret_df()$ret[!is.na(ret_df()$ret)]
    if (length(r) < 3) return(NULL)
    n       <- n_days()
    total_n <- length(r)

    h   <- hist(r, breaks=21, plot=FALSE)
    pct <- h$counts / total_n * 100
    bar_colors <- ifelse(h$mids < 0, "#DC2626", "#16A34A")
    log_y <- identical(input$hist_yscale, "log")

    xlim <- range(h$breaks)
    if (log_y) {
      ylim <- c(0.8, max(h$counts) * 2.2)   # extra headroom for bar labels
    } else {
      ylim <- c(0, max(h$counts) * 1.18)
    }

    par(mar=c(3.2, 4.5, 0.7, 1), mgp=c(2.2, 0.6, 0), family="sans")
    plot(NA, xlim=xlim, ylim=ylim, axes=FALSE, xlab="", ylab="",
         log=if (log_y) "y" else "")

    for (i in seq_along(h$counts)) {
      if (h$counts[i] > 0) {
        y0 <- if (log_y) 1 else 0
        rect(h$breaks[i], y0, h$breaks[i+1], h$counts[i],
             col=bar_colors[i], border="white", lwd=0.6)
        text(h$mids[i], h$counts[i], sprintf("%.2f%%", pct[i]),
             pos=3, cex=0.68, col="#64748B", xpd=TRUE)
      }
    }

    abline(v=0, col="#94A3B8", lwd=1.5)

    xticks <- pretty(xlim)
    axis(1, at=xticks, labels=sprintf("%.1f%%", xticks * 100),
         col="#CBD5E1", col.axis="#475569", cex.axis=0.85)
    axis(2, las=1, col="#CBD5E1", col.axis="#475569", cex.axis=0.85)
    mtext(paste0(n, "-day return"), side=1, line=2.1, cex=0.88, col="#334155")
    mtext("Frequency (count)", side=2, line=3.3, cex=0.88, col="#334155")
  }, bg="transparent")

  output$episodes_context <- renderUI({
    total_n <- sum(!is.na(ret_df()$ret))
    span(class="text-muted ms-2", style="font-size:.78rem; font-weight:400",
         sprintf("(out of %s %d-day observations \u2014 shaded on the price chart)",
                 format(total_n, big.mark=","), n_days()))
  })

  # ── Episodes table ─────────────────────────────────────────────────────────
  output$episodes_table <- renderUI({
    ep <- compute_episodes(ret_df())
    if (is.null(ep)) return(NULL)
    ep_show <- ep[, c("Rank","Type","Return","Start","End")]
    ep_show$Return <- fmt_pct(ep$Return)
    ep_show$Rank   <- as.character(ep_show$Rank)

    # Colour-code the "Worst"/"Best" label text itself (matches the
    # red/green convention used on the price chart and histogram)
    type_color <- ifelse(ep_show$Type == "Worst", "#DC2626", "#16A34A")
    ep_show$Type <- sprintf('<span style="font-weight:700; color:%s">%s</span>',
                            type_color, ep_show$Type)

    row_bg <- ifelse(ep$Type == "Worst", "#FEF2F2", "#F0FDF4")

    html_table(ep_show, numeric_cols="Return",
               row_style=function(i) list(`background-color`=row_bg[i]))
  })

  # ── Rolling Windows chart ──────────────────────────────────────────────────
  # Unfiltered: every anchor with >=2 obs, tagged with Coverage (yrs) & Complete
  rw_data_full <- reactive({
    req(is_pos_num(input$obs_years))
    compute_rolling_windows(ret_df(), input$obs_years, n_anchors=40)
  })

  # Filtered per the "Anchor Windows" switcher
  rw_data <- reactive({
    df <- rw_data_full()
    if (is.null(df)) return(NULL)
    if (identical(input$rw_mode, "full")) df[df$Complete, , drop=FALSE] else df
  })

  output$rw_avg_badges <- renderUI({
    full_df <- rw_data_full()
    if (is.null(full_df) || nrow(full_df) == 0) return(NULL)
    complete_df <- full_df[full_df$Complete, ]
    n_partial <- sum(!full_df$Complete)
    n_total   <- nrow(full_df)

    if (nrow(complete_df) == 0) {
      return(span(class="text-muted ms-2", style="font-size:.78rem",
        sprintf("No anchors have a full %s-yr window for this index \u2014 averages unavailable. Try \u201cShow all (flag partial)\u201d.",
                input$obs_years)))
    }

    avg_worst <- mean(complete_df$Worst, na.rm=TRUE)
    avg_best  <- mean(complete_df$Best,  na.rm=TRUE)

    note <- NULL
    if (n_partial > 0 && identical(input$rw_mode, "full")) {
      note <- sprintf("showing %d of %d anchors with a full %s-yr window",
                       nrow(complete_df), n_total, input$obs_years)
    } else if (n_partial > 0 && identical(input$rw_mode, "all")) {
      note <- sprintf("%d of %d anchors have a partial window (open markers, shaded); excluded from averages",
                       n_partial, n_total)
    }

    tagList(
      span(class="legend-pill", style="background:#DC2626",
           sprintf("Avg Worst: %s", fmt_pct(avg_worst))),
      span(class="legend-pill", style="background:#16A34A",
           sprintf("Avg Best: %s", fmt_pct(avg_best))),
      if (!is.null(note)) span(class="text-muted ms-2", style="font-size:.78rem", note)
    )
  })

  output$rw_chart <- renderPlot({
    df <- rw_data()
    n  <- n_days()

    if (is.null(df) || nrow(df) == 0) {
      plot.new()
      text(0.5, 0.5,
           sprintf("No anchors have a full %s-year window for %s.\nTry reducing the Observation Period, or switch to \u201cShow all (flag partial)\u201d.",
                   input$obs_years, cfg()$label),
           cex=1, col="#64748B")
      return(invisible(NULL))
    }

    df    <- df[order(df$Anchor), ]
    dates <- as.Date(df$Anchor)

    full_df     <- rw_data_full()
    complete_df <- full_df[full_df$Complete, ]
    avg_worst   <- if (nrow(complete_df) > 0) mean(complete_df$Worst, na.rm=TRUE) else NA_real_
    avg_best    <- if (nrow(complete_df) > 0) mean(complete_df$Best,  na.rm=TRUE) else NA_real_

    show_all    <- identical(input$rw_mode, "all")
    has_partial <- any(!df$Complete)

    ylim <- range(c(df$Worst, df$Best, avg_worst, avg_best), na.rm=TRUE)
    pad  <- diff(ylim) * 0.10
    legend_y_top <- ylim[2] + pad         # "normal" top, used for shading/labels
    ylim <- c(ylim[1] - pad, ylim[2] + pad + diff(ylim) * 0.16)  # +legend headroom
    xlim <- range(dates)

    par(mar=c(3.2, 4, 0.7, 1), mgp=c(2.2, 0.6, 0), family="sans")
    plot(NA, xlim=xlim, ylim=ylim, axes=FALSE, xlab="", ylab="")

    # Shaded "partial window" region + caption (Show-all mode only)
    if (show_all && has_partial) {
      partial_dates <- dates[!df$Complete]
      x0 <- min(partial_dates)
      x1 <- min(max(partial_dates) + 182, max(dates))   # extend ~6mo for visual grouping
      rect(x0, ylim[1], x1, legend_y_top, col=adjustcolor("#94A3B8", alpha.f=0.18), border=NA)
      text(x0, legend_y_top,
           sprintf("Partial window\n(< %s yr of data)", input$obs_years),
           adj=c(0, 1), cex=0.75, col="#64748B")
    }

    yticks <- pretty(ylim)
    axis(2, at=yticks, labels=sprintf("%.1f%%", yticks * 100), las=1,
         col="#CBD5E1", col.axis="#475569", cex.axis=0.85)
    abline(h=0, col="#94A3B8")

    # Shaded band between the Worst and Best lines (replaces plotly's "tonexty")
    polygon(c(dates, rev(dates)), c(df$Worst, rev(df$Best)),
            col=adjustcolor("#64748B", alpha.f=0.08), border=NA)

    lines(dates, df$Worst, col="#DC2626", lwd=2)
    points(dates, df$Worst, col="#DC2626", pch=16, cex=0.55)
    lines(dates, df$Best, col="#16A34A", lwd=2)
    points(dates, df$Best, col="#16A34A", pch=16, cex=0.55)

    if (!is.na(avg_worst)) abline(h=avg_worst, col="#DC2626", lwd=1.5, lty=2)
    if (!is.na(avg_best))  abline(h=avg_best,  col="#16A34A", lwd=1.5, lty=2)

    # Open-circle markers on partial-coverage anchors (Show-all mode only)
    if (show_all && has_partial) {
      pdf <- df[!df$Complete, ]
      points(as.Date(pdf$Anchor), pdf$Worst, pch=1, cex=1.3, col="#DC2626", lwd=2)
      points(as.Date(pdf$Anchor), pdf$Best,  pch=1, cex=1.3, col="#16A34A", lwd=2)
    }

    axis.Date(1, x=dates, col="#CBD5E1", col.axis="#475569", cex.axis=0.85)
    box(col="#E2E8F0")
    mtext("Anchor Date", side=1, line=2.1, cex=0.88, col="#334155")
    mtext(paste0(n, "-day return"), side=2, line=3.2, cex=0.88, col="#334155")

    leg <- list(text=c("Worst","Best"), col=c("#DC2626","#16A34A"),
                 lty=c(1,1), pch=c(16,16))
    if (!is.na(avg_worst))
      leg <- Map(c, leg, list(text="Avg Worst", col="#DC2626", lty=2, pch=NA))
    if (!is.na(avg_best))
      leg <- Map(c, leg, list(text="Avg Best",  col="#16A34A", lty=2, pch=NA))
    if (show_all && has_partial)
      leg <- Map(c, leg, list(text="Partial window", col="#64748B", lty=NA, pch=1))

    legend("top", inset=c(0, 0.01), legend=leg$text, col=leg$col,
           lty=leg$lty, pch=leg$pch, lwd=1.5, horiz=TRUE, bty="n",
           cex=0.78, seg.len=1.4, text.col="#334155")
  }, bg="transparent")

  output$rw_table <- renderUI({
    df <- rw_data()
    if (is.null(df) || nrow(df) == 0) return(NULL)

    window <- ifelse(df$Complete, "Full", "Partial")
    show <- data.frame(
      Anchor          = fmt_date(df$Anchor),
      Worst           = fmt_pct(df$Worst),
      `Worst Start`   = fmt_date(df$WorstStart),
      `Worst End`     = fmt_date(df$WorstEnd),
      Best            = fmt_pct(df$Best),
      `Best Start`    = fmt_date(df$BestStart),
      `Best End`      = fmt_date(df$BestEnd),
      `Data Coverage` = sprintf("%.1f / %s yr", df$Coverage, format(input$obs_years)),
      Window          = window,
      stringsAsFactors=FALSE, check.names=FALSE
    )

    html_table(show, numeric_cols=c("Worst","Best"), right_cols="Data Coverage",
               height="180px",
               row_style=function(i) {
                 if (window[i] == "Partial")
                   list(`background-color`="#F8FAFC", color="#94A3B8", `font-style`="italic")
                 else NULL
               })
  })

  # ── Term Structure chart (multi-index) ───────────────────────────────────────
  ts_data <- reactive({
    data_version()
    tickers <- input$ts_tickers
    if (is.null(tickers) || length(tickers) == 0) return(NULL)
    req(is_pos_num(input$rank_k))
    rk <- max(1L, as.integer(input$rank_k))
    ranks <- if (identical(input$rank_mode, "range")) seq_len(rk) else rk
    compute_term_structure_multi(tickers, raw_data, all_returns, ranks=ranks,
                                  limit_active=input$limit_active,
                                  limit_years=input$limit_years)
  })

  # ── Per-index date range list (Term Structure) ──────────────────────────────
  # Term Structure can show several indices at once, each with its own
  # history length and (if Data Window is active) its own "last X years"
  # slice measured from ITS OWN most recent date (see Definitions \u00a79) \u2014
  # so unlike the single-ticker badge above, this needs one line per index.
  output$ts_range_list <- renderUI({
    data_version()
    tickers <- input$ts_tickers
    if (is.null(tickers) || length(tickers) == 0) return(NULL)

    rows <- lapply(tickers, function(t) {
      full <- raw_data[[t]]
      if (is.null(full)) return(NULL)
      cd        <- years_cutoff(full$date, input$limit_active, input$limit_years)
      eff_start <- if (!is.null(cd)) max(min(full$date), cd) else min(full$date)
      eff_end   <- max(full$date)
      eff_yrs   <- series_years(data.frame(date=c(eff_start, eff_end)))
      full_yrs  <- series_years(full)
      limited   <- !is.null(cd) && eff_yrs < full_yrs - 0.05
      n_rows    <- sum(full$date >= eff_start)

      div(class="idx-range-item",
        span(class="idx-range-dot", style=paste0("background:", INDEX_CFG[[t]]$color)),
        sprintf("%s (%s): %s \u2192 %s, %s rows, %.1f yr%s",
                INDEX_CFG[[t]]$label, t, fmt_date(eff_start), fmt_date(eff_end),
                format(n_rows, big.mark=","), eff_yrs,
                if (limited) sprintf(" (full: %.1f yr)", full_yrs) else "")
      )
    })
    rows <- Filter(Negate(is.null), rows)
    if (length(rows) == 0) return(NULL)
    div(class="idx-range-row", rows)
  })

  output$ts_chart <- renderPlot({
    df <- ts_data()
    if (is.null(df) || nrow(df) == 0) return(invisible(NULL))
    rk      <- max(1L, as.integer(input$rank_k))
    mode    <- input$rank_mode
    tickers <- unique(df$Ticker)
    n_tk    <- length(tickers)
    ranks   <- sort(unique(df$Rank))
    n_ranks <- length(ranks)
    multi   <- identical(mode, "range") && n_ranks > 1

    xlim <- range(N_VALUES)
    ylim <- range(c(df$Worst, df$Best), na.rm=TRUE)
    # extra headroom: more when multi-rank legend is taller
    ylim <- c(ylim[1] - diff(ylim)*0.08, ylim[2] + diff(ylim)*(if (multi) 0.38 else 0.34))

    par(mar=c(3.2, 4, 0.7, 1), mgp=c(2.2, 0.6, 0), family="sans")
    plot(NA, xlim=xlim, ylim=ylim, axes=FALSE, xlab="", ylab="")

    yticks <- pretty(ylim)
    axis(2, at=yticks, labels=sprintf("%.1f%%", yticks*100), las=1,
         col="#CBD5E1", col.axis="#475569", cex.axis=0.85)
    abline(h=0, col="#94A3B8")
    axis(1, at=N_VALUES, labels=paste0(N_VALUES, "d"),
         col="#CBD5E1", col.axis="#475569", cex.axis=0.8)
    box(col="#E2E8F0")

    leg_items <- character(0)
    leg_col   <- character(0)
    leg_lty   <- integer(0)
    leg_pch   <- integer(0)

    for (tkr in tickers) {
      clr <- INDEX_CFG[[tkr]]$color
      lbl <- INDEX_CFG[[tkr]]$label
      sub_tk <- df[df$Ticker == tkr, ]

      for (rki in ranks) {
        sub <- sub_tk[sub_tk$Rank == rki, ]
        sub <- sub[order(sub[["N (days)"]]), ]
        if (nrow(sub) == 0) next

        # Rank 1 = most extreme = full colour; higher ranks fade toward 30%
        alpha <- if (multi) max(0.30, 1.0 - (rki - 1) * 0.70 / max(1, n_ranks - 1)) else 1.0
        c_rki <- adjustcolor(clr, alpha.f=alpha)
        lwd_w <- if (multi) max(1.0, 2.5 - (rki - 1)*0.4) else 2.5
        lwd_b <- if (multi) max(0.8, 2.0 - (rki - 1)*0.4) else 2.0

        lines(sub[["N (days)"]], sub$Worst, col=c_rki, lwd=lwd_w, lty=1)
        points(sub[["N (days)"]], sub$Worst, col=c_rki, pch=16, cex=0.9)
        lines(sub[["N (days)"]], sub$Best,  col=c_rki, lwd=lwd_b, lty=3)
        points(sub[["N (days)"]], sub$Best,  col=c_rki, bg="white", pch=23, cex=0.9)
      }

      # Legend entry — one pair per ticker in multi mode (note rank gradient),
      # two entries per ticker in single mode (one Worst, one Best line)
      if (multi) {
        leg_items <- c(leg_items, paste0(lbl, " (Worst)"), paste0(lbl, " (Best)"))
        leg_col   <- c(leg_col, clr, clr)
        leg_lty   <- c(leg_lty, 1L, 3L)
        leg_pch   <- c(leg_pch, 16L, 23L)
      } else {
        leg_items <- c(leg_items, paste0(lbl, " - Worst"), paste0(lbl, " - Best"))
        leg_col   <- c(leg_col, clr, clr)
        leg_lty   <- c(leg_lty, 1L, 3L)
        leg_pch   <- c(leg_pch, 16L, 23L)
      }
    }

    y_label <- if (multi) sprintf("Ranks 1\u2013%d", rk) else sprintf("Rank-%d", rk)
    mtext("Window Length N (days)", side=1, line=2.1, cex=0.88, col="#334155")
    mtext(paste0(y_label, " Return"), side=2, line=3.2, cex=0.88, col="#334155")
    if (multi)
      mtext("darker = more extreme rank", side=3, line=-0.7, cex=0.72, col="#64748B", adj=0.5)

    legend("top", inset=c(0, 0.01), legend=leg_items, col=leg_col,
           lty=leg_lty, pch=leg_pch, pt.bg="white",
           lwd=1.5, ncol=n_tk, bty="n", cex=0.72, seg.len=1.3,
           text.col="#334155")
  }, bg="transparent")

  # Simple single-column click-to-sort state for the Term Structure table
  # (replaces DT's built-in column sorting). Default: grouped by Ticker,
  # then N (days) within each ticker.
  ts_sort <- reactiveValues(col="Ticker", dir="asc")
  observeEvent(input$ts_sort_click, {
    col <- input$ts_sort_click
    if (identical(ts_sort$col, col)) {
      ts_sort$dir <- if (identical(ts_sort$dir, "asc")) "desc" else "asc"
    } else {
      ts_sort$col <- col
      ts_sort$dir <- "asc"
    }
  })

  output$ts_table <- renderUI({
    df <- ts_data()
    if (is.null(df)) return(NULL)
    # Labels of imported indices are user-supplied; html_table renders raw HTML
    df$Label <- htmltools::htmlEscape(sapply(df$Ticker, function(t) INDEX_CFG[[t]]$label))
    multi <- identical(input$rank_mode, "range") && length(unique(df$Rank)) > 1

    if (multi) {
      df <- df[, c("Ticker","Label","Rank","N (days)","Worst","Worst Start","Worst End",
                    "Best","Best End")]
      df <- df[order(df$Ticker, df$Rank, df[["N (days)"]]), ]
    } else {
      df <- df[, c("Ticker","Label","N (days)","Worst","Worst Start","Worst End",
                    "Best","Best End")]
      df <- df[order(df$Ticker, df[["N (days)"]]), ]
    }

    # The sort column can disappear (e.g. "Rank" after switching back to
    # Single rank mode); keep the default order then instead of emptying the table
    if (ts_sort$col %in% names(df)) {
      ord <- order(df[[ts_sort$col]], decreasing=identical(ts_sort$dir, "desc"))
      df  <- df[ord, ]
    }

    df$Worst <- fmt_pct(df$Worst)
    df$Best  <- fmt_pct(df$Best)

    html_table(df, numeric_cols=c("Worst","Best"), height="180px",
               sort_input_id="ts_sort_click",
               sort_col=ts_sort$col, sort_dir=ts_sort$dir)
  })

  # ── Definitions live example ───────────────────────────────────────────────
  # ── Rolling-window coverage diagram (static, illustrative) ──────────────────
  output$coverage_diagram <- renderPlot({
    draw_coverage_diagram()
  }, bg="transparent")

  output$def_example_table <- renderUI({
    req(is_pos_num(input$def_n))
    n_def  <- max(1L, as.integer(input$def_n))
    prices <- c(100, 102, 98, 95, 90, 93, 97, 101, 99, 104, 108)
    rets   <- prices / lag_vec(prices, n_def) - 1
    df <- data.frame(
      Day    = as.character(seq_along(prices) - 1L),
      Price  = as.character(prices),
      Return = ifelse(is.na(rets), "\u2014", fmt_pct(rets)),
      stringsAsFactors = FALSE
    )
    html_table(df, numeric_cols="Return", striped=TRUE)
  })
}

# ══════════════════════════════════════════════════════════════════════════════
shinyApp(ui=ui, server=server)
