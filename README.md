# Financial Crises Analyzer — Posit Connect Cloud Deployment

A Shiny app for analysing historical financial crises across equity indices and FX pairs.

Live at: `https://YOUR-USERNAME.share.connect.posit.cloud/financial-crises-analyzer/`
(URL is set after the initial deploy — see Step 5 below.)

---

## One-time setup (~10 minutes)

### Step 1 — Create a free Posit Connect Cloud account

Go to [connect.posit.cloud](https://connect.posit.cloud) and sign up.
Note your **username** (shown in the URL once logged in).

---

### Step 2 — Push this repo to GitHub

Create a new GitHub repo (public or private) and push all files:

```bash
git init
git add .
git commit -m "Initial commit"
git remote add origin https://github.com/YOUR-USERNAME/financial-crises-analyzer.git
git push -u origin main
```

---

### Step 3 — Do the first deploy from RStudio (gets you the Content ID)

This one-time manual deploy is needed to obtain the **Content ID** (a UUID)
that tells GitHub Actions which piece of content to update on subsequent pushes.

```r
# In RStudio — run once
install.packages("rsconnect")

# Connect your account (get token from connect.posit.cloud → your name → Credentials)
rsconnect::connectCloudClientCredentials(
  clientId     = "YOUR_CLIENT_ID",
  clientSecret = "YOUR_CLIENT_SECRET",
  accountName  = "YOUR_USERNAME"
)

# Deploy
rsconnect::deployApp(
  appDir   = ".",          # run from inside the repo folder
  appName  = "financial-crises-analyzer",
  appTitle = "Financial Crises Analyzer"
)
```

After the deploy completes, open [connect.posit.cloud](https://connect.posit.cloud),
find the app, go to **Settings → URL**, and copy the UUID from the **Default URL**:

```
https://019eb78b-0c21-3b55-3fe6-38ae4d03dee4.share.connect.posit.cloud
         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
         This is your CONNECT_CONTENT_ID
```

---

### Step 4 — Create Posit Cloud credentials for GitHub Actions

Go to [login.posit.cloud/identity/credentials](https://login.posit.cloud/identity/credentials)
→ **New Credentials** → name it "GitHub Actions" → **Use with: Connect Cloud** → **OK**.

You'll see:
```r
rsconnect::connectCloudClientCredentials(
  clientId     = "01234567-...",   ← copy this
  clientSecret = "SuPeR/SeCrEt",  ← copy this
  account      = "your-username"
)
```

---

### Step 5 — Add secrets and variables to your GitHub repo

Go to your GitHub repo → **Settings → Secrets and variables → Actions**.

**Secrets** (hidden in logs):

| Name | Value |
|------|-------|
| `RSCONNECT_CLIENT_ID` | `clientId` from Step 4 |
| `RSCONNECT_CLIENT_SECRET` | `clientSecret` from Step 4 |

**Variables** (visible in logs):

| Name | Value |
|------|-------|
| `RSCONNECT_USERNAME` | Your Posit Connect Cloud username |
| `APP_NAME` | `financial-crises-analyzer` |
| `APP_TITLE` | `Financial Crises Analyzer` |
| `CONNECT_CONTENT_ID` | UUID from Step 3 |

---

### Step 6 — Push a commit and watch it deploy

Any commit to `main` or `master` now triggers an automatic deploy
(`.github/workflows/deploy.yml`; until `CONNECT_CONTENT_ID` is set, the deploy job is skipped).
You can also trigger one manually: **Actions tab → Deploy to Posit Connect Cloud → Run workflow**.

---

## Updating the app

Edit `app.R` locally, commit, and push. The GitHub Actions workflow picks it up,
restores packages from `renv.lock`, and redeploys within ~2–3 minutes.

## Running the tests

`tests/server.R` checks the server logic with shiny's built-in `testServer()`:
every output renders, cleared inputs don't error, window start dates are exact,
imports stay isolated per session on hosted deployments, and imported index
names are HTML-escaped. From the app folder:

```r
shiny::runTests()
```

The **Tests** workflow (`.github/workflows/test.yml`) runs the same tests, plus a
check that the app starts, on every pull request and on pushes to `main`.

---

## Notes on the free tier

- Apps are **public** on the free tier.
- The number of active applications may be limited; check your plan.
- Apps may sleep after inactivity (cold-start of a few seconds on the next visit).

## Notes on imports

Run locally, imported indices are saved to `data/` and are there after a restart.

On a hosted deployment one R process serves many visitors, so the app switches to
**hosted mode**: each visitor's imports (and any replaced or deleted index) stay in
memory for that browser session only. Other visitors never see them, and nothing
is written to disk. Imports are lost when the session ends — a yellow warning
banner in the Import panel makes this clear. The 7 built-in indices
(SPX, DJI, EUR/CHF, VIX, USD/CHF, GBP/CHF, JPY/CHF) are always available.

Hosted mode is detected automatically: the app bundle is read-only on Connect Cloud
and shinyapps.io, and Posit Connect and shinyapps.io set `R_CONFIG_ACTIVE`. To force
either mode, set the environment variable `FCA_HOSTED` to `true` or `false`.

---

## Updating the `renv.lock`

If you add R packages, regenerate the lockfile locally:

```r
install.packages("renv")
renv::init()
renv::snapshot()
```

Then commit the updated `renv.lock`.
