# deploy.R — publish the redenv Shiny app to shinyapps.io --------------------
#
# ONE-TIME SETUP
#   1. Create a free account at https://www.shinyapps.io
#   2. In the dashboard: Account > Tokens > (Show) > Copy to clipboard.
#      That copies a setAccountInfo(name=, token=, secret=) call already
#      filled in with your credentials.
#   3. Paste your three values into setAccountInfo() below.
#
#      >>> SECURITY: the token/secret are like a password. If you paste real
#      >>> values here, do NOT commit that change. (Better: run the copied
#      >>> setAccountInfo() line once in the R console — it's saved to your
#      >>> machine — and leave the placeholders here untouched.)
#
#   >>> GATE: do not deploy until Chris Lim has signed off on externalizing
#   >>> this derived HOLC-zone subset (same gate phxmob cleared 2026-07-08).
#
# DEPLOY / REDEPLOY
#   Run this whole script from the app folder. Redeploys reuse the same URL.

if (!requireNamespace("rsconnect", quietly = TRUE)) install.packages("rsconnect")
library(rsconnect)

# --- your shinyapps.io credentials (from Account > Tokens) -------------------
setAccountInfo(
  name   = "YOUR_ACCOUNT_NAME",   # e.g. "andrewcwees" -> andrewcwees.shinyapps.io
  token  = "YOUR_TOKEN",
  secret = "YOUR_SECRET"
)

# --- deploy ------------------------------------------------------------------
deployApp(
  appDir      = ".",
  appName     = "redenv",   # URL: https://YOUR_ACCOUNT_NAME.shinyapps.io/redenv
  appTitle    = "Redlining & Transportation-Sector Environmental Exposure",
  appFiles    = c("app.R", "redenv_data.rds"),  # lean runtime-only bundle
  forceUpdate = TRUE
)
