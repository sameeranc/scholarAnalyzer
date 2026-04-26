# ============================================================
# inst/app/app.R
#
# This file is used ONLY for shinyapps.io deployment.
# It is a thin wrapper that loads the package and launches the app.
#
# Deploy with:
#   rsconnect::deployApp("inst/app")
# ============================================================

library(scholarAnalyzer)

scholarAnalyzer::run_app()
