# scholarAnalyzer

An R package that provides a Shiny app for analyzing Google Scholar publications
and detecting self-citations by cross-referencing with the Semantic Scholar API.

## Features

- Fetch all publications from any Google Scholar profile
- Display total citations (Google Scholar as authoritative source)
- Detect self-citations — checks **all co-authors**, not just the profile owner
- Compute adjusted citation counts (GS total minus self-citations)
- Interactive tables with download options (self-citations, external, all)
- Citation pattern plots (with self vs external breakdown after analysis)

## Installation

### From GitHub (recommended)

```r
# Install devtools if you don't have it
install.packages("devtools")

# Install the package
devtools::install_github("YOUR_USERNAME/scholarAnalyzer")
```

### From local source

```r
# From the folder containing the scholarAnalyzer directory
devtools::install("scholarAnalyzer")
```

## Usage

```r
library(scholarAnalyzer)
run_app()
```

The app opens in your browser. Enter a Google Scholar ID to get started.

**Finding your Google Scholar ID:**  
Go to your profile. The URL looks like:  
`https://scholar.google.com/citations?user=XXXXXXXXXXXX`  
The string after `user=` is your ID.

---

## Package structure

```
scholarAnalyzer/
├── DESCRIPTION              # Package metadata and dependencies
├── NAMESPACE                # Exported functions and imports
├── .Rbuildignore
├── .gitignore
├── R/
│   ├── run_app.R            # Exported run_app() launch function
│   ├── ui.R                 # Shiny UI (app_ui)
│   ├── server.R             # Shiny server (app_server)
│   └── helpers.R            # All pure helper functions
├── man/
│   └── run_app.Rd           # Documentation for run_app()
└── inst/
    └── app/
        └── app.R            # Thin wrapper for shinyapps.io deployment
```

---

## Deploying to shinyapps.io (free)

### Step 1 — Install rsconnect

```r
install.packages("rsconnect")
```

### Step 2 — Connect your shinyapps.io account

1. Go to [shinyapps.io](https://www.shinyapps.io) and sign up (free)
2. Click your name (top-right) → **Tokens** → **Show** → **Copy to clipboard**
3. Paste and run the token command in R — it looks like:

```r
rsconnect::setAccountInfo(
  name   = "your-account-name",
  token  = "YOUR_TOKEN",
  secret = "YOUR_SECRET"
)
```

### Step 3 — Install and deploy

```r
# First install the package locally
devtools::install("path/to/scholarAnalyzer")

# Then deploy the inst/app/ folder
rsconnect::deployApp(
  appDir  = "path/to/scholarAnalyzer/inst/app",
  appName = "scholarAnalyzer"
)
```

Your app will be live at:  
`https://your-account-name.shinyapps.io/scholarAnalyzer`

### Updating after changes

```r
# Re-install the package with your changes
devtools::install("path/to/scholarAnalyzer")

# Redeploy
rsconnect::deployApp(
  appDir  = "path/to/scholarAnalyzer/inst/app",
  appName = "scholarAnalyzer"
)
```

---

## Development workflow

```r
# Load all functions without installing (faster during development)
devtools::load_all(".")

# Check for issues
devtools::check()

# Rebuild documentation
devtools::document()

# Run the app directly
run_app()
```

---

## How self-citation detection works

1. Each paper is looked up on **Semantic Scholar** via DOI (preferred) or title
2. The full list of citing papers is retrieved
3. A citing paper is flagged as a self-citation if **any** of the original
   paper's co-authors appears in the citing paper's author list
4. Matching uses both last-name comparison (robust to "J. Smith" vs "John Smith")
   and full lowercase name matching
5. **Adjusted citations = Google Scholar total − detected self-citations**

If a paper cannot be found on Semantic Scholar, the Google Scholar count is
shown unchanged and a warning is displayed.

---

## Dependencies

| Package | Purpose |
|---|---|
| `shiny` | Web application framework |
| `shinydashboard` | Dashboard layout |
| `DT` | Interactive tables |
| `scholar` | Google Scholar scraping |
| `httr` | HTTP requests to Semantic Scholar API |
| `jsonlite` | JSON parsing |
| `shinycssloaders` | Loading spinners |
| `shinyWidgets` | Enhanced UI widgets |
| `ggplot2` | Citation pattern plots |
