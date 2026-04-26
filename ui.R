# ============================================================
# scholarAnalyzer — Shiny UI
# ============================================================

#' Build the Shiny UI for the Scholar Citation Analyzer
#'
#' @return A \code{shinydashboard::dashboardPage} object.
#' @importFrom shiny tags HTML icon br hr h3 h4 h5 p code strong
#'   fluidRow column textInput selectInput actionButton
#'   verbatimTextOutput plotOutput uiOutput conditionalPanel
#'   downloadButton
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar
#'   dashboardBody sidebarMenu menuItem tabItems tabItem box
#' @importFrom DT dataTableOutput
#' @importFrom shinycssloaders withSpinner
#' @keywords internal
app_ui <- function() {
  shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "Scholar Citations Analyzer"),

    shinydashboard::dashboardSidebar(
      shinydashboard::sidebarMenu(
        shinydashboard::menuItem("Publications",       tabName = "publications",   icon = shiny::icon("book")),
        shinydashboard::menuItem("Citations Analysis", tabName = "citations",      icon = shiny::icon("quote-right")),
        shinydashboard::menuItem("Self-Citations",     tabName = "self_citations", icon = shiny::icon("user-check")),
        shinydashboard::menuItem("Summary Statistics", tabName = "statistics",     icon = shiny::icon("chart-bar")),
        shinydashboard::menuItem("About & How to use", tabName = "about",          icon = shiny::icon("info-circle"))
      )
    ),

    shinydashboard::dashboardBody(
      shiny::tags$head(shiny::tags$style(shiny::HTML("
        .content-wrapper, .right-side { background-color: #f9f9f9; }
        .self-citation {
          background-color: #fff3cd; border-left: 4px solid #ffc107;
          padding: 10px; margin: 5px 0; border-radius: 4px;
        }
        .external-citation {
          background-color: #d1ecf1; border-left: 4px solid #17a2b8;
          padding: 10px; margin: 5px 0; border-radius: 4px;
        }
        .note-box {
          background-color: #e8f4fd; border-left: 4px solid #3498db;
          padding: 10px; margin: 8px 0; border-radius: 4px; font-size: 13px;
        }
      "))),

      shinydashboard::tabItems(

        # ── Publications Tab ─────────────────────────────────────────────
        shinydashboard::tabItem(tabName = "publications",
          shiny::fluidRow(
            shinydashboard::box(
              title = "Enter Google Scholar ID", status = "primary",
              solidHeader = TRUE, width = 12,
              shiny::textInput("scholar_id", "Google Scholar ID:",
                               value = "", placeholder = "e.g., fzAQV6wAAAAJ"),
              shiny::p("Find your Scholar ID in the URL of your Google Scholar profile: ",
                       shiny::code("scholar.google.com/citations?user=YOUR_ID")),
              shiny::fluidRow(
                shiny::column(6, shiny::actionButton("load_pubs",          "Load Publications",  class = "btn-primary")),
                shiny::column(6, shiny::actionButton("reset_publications", "Reset Publications", class = "btn-warning"))
              ),
              shiny::br(),
              shinycssloaders::withSpinner(shiny::verbatimTextOutput("load_status"))
            )
          ),
          shiny::fluidRow(
            shinydashboard::box(
              title = "Your Publications", status = "info",
              solidHeader = TRUE, width = 12,
              shinycssloaders::withSpinner(DT::dataTableOutput("publications_table"))
            )
          )
        ),

        # ── Citations Analysis Tab ────────────────────────────────────────
        shinydashboard::tabItem(tabName = "citations",
          shiny::fluidRow(
            shinydashboard::box(
              title = "Citation Analysis Options", status = "primary",
              solidHeader = TRUE, width = 12,

              shiny::div(class = "note-box",
                shiny::tags$b("How citation counts work:"),
                shiny::tags$ul(
                  shiny::tags$li("Total citations come from ", shiny::tags$b("Google Scholar"), " (authoritative)."),
                  shiny::tags$li("Self-citations are identified via ", shiny::tags$b("Semantic Scholar"),
                                 " — a citing paper is a self-citation if any co-author of the original paper",
                                 " also authored the citing paper."),
                  shiny::tags$li("Adjusted citations = Google Scholar total ",
                                 shiny::tags$b("minus"), " self-citations detected.")
                )
              ),

              shiny::fluidRow(
                shiny::column(10),
                shiny::column(2, shiny::actionButton("reset_analysis", "Reset Analysis",
                                                     class = "btn-warning btn-sm",
                                                     style = "float:right;"))
              ),
              shiny::hr(),

              shiny::h4("Option 1: Analyze a specific paper"),
              shiny::selectInput("selected_paper", "Select a paper:", choices = NULL, width = "100%"),
              shiny::actionButton("analyze_single", "Analyze Selected Paper", class = "btn-info"),
              shiny::br(), shiny::br(),

              shiny::h4("Option 2: Analyze all papers"),
              shiny::p("Analyzes every publication. May take several minutes for large profiles."),
              shiny::actionButton("analyze_all", "Analyze All Papers", class = "btn-success"),
              shiny::br(), shiny::br(),

              shinycssloaders::withSpinner(shiny::verbatimTextOutput("analysis_status"))
            )
          ),
          shiny::fluidRow(
            shinydashboard::box(
              title = "Citation Analysis Results", status = "success",
              solidHeader = TRUE, width = 12,
              shiny::uiOutput("citation_results")
            )
          )
        ),

        # ── Self-Citations Tab ────────────────────────────────────────────
        shinydashboard::tabItem(tabName = "self_citations",
          shiny::fluidRow(
            shinydashboard::box(
              title = "Self-Citation Analysis", status = "warning",
              solidHeader = TRUE, width = 12,
              shiny::fluidRow(
                shiny::column(10),
                shiny::column(2, shiny::actionButton("reset_self_citations", "Reset Data",
                                                     class = "btn-warning btn-sm",
                                                     style = "float:right;"))
              ),
              shiny::hr(),

              shiny::conditionalPanel(
                condition = "output.has_analysis_results",
                shiny::h4("Self-Citation Statistics per Paper"),
                shinycssloaders::withSpinner(DT::dataTableOutput("self_citation_stats")),
                shiny::br(),
                shiny::h4("Download Options"),
                shiny::fluidRow(
                  shiny::column(4, shiny::downloadButton("download_self_citations",
                                                         "Download Self-Citations",     class = "btn-warning")),
                  shiny::column(4, shiny::downloadButton("download_external_citations",
                                                         "Download External Citations", class = "btn-info")),
                  shiny::column(4, shiny::downloadButton("download_all_separated",
                                                         "Download All (Separated)",    class = "btn-success"))
                )
              ),
              shiny::conditionalPanel(
                condition = "!output.has_analysis_results",
                shiny::div(class = "alert alert-info",
                           shiny::h4("No Analysis Results Yet"),
                           shiny::p("Run citation analysis from the 'Citations Analysis' tab first."))
              )
            )
          )
        ),

        # ── Statistics Tab ────────────────────────────────────────────────
        shinydashboard::tabItem(tabName = "statistics",
          shiny::fluidRow(
            shinydashboard::box(
              title = "Publication Statistics (Google Scholar)", status = "info",
              solidHeader = TRUE, width = 6,
              shinycssloaders::withSpinner(DT::dataTableOutput("summary_stats"))
            ),
            shinydashboard::box(
              title = "Citation Pattern (Google Scholar)", status = "primary",
              solidHeader = TRUE, width = 6,
              shiny::p("Visualise citations per paper over time."),
              shiny::actionButton("generate_plot", "Generate Plot", class = "btn-primary"),
              shiny::br(), shiny::br(),
              shinycssloaders::withSpinner(shiny::plotOutput("citation_pattern_plot"))
            )
          ),
          shiny::fluidRow(
            shinydashboard::box(
              title = "Detailed Self-Citation Metrics", status = "warning",
              solidHeader = TRUE, width = 10,
              shiny::div(class = "note-box",
                         "GS Citations = Google Scholar total (authoritative). ",
                         "Adjusted = GS Citations minus self-citations detected. ",
                         "SS Found = whether paper was found on Semantic Scholar."),
              shinycssloaders::withSpinner(DT::dataTableOutput("detailed_citation_metrics"))
            ),
            shinydashboard::box(
              title = "Reset", status = "warning", solidHeader = TRUE, width = 2,
              shiny::br(),
              shiny::actionButton("reset_statistics", "Reset All Data", class = "btn-warning")
            )
          )
        ),

        # ── About Tab ─────────────────────────────────────────────────────
        shinydashboard::tabItem(tabName = "about",
          shiny::fluidRow(
            shinydashboard::box(
              title = "About This App", status = "primary",
              solidHeader = TRUE, width = 12,
              shiny::h3("Google Scholar Citations Analyzer v1.0"),
              shiny::p("Analyzes your Google Scholar publications and detects self-citations",
                       " by cross-referencing with the Semantic Scholar API."),

              shiny::h4("How it works:"),
              shiny::tags$ol(
                shiny::tags$li("Publications and citation counts are fetched from ",
                               shiny::tags$b("Google Scholar"), " using your Scholar ID."),
                shiny::tags$li("Each paper is looked up on ",
                               shiny::tags$b("Semantic Scholar"),
                               " (via DOI first, then title) to get its citing papers."),
                shiny::tags$li("A citing paper is a self-citation if ",
                               shiny::tags$b("any"), " co-author of the original paper",
                               " also appears in the citing paper's author list."),
                shiny::tags$li("Adjusted count = Google Scholar total minus self-citations found.")
              ),

              shiny::h4("Self-citation detection:"),
              shiny::tags$ul(
                shiny::tags$li(shiny::strong("All co-authors checked:"),
                               " Every author of the original paper is compared (not just the profile owner)."),
                shiny::tags$li(shiny::strong("Last-name matching:"),
                               " Robust to 'J. Smith' vs 'John Smith'."),
                shiny::tags$li(shiny::strong("Full-name matching:"),
                               " Additional exact match for higher precision.")
              ),

              shiny::h4("Limitations:"),
              shiny::tags$ul(
                shiny::tags$li("Semantic Scholar may not index all papers."),
                shiny::tags$li("If a paper is not found on Semantic Scholar, ",
                               "the Google Scholar count is shown unchanged."),
                shiny::tags$li("Very common last names may occasionally cause false positives.")
              ),

              shiny::h4("Finding your Google Scholar ID:"),
              shiny::p("Open your Google Scholar profile. The URL looks like:"),
              shiny::code("https://scholar.google.com/citations?user=XXXXXXXXXXXX"),
              shiny::p("The string after ", shiny::code("user="), " is your ID.")
            )
          )
        )
      )
    )
  )
}
