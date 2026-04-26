# ============================================================
# scholarAnalyzer — Shiny Server
# ============================================================

#' Build the Shiny server function for the Scholar Citation Analyzer
#'
#' @return A Shiny server function.
#' @importFrom shiny reactive reactiveValues observeEvent req renderText
#'   renderUI renderPlot outputOptions showNotification updateTextInput
#'   updateSelectInput downloadHandler div h4 h5 p strong
#' @importFrom utils head tail
#' @importFrom DT renderDataTable datatable formatStyle styleInterval styleEqual
#' @importFrom ggplot2 ggplot aes geom_col geom_point geom_smooth
#'   scale_fill_manual scale_size_continuous labs theme theme_minimal
#'   element_text factor
#' @importFrom scholar get_publications
#' @keywords internal
app_server <- function(input, output, session) {

  values <- shiny::reactiveValues(
    publications      = NULL,
    scholar_id        = NULL,
    author_info       = NULL,
    citation_analysis = NULL,
    has_analysis      = FALSE
  )

  # ── Reset helpers ────────────────────────────────────────────────────────
  reset_all_data <- function() {
    values$publications      <- NULL
    values$scholar_id        <- NULL
    values$author_info       <- NULL
    values$citation_analysis <- NULL
    values$has_analysis      <- FALSE
    output$load_status      <- shiny::renderText("")
    output$analysis_status  <- shiny::renderText("")
    output$citation_results <- shiny::renderUI({})
    shiny::updateTextInput(session,  "scholar_id",      value   = "")
    shiny::updateSelectInput(session, "selected_paper", choices = NULL)
  }

  shiny::observeEvent(input$reset_publications, {
    reset_all_data()
    shiny::showNotification("Data reset.", type = "warning")
  })
  shiny::observeEvent(input$reset_analysis, {
    values$citation_analysis <- NULL
    values$has_analysis      <- FALSE
    output$analysis_status   <- shiny::renderText("")
    output$citation_results  <- shiny::renderUI({})
    shiny::showNotification("Analysis reset.", type = "warning")
  })
  shiny::observeEvent(input$reset_self_citations, {
    values$citation_analysis <- NULL
    values$has_analysis      <- FALSE
    shiny::showNotification("Self-citation data reset.", type = "warning")
  })
  shiny::observeEvent(input$reset_statistics, {
    reset_all_data()
    shiny::showNotification("All data reset.", type = "warning")
  })

  # ── Load publications ────────────────────────────────────────────────────
  shiny::observeEvent(input$load_pubs, {
    shiny::req(input$scholar_id)
    if (nchar(trimws(input$scholar_id)) == 0) {
      output$load_status <- shiny::renderText("Please enter a valid Scholar ID.")
      return()
    }

    output$load_status <- shiny::renderText("Loading publications...")

    tryCatch({
      pubs        <- scholar::get_publications(trimws(input$scholar_id))
      author_info <- get_scholar_author_info(trimws(input$scholar_id))

      if (nrow(pubs) == 0) {
        output$load_status <- shiny::renderText(
          "No publications found. Check your Scholar ID.")
        return()
      }

      values$publications <- pubs
      values$scholar_id   <- trimws(input$scholar_id)
      values$author_info  <- author_info

      shiny::updateSelectInput(
        session, "selected_paper",
        choices = setNames(
          pubs$title,
          paste0(substr(pubs$title, 1, 80), " (", pubs$cites, " citations)")
        )
      )

      output$load_status <- shiny::renderText(
        paste0("Loaded ", nrow(pubs), " publications for: ",
               author_info$name,
               if (!is.null(author_info$affiliation))
                 paste0(" — ", author_info$affiliation)
               else "")
      )
    }, error = function(e) {
      output$load_status <- shiny::renderText(paste("Error:", e$message))
    })
  })

  # ── Publications table ───────────────────────────────────────────────────
  output$publications_table <- DT::renderDataTable({
    shiny::req(values$publications)
    cols <- intersect(c("title", "author", "journal", "year", "cites"),
                      names(values$publications))
    df   <- values$publications[, cols, drop = FALSE]
    colnames(df) <- c("Title", "Authors", "Journal", "Year",
                      "Citations")[seq_along(cols)]
    DT::datatable(df,
                  options  = list(pageLength = 10, scrollX = TRUE,
                                  columnDefs = list(
                                    list(width = "300px", targets = 0))),
                  rownames = FALSE)
  })

  # ── Reactive flag for conditionalPanel ──────────────────────────────────
  output$has_analysis_results <- shiny::reactive({ values$has_analysis })
  shiny::outputOptions(output, "has_analysis_results",
                       suspendWhenHidden = FALSE)

  # ── Analyze single paper ─────────────────────────────────────────────────
  shiny::observeEvent(input$analyze_single, {
    shiny::req(input$selected_paper, values$author_info, values$publications)
    output$analysis_status <- shiny::renderText("Analyzing citations...")

    tryCatch({
      paper_title <- input$selected_paper
      pub_row     <- values$publications[
        values$publications$title == paper_title, , drop = FALSE]

      if (nrow(pub_row) == 0) {
        output$analysis_status <- shiny::renderText("Paper not found.")
        return()
      }

      result <- process_single_paper(pub_row[1, ], values$publications)

      values$citation_analysis                <- list()
      values$citation_analysis[[paper_title]] <- result
      values$has_analysis                     <- TRUE

      output$citation_results <- shiny::renderUI({
        not_found_note <- if (!result$sem_scholar_found) {
          shiny::div(class = "alert alert-warning",
                     "This paper was not found on Semantic Scholar. ",
                     "Self-citation detection was skipped. ",
                     "Google Scholar citation count is shown unchanged.")
        }

        shiny::div(
          shiny::h4(paste("Results for:", substr(paper_title, 1, 100))),
          not_found_note,
          shiny::div(class = "alert alert-info",
            shiny::h5("Citation Statistics:"),
            shiny::p(shiny::strong("Google Scholar citations: "),
                     result$gs_citations),
            shiny::p(shiny::strong("Self-citations detected: "),
                     result$self_count,
                     sprintf(" (%.1f%%)",
                             100 * result$self_count /
                               max(result$gs_citations, 1))),
            shiny::p(shiny::strong("Adjusted citations: "),
                     result$adjusted_citations,
                     " (GS total minus self-citations)")
          ),
          if (length(result$self_formatted) > 0) {
            shiny::div(
              shiny::h5(paste("Self-Citations (", length(result$self_formatted), "):")),
              shiny::div(class = "self-citation",
                         lapply(result$self_formatted,
                                function(cit) shiny::p(cit)))
            )
          },
          if (length(result$external_formatted) > 0) {
            shiny::div(
              shiny::h5(paste("External Citations (first 10 of",
                              length(result$external_formatted), "):")),
              shiny::div(class = "external-citation",
                         lapply(utils::head(result$external_formatted, 10),
                                function(cit) shiny::p(cit)))
            )
          }
        )
      })

      output$analysis_status <- shiny::renderText("Analysis complete.")

    }, error = function(e) {
      output$analysis_status <- shiny::renderText(paste("Error:", e$message))
    })
  })

  # ── Analyze all papers ───────────────────────────────────────────────────
  shiny::observeEvent(input$analyze_all, {
    shiny::req(values$author_info, values$publications)
    output$analysis_status <- shiny::renderText(
      "Analyzing all papers — this may take several minutes...")

    tryCatch({
      pubs       <- values$publications
      total_pubs <- nrow(pubs)

      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Starting...", value = 0)

      values$citation_analysis <- list()
      total_gs <- 0L; total_self <- 0L; total_adj <- 0L

      for (i in seq_len(total_pubs)) {
        progress$set(value = i / total_pubs,
                     detail = paste("Paper", i, "of", total_pubs))
        pub_row     <- pubs[i, , drop = FALSE]
        paper_title <- pub_row$title
        result      <- process_single_paper(pub_row, pubs, progress)
        values$citation_analysis[[paper_title]] <- result
        total_gs   <- total_gs   + result$gs_citations
        total_self <- total_self + result$self_count
        total_adj  <- total_adj  + result$adjusted_citations
        Sys.sleep(0.8)
      }

      values$has_analysis <- TRUE

      output$citation_results <- shiny::renderUI({
        shiny::div(
          shiny::h4("Results for All Publications"),
          shiny::div(class = "alert alert-success",
            shiny::h5("Overall Statistics:"),
            shiny::p(shiny::strong("Papers analyzed: "),          total_pubs),
            shiny::p(shiny::strong("Total GS citations: "),       total_gs),
            shiny::p(shiny::strong("Total self-citations: "),     total_self,
                     sprintf(" (%.1f%%)",
                             100 * total_self / max(total_gs, 1))),
            shiny::p(shiny::strong("Total adjusted citations: "), total_adj)
          ),
          shiny::div(class = "alert alert-info",
                     shiny::p("Per-paper details and downloads are in the ",
                              shiny::strong("Self-Citations"), " tab."))
        )
      })

      output$analysis_status <- shiny::renderText(
        paste0("Done. Analyzed ", total_pubs, " papers. ",
               total_self, " self-citations detected.")
      )

    }, error = function(e) {
      output$analysis_status <- shiny::renderText(paste("Error:", e$message))
    })
  })

  # ── Self-citation stats table ────────────────────────────────────────────
  output$self_citation_stats <- DT::renderDataTable({
    shiny::req(values$citation_analysis)

    rows <- lapply(names(values$citation_analysis), function(ttl) {
      r <- values$citation_analysis[[ttl]]
      data.frame(
        Paper          = substr(ttl, 1, 60),
        GS_Citations   = r$gs_citations,
        Self_Citations = r$self_count,
        Adjusted       = r$adjusted_citations,
        Self_Rate      = paste0(round(
          100 * r$self_count / max(r$gs_citations, 1), 1), "%"),
        SS_Found       = if (r$sem_scholar_found) "Yes" else "No",
        stringsAsFactors = FALSE
      )
    })

    df <- do.call(rbind, rows)
    DT::datatable(df,
                  options  = list(pageLength = 10, scrollX = TRUE),
                  rownames = FALSE) %>%
      DT::formatStyle("Self_Rate",
        backgroundColor = DT::styleInterval(c(10, 25),
                            c("#d4edda", "#fff3cd", "#f8d7da")))
  })

  # ── Summary stats table ──────────────────────────────────────────────────
  output$summary_stats <- DT::renderDataTable({
    shiny::req(values$publications)
    DT::datatable(
      generate_citation_summary(values$publications),
      options  = list(pageLength = 10, searching = FALSE, paging = FALSE),
      rownames = FALSE
    )
  })

  # ── Detailed metrics table ───────────────────────────────────────────────
  shiny::observeEvent(values$has_analysis, {
    shiny::req(values$has_analysis, values$citation_analysis,
               values$publications)

    output$detailed_citation_metrics <- DT::renderDataTable({
      rows <- lapply(names(values$citation_analysis), function(ttl) {
        r       <- values$citation_analysis[[ttl]]
        pub_row <- values$publications[
          values$publications$title == ttl, , drop = FALSE]
        year <- if (nrow(pub_row) > 0) pub_row$year[1] else NA

        data.frame(
          Paper              = substr(ttl, 1, 45),
          Year               = year,
          GS_Citations       = r$gs_citations,
          Self_Citations     = r$self_count,
          Adjusted_Citations = r$adjusted_citations,
          Self_Rate          = paste0(round(
            100 * r$self_count / max(r$gs_citations, 1), 1), "%"),
          SS_Found           = if (r$sem_scholar_found) "Yes" else "No",
          stringsAsFactors   = FALSE
        )
      })

      df <- do.call(rbind, rows)
      DT::datatable(df,
                    options  = list(pageLength = 10, scrollX = TRUE),
                    rownames = FALSE) %>%
        DT::formatStyle("Self_Rate",
          backgroundColor = DT::styleInterval(c(10, 25),
                              c("#d4edda", "#fff3cd", "#f8d7da"))) %>%
        DT::formatStyle("SS_Found",
          backgroundColor = DT::styleEqual(c("Yes", "No"),
                              c("#d4edda", "#fff3cd")))
    })
  })

  # ── Citation pattern plot ─────────────────────────────────────────────────
  shiny::observeEvent(input$generate_plot, {
    shiny::req(values$publications)

    tryCatch({
      pubs_data <- values$publications
      plot_data <- pubs_data[!is.na(pubs_data$year) &
                               !is.na(pubs_data$cites), ]

      if (nrow(plot_data) == 0) {
        output$citation_pattern_plot <- shiny::renderPlot({
          graphics::plot(1, type = "n", axes = FALSE, xlab = "", ylab = "",
                         main = "No valid year/citation data available.")
        })
        return()
      }

      if (values$has_analysis && !is.null(values$citation_analysis)) {
        self_counts <- sapply(plot_data$title, function(ttl) {
          if (!is.null(values$citation_analysis[[ttl]]))
            values$citation_analysis[[ttl]]$self_count
          else 0L
        })
        plot_data$self_citations     <- self_counts
        plot_data$external_citations <- pmax(0L, plot_data$cites - self_counts)

        output$citation_pattern_plot <- shiny::renderPlot({
          pd_long <- data.frame(
            year  = rep(plot_data$year, 2),
            count = c(plot_data$external_citations, plot_data$self_citations),
            type  = rep(c("External", "Self"), each = nrow(plot_data))
          )
          ggplot2::ggplot(pd_long,
                          ggplot2::aes(x = factor(year), y = count,
                                       fill = type)) +
            ggplot2::geom_col(position = "stack", width = 0.6, alpha = 0.85) +
            ggplot2::scale_fill_manual(
              values = c("External" = "steelblue", "Self" = "orange")) +
            ggplot2::labs(
              title    = "Citations by Year (Self vs External)",
              subtitle = "GS totals; self-citations via Semantic Scholar",
              x = "Publication Year", y = "Citations", fill = "Type") +
            ggplot2::theme_minimal() +
            ggplot2::theme(
              plot.title    = ggplot2::element_text(hjust = 0.5, size = 13,
                                                    face = "bold"),
              plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 10,
                                                    colour = "grey50"),
              axis.title    = ggplot2::element_text(size = 11),
              legend.position = "bottom")
        }, height = 420)

      } else {
        output$citation_pattern_plot <- shiny::renderPlot({
          ggplot2::ggplot(plot_data,
                          ggplot2::aes(x = year, y = cites)) +
            ggplot2::geom_point(ggplot2::aes(size = cites),
                                alpha = 0.7, colour = "steelblue") +
            ggplot2::geom_smooth(method = "loess", se = TRUE,
                                 colour = "red", alpha = 0.3) +
            ggplot2::labs(
              title = "Citation Patterns Over Time (Google Scholar)",
              x = "Publication Year", y = "Citations", size = "Citations") +
            ggplot2::theme_minimal() +
            ggplot2::theme(
              plot.title = ggplot2::element_text(hjust = 0.5, size = 13,
                                                 face = "bold"),
              axis.title = ggplot2::element_text(size = 11),
              legend.position = "bottom") +
            ggplot2::scale_size_continuous(range = c(2, 10))
        }, height = 420)
      }

      shiny::showNotification("Plot generated.", type = "message", duration = 3)

    }, error = function(e) {
      output$citation_pattern_plot <- shiny::renderPlot({
        graphics::plot(1, type = "n", axes = FALSE, xlab = "", ylab = "",
                       main = paste("Error:", e$message))
      })
      shiny::showNotification("Error generating plot.", type = "error")
    })
  })

  # ── Downloads ─────────────────────────────────────────────────────────────
  output$download_self_citations <- shiny::downloadHandler(
    filename = function() paste0("self_citations_", Sys.Date(), ".txt"),
    content  = function(file) {
      shiny::req(values$citation_analysis)
      lines <- c()
      for (ttl in names(values$citation_analysis)) {
        r <- values$citation_analysis[[ttl]]
        if (length(r$self_formatted) > 0) {
          lines <- c(lines,
                     paste0("\n=== SELF-CITATIONS FOR: ", ttl, " ===\n"),
                     r$self_formatted, "")
        }
      }
      if (length(lines) == 0) lines <- "No self-citations found."
      writeLines(lines, file)
    }
  )

  output$download_external_citations <- shiny::downloadHandler(
    filename = function() paste0("external_citations_", Sys.Date(), ".txt"),
    content  = function(file) {
      shiny::req(values$citation_analysis)
      lines <- c()
      for (ttl in names(values$citation_analysis)) {
        r <- values$citation_analysis[[ttl]]
        if (length(r$external_formatted) > 0) {
          lines <- c(lines,
                     paste0("\n=== EXTERNAL CITATIONS FOR: ", ttl, " ===\n"),
                     r$external_formatted, "")
        }
      }
      if (length(lines) == 0) lines <- "No external citations found."
      writeLines(lines, file)
    }
  )

  output$download_all_separated <- shiny::downloadHandler(
    filename = function() paste0("all_citations_", Sys.Date(), ".txt"),
    content  = function(file) {
      shiny::req(values$citation_analysis)
      lines <- c()
      for (ttl in names(values$citation_analysis)) {
        r <- values$citation_analysis[[ttl]]
        lines <- c(lines,
          "\n============================",
          paste0("PAPER: ", ttl),
          paste0("GS Citations:       ", r$gs_citations),
          paste0("Self-Citations:     ", r$self_count),
          paste0("Adjusted Citations: ", r$adjusted_citations),
          "============================\n"
        )
        if (length(r$self_formatted) > 0)
          lines <- c(lines, "-- Self-Citations --", r$self_formatted, "")
        if (length(r$external_formatted) > 0)
          lines <- c(lines, "-- External Citations --",
                     r$external_formatted, "")
      }
      writeLines(lines, file)
    }
  )
}
