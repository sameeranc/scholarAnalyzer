# ============================================================
# scholarAnalyzer — Helper Functions
# All pure (non-Shiny) functions used by the app.
# ============================================================

# ── Author / profile helpers ─────────────────────────────────────────────────

#' Fetch basic author info from a Google Scholar profile
#'
#' @param scholar_id Character. The Google Scholar user ID (from the profile URL).
#' @return A named list with elements \code{name} and \code{affiliation}.
#' @importFrom scholar get_profile
#' @keywords internal
get_scholar_author_info <- function(scholar_id) {
  tryCatch({
    profile <- scholar::get_profile(scholar_id)
    list(name = profile$name, affiliation = profile$affiliation)
  }, error = function(e) {
    list(name = NULL, affiliation = NULL)
  })
}

#' Parse a Google Scholar author string into a character vector of names
#'
#' Google Scholar returns authors as a single comma-separated string such as
#' \code{"J Smith, A Jones, B Lee"}. This function splits and cleans that
#' string into individual lowercase names suitable for comparison.
#'
#' @param author_string Character scalar.
#' @return Character vector of lowercase author names, or \code{character(0)}.
#' @keywords internal
parse_author_string <- function(author_string) {
  if (is.null(author_string) || is.na(author_string) ||
      nchar(trimws(author_string)) == 0) {
    return(character(0))
  }
  parts <- unlist(strsplit(author_string, ",\\s*|\\s+and\\s+"))
  parts <- trimws(parts)
  parts <- parts[nchar(parts) > 0]
  tolower(parts)
}

#' Extract the last name from a full name string
#'
#' @param name Character scalar.
#' @return Lowercase last name, or \code{""} if name is empty.
#' @keywords internal
last_name <- function(name) {
  parts <- unlist(strsplit(trimws(name), "\\s+"))
  if (length(parts) == 0) return("")
  tolower(utils::tail(parts, 1))
}

# ── Self-citation detection ───────────────────────────────────────────────────

#' Determine whether a citing paper is a self-citation
#'
#' A citing paper is a self-citation if \emph{any} author of the original paper
#' (the full co-author list, not just the searched author) also appears as an
#' author of the citing paper. Matching is done on last names (robust to
#' initials vs full first names) and also on full lowercase names.
#'
#' @param citing_paper List. A paper object returned by the Semantic Scholar
#'   \code{/citations} endpoint (must contain an \code{authors} field).
#' @param original_authors Character vector of lowercase author names from
#'   \code{parse_author_string()}.
#' @return Logical scalar.
#' @keywords internal
is_self_citation <- function(citing_paper, original_authors) {
  if (is.null(citing_paper) || length(original_authors) == 0) return(FALSE)

  citing_authors <- character(0)
  if (!is.null(citing_paper$authors) && length(citing_paper$authors) > 0) {
    citing_authors <- tolower(trimws(sapply(citing_paper$authors, function(a) {
      if (is.list(a) && !is.null(a$name)) a$name else ""
    })))
    citing_authors <- citing_authors[nchar(citing_authors) > 0]
  }
  if (length(citing_authors) == 0) return(FALSE)

  orig_lastnames <- sapply(original_authors, last_name)
  cite_lastnames <- sapply(citing_authors,   last_name)

  full_match     <- any(original_authors %in% citing_authors)
  lastname_match <- any(orig_lastnames   %in% cite_lastnames)

  full_match || lastname_match
}

# ── Semantic Scholar lookup ───────────────────────────────────────────────────

#' Find a paper's Semantic Scholar paper ID
#'
#' Tries DOI first (exact and reliable), then falls back to a title search
#' and returns the first result. Returns \code{NULL} if neither method
#' succeeds.
#'
#' @param pub_row A single-row data frame from \code{scholar::get_publications()}.
#' @return Character scalar (paper ID) or \code{NULL}.
#' @importFrom httr GET status_code content
#' @importFrom jsonlite fromJSON
#' @importFrom utils URLencode
#' @keywords internal
find_semantic_scholar_id <- function(pub_row) {
  base <- "https://api.semanticscholar.org/graph/v1/paper"

  # --- Try DOI first ---
  doi <- tryCatch(pub_row$doi, error = function(e) NULL)
  if (!is.null(doi) && !is.na(doi) && nchar(trimws(doi)) > 0) {
    doi_clean <- trimws(doi)
    res <- tryCatch(
      httr::GET(paste0(base, "/DOI:", utils::URLencode(doi_clean, reserved = TRUE)),
                query = list(fields = "paperId")),
      error = function(e) NULL
    )
    if (!is.null(res) && httr::status_code(res) == 200) {
      data <- tryCatch(
        jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"),
                           simplifyDataFrame = FALSE),
        error = function(e) NULL
      )
      if (!is.null(data$paperId)) return(data$paperId)
    }
  }

  # --- Fall back to title search ---
  title <- tryCatch(pub_row$title, error = function(e) NULL)
  if (!is.null(title) && !is.na(title) && nchar(trimws(title)) > 0) {
    res <- tryCatch(
      httr::GET(paste0(base, "/search"),
                query = list(query = trimws(title), limit = 1,
                             fields = "paperId,title")),
      error = function(e) NULL
    )
    if (!is.null(res) && httr::status_code(res) == 200) {
      data <- tryCatch(
        jsonlite::fromJSON(httr::content(res, "text", encoding = "UTF-8"),
                           simplifyDataFrame = FALSE),
        error = function(e) NULL
      )
      if (!is.null(data$data) && length(data$data) > 0) {
        return(data$data[[1]]$paperId)
      }
    }
  }
  NULL
}

#' Fetch citing papers from Semantic Scholar and split into self / external
#'
#' Google Scholar is the authoritative source for total citation counts.
#' Semantic Scholar is used only to identify \emph{which} citing papers are
#' self-citations, so an adjusted count can be computed.
#'
#' @param pub_row Single-row data frame from \code{scholar::get_publications()}.
#' @param user_publications Full publications data frame (used for context).
#' @param limit Integer. Max citing papers to retrieve from Semantic Scholar.
#' @return Named list with elements \code{self_citations},
#'   \code{external_citations} (both lists of paper objects), and
#'   \code{sem_scholar_found} (logical).
#' @importFrom httr GET status_code content
#' @importFrom jsonlite fromJSON
#' @keywords internal
get_citations_with_self_detection <- function(pub_row, user_publications,
                                               limit = 500) {
  tryCatch({
    paper_id <- find_semantic_scholar_id(pub_row)
    if (is.null(paper_id)) {
      return(list(self_citations = NULL, external_citations = NULL,
                  sem_scholar_found = FALSE))
    }

    cite_url <- paste0("https://api.semanticscholar.org/graph/v1/paper/",
                       paper_id, "/citations")
    cite_res <- httr::GET(cite_url,
                          query = list(
                            fields = "title,authors,year,venue,externalIds,url",
                            limit  = limit
                          ))

    if (httr::status_code(cite_res) != 200) {
      return(list(self_citations = NULL, external_citations = NULL,
                  sem_scholar_found = TRUE))
    }

    cites <- jsonlite::fromJSON(
      httr::content(cite_res, "text", encoding = "UTF-8"),
      simplifyDataFrame = FALSE
    )

    if (is.null(cites$data) || length(cites$data) == 0) {
      return(list(self_citations = list(), external_citations = list(),
                  sem_scholar_found = TRUE))
    }

    citing_list <- Filter(Negate(is.null),
                          lapply(cites$data, function(x) x$citingPaper))

    # Parse the original paper's full co-author list
    orig_authors <- parse_author_string(pub_row$author)

    self_citations     <- list()
    external_citations <- list()

    for (paper in citing_list) {
      if (is_self_citation(paper, orig_authors)) {
        self_citations <- append(self_citations, list(paper))
      } else {
        external_citations <- append(external_citations, list(paper))
      }
    }

    list(
      self_citations     = self_citations,
      external_citations = external_citations,
      sem_scholar_found  = TRUE
    )
  }, error = function(e) {
    list(self_citations = NULL, external_citations = NULL,
         sem_scholar_found = FALSE)
  })
}

# ── Formatting ────────────────────────────────────────────────────────────────

#' Format a Semantic Scholar paper as an APS-style reference string
#'
#' @param paper List. A paper object from the Semantic Scholar API.
#' @param is_self Logical. If \code{TRUE}, prepends \code{[SELF-CITATION]}.
#' @return Character scalar, or \code{NA_character_} if input is invalid.
#' @keywords internal
format_APS_citation <- function(paper, is_self = FALSE) {
  if (is.null(paper) || !is.list(paper)) return(NA_character_)

  authors <- tryCatch({
    if (!is.null(paper$authors) && length(paper$authors) > 0) {
      nms <- sapply(paper$authors, function(a) {
        if (is.list(a) && !is.null(a$name)) a$name else "Unknown"
      })
      paste(nms, collapse = ", ")
    } else { "Unknown Authors" }
  }, error = function(e) "Unknown Authors")

  title <- tryCatch(
    if (!is.null(paper$title)) paper$title else "Untitled",
    error = function(e) "Untitled"
  )

  venue <- tryCatch(
    if (!is.null(paper$venue) && nchar(paper$venue) > 0) paper$venue
    else "Unknown Journal",
    error = function(e) "Unknown Journal"
  )

  year <- tryCatch(
    if (!is.null(paper$year)) paper$year else "n.d.",
    error = function(e) "n.d."
  )

  url <- tryCatch({
    if (!is.null(paper$externalIds) && !is.null(paper$externalIds$DOI)) {
      paste0("https://doi.org/", paper$externalIds$DOI)
    } else if (!is.null(paper$url)) {
      paper$url
    } else { "No URL" }
  }, error = function(e) "No URL")

  prefix <- if (is_self) "[SELF-CITATION] " else ""
  paste0(prefix, authors, ", ", title, ", *", venue, "* (", year, "). ", url)
}

# ── Per-paper processing ──────────────────────────────────────────────────────

#' Process a single publication and detect self-citations
#'
#' Fetches citing papers from Semantic Scholar, classifies each as self or
#' external, and returns a summary list. The Google Scholar \code{cites}
#' value is the authoritative total; adjusted citations = GS total minus
#' detected self-citations.
#'
#' @param pub_row Single-row data frame from \code{scholar::get_publications()}.
#' @param user_publications Full publications data frame.
#' @param progress Optional \code{shiny::Progress} object for progress updates.
#' @return Named list with elements: \code{gs_citations}, \code{sem_scholar_found},
#'   \code{self_count}, \code{external_count}, \code{adjusted_citations},
#'   \code{self_formatted}, \code{external_formatted}.
#' @keywords internal
process_single_paper <- function(pub_row, user_publications, progress = NULL) {
  if (!is.null(progress)) {
    progress$set(message = paste("Fetching:", substr(pub_row$title, 1, 50), "..."))
  }

  gs_cites <- if (!is.null(pub_row$cites) && !is.na(pub_row$cites)) {
    as.integer(pub_row$cites)
  } else { 0L }

  citations_data <- get_citations_with_self_detection(pub_row, user_publications)

  result <- list(
    gs_citations       = gs_cites,
    sem_scholar_found  = citations_data$sem_scholar_found,
    self_count         = 0L,
    external_count     = 0L,
    adjusted_citations = gs_cites,
    self_formatted     = character(0),
    external_formatted = character(0)
  )

  if (!is.null(citations_data$self_citations)) {
    n_self <- length(citations_data$self_citations)
    n_ext  <- length(citations_data$external_citations)

    result$self_count         <- n_self
    result$external_count     <- n_ext
    result$adjusted_citations <- max(0L, gs_cites - n_self)

    if (n_self > 0) {
      result$self_formatted <- Filter(Negate(is.na),
        sapply(citations_data$self_citations,
               function(p) format_APS_citation(p, TRUE))
      )
    }
    if (n_ext > 0) {
      result$external_formatted <- Filter(Negate(is.na),
        sapply(citations_data$external_citations,
               function(p) format_APS_citation(p, FALSE))
      )
    }
  }
  result
}

# ── Summary statistics ────────────────────────────────────────────────────────

#' Compute the h-index from a numeric vector of citation counts
#'
#' @param cites_vec Integer or numeric vector.
#' @return Integer scalar h-index.
#' @keywords internal
compute_hindex <- function(cites_vec) {
  cites_vec <- sort(cites_vec[!is.na(cites_vec)], decreasing = TRUE)
  h <- 0L
  for (i in seq_along(cites_vec)) {
    if (cites_vec[i] >= i) h <- i else break
  }
  h
}

#' Build the summary statistics data frame for the Statistics tab
#'
#' @param publications_data Data frame from \code{scholar::get_publications()}.
#' @return Data frame with columns \code{Metric} and \code{Value}.
#' @keywords internal
generate_citation_summary <- function(publications_data) {
  total_papers    <- nrow(publications_data)
  total_citations <- sum(publications_data$cites, na.rm = TRUE)
  avg_citations   <- if (total_papers > 0) {
    round(total_citations / total_papers, 2)
  } else { 0 }
  hindex <- compute_hindex(publications_data$cites)

  data.frame(
    Metric = c("Total Publications",
               "Total Citations (Google Scholar)",
               "Average Citations per Paper",
               "H-index (Google Scholar)"),
    Value  = c(total_papers, total_citations, avg_citations, hindex),
    stringsAsFactors = FALSE
  )
}
