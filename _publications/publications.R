# Builds the Publications page.
#
# - The list of works comes from Andrew's public ORCID record.
# - Full author lists, journal and volume details come from Crossref (by DOI).
# - Lab members (lab-members.yml) are highlighted in the author lists.
# - Works can be hidden or added in publications-overrides.yml.
# - Everything fetched is cached in _publications/cache.json, so the page still
#   builds if ORCID or Crossref is down, and older papers aren't re-fetched.
# - Recent preprints are also found by searching Crossref, since ORCID often
#   misses them.

orcid_id       <- "0000-0001-6436-7942"
cache_file     <- "_publications/cache.json"
members_file   <- "lab-members.yml"
overrides_file <- "publications-overrides.yml"
user_agent     <- "lettenlab-website (https://andrewletten.net)"


# Helpers ---------------------------------------------------------------------

is_blank <- function(x) {
  is.null(x) || length(x) == 0 || (length(x) == 1 && (is.na(x) || identical(x, "")))
}

`%||%` <- function(x, y) if (is_blank(x)) y else x

first <- function(x) if (length(x)) x[[1]] else NULL

get_json <- function(url) {
  httr2::request(url) |>
    httr2::req_headers(Accept = "application/json") |>
    httr2::req_user_agent(user_agent) |>
    httr2::req_timeout(30) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
}

normalise_doi <- function(x) {
  if (is_blank(x)) return(NA_character_)
  sub("^(https?://(dx\\.)?doi\\.org/|doi:\\s*)", "", tolower(trimws(x)))
}

# Lower-case ASCII letters only, for comparing names and titles
simplify <- function(x) {
  x <- iconv(x %||% "", from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  x[is.na(x)] <- ""
  tolower(gsub("[^A-Za-z]", "", x))
}

clean_title <- function(x) {
  x <- gsub("\\s+", " ", x)
  x <- gsub("</?(i|em)>", "*", x, ignore.case = TRUE)
  x <- gsub("<[^>]+>", "", x)
  x <- gsub("&amp;", "&", x, fixed = TRUE)
  x <- gsub("&lt;", "<", x, fixed = TRUE)
  x <- gsub("&gt;", ">", x, fixed = TRUE)
  x <- gsub("&quot;", "\"", x, fixed = TRUE)
  x <- gsub("&#39;", "'", x, fixed = TRUE)
  trimws(x)
}


# Fetching --------------------------------------------------------------------

fetch_orcid_works <- function() {
  groups <- get_json(sprintf("https://pub.orcid.org/v3.0/%s/works", orcid_id))$group
  lapply(groups, function(g) {
    s <- first(g$`work-summary`)
    doi <- NA_character_
    for (id in g$`external-ids`$`external-id`) {
      if (identical(id$`external-id-type`, "doi")) {
        doi <- normalise_doi(id$`external-id-value`)
        break
      }
    }
    list(
      doi     = doi,
      type    = s$type %||% NA_character_,
      title   = s$title$title$value %||% NA_character_,
      journal = s$`journal-title`$value %||% NA_character_,
      year    = as.integer(s$`publication-date`$year$value %||% NA)
    )
  })
}

# Preprint servers whose Crossref records only name the publisher
preprint_servers <- c(
  "10.1101"  = "bioRxiv",
  "10.32942" = "EcoEvoRxiv",
  "10.21203" = "Research Square",
  "10.48550" = "arXiv",
  "10.31219" = "OSF Preprints",
  "10.20944" = "Preprints.org",
  "10.2139"  = "SSRN"
)

# Note: [[ ]] rather than $ throughout, because $ matches partial names
# (e.g. a missing "issue" field would pick up "issued")
fetch_crossref <- function(doi) {
  m <- get_json(paste0("https://api.crossref.org/works/", utils::URLencode(doi, reserved = TRUE)))[["message"]]
  date_parts <- first(m[["issued"]][["date-parts"]]) %||% list()
  server <- unname(preprint_servers[sub("/.*", "", doi)])
  list(
    doi          = doi,
    type         = m[["type"]] %||% NA_character_,
    title        = first(m[["title"]]) %||% NA_character_,
    journal      = first(m[["container-title"]]) %||% first(m[["institution"]])[["name"]] %||%
                     server %||% m[["publisher"]] %||% NA_character_,
    volume       = m[["volume"]] %||% NA_character_,
    issue        = m[["issue"]] %||% NA_character_,
    pages        = m[["page"]] %||% m[["article-number"]] %||% NA_character_,
    year         = as.integer(first(date_parts) %||% NA),
    month        = as.integer(if (length(date_parts) >= 2) date_parts[[2]] %||% NA else NA),
    published_as = vapply(m[["relation"]][["is-preprint-of"]] %||% list(), function(r) normalise_doi(r[["id"]]), ""),
    authors      = lapply(m[["author"]] %||% list(), function(a) list(
      given  = a[["given"]] %||% "",
      family = a[["family"]] %||% a[["name"]] %||% "",
      orcid  = sub("^https?://orcid\\.org/", "", a[["ORCID"]] %||% "")
    ))
  )
}

# ORCID often misses preprints (they're only added automatically when the
# preprint server records Andrew's ORCID iD), so also search Crossref for
# recent preprints with an author "A... Letten"
find_recent_preprints <- function(since_year) {
  url <- paste0("https://api.crossref.org/works?query.author=Letten&rows=100&select=DOI,author",
                "&filter=type:posted-content,from-posted-date:", since_year, "-01-01")
  dois <- character()
  for (item in get_json(url)[["message"]][["items"]]) {
    mine <- any(vapply(item[["author"]] %||% list(), function(a) {
      simplify(a[["family"]]) == "letten" && startsWith(simplify(a[["given"]]), "a")
    }, TRUE))
    if (mine) dois <- c(dois, normalise_doi(item[["DOI"]]))
  }
  dois
}

# For works on ORCID without a DOI: find the DOI on Crossref by exact title
find_doi_by_title <- function(title) {
  url <- paste0("https://api.crossref.org/works?rows=5&select=DOI,title&query.bibliographic=",
                utils::URLencode(title, reserved = TRUE))
  for (item in get_json(url)[["message"]][["items"]]) {
    if (simplify(first(item[["title"]])) == simplify(title)) return(normalise_doi(item[["DOI"]]))
  }
  NA_character_
}


# Lab members -----------------------------------------------------------------

load_members <- function() {
  if (!file.exists(members_file)) return(list())
  lapply(yaml::read_yaml(members_file), function(m) {
    names <- c(m$name, unlist(m$aliases))
    list(
      orcid = m$orcid %||% "",
      keys  = lapply(names, function(n) {
        parts <- strsplit(trimws(n), "\\s+")[[1]]
        # Everything after the first name is the surname (e.g. "Vander Velde")
        list(given = simplify(parts[1]), family = simplify(paste(parts[-1], collapse = " ")))
      })
    )
  })
}

# An initial matches any name starting with it; a short form matches the full
# name it starts (e.g. "Matt" and "Matthew"); otherwise names must be identical
given_matches <- function(a, b) {
  if (!nzchar(a) || !nzchar(b)) return(FALSE)
  if (nchar(a) == 1 || nchar(b) == 1) return(substr(a, 1, 1) == substr(b, 1, 1))
  if (min(nchar(a), nchar(b)) >= 3) return(startsWith(a, b) || startsWith(b, a))
  a == b
}

# Split names on any kind of space (Crossref sometimes uses non-breaking spaces)
name_tokens <- function(x, split_hyphens = FALSE) {
  pattern <- if (split_hyphens) "[\\s\\p{Z}.\\p{Pd}]+" else "[\\s\\p{Z}.]+"
  tokens <- strsplit(x %||% "", pattern, perl = TRUE)[[1]]
  tokens[nzchar(tokens)]
}

is_member <- function(author, members) {
  family <- simplify(author$family)
  given  <- simplify(first(name_tokens(author$given)) %||% "")
  orcid  <- author$orcid %||% ""
  for (m in members) {
    if (nzchar(orcid) && nzchar(m$orcid)) {
      if (orcid == m$orcid) return(TRUE)
      next  # both have ORCID iDs and they differ: a different person
    }
    for (k in m$keys) {
      if (family == k$family && given_matches(given, k$given)) return(TRUE)
    }
  }
  FALSE
}


# Formatting ------------------------------------------------------------------

# "GENETICS" -> "Genetics"
fix_caps <- function(x) {
  if (!is_blank(x) && nchar(x) > 2 && x == toupper(x)) tools::toTitleCase(tolower(x)) else x
}

format_author <- function(author, members) {
  family   <- fix_caps(author$family %||% "")
  tokens   <- name_tokens(author$given, split_hyphens = TRUE)
  initials <- toupper(paste(substr(tokens, 1, 1), collapse = ""))
  label    <- trimws(paste(family, initials))
  if (is_member(author, members)) sprintf("[%s]{.lab-member}", label) else label
}

format_entry <- function(e, members) {
  authors <- vapply(e$authors, format_author, "", members = members)
  title   <- clean_title(e$title %||% "Untitled")
  if (!grepl("[.?!]$", title)) title <- paste0(title, ".")
  if (!is_blank(e$doi)) title <- sprintf("[%s](https://doi.org/%s)", title, e$doi)

  venue <- if (is_blank(e$journal)) "" else sprintf("*%s*", fix_caps(clean_title(e$journal)))
  if (!is_blank(e$volume)) venue <- paste0(venue, " ", e$volume)
  if (!is_blank(e$issue))  venue <- paste0(venue, "(", e$issue, ")")
  if (!is_blank(e$pages))  venue <- paste0(venue, ": ", e$pages)
  if (nzchar(venue)) venue <- paste0(venue, ".")

  byline <- if (length(authors)) paste0(paste(authors, collapse = ", "), " ") else ""
  paste0(byline, "(", e$year %||% "n.d.", "). ", title, " ", venue)
}

format_list <- function(entries, members) {
  c("::: {.pub-list}", paste("-", vapply(entries, format_entry, "", members = members)), ":::", "")
}


# Main ------------------------------------------------------------------------

render_publications <- function() {
  this_year <- as.integer(format(Sys.Date(), "%Y"))
  cache <- if (file.exists(cache_file)) jsonlite::read_json(cache_file) else list()

  orcid_works <- tryCatch(fetch_orcid_works(), error = function(e) {
    message("ORCID unavailable, using cached list: ", conditionMessage(e))
    cache$orcid %||% list()
  })

  overrides <- if (file.exists(overrides_file)) yaml::read_yaml(overrides_file) else list()
  exclude <- vapply(overrides$exclude %||% list(), normalise_doi, "")
  include <- vapply(overrides$include %||% list(), normalise_doi, "")
  recent_preprints <- tryCatch(find_recent_preprints(this_year - 1), error = function(e) {
    message("Crossref preprint search failed, using cached list: ", conditionMessage(e))
    unlist(cache$recent_preprints) %||% character()
  })

  orcid_dois <- vapply(orcid_works, function(w) w$doi %||% NA_character_, "")
  extra <- setdiff(unique(c(include, recent_preprints)), orcid_dois)
  works <- c(orcid_works, lapply(extra, function(d) list(doi = d)))

  # Find DOIs for works that ORCID lists without one (remembered in the cache)
  title_dois <- cache$title_dois %||% list()
  for (i in seq_along(works)) {
    w <- works[[i]]
    if (!is_blank(w$doi) || is_blank(w$title)) next
    key <- simplify(w$title)
    if (is.null(title_dois[[key]])) {
      found <- tryCatch(find_doi_by_title(w$title), error = function(e) NA_character_)
      if (!is_blank(found)) title_dois[[key]] <- found
    }
    works[[i]]$doi <- title_dois[[key]] %||% NA_character_
  }

  # Fetch Crossref details for new papers, and refresh recent ones (volume and
  # page numbers often arrive after a paper first appears online)
  crossref <- cache$crossref %||% list()
  for (w in works) {
    d <- w$doi
    if (is_blank(d) || d %in% exclude) next
    cached <- crossref[[d]]
    stale <- is.null(cached) || is_blank(cached$year) || cached$year >= this_year - 1 ||
      (identical(cached$type, "journal-article") && is_blank(cached$volume))
    if (stale) {
      fresh <- tryCatch(fetch_crossref(d), error = function(e) {
        message("Crossref lookup failed for ", d, ": ", conditionMessage(e))
        NULL
      })
      if (!is.null(fresh)) crossref[[d]] <- fresh
    }
  }

  new_cache <- jsonlite::toJSON(
    list(orcid = orcid_works, recent_preprints = I(recent_preprints), title_dois = title_dois,
         crossref = crossref[order(names(crossref))]),
    auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null"
  )
  old_cache <- if (file.exists(cache_file)) paste(readLines(cache_file, warn = FALSE, encoding = "UTF-8"), collapse = "\n") else ""
  if (!identical(as.character(new_cache), old_cache)) writeLines(new_cache, cache_file, useBytes = TRUE)

  # Combine ORCID and Crossref details into one entry per paper
  entries <- list()
  for (w in works) {
    d <- w$doi
    if (!is_blank(d) && d %in% exclude) next
    cr <- if (is_blank(d)) NULL else crossref[[d]]
    type <- cr$type %||% w$type %||% ""
    kind <- if (type %in% c("posted-content", "preprint")) "preprint" else if (type == "journal-article") "article" else NA
    if (is.na(kind)) next
    title <- cr$title %||% w$title %||% ""
    if (grepl("^(publisher |author )?(correction|erratum|corrigendum)\\b", title, ignore.case = TRUE)) next
    entries[[length(entries) + 1]] <- list(
      kind = kind, doi = d,
      title = cr$title %||% w$title, journal = cr$journal %||% w$journal,
      volume = cr$volume, issue = cr$issue, pages = cr$pages,
      year = cr$year %||% w$year, month = cr$month %||% 0L,
      authors = cr$authors %||% list(), published_as = unlist(cr$published_as)
    )
  }

  # Drop repeats (same DOI or same title)
  keys <- vapply(entries, function(e) paste(e$kind, e$doi %||% simplify(e$title)), "")
  titles <- vapply(entries, function(e) simplify(e$title), "")
  entries <- entries[!duplicated(keys) & !duplicated(paste(vapply(entries, `[[`, "", "kind"), titles))]

  newest_first <- function(x) {
    x[order(-vapply(x, function(e) as.numeric(e$year %||% 0), 0),
            -vapply(x, function(e) as.numeric(e$month %||% 0), 0))]
  }

  articles <- newest_first(Filter(function(e) e$kind == "article", entries))
  article_titles  <- vapply(articles, function(e) simplify(e$title), "")
  author_list     <- function(e) paste(vapply(e$authors, function(a) simplify(a$family), ""), collapse = "|")
  article_authors <- vapply(articles, author_list, "")

  # Only recent preprints that haven't since been published. A preprint counts
  # as published if Crossref links it to a paper, or a paper has the same title
  # or exactly the same author list (titles often change before publication).
  preprints <- newest_first(Filter(function(e) {
    e$kind == "preprint" &&
      !is_blank(e$year) && e$year >= this_year - 1 &&
      length(e$published_as) == 0 &&
      !(simplify(e$title) %in% article_titles) &&
      !(length(e$authors) > 0 && author_list(e) %in% article_authors)
  }, entries))

  members <- load_members()
  out <- character()
  if (length(preprints)) {
    out <- c(out, "## Preprints", "", format_list(preprints, members))
  }
  years <- vapply(articles, function(e) as.character(e$year %||% "Undated"), "")
  for (y in unique(years)) {
    out <- c(out, paste("##", y), "", format_list(articles[years == y], members))
  }
  cat(out, sep = "\n")
}
