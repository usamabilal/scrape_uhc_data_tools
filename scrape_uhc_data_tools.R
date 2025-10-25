#!/usr/bin/env Rscript

# Brief: scrape toolGalleryItem entries from an index.html and build image URLs
# Usage: source("scrape_tools.R")  # or run as a script

library(httr)
library(rvest)
library(xml2)
library(stringr)
library(purrr)
library(tibble) 
library(dplyr)

scrape_tools <- function(html_url,
                         image_base = "https://drexel-uhc.github.io/data-tools/") {
  resp <- httr::GET(html_url, httr::user_agent("Positron-scraper/1.0"))
  if (httr::status_code(resp) != 200) {
    stop("Failed to fetch HTML: ", httr::status_code(resp))
  }
  page <- read_html(resp)

  items <- html_nodes(page, ".toolGalleryItem")
  if (length(items) == 0) {
    return(tibble::tibble())
  }

  safe_text <- function(node, selectors) {
    for (sel in selectors) {
      el <- html_node(node, sel)
      if (length(el) > 0) {
        txt <- html_text(el, trim = TRUE)
        if (!is.na(txt) && nzchar(txt)) return(txt)
      }
    }
    NA_character_
  }

  build_image_url <- function(href, base, page_url) {
    if (is.na(href) || !nzchar(href)) return(NA_character_)
    # absolute href relative to page
    abs_href <- xml2::url_absolute(href, page_url)
    # return directly if it already points to an image
    if (grepl("\\.(png|jpg|jpeg|gif|svg)([?#].*)?$", abs_href, ignore.case = TRUE)) {
      return(abs_href)
    }
    # remove scheme+host and query/fragment for path operations
    path <- sub("^[a-zA-Z]+://[^/]+/", "", abs_href)
    path <- sub("[?#].*$", "", path)
    path <- sub("^\\./+", "", path)

    # if the path already contains img/gallery, use that path under the provided base
    if (grepl("(^|/)img/gallery(/|$)", path, ignore.case = TRUE)) {
      base_fixed <- sub("/+$", "/", base)
      return(paste0(base_fixed, sub("^/+", "", path)))
    }

    # fallback: build img/gallery/{basename}.PNG from the target page or path
    bn <- basename(path)
    bn_no_ext <- sub("\\.html?$", "", bn, ignore.case = TRUE)
    if (!nzchar(bn_no_ext)) return(NA_character_)
    img_path <- file.path("img", "gallery", paste0(bn_no_ext, ".PNG"))
    base_fixed <- sub("/+$", "/", base)
    paste0(base_fixed, sub("^/+", "", img_path))
  }

  rows <- map_df(items, function(item) {
    title <- safe_text(item, c(".title", ".toolTitle", "h1", "h2", "h3", ".card-title"))
    date  <- safe_text(item, c(".date", ".toolDate", ".meta .date"))
    desc  <- safe_text(item, c(".desc", ".description", "p", ".card-text"))
    # link inside element with class outputsItem
    link_node <- html_node(item, ".outputsItem a, a.outputsItem, .outputsItem > a")
    href <- NA_character_
    if (!is.na(link_node) && length(link_node) > 0) {
      href <- html_attr(link_node, "href")
    } else {
      # fallback: any anchor in the item
      any_a <- html_node(item, "a")
      if (!is.na(any_a) && length(any_a) > 0) href <- html_attr(any_a, "href")
    }
    link_resolved <- if (!is.na(href) && nzchar(href)) xml2::url_absolute(href, html_url) else NA_character_
    
    # Extract image src directly from <img> tag
    img_node <- html_node(item, "img")
    img_src <- NA_character_
    if (!is.na(img_node) && length(img_node) > 0) {
      img_src <- html_attr(img_node, "src")
    }
    # Build full image URL by prepending base
    image_url <- if (!is.na(img_src) && nzchar(img_src)) {
      paste0(image_base, sub("^/+", "", img_src))
    } else {
      NA_character_
    }

    tibble::tibble(
      title = ifelse(is.na(title), NA_character_, title),
      date  = ifelse(is.na(date),  NA_character_, date),
      desc  = ifelse(is.na(desc),  NA_character_, desc),
      link  = ifelse(is.na(href),  NA_character_, href),
      link_abs = ifelse(is.na(link_resolved), NA_character_, link_resolved),
      image_url = ifelse(is.na(image_url), NA_character_, image_url)
    )
  })

  rows
}

# default URL you supplied
url <- "https://raw.githubusercontent.com/Drexel-UHC/data-tools/refs/heads/main/docs/index.html"
results <- scrape_tools(url)
results
readr::write_csv(file="uhc_data_tools.csv", results, na = "")
