#!/usr/bin/env Rscript

# Brief: scrape toolGalleryItem entries from an index.html and build image URLs
# Usage: source("scrape_uhc_data_tools.R")  # or run as a script

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

  rows <- map_df(items, function(item) {
    title <- safe_text(item, c(".title", ".toolTitle", "h1", "h2", "h3", ".card-title"))
    date  <- safe_text(item, c(".date", ".toolDate", ".meta .date"))
    desc  <- safe_text(item, c(".desc", ".description", "p", ".card-text"))
    # link inside element with class outputsItem
    link_node <- html_node(item, ".outputsItem a, a.outputsItem, .outputsItem > a")
    href <- NA_character_
    if (length(link_node) > 0) {
      href <- html_attr(link_node, "href")
    } else {
      # fallback: any anchor in the item
      any_a <- html_node(item, "a")
      if (length(any_a) > 0) href <- html_attr(any_a, "href")
    }
    link_resolved <- if (!is.na(href) && nzchar(href)) xml2::url_absolute(href, html_url) else NA_character_
    
    # Extract image src directly from <img> tag
    img_node <- html_node(item, "img")
    img_src <- NA_character_
    if (length(img_node) > 0) {
      img_src <- html_attr(img_node, "src")
    }
    # Build full image URL by prepending base
    image_url <- if (!is.na(img_src) && nzchar(img_src)) {
      paste0(image_base, sub("^/+", "", img_src))
    } else {
      NA_character_
    }

    tibble::tibble(
      title = title,
      date  = date,
      desc  = desc,
      link  = href,
      link_abs = link_resolved,
      image_url = image_url
    )
  })

  rows
}

# default URL you supplied
url <- "https://raw.githubusercontent.com/Drexel-UHC/data-tools/refs/heads/main/docs/index.html"
results <- scrape_tools(url)
results
readr::write_csv(file="uhc_data_tools.csv", results, na = "")
