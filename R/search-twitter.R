## load rtweet, h.rtweet, and kmw packages
library(rtweet)
library(h.rtweet)

## search most recent 10,000 tweets mentioning shinyapps.io
sa <- h.search_tweets("shinyapps.io", n = 10000)

## lookup full info from API
sa <- lookup_tweets(sa$status_id)

## read-in README to get links already collected
l <- tfse::readlines("README.Rmd")

## parse those links
readme_links <- grep("^\\+ \\[", l, value = TRUE)

## get the readme top-matter
readme_prem <- c(
  '---',
  'title: "ShinyApps"',
  'output: github_document',
  '---',
  '',
  'A collection of links to [Shiny applications](https://shinyapps.io) that have been shared on Twitter.',
  '')

## function to create the link entry
account_app_href_li <- function(x) {
  account <- tfse::regmatches_(x, "(?<=https://)\\S+(?=\\.shiny)")
  app <- tfse::regmatches_(x, "(?<=shinyapps\\.io/)\\S+(?=/)")
  paste0("+ [", account, "/", app, "](", x, ")")
}

## combine and sort new and old links, and then format/write output
sa$urls_expanded_url %>%
  unlist() %>%
  grep("shinyapps\\.io/\\S+", ., value = TRUE) %>%
  gsub("^http:", "https:", .) %>%
  paste0("/") %>%
  tfse::regmatches_("https://\\S+\\.shinyapps\\.io/[^/]+/?", drop = TRUE) %>%
  account_app_href_li() %>%
  c(readme_links) %>%
  unique() %>%
  sort() %>%
  c(readme_prem, .) %>%
  writeLines("README.Rmd")

## render to markdown
rmarkdown::render("README.Rmd")

## preview and then remove HTML version
browseURL("README.html")
unlink("README.html")
