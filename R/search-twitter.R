## load rtweet, h.rtweet, and kmw packages
if (!requireNamespace("tfse", quietly = TRUE)) {
  install.packages("tfse")
}

## install if not already
tfse::install_if(c("rtweet", "rmarkdown", "dapr"))

## install at least version 0.1.0 of tbltools
if (packageVersion("tbltools") < "0.1.0") {
  tfse::install_if("remotes")
  remotes::install_github("mkearney/tbltools")
}

## load tbltools
library(tbltools)

## if {h.rtweet} is available
if (requireNamespace("h.rtweet", quietly = TRUE)) {
  ## search most recent 10,000 tweets mentioning shinyapps.io
  sa <- h.rtweet::h.search_tweets("shinyapps.io", n = 10000)
} else {
  sa <- rtweet::search_tweets(
    "url:shinyapps.io", n = 10000, include_rts = FALSE)
}

## lookup full info from API
sa <- rtweet::lookup_tweets(sa$status_id)

## read-in README to get links already collected
l <- tfse::readlines("README.Rmd")

## parse those links
readme_links <- gsub(".*\\(|\\)$", "", grep("^\\+ \\[", l, value = TRUE))

## get the readme top-matter
readme_prem <- c(
  '---',
  'title: "ShinyApps"',
  'output: github_document',
  '---',
  '',
  'A collection of links to [Shiny applications](https://shinyapps.io)",
  "that have been shared on Twitter.',
  '')

## function to convert to markdown lists
account_app_href_li <- function(x) {
  account <- tfse::regmatches_(x, "(?<=https://)\\S+(?=\\.shiny)")
  app <- tfse::regmatches_(x, "(?<=shinyapps\\.io/)\\S+(?=/)")
  fst_let <- toupper(substr(account, 1, 1))
  fst_let <- ifelse(!duplicated(fst_let), paste0("\n## ", fst_let, "\n"), "")
  x <- paste0(fst_let, "+ [", account, "/", app, "](", x, ")")
  unlist(strsplit(x, "\n"))
}

## combine and sort new and old links, and then format/write output
sa$urls_expanded_url %>%
  unlist() %>%
  tfse::regmatches_("https://\\S+\\.shinyapps\\.io/[^/]+/?", drop = TRUE) %>%
  c(readme_links) %>%
  sub("^http:", "https:", .) %>%
  sub("/?$", "/", .) %>%
  unique() %>%
  sort() %>%
  account_app_href_li() %>%
  c(readme_prem, .) %>%
  writeLines("README.Rmd")

## render to markdown
rmarkdown::render("README.Rmd")

## preview and then remove HTML version
browseURL("README.html")
unlink("README.html")

## add to git
git2r::add(path = c("README.Rmd", "README.md"))
git2r::commit(message = "Update")
git2r::push()
