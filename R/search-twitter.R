## load rtweet
library(rtweet)

## search tweets
sh <- search_tweets("url:shinyapps.io", n = 5000, include_rts = FALSE)

## read-in README to get links already collected
l <- tfse::readlines("README.Rmd")

## parse those links
readme_links <- grep("^\\+ \\[", l, value = TRUE)

## get the readme top-matter
readme_prem <- l[1:(grep("^\\+ \\[", l)[1] - 1)]

## function to create the link entry
account_app_href_li <- function(x) {
  account <- tfse::regmatches_(x, "(?<=https://)\\S+(?=\\.shiny)")
  app <- tfse::regmatches_(x, "(?<=shinyapps\\.io/)\\S+(?=/)")
  paste0("+ [", account, "/", app, "](", x, ")")
}

## combine and sort new and old links, and then format/write output
sh$urls_expanded_url %>%
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
