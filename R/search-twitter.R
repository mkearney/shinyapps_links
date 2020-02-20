## load the pipe
library(magrittr)

## use rtweet
sa <- rtweet::search_tweets(
  "url:shinyapps.io OR (shiny app OR application OR rstudio) OR shinyapps OR shinyapp",
  n = 10000, include_rts = FALSE)

## read-in README to get links already collected
l <- tfse::readlines("README.Rmd")

## parse those links
links <- unique(sub("/+$", "", gsub(".*\\(|\\)$", "", grep("^\\+ \\[", l, value = TRUE))))
links <- grep("https://[^/]+\\.shinyapps\\.io/[^/]+$", links, value = TRUE)

## get shiny links add to links and sort
links <- c(sa$urls_expanded_url, sa$urls_url, sa$media_expanded_url, sa$ext_media_expanded_url) %>%
  unlist() %>%
  sub("/?(\\#|\\?).*$", "", .) %>%
  tfse::regmatches_("^https?://[^/]+.shinyapps\\.io/[^/]+/?", drop = TRUE) %>%
  sub("^http:", "https:", .) %>%
  sub("/+$", "", .) %>%
  unique() %>%
  c(links) %>%
  unique() %>%
  sort() ->
  links

## shinyapps user name
user <- regexpr("(?<=//)[^/]+(?=\\.shinyapps)", links, perl = TRUE)
user <- regmatches(links, user)

## shinyapps app name
app <- regexpr("(?<=shinyapps.io/)[^/]+", links, perl = TRUE)
app <- regmatches(links, app)

## as data table
d <- data.table::data.table(
  user = user,
  app = app,
  url = links
)

## markdown link (as list item)
d[, md_url := paste0("+ [**", app, "** by *", user, "*](", url, ")")]

## get title/description to include with link
get_title <- function(url) {
  tryCatch({
    h <- tryCatch(readthat::read(url), error = function(e) NULL)
    if (is.null(h) || nchar(h[1]) == 0) {
      return("")
    }
    Sys.sleep(2)
    h <- xml2::read_html(url)
    title <- rvest::html_text(rvest::html_nodes(h, "h1,h2,h3,h4,title,.title"), trim = TRUE)
    if (length(title) == 0) {
      return("")
    }
    title <- title[nchar(title) > 0][1]
    if (grepl("Please.{0,4}Wait", title, ignore.case = TRUE)) {
      return("")
    }
    title
  }, error = function(e) "")
}
o <- vector("list", nrow(d))
for (i in seq_along(o)) {
  if (length(o[[i]]) > 0) {
    cat(i, "\n")
    next
  }
  o[[i]] <- get_title(d[, url][i])
  cat(i, "\n")
}
d[, title := unlist(o)]
dd <- d[!is.na(title), ]
dd[, md_url := paste0(md_url, ": ", title)]

by_app <- data.table::copy(dd[order(!grepl("^[[:alpha:]]", app), tolower(app)), ])
by_app[, letter := toupper(substr(by_app[, sub("^[[:punct:]]", "", app)], 1, 1))]
by_app[, md_url := ifelse(duplicated(letter), md_url, paste0("\n## ", letter, "\n", md_url))]
by_app[, md_url := tfse::trim_ws(gsub("\n+", " ", md_url))]
by_app[, md_url := ifelse(grepl("^#", md_url), paste0("\n", md_url), md_url)]
by_app[, md_url := ifelse(grepl("^#", md_url), paste0(md_url, "\n"), md_url)]
by_app2 <- data.table::copy(by_app)
by_app[, md_url := sub("(?<=## \\S) (?=\\+)", "\n", md_url, perl = TRUE)]

## get the readme top-matter
toc <- unique(by_app[, letter])
toc <- paste0("## Table of Contents (alphabetically by app name)\n",
  paste0("+ [", toc, "](#", tolower(toc), ")", collapse = "\n"))
readme_prem <- c(
  '---',
  'title: "ShinyApps"',
  'output: github_document',
  '---',
  '',
  'A collection of links to [Shiny apps](https://shinyapps.io)',
  'that have been shared on Twitter.',
  '',
  toc)

writeLines(c(readme_prem, by_app_no_title[, md_url]), "README-notitle.RMD")

## combine and sort new and old links, and then format/write output
writeLines(c(readme_prem, by_app[, md_url]), "README.Rmd")


## render to markdown
rmarkdown::render("README.Rmd")

## preview and then remove HTML version
browseURL("README.html")
unlink("README.html")

## add to git
git2r::add(path = c("README.Rmd", "README.md"))
git2r::commit(message = "Update")
git2r::push()
