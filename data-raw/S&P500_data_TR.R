### S&P500 company descriptions dataset

library(tidyr)
library(purrr)
library(dplyr)
library(rvest)

# Clean wd
rm(list=ls())

# Params
start <- 1
base_url <- 'https://www.reuters.com/finance/markets/index/.SPX?sortBy=last&sortDir=DESC&pn='

# Function to crawl out urls from companies
crawl <- function(start, base, res = c(), max_depth = 50) {

  # Create page url
  base_cur <- paste0(base, start)

  # Load webpage and extract links
  res_cur <- read_html(base_cur) %>%
    html_node(xpath = '//*[@id="content"]') %>%
    html_node(xpath = '//*[@id="sectionHeader"]') %>%
    html_node(xpath = '//*[@id="topContent"]/div/div[2]/div[1]/table') %>%
    html_nodes("a") %>%
    html_attr("href")

  if(length(res_cur) == 0 | start == max_depth) {

    # Append results
    res <- c(res, res_cur)

    # Return
    return(res)

  } else {

    crawl(start + 1, base, res = c(res, res_cur))

  }

}

# Call
get_desc <- crawl(start = 1, base = base_url)

# Remove evrything that starts with 'javascript'
library(stringr)
filt <- vapply(get_desc, function(x) str_detect(x, "javascript"), TRUE)
get_desc_filterd <- get_desc[!filt]

# Get ticker symbol
tickers <- vapply(get_desc_filterd,
                  function(x) {
                    r <- str_split(x, "/")[[1]]
                    r[length(r)]
                  },
                  "character") %>%
  unique()

# Write a crawler for individual pages
pg <- paste0("https://www.reuters.com/finance/stocks/company-profile/",
             tickers)

# Crawler
crawl_page <- function(page, ticker) {

  # Load
  aboutPage <- read_html(page)

  # Get company name
  genInfo <- aboutPage %>%
    html_node(css = "#sectionHeader") %>%
    html_node(css = "#sectionTitle") %>%
    html_node("h1") %>%
    html_text() %>%
    str_replace_all(., "Profile:\\s|\\s\\([A-Z]{1,5}\\.[A-Z]{1,3}\\)", "")

  # This is the text of a description
  desc <- aboutPage %>%
    # Navigate down the tree
    html_node(xpath = '//*[@id="content"]') %>%
    html_node(xpath = '//*[@id="companyNews"]') %>%
    html_node(css = 'div > div.moduleBody') %>%
    html_nodes("p") %>%
    # Extract text from each paragraph
    html_text() %>%
    # Rather do this with a list, but stuck with NULL characters
    data_frame("paragraph" = .) %>%
    # Add number of characters
    mutate("chars" = nchar(paragraph)) %>%
    # Filter if chars < 50
    filter(chars > 50) %>%
    # Take resulting paragraphs
    select(paragraph) %>%
    pull() %>%
    # Collapse to single string
    paste(., collapse = " ")

  # Return a list
  list(
    "company" = genInfo,
    "url" = page,
    "desc" = desc,
    "ticker" = ticker
  )

}

# Lapply
desc <- mapply(crawl_page,
               page = pg,
               ticker = tickers,
               SIMPLIFY = FALSE)

# Names
nams <- vapply(desc, function(x) x$company, "character")
names(desc) <- nams

# Change object name
SAP500 <- desc

# Dump
devtools::use_data(SAP500, overwrite = TRUE)
