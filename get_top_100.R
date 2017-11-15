# Step 1: get_top_100()
library(rvest)

get_top_100 <- function() {
  # get html documents
  tse_url <- "https://tw.stock.yahoo.com/d/i/rank.php?t=pri&e=tse&n=100"
  otc_url <- "https://tw.stock.yahoo.com/d/i/rank.php?t=pri&e=otc&n=100"
  urls <- c(tse_url, otc_url)
  html_docs <- list()
  
  for (i in 1:length(urls)) {
    html_docs[[i]] <- read_html(urls[i])
  }
  names(html_docs) <- c("tse_doc", "otc_doc")
  
  # get stock_code_name, price
  stock_code_name_css <- ".name"
  stock_price_css <- ".name+ td"
  stock_code_names <- c()
  stock_prices <- c()
  for (i in 1:length(html_docs)) {
    stock_code_name <- html_docs[[i]] %>%
      html_nodes(css = stock_code_name_css) %>%
      html_text
    stock_code_names <- c(stock_code_names, stock_code_name)
    
    stock_price <- html_docs[[i]] %>%
      html_nodes(css = stock_price_css) %>%
      html_text %>%
      as.numeric
    stock_prices <- c(stock_prices, stock_price)
  }
  
  # split stock_code_name
  stock_codes <- c()
  stock_names <- c()
  split_result <- stock_code_names %>%
    strsplit(split = " ") %>%
    unlist
  for (i in 1:length(split_result)) {
    if (i %% 2 == 1) {
      stock_codes <- c(stock_codes, split_result[i])
    } else {
      stock_names <- c(stock_names, split_result[i])
    }
  }
  
  # form dataframe and return
  top_100_df <- data.frame(
    stock_code = stock_codes,
    stock_name = stock_names,
    stock_price = stock_prices,
    stock_type = c(rep("上市", times = 100), rep("上櫃", times = 100)),
    stringsAsFactors = FALSE
  )
  return(top_100_df)
}