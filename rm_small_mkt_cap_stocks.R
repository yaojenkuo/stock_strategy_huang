# Step 4: rm_small_mkt_cap_stocks
rm_small_mkt_cap_stocks <- function(profitable_df) {
  stock_codes <- profitable_df$stock_code
  mkt_caps <- c()
  for (stock_code in stock_codes) {
    google_fin_url <- sprintf("https://finance.google.com/finance?q=TPE%%3A%s&ei=7isMWpmDJoim0QTX-6_YBw", stock_code)
    mkt_cap <- google_fin_url %>%
      read_html %>%
      html_nodes(css = ".val") %>%
      html_text
    mkt_cap <- mkt_cap[5] %>%
      gsub(pattern = "B\n", replacement = "", .) %>%
      as.numeric
    mkt_caps <- c(mkt_caps, mkt_cap * 30)
  }
  profitable_df$mkt_cap_in_billion_ntd <- mkt_caps
  large_mkt_cap_df <- profitable_df[profitable_df$mkt_cap_in_billion_ntd > 5, ]
  return(large_mkt_cap_df)
}