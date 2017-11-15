# Step 6: rm_free_negative_cf
rm_free_negative_cf <- function(yoy_df) {
  stock_codes <- yoy_df$stock_code
  y_fin_url <- sprintf("https://finance.yahoo.com/quote/%s.TW/cash-flow?p=%s.TW", stock_codes, stock_codes)
  op_cf_xpath <- "//tr[@class='Bdbw(0px)! H(36px)'][2]/td[@class='Fw(b) Fz(s) Ta(end) Pb(20px)']/span"
  inv_cf_xpath <- "//tr[@class='Bdbw(0px)! H(36px)'][3]/td[@class='Fw(b) Fz(s) Ta(end) Pb(20px)']/span"
  fin_cf_xpath <- "//tr[@class='Bdbw(0px)! H(36px)'][4]/td[@class='Fw(b) Fz(s) Ta(end) Pb(20px)']/span"
  free_cf_list <- list()
  for (i in 1:length(stock_codes)) {
    html_doc <- read_html(y_fin_url[i])
    op_cf <- html_doc %>%
      html_nodes(xpath = op_cf_xpath) %>%
      html_text %>%
      gsub(pattern = ",", replacement = "", .) %>%
      as.numeric
    inv_cf <- html_doc %>%
      html_nodes(xpath = inv_cf_xpath) %>%
      html_text %>%
      gsub(pattern = ",", replacement = "", .) %>%
      as.numeric
    fin_cf <- html_doc %>%
      html_nodes(xpath = fin_cf_xpath) %>%
      html_text %>%
      gsub(pattern = ",", replacement = "", .) %>%
      as.numeric
    free_cf <- op_cf + inv_cf + fin_cf
    single_df <- data.frame(
      stock_code = stock_codes[i],
      year = c("free_cf_year_1", "free_cf_year_2", "free_cf_year_3", "free_cf_year_4"),
      free_cf = free_cf
    )
    wide_df <- spread(single_df, key = year, value = free_cf)
    free_cf_list[[i]] <- wide_df
    Sys.sleep(runif(1, min = 1, max = 3))
  }
  final_wide_df <- free_cf_list[[1]]
  for (i in 2:length(free_cf_list)) {
    final_wide_df <- rbind(final_wide_df, free_cf_list[[i]])
  }
  positive_free_cf <- merge(yoy_df, final_wide_df)
  positive_free_cf$is_pos_free_cf <- (positive_free_cf$free_cf_year_1 > 0) & (positive_free_cf$free_cf_year_2 > 0) & (positive_free_cf$free_cf_year_3 > 0) & (positive_free_cf$free_cf_year_4 > 0)
  positive_free_cf <- positive_free_cf[positive_free_cf$is_pos_free_cf, ]
  return(positive_free_cf)
}