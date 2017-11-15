# Step 5: rm_monthly_sales_decline
rm_monthly_sales_decline <- function(large_mkt_cap_df) {
  yoy_css_1 <- "td td tr~ tr+ tr .ttt:nth-child(3)"
  yoy_css_2 <- "td td tr~ tr+ tr .ttt:nth-child(6)"
  yoy_list <- list()
  for (i in 1:length(large_mkt_cap_df$stock_code)) {
    y_stock_url <- sprintf("https://tw.stock.yahoo.com/d/s/earning_%s.html", large_mkt_cap_df$stock_code[i])
    html_doc <- read_html(y_stock_url)
    yoy_1 <- html_doc %>%
      html_nodes(css = yoy_css_1) %>%
      html_text
    yoy_2 <- html_doc %>%
      html_nodes(css = yoy_css_2) %>%
      html_text
    cleaned_yoy_2 <- yoy_2[!(yoy_2 == "-")]
    yoy_vec <- c(yoy_1, cleaned_yoy_2)
    yoy_vec <- yoy_vec %>%
      gsub(pattern = "%", replacement = "", .) %>%
      as.numeric
    latest_yoy_vec <- yoy_vec[(length(yoy_vec) - 2):length(yoy_vec)]
    single_df <- data.frame(
      stock_code = large_mkt_cap_df$stock_code[i],
      yoy_month = c("yoy_3m", "yoy_2m", "yoy_1m"),
      yoy = latest_yoy_vec
    )
    wide_df <- spread(single_df, key = yoy_month, value = yoy)
    yoy_list[[i]] <- wide_df
  }
  final_wide_df <- yoy_list[[1]]
  for (i in 2:length(yoy_list)) {
    final_wide_df <- rbind(final_wide_df, yoy_list[[i]])
  }
  yoy_df <- merge(large_mkt_cap_df, final_wide_df)
  is_declined <- (yoy_df$yoy_3m < 0) | (yoy_df$yoy_2m < 0) | (yoy_df$yoy_1m < 0)
  yoy_df$is_declined <- is_declined
  yoy_df <- yoy_df[yoy_df$is_declined == FALSE, ]
  return(yoy_df)
}