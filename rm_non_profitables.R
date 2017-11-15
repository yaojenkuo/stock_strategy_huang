# Step 3: rm_non_profitables
library(tidyr)

rm_non_profitables <- function(stable_df) {
  margin_after_tax <- list()
  periods <- list()
  periods_css <- "#ctl00_ContentPlaceHolder1_tb1 td:nth-child(1)"
  margin_after_tax_css <- ".rt:nth-child(6)"
  
  # get data from cnyes
  cnyes_bratio_urls <- paste0("https://www.cnyes.com/twstock/bratio/", stable_df$stock_code, ".htm")
  for (i in 1:length(cnyes_bratio_urls)) {
    html_doc <- cnyes_bratio_urls[i] %>%
      read_html
    company_periods <- html_doc %>%
      html_nodes(css = periods_css) %>%
      html_text
    company_margin_after_tax <- html_doc %>%
      html_nodes(css = margin_after_tax_css) %>%
      html_text %>%
      as.numeric
    periods[[i]] <- company_periods
    margin_after_tax[[i]] <- company_margin_after_tax
    Sys.sleep(round(runif(1, min = 1, max = 3)))
  }
  
  # filter 4 years of margin after tax
  df_list <- list()
  for (i in 1:length(periods)) {
    four_yr_logical <- grepl(pattern = "1~12月", periods[[i]])
    four_yr_logical[1] <- TRUE
    four_yr_period <- periods[[i]][four_yr_logical]
    four_yr_margin_after_tax <- margin_after_tax[[i]][four_yr_logical]
    single_df <- data.frame(
      stock_code = stable_df$stock_code[i],
      period = four_yr_period,
      margin_rate_after_tax = four_yr_margin_after_tax
    )
    df_list[[i]] <- single_df
  }
  
  # transform to wide form
  wide_df_list <- list()
  for (i in 1:length(df_list)) {
    wide_df <- spread(df_list[[i]], key = period, value = margin_rate_after_tax)
    names(wide_df) <- c("stock_code", "margin_rate_after_tax_1", "margin_rate_after_tax_2", "margin_rate_after_tax_3", "margin_rate_after_tax_4")
    wide_df_list[[i]] <- wide_df
  }
  
  # append to a single df
  all_wide_df <- wide_df_list[[1]]
  for (i in 2:length(wide_df_list)) {
    all_wide_df <- rbind(all_wide_df, wide_df_list[[i]])
  }
  profitable_df <- merge(stable_df, all_wide_df)
  is_profitable_flag <- c()
  for (i in 1:nrow(profitable_df)) {
    is_profitable <- (profitable_df[i, "margin_rate_after_tax_1"] > 0) & (profitable_df[i, "margin_rate_after_tax_2"] > 0) & (profitable_df[i, "margin_rate_after_tax_3"] > 0) & (profitable_df[i, "margin_rate_after_tax_4"] > 0)
    is_profitable_flag <- c(is_profitable_flag, is_profitable)
  }
  profitable_df$is_profitable <- is_profitable_flag
  profitable_df <- profitable_df[profitable_df$is_profitable,]
  return(profitable_df)
}