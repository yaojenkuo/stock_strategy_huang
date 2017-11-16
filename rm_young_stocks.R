# Step 2-2: rm_young_stocks
rm_young_stocks <- function(non_ky_df) {
  stock_codes <- non_ky_df$stock_code
  c_urls <- paste0("https://tw.stock.yahoo.com/d/s/company_", stock_codes, ".html")
  listed_dates <- c()
  
  for (c_url in c_urls) {
    listed_date <- tryCatch({
      c_url %>%
        read_html %>%
        html_nodes(css = "table:nth-child(1) tr:nth-child(4) td:nth-child(2)") %>%
        html_text
    }, error = function(e){
      "999/99/99"
    })
    listed_dates <- c(listed_dates, listed_date)
    Sys.sleep(round(runif(1, min = 1, max = 3)))
  }
  year_month_day <- listed_dates %>%
    strsplit(split = "/") %>%
    unlist
  listed_years <- c()
  listed_months <- c()
  listed_days <- c()
  for (i in 1:length(year_month_day)) {
    if (i %% 3 == 1) {
      listed_years <- c(listed_years, year_month_day[i])
    } else if (i %% 3 == 2) {
      listed_months <- c(listed_months, year_month_day[i])
    } else {
      listed_days <- c(listed_days, year_month_day[i])
    }
  }
  listed_years <- as.integer(listed_years) + 1911
  listed_dates <- paste(listed_years, listed_months, listed_days, sep = "-")
  listed_dates <- as.Date(listed_dates)
  two_year_threshold <- seq(Sys.Date() - 1, by = "-2 years", length.out = 2)[2]
  stable_logical <- listed_dates < two_year_threshold
  stable_df <- non_ky_df[stable_logical, ]
  stable_df$listed_date <- listed_dates[stable_logical]
  return(stable_df)
}