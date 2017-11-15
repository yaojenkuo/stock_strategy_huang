# Step 2-1: rm_ky_stocks()
rm_ky_stocks <- function(top_100_df) {
  rm_pattern <- "-KY"
  non_ky_logical <- !(grepl(pattern = "-KY", top_100_df$stock_name))
  non_ky_df <- top_100_df[non_ky_logical, ]
  return(non_ky_df)
}