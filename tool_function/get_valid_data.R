library(assertthat)
library(dplyr)
library(data.table)


#===============================================================================
# function_name: get_valid_data
# parameters:
#   - dt(data_table): 門診資料
#   - standard_ID_cols(vector): C(group by_ID, DATE)
#   - k(numeric): 一年內看診次數
# return:
#   - data_table 一年內超過k筆數據的資料

get_valid_data <- function(dt, Standard_ID_cols, k){
  
  # Data type restrictions
  assert_that(is.data.table(dt), msg="Error: 'dt' must be a data.table")
  assert_that(is.vector(Standard_ID_cols), 
              msg="Error: 'target_ID_cols' must be a list.")
  assert_that(is.numeric(k), msg="Error: 'k' must be a numeric.")
  
  # ADD "DATE" col
  date_cols_index <- grep("date", names(dt)[names(dt) %in% Standard_ID_cols], 
                          ignore.case = TRUE)
  std_id <- names(dt)[date_cols_index]
  dt[, DATE := .SD[[std_id]]]
  
  # Transfer date type
  years <- substr(dt$DATE, 1, 3)
  years_ad <- as.numeric(years) + 1911
  dt$DATE <- paste0(years_ad, "-", substr(dt$DATE, 4, 5), 
                        "-", substr(dt$DATE, 6, 7))
  dt$DATE <- as.Date(dt$DATE, format = "%Y-%m-%d")
  
  # Drop Duplicate
  dt <- unique(dt, by = c(Standard_ID_cols[1], "DATE"))
  
  # Count Data
  calculate_count <- function(sub_dt) {
    start_intervals <- sub_dt[["DATE"]]
    end_intervals <- start_intervals + 365
    counts <- sapply(start_intervals, function(start_interval) {
      sum(sub_dt$DATE >= start_interval & sub_dt$DATE <= (start_interval + 365))
    })
    return(counts)
  }
  
  dt[, counts := calculate_count(.SD), by = CHR_NO]
  dt <- dt[counts >= k]
  
  return(as.data.table(dt))
}
