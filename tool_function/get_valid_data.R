library(assertthat)
library(dplyr)
library(data.table)

# function_name: standardized date format
standardized_date <- function(dt, date_col){
  # Transfer date type 
  if (!grepl("^\\d{4}\\d{2}\\d{2}$", dt[[date_col]][1])) {
    years <- substr(dt[[date_col]], 1, 3)
    years_ad <- as.numeric(years) + 1911
    dt[[date_col]] <- paste0(years_ad, "-", substr(dt[[date_col]], 4, 5), 
                      "-", substr(dt[[date_col]], 6, 7))
    dt[[date_col]] <- as.Date(dt[[date_col]], format = "%Y-%m-%d")
  }else {
    dt[[date_col]] <- as.Date(dt[[date_col]], format = "%Y%m%d")
  }
  
  return(dt)
}

#===============================================================================
# function_name: get_valid_data
# parameters:
#   - dt(data_table): 門診資料
#   - group_id_col: 病人ID欄位
#   - date_col: 日期欄位
#   - k(numeric): 一年內看診次數
# return:
#   - data_table 一年內超過k筆數據的資料

get_valid_data <- function(dt, group_id_col, date_col, k){
  
  # Data type restrictions
  assert_that(is.data.table(dt), msg="Error: 'dt' must be a data.table")
  assert_that(is.character(group_id_col), 
              msg="Error: 'group_id_col' must be a character")
  assert_that(is.character(date_col), 
              msg="Error: 'date_col' must be a character")
  assert_that(is.numeric(k), msg="Error: 'k' must be a numeric.")
  
  # rename data col
  dt <- setnames(dt, date_col, "DATE")
  
  # Drop Duplicate
  dt <- unique(dt, by = c(group_id_col, "DATE"))
  
  # Standardized date format
  dt <- standardized_date(dt, "DATE")
  
  # sort date
  dt <- setorderv(dt, c(group_id_col, "DATE"))
  dt_copy <- copy(dt) #???
  dt <- dt_copy[, k_times_data := shift(DATE, n = -(k-1)), by = group_id_col] 
  
  # cal diff => filter < 365 days => remove diff
  dt[, diff := k_times_data - DATE]
  dt <- dt[diff <= 365]
  dt[, diff := NULL]
  
  return(dt)
}
