library(reshape2)
library(assertthat)
library(data.table)
library(dplyr)

#===============================================================================
# function_name: fetch_data
# parameters:
#   - data(data_table): 包含需要處理的資料
#   - target_ID(vector): 要標準化的目標 ID 列表 
#   - disease_ID(vector): 疾病 ID 所在的欄位 (疾病col1, 疾病col2, 疾病col2)
#   - disease_codes(vector): 要配對的疾病碼 
# return:
#   - data_table

fetch_data <- function(dt, target_ID_cols, disease_ID_cols, disease_codes){
  
  # Data type restrictions
  assert_that(is.data.table(dt), msg="Error: 'df' must be a data.table")
  assert_that(is.vector(target_ID_cols), 
              msg="Error: 'target_ID_cols' must be a list.")
  assert_that(is.character(disease_codes), 
              msg="Error: 'search_ID' must be a character vector.")
  
  dt <- dt[, c(target_ID_cols,disease_ID_cols),with = FALSE]
  
  # Step1: Melt disease_ID
  melted_data <- melt(dt, id.var=target_ID_cols)
  melted_data <- as.data.table(melted_data)
  
  # Step2: Match disease codes: more
  melted_data[, value := as.character(value)]
  filtered_data <- melted_data[grepl(paste0("^", paste(disease_codes, collapse="|^")), 
                                     value)]
  return(filtered_data)
}

#===============================================================================
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

