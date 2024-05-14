library(reshape2)
library(assertthat)
library(data.table)

# function sets
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
