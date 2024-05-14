new_dir <- "C:/Users/USER/Downloads/function_tool/"
setwd(new_dir)
source("tool_function/fetch_function.R")
library(testthat)

# unit_test
#===============================================================================
# 定義一個測試集
test_that("fetch_data() function test", {
  
  # 創建一個測試用的假資料表
  dt <- data.table(
    CHR_NO = c("ID1", "ID2", "ID3"),
    OPD_DATE = c("2024-01-01", "2024-01-02", "2024-01-03"),
    ICD9_CODE1 = c("5434.01", "434.01", "434.02"),
    ICD9_CODE2 = c("585", "586", "587")
  )
  
  # 定義目標 ID 列表、疾病 ID 列表和疾病碼
  target_ID_cols <- c("CHR_NO", "OPD_DATE")
  disease_ID_cols <- c("ICD9_CODE1", "ICD9_CODE2")
  disease_codes <- c("434.01", "585")
  
  # 測試 fetch_data() 函數是否能正常運行並返回正確的結果
  filtered_data <- fetch_data(dt, target_ID_cols, disease_ID_cols, disease_codes)
  
  answer <- data.table(
    CHR_NO = c("ID2", "ID1"),
    OPD_DATE = c("2024-01-02", "2024-01-01"),
    variable = factor(c("ICD9_CODE1", "ICD9_CODE2")),
    value = c("434.01", "585")
  )
  
  # 比較兩個data table 是否一致
  expect_equal(filtered_data, answer)
  
})
