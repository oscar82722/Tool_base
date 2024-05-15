new_dir <- "C:/Users/USER/Downloads/function_tool/"
setwd(new_dir)
source("tool_function/fetch_function.R")
library(testthat)

#===============================================================================
# fetch_data: unit_test
# test_item: 
# 1.function work，2.duplicate date，3.diff group id，4.diff date type
# 5.different k=-1,0,1,3

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


#===============================================================================
# get_valid_data: unit_test
# test_item: 
# 1.function work，2.duplicate date，3.diff group id，4.diff date type
# 5.different k=-1,0,1,3
test_that("get_valid_data() function test", {
  
  # example1
  dt <- data.table(
    ID_TEST = c("ID1", "ID2", "ID2", "ID1", "ID2", "ID1"),
    IPD_DATE = c("20160101", "20160202", "20160303", "20160404", "20160505", 
                 "20160404"))
  
  k <- 2
  valid_data <- get_valid_data(dt, "ID_TEST", "IPD_DATE", k)
  
  # create answer
  answer <- data.table(
    ID_TEST = c("ID1", "ID2", "ID2"),
    DATE = as.Date(c("2016-01-01", "2016-02-02", "2016-03-03")),
    k_times_data = as.Date(c("2016-04-04", "2016-03-03", "2016-05-05"))
  )
  
  # test same result 
  expect_equal(valid_data, answer)
  
  # example2
  dt2 <- data.table(
    CHR_NO = c("ID1", "ID1", "ID1", "ID1", "ID2", "ID1"),
    OPD_DATE = c("1050101", "1050202", "1050303", "1050404", "1050505", 
                 "1020404"))
  
  k2 <- 3
  valid_data2 <- get_valid_data(dt2, "CHR_NO", "OPD_DATE", k2)
  
  
  # create answer
  answer2 <- data.table(
    CHR_NO = c("ID1", "ID1"),
    DATE = as.Date(c("2016-01-01", "2016-02-02")),
    k_times_data = as.Date(c("2016-03-03", "2016-04-04"))
  )
  
  # test same result 
  expect_equal(valid_data2, answer2)
  
})

