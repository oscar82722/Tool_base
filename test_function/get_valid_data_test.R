new_dir <- "C:/Users/USER/Downloads/function_tool/"
setwd(new_dir)
source("tool_function/get_valid_data.R")
library(testthat)

# unit_test_test_item: 
# 1.function work，2.duplicate date，3.diff group id，4.diff date type
# 5.different k
#===============================================================================
test_that("get_valid_data() function test", {
  
  # example1
  dt <- data.table(
    ID_TEST = c("ID1", "ID2", "ID2", "ID1", "ID2", "ID1"),
    IPD_DATE = c("20160101", "20160202", "20160303", "20160404", "20160505", 
                 "20160404"))
  
  Standard_ID_cols <- c("ID_TEST","IPD_DATE")
  group_id <- Standard_ID_cols[1]
  k <- 2

  valid_data <- get_valid_data(dt, Standard_ID_cols, k)
  
  # create answer
  answer <- data.table(
    ID_TEST = c("ID1", "ID2", "ID2"),
    IPD_DATE = c("20160101","20160202","20160303"),
    DATE = as.Date(c("2016-01-01","2016-02-02","2016-03-03")),
    counts = as.integer(c(2,3,2)) 
  )
  
  # test same result 
  expect_equal(valid_data, answer)
  
  # example2
  dt2 <- data.table(
    CHR_NO = c("ID1", "ID1", "ID1", "ID1", "ID2", "ID1"),
    OPD_DATE = c("1050101", "1050202", "1050303", "1050404", "1050505", 
                 "1020404"))
  
  Standard_ID_cols2 <- c("CHR_NO","OPD_DATE")
  group_id2 <- Standard_ID_cols2[1]
  k2 <- 3
  
  valid_data2 <- get_valid_data(dt2, Standard_ID_cols2, k2)
  
  # create answer
  answer2 <- data.table(
    CHR_NO = c("ID1", "ID1"),
    OPD_DATE = c("1050101","1050202"),
    DATE = as.Date(c("2016-01-01","2016-02-02")),
    counts = as.integer(c(4,3)) 
  )
  
  # test same result 
  expect_equal(valid_data2, answer2)
  
})

