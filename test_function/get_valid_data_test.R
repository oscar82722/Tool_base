new_dir <- "C:/Users/USER/Downloads/function_tool/"
setwd(new_dir)
source("tool_function/get_valid_data.R")
library(testthat)

# unit_test
# test_item: 1.function work、2.duplicate date 
#===============================================================================
test_that("get_valid_data() function test", {
  dt <- data.table(
    CHR_NO = c("ID1", "ID2", "ID2", "ID1", "ID2", "ID1"),
    IPD_DATE = c("1050101", "1050202", "1050303", "1050404", "1050505", 
                 "1050404"))
  
  Standard_ID_cols <- c("CHR_NO","IPD_DATE")
  k <- 2
  
  valid_data <- get_valid_data(dt, Standard_ID_cols, k)
  
  # create answer
  answer <- data.table(
    CHR_NO = c("ID1", "ID2", "ID2"),
    IPD_DATE = c("1050101","1050202","1050303"),
    DATE = as.Date(c("2016-01-01","2016-02-02","2016-03-03")),
    counts = as.integer(c(2,3,2)) 
  )
  
  # test same result 
  expect_equal(valid_data, answer)
  
})

