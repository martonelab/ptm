
test_that("check that the columns have not changed",{
  test_list <- readr::read_csv("../testfiles/PTM_data_test.csv", skip = 1)
  expect_equal(names(masterlist()), names(test_list))
})
