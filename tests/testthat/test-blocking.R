test_that("blocking_fun creates correct block hashes", {
  # Create test data
  test_data <- data.table::data.table(
    input = c("123 MAIN STREET BRISBANE 4000", "456 QUEEN STREET MELBOURNE 3000"),
    short_normalised_input = c("MAINST", "QUEENST")
  )
  
  # Test with not_gnaf = TRUE
  result <- blocking_fun(test_data, "input", not_gnaf = TRUE)
  
  # Check that blocking columns are created
  expect_true("block_1" %in% names(result))
  expect_true("block_2" %in% names(result))
  expect_true("block_3" %in% names(result))
  expect_true("block_4" %in% names(result))
  
  # Check that block_1 contains only digits and spaces
  expect_true(all(grepl("^[0-9\\s]+$", result$block_1)))
  
  # Check that block_4 contains only letters
  expect_true(all(grepl("^[A-Z]+$", result$block_4)))
})

test_that("blocking_fun handles GNAF vs non-GNAF data", {
  # Create test data with required columns
  gnaf_data <- data.table::data.table(
    address = c("123 MAIN STREET BRISBANE 4000"),
    short_address = c("MAINST")
  )
  
  non_gnaf_data <- data.table::data.table(
    input = c("123 MAIN STREET BRISBANE 4000"),
    short_normalised_input = c("MAINST")
  )
  
  # Test GNAF data (not_gnaf = FALSE)
  gnaf_result <- blocking_fun(gnaf_data, "address", not_gnaf = FALSE)
  expect_true("block_3" %in% names(gnaf_result))
  
  # Test non-GNAF data (not_gnaf = TRUE)
  non_gnaf_result <- blocking_fun(non_gnaf_data, "input", not_gnaf = TRUE)
  expect_true("block_3" %in% names(non_gnaf_result))
})

test_that("blocking_fun handles edge cases", {
  # Empty data
  empty_data <- data.table::data.table(input = character(0))
  result <- blocking_fun(empty_data, "input", not_gnaf = TRUE)
  expect_equal(nrow(result), 0)
  expect_true("block_1" %in% names(result))
  
  # Single row
  single_row <- data.table::data.table(
    input = "123 MAIN ST",
    short_normalised_input = "MAINST"
  )
  result <- blocking_fun(single_row, "input", not_gnaf = TRUE)
  expect_equal(nrow(result), 1)
})
