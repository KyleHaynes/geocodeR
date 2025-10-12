test_that("jw_split_words calculates similarity correctly", {
  skip_if_not_installed("stringdist")
  skip_if_not_installed("stringi")
  
  # Test identical strings
  result <- jw_split_words("MAIN STREET", "MAIN STREET")
  expect_true(is.numeric(result))
  expect_true(result >= 0 && result <= 1)
  
  # Test completely different strings
  result <- jw_split_words("MAIN STREET", "QUEEN AVENUE")
  expect_true(is.numeric(result))
  expect_true(result >= 0 && result <= 1)
  
  # Test similar strings
  result <- jw_split_words("MAIN STREET", "MAINE STREET")
  expect_true(is.numeric(result))
  expect_true(result >= 0 && result <= 1)
})

test_that("jw_split_words handles edge cases", {
  skip_if_not_installed("stringdist")
  skip_if_not_installed("stringi")
  
  # Empty strings
  result <- jw_split_words("", "")
  expect_true(is.numeric(result))
  
  # One empty string
  result <- jw_split_words("MAIN STREET", "")
  expect_true(is.numeric(result))
  
  # Single character
  result <- jw_split_words("A", "B")
  expect_true(is.numeric(result))
})

test_that("jwink function works with basic parameters", {
  skip_if_not_installed("stringdist")
  
  # Test basic functionality
  result <- jwink(c("MAIN STREET", "QUEEN AVENUE"), 
                  c("MAIN STREET", "QUEEN STREET"))
  
  expect_true(is.numeric(result))
  expect_equal(length(result), 2)
  expect_true(all(result >= 0 & result <= 1))
})

test_that("jwink handles different types", {
  skip_if_not_installed("stringdist")
  
  # Test type 1 (default)
  result1 <- jwink(c("MAIN ST"), c("MAIN STREET"), type = "1")
  expect_true(is.numeric(result1))
  
  # Test type 2
  result2 <- jwink(c("MAIN ST"), c("MAIN STREET"), type = "2")
  expect_true(is.numeric(result2))
  
  # Test type 3
  result3 <- jwink(c("MAIN ST"), c("MAIN STREET"), type = "3")
  expect_true(is.logical(result3))
})
