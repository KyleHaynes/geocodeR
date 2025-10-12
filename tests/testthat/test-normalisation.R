test_that("normalise_fun handles basic transformations", {
  # Test mountain normalization
  expect_equal(normalise_fun("MOUNT PLEASANT", not_gnaf = TRUE), "MT PLEASANT")
  expect_equal(normalise_fun("MOUNTAINS VIEW", not_gnaf = TRUE), "MT VIEW")
  
  # Test saint normalization
  expect_equal(normalise_fun("SAINT JOHNS", not_gnaf = TRUE), "ST JOHNS")
  
  # Test doctor normalization
  expect_equal(normalise_fun("DOCTOR SMITH", not_gnaf = TRUE), "DR SMITH")
  
  # Test apostrophe removal
  expect_equal(normalise_fun("O'BRIEN STREET", not_gnaf = TRUE), "OBRIEN STREET")
  
  # Test AND replacement
  expect_equal(normalise_fun("SMITH AND JONES", not_gnaf = TRUE), "SMITH & JONES")
  
  # Test city removal
  expect_equal(normalise_fun("BRISBANE CITY 4000", not_gnaf = TRUE), "BRISBANE 4000")
})

test_that("normalise_fun handles GNAF-specific transformations", {
  # Test postcode replacement
  expect_equal(normalise_fun("TAMBORINE MOUNTAIN 4271", not_gnaf = TRUE), "TAMBORINE MOUNTAIN 4272")
  
  # Test slash replacement
  expect_equal(normalise_fun("UNIT 5/123 MAIN ST", not_gnaf = TRUE), "UNIT 5 123 MAIN ST")
  
  # Test comma replacement
  expect_equal(normalise_fun("123 MAIN ST, BRISBANE", not_gnaf = TRUE), "123 MAIN ST BRISBANE")
})

test_that("st function standardizes whitespace", {
  expect_equal(st("  MULTIPLE   SPACES  "), "MULTIPLE SPACES")
  expect_equal(st("TRAILING SPACE "), "TRAILING SPACE")
  expect_equal(st(" LEADING SPACE"), "LEADING SPACE")
  expect_equal(st(""), "")
})

test_that("normalise_fun handles edge cases", {
  expect_equal(normalise_fun(""), "")
  expect_equal(normalise_fun(NA_character_, not_gnaf = TRUE), "")
  expect_equal(normalise_fun(NULL, not_gnaf = TRUE), character(0))
})
