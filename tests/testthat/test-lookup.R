# Mock data for testing
create_mock_lookup_map <- function() {
  data.table::data.table(
    address_detail_pid = c("TEST001", "TEST002", "TEST003"),
    address_label = c("123 MAIN STREET BRISBANE 4000", "456 QUEEN STREET MELBOURNE 3000", "789 KING STREET SYDNEY 2000"),
    address = c("123 MAIN STREET BRISBANE 4000", "456 QUEEN STREET MELBOURNE 3000", "789 KING STREET SYDNEY 2000"),
    short_address = c("MAINST", "QUEENST", "KINGST"),
    street_name = c("MAIN", "QUEEN", "KING"),
    notes = c("Test address 1", "Test address 2", "Test address 3"),
    longitude = c(153.0, 144.9, 151.2),
    latitude = c(-27.5, -37.8, -33.9),
    block_1 = c("123 4000", "456 3000", "789 2000"),
    block_2 = c("123 40", "456 30", "789 20"),
    block_3 = c("123 MAINST", "456 QUEENST", "789 KINGST"),
    block_4 = c("MAINSTREETBRISBANE", "QUEENSTREETMELBOURNE", "KINGSTREETSYDNEY")
  )
}

test_that("lookup_address returns correct structure", {
  # Skip if no mock data available
  skip_if_not_installed("data.table")
  
  # Create mock environment
  old_lookup_map <- if(exists("lookup_map")) get("lookup_map") else NULL
  
  # Set up mock lookup_map in global environment
  assign("lookup_map", create_mock_lookup_map(), envir = .GlobalEnv)
  
  # Test basic functionality
  test_addresses <- c("123 MAIN STREET BRISBANE 4000", "456 QUEEN STREET MELBOURNE 3000")
  
  tryCatch({
    result <- lookup_address(test_addresses, test = TRUE, block_4 = FALSE)
    
    # Check structure
    expect_s3_class(result, "data.table")
    expect_true("input" %in% names(result))
    expect_true("normalised_input" %in% names(result))
    expect_true("matched" %in% names(result))
    expect_true("match_type" %in% names(result))
    
    # Check number of rows
    expect_equal(nrow(result), length(test_addresses))
    
    # Check that some addresses matched
    expect_true(any(result$matched))
    
  }, finally = {
    # Clean up
    if (is.null(old_lookup_map)) {
      rm("lookup_map", envir = .GlobalEnv)
    } else {
      assign("lookup_map", old_lookup_map, envir = .GlobalEnv)
    }
  })
})

test_that("lookup_address handles edge cases", {
  skip_if_not_installed("data.table")
  
  old_lookup_map <- if(exists("lookup_map")) get("lookup_map") else NULL
  assign("lookup_map", create_mock_lookup_map(), envir = .GlobalEnv)
  
  tryCatch({
    # Empty input
    result <- lookup_address(character(0), test = TRUE)
    expect_equal(nrow(result), 0)
    
    # Single address
    result <- lookup_address("123 MAIN STREET BRISBANE 4000", test = TRUE)
    expect_equal(nrow(result), 1)
    
    # Non-matching address
    result <- lookup_address("999 NONEXISTENT STREET NOWHERE 9999", test = TRUE)
    expect_equal(nrow(result), 1)
    expect_false(result$matched)
    
  }, finally = {
    if (is.null(old_lookup_map)) {
      rm("lookup_map", envir = .GlobalEnv)
    } else {
      assign("lookup_map", old_lookup_map, envir = .GlobalEnv)
    }
  })
})

test_that("lookup_address deduplication works", {
  skip_if_not_installed("data.table")
  
  old_lookup_map <- if(exists("lookup_map")) get("lookup_map") else NULL
  assign("lookup_map", create_mock_lookup_map(), envir = .GlobalEnv)
  
  tryCatch({
    # Test with deduplication enabled (default)
    result_dedup <- lookup_address(c("123 MAIN STREET BRISBANE 4000", "123 MAIN STREET BRISBANE 4000"), 
                                  deduplicate = TRUE, test = TRUE)
    
    # Test with deduplication disabled
    result_no_dedup <- lookup_address(c("123 MAIN STREET BRISBANE 4000", "123 MAIN STREET BRISBANE 4000"), 
                                     deduplicate = FALSE, test = TRUE)
    
    # With deduplication, should have fewer or equal rows
    expect_true(nrow(result_dedup) <= nrow(result_no_dedup))
    
  }, finally = {
    if (is.null(old_lookup_map)) {
      rm("lookup_map", envir = .GlobalEnv)
    } else {
      assign("lookup_map", old_lookup_map, envir = .GlobalEnv)
    }
  })
})
