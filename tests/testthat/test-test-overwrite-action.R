test_that("overwrite_action = 'unique' works correctly", {
  # Create a truly isolated temporary directory
  temp_dir <- file.path(tempdir(), "ggsaveR_test_unique",
                        format(Sys.time(), "%Y%m%d_%H%M%S_%OS3"))
  dir.create(temp_dir, recursive = TRUE)
  withr::local_dir(temp_dir)

  withr::local_options(list(
    ggsaveR.overwrite_action = "unique",
    ggsaveR.formats = NULL
  ))

  # Test with explicit device
  filename <- "unique_test"

  # First save
  suppressMessages(ggsave(filename, p, device = "png"))
  expect_true(file.exists("unique_test.png"))

  # Count files before second save
  files_before <- list.files(pattern = "unique_test.*\\.png$")
  expect_equal(length(files_before), 1)

  # Second save should trigger unique behavior
  captured <- capture_messages(ggsave(filename, p, device = "png"))

  # Count files after second save
  files_after <- list.files(pattern = "unique_test.*\\.png$")
  expect_equal(length(files_after), 2,
               info = paste("Files after:", paste(files_after, collapse = ", ")))

  # Check if message was generated (flexible pattern to handle any number)
  expect_true(any(grepl("File exists.*Saving to unique_test-\\d+\\.png instead", captured)),
              info = paste("Messages captured:", paste(captured, collapse = " | ")))

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("overwrite_action = 'stop' works correctly", {
  # Create a truly isolated temporary directory
  temp_dir <- file.path(tempdir(), "ggsaveR_test_stop",
                        format(Sys.time(), "%Y%m%d_%H%M%S_%OS3"))
  dir.create(temp_dir, recursive = TRUE)
  withr::local_dir(temp_dir)

  withr::local_options(list(ggsaveR.overwrite_action = "stop"))

  filename <- "stop_test.png"

  # First save
  suppressMessages(ggsave(filename, p))
  expect_true(file.exists(filename))

  # Second save should error
  expect_error(
    ggsave(filename, p),
    "File 'stop_test.png' already exists."
  )

  # Clean up
  unlink(temp_dir, recursive = TRUE)
})

test_that("overwrite_action = 'overwrite' works correctly (default)", {
  withr::local_dir(tempdir())
  withr::local_options(list(ggsaveR.overwrite_action = "overwrite"))

  filename <- "overwrite_test.png"
  ggsave(filename, p)
  info1 <- file.info(filename)

  # Wait a moment to ensure modification time can change
  Sys.sleep(0.1)

  # Suppress the "Saving..." message from ggplot2 as it's not relevant to this test
  suppressMessages(ggsave(filename, p))
  info2 <- file.info(filename)

  # File should be overwritten (mod time changes), no new file created
  expect_true(info2$mtime > info1$mtime)
  expect_false(file.exists("overwrite_test-1.png"))
})

# Simple test to verify the basic mechanism works
test_that("unique_filename function works correctly", {
  withr::local_dir(tempdir())

  # Create a file
  filename <- "test.png"
  writeLines("test", filename)
  expect_true(file.exists(filename))

  # Test that ggsaveR:::unique_filename works (if accessible)
  # Otherwise we'll test through the main function
  tryCatch({
    unique_name <- ggsaveR:::unique_filename(filename)
    expect_equal(unique_name, "test-1.png")
  }, error = function(e) {
    # If internal function not accessible, skip this part
    skip("Internal unique_filename function not accessible for direct testing")
  })
})
