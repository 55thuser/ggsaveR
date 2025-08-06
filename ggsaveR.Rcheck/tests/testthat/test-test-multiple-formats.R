library(testthat)
library(ggplot2)
library(ggsaveR) # Load ggsaveR after ggplot2 to ensure ggsave is masked correctly

# A simple plot object for use in all tests
p <- ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) +
  ggplot2::geom_point()

# --- Test the new, primary feature: saving via global options ---
test_that("ggsave saves multiple formats via ggsaveR.formats option", {
  # Use withr to set options and directory temporarily for this test
  withr::local_dir(tempdir())
  withr::local_options(list(ggsaveR.formats = list(
    list(device = "png"),
    list(device = "pdf"),
    list(device = "svg")
  )))

  # The filename extension here should be ignored by the function
  base_filename <- "plot_via_options"
  saved_files <- ggsave(paste0(base_filename, ".foo"), p) # Use .foo to confirm it's ignored

  # Check that the files defined in the option were created
  expected_files <- paste0(base_filename, c(".png", ".pdf", ".svg"))
  unexpected_file <- paste0(base_filename, ".foo")

  expect_true(all(file.exists(expected_files)))
  expect_false(file.exists(unexpected_file))

  # Check that the function returns the paths of the created files
  expect_setequal(saved_files, expected_files)
})

# --- Test the fallback feature: ensuring backward compatibility ---
test_that("ggsave still supports a vector for the device argument as a fallback", {
  withr::local_dir(tempdir())
  # Ensure the primary option is NULL so the function uses the fallback logic
  withr::local_options(list(ggsaveR.formats = NULL))

  base_filename <- "plot_via_device_arg"

  # This is the original test logic
  saved_files <- ggsave(base_filename, p, device = c("png", "pdf"))

  expected_files <- paste0(base_filename, c(".png", ".pdf"))
  expect_true(all(file.exists(expected_files)))

  # Check that the function returns the paths of the created files
  expect_setequal(saved_files, expected_files)
})

# --- Test the precedence rule: option should override argument ---
test_that("ggsaveR.formats option takes precedence over the device argument", {
  withr::local_dir(tempdir())
  # Set the option to save an SVG
  withr::local_options(list(ggsaveR.formats = list(
    list(device = "svg")
  )))

  base_filename <- "precedence_test"

  # Call ggsave with a conflicting `device` argument
  saved_files <- ggsave(base_filename, p, device = c("png", "pdf"))

  # Check that ONLY the file from the option was created
  expected_file <- paste0(base_filename, ".svg")
  unexpected_files <- paste0(base_filename, c(".png", ".pdf"))

  expect_true(file.exists(expected_file))
  expect_false(any(file.exists(unexpected_files)))

  # Check that the function returns only the path of the SVG file
  expect_equal(saved_files, expected_file)
})
