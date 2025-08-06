library(testthat)
library(ggplot2)
library(ggsaveR) # Load ggsaveR after ggplot2 to ensure ggsave is masked correctly

# A simple plot object for use in all tests
p <- ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) +
  ggplot2::geom_point()

test_that("ggsave saves multiple formats with varying dimensions via options", {
  withr::local_dir(tempdir())

  # Define the desired output formats with dimensions
  figure_output_formats <- list(
    list(device = "png", width = 8, height = 6, units = "in", dpi = 72),
    list(device = "pdf", width = 18, height = 12, units = "cm"),
    list(device = "jpg", width = 800, height = 600, units = "px"),
    list(device = "tiff", width = 10, height = 10, units = "in", dpi = 300)
  )

  # Set the options for ggsaveR to use
  withr::local_options(list(ggsaveR.formats = figure_output_formats))

  base_filename <- "multi_dim_plot"

  # The device argument and filename extension are ignored
  saved_files <- ggsave(filename = base_filename, plot = p)

  # IMPORTANT: The filenames no longer contain the dimensions.
  # The dimensions are passed to the graphics device but do not alter the name.
  expected_files <- c(
    "multi_dim_plot.png",
    "multi_dim_plot.pdf",
    "multi_dim_plot.jpg",
    "multi_dim_plot.tiff"
  )

  # Check that all the expected files exist
  expect_true(all(file.exists(expected_files)))

  # Check that the function returns the correct set of file paths
  expect_setequal(saved_files, expected_files)

  # Optional: A more advanced test to verify dimensions were applied.
  # This requires the 'png' package.
  if (requireNamespace("png", quietly = TRUE)) {
    img <- png::readPNG(expected_files[1])
    # Dimensions in pixels = inches * dpi
    # Width = 8 * 72 = 576, Height = 6 * 72 = 432
    # Note: `dim(img)` returns [height, width, channels]
    expect_equal(dim(img)[2], 8 * 72)
    expect_equal(dim(img)[1], 6 * 72)
  }
})
