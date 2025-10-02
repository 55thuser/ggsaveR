test_that("ggsave can save to multiple formats at once (legacy vector format)", {
  withr::local_dir(tempdir())

  base_filename <- "multi_format_plot"

  # Save to two formats
  saved_files <- ggsave(base_filename, p, device = c("png", "pdf"))

  # Check that both files were created
  expected_png <- paste0(base_filename, ".png")
  expected_pdf <- paste0(base_filename, ".pdf")

  expect_true(file.exists(expected_png))
  expect_true(file.exists(expected_pdf))

  # Check that the function returns the paths of the created files
  expect_equal(sort(saved_files), sort(c(expected_png, expected_pdf)))
})

test_that("ggsave can save with multiple dimensions using list format", {
  withr::local_dir(tempdir())
  
  base_filename <- "multi_dimension_plot"
  
  # Define multiple format specifications with different dimensions
  format_specs <- list(
    list(filetype = "png", width = 180, height = 120, units = "mm"),
    list(filetype = "png", width = 90, height = 120, units = "mm"),
    list(filetype = "pdf", width = 180, height = 120, units = "mm")
  )
  
  # Save using the new list format
  saved_files <- ggsave(base_filename, p, device = format_specs)
  
  # Check that all files were created with appropriate dimension suffixes
  expected_files <- c(
    "multi_dimension_plot_180x120mm.png",
    "multi_dimension_plot_90x120mm.png", 
    "multi_dimension_plot_180x120mm.pdf"
  )
  
  for (file in expected_files) {
    expect_true(file.exists(file), info = paste("File should exist:", file))
  }
  
  # Check return values
  expect_equal(sort(saved_files), sort(expected_files))
})

test_that("ggsave supports new file formats (jpg, eps, tiff)", {
  withr::local_dir(tempdir())
  
  base_filename <- "new_formats_test"
  
  # Test new file formats
  format_specs <- list(
    list(filetype = "jpg", width = 100, height = 80, units = "mm"),
    list(filetype = "eps", width = 100, height = 80, units = "mm"),
    list(filetype = "tiff", width = 100, height = 80, units = "mm")
  )
  
  saved_files <- ggsave(base_filename, p, device = format_specs)
  
  expected_files <- c(
    "new_formats_test_100x80mm.jpg",
    "new_formats_test_100x80mm.eps",
    "new_formats_test_100x80mm.tiff"
  )
  
  for (file in expected_files) {
    expect_true(file.exists(file), info = paste("File should exist:", file))
  }
  
  expect_equal(sort(saved_files), sort(expected_files))
})

test_that("ggsave handles mixed legacy and new format specifications", {
  withr::local_dir(tempdir())
  
  base_filename <- "mixed_test"
  
  # Test legacy vector format still works
  saved_files_legacy <- ggsave(paste0(base_filename, "_legacy"), p, device = c("png", "pdf"))
  expect_length(saved_files_legacy, 2)
  
  # Test new list format  
  format_specs <- list(
    list(filetype = "png", width = 50, height = 50, units = "mm")
  )
  saved_files_new <- ggsave(paste0(base_filename, "_new"), p, device = format_specs)
  expect_length(saved_files_new, 1)
  expect_true(file.exists("mixed_test_new_50x50mm.png"))
})

test_that("ggsave generates appropriate filenames for different dimension combinations", {
  withr::local_dir(tempdir())
  
  base_filename <- "filename_test"
  
  # Test various dimension combinations
  format_specs <- list(
    list(filetype = "png", width = 100, height = 200),  # no units
    list(filetype = "pdf", width = 150, height = 300, units = "mm"), # with units
    list(filetype = "jpg")  # no dimensions
  )
  
  saved_files <- ggsave(base_filename, p, device = format_specs)
  
  expected_files <- c(
    "filename_test_100x200.png",     # dimensions without units
    "filename_test_150x300mm.pdf",   # dimensions with units  
    "filename_test.jpg"              # no dimensions
  )
  
  expect_equal(sort(saved_files), sort(expected_files))
  
  for (file in expected_files) {
    expect_true(file.exists(file), info = paste("File should exist:", file))
  }
})

test_that("ggsave format specifications override global width/height arguments", {
  withr::local_dir(tempdir())
  
  base_filename <- "override_test"
  
  format_specs <- list(
    list(filetype = "png", width = 50, height = 60, units = "mm")
  )
  
  # Pass global width/height that should be overridden
  saved_files <- ggsave(base_filename, p, device = format_specs, 
                       width = 999, height = 999, units = "in")
  
  # The file should use the format-specific dimensions (50x60mm), not global (999x999in)
  expect_true(file.exists("override_test_50x60mm.png"))
  
  # Read the image and verify dimensions (approximate check)
  # 50mm at 72 dpi ≈ 142 pixels, 60mm ≈ 170 pixels
  img <- png::readPNG("override_test_50x60mm.png")
  # We'll just check that it's not the massive size that would result from 999 inches
  expect_lt(dim(img)[2], 1000)  # width should be reasonable
  expect_lt(dim(img)[1], 1000)  # height should be reasonable
})

test_that("ggsave throws appropriate errors for invalid format specifications", {
  withr::local_dir(tempdir())
  
  # Test missing filetype
  expect_error(
    ggsave("test", p, device = list(list(width = 100, height = 100))),
    "missing 'filetype' field"
  )
  
  # Test unsupported format
  expect_error(
    ggsave("test", p, device = list(list(filetype = "invalid_format"))),
    "Unsupported file format"
  )
  
  # Test unsupported format in legacy vector format
  expect_error(
    ggsave("test", p, device = c("invalid_format")),
    "Unsupported file format"
  )
})

test_that("ggsave supports jpeg file extensions and aliases", {
  withr::local_dir(tempdir())
  
  base_filename <- "jpeg_test"
  
  # Test both jpg and jpeg extensions
  format_specs <- list(
    list(filetype = "jpg", width = 100, height = 100, units = "mm"),
    list(filetype = "jpeg", width = 100, height = 100, units = "mm")
  )
  
  saved_files <- ggsave(base_filename, p, device = format_specs)
  
  expected_files <- c(
    "jpeg_test_100x100mm.jpg",
    "jpeg_test_100x100mm.jpeg"
  )
  
  for (file in expected_files) {
    expect_true(file.exists(file), info = paste("File should exist:", file))
  }
})

test_that("ggsave supports tiff file extensions and aliases", {
  withr::local_dir(tempdir())
  
  base_filename <- "tiff_test"
  
  # Test both tif and tiff extensions
  format_specs <- list(
    list(filetype = "tif", width = 100, height = 100, units = "mm"),
    list(filetype = "tiff", width = 100, height = 100, units = "mm")
  )
  
  saved_files <- ggsave(base_filename, p, device = format_specs)
  
  expected_files <- c(
    "tiff_test_100x100mm.tif",
    "tiff_test_100x100mm.tiff"
  )
  
  for (file in expected_files) {
    expect_true(file.exists(file), info = paste("File should exist:", file))
  }
})
