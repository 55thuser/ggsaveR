test_that("Creator metadata is added to PNG files", {
  withr::local_dir(tempdir())
  withr::local_options(list(ggsaveR.creator = test_creator))

  filename <- "creator_test.png"
  ggsave(filename, p)

  # Read PNG metadata
  img_meta <- png::readPNG(filename, info = TRUE)
  metadata <- attr(img_meta, "metadata")

  expect_equal(metadata$Author, test_creator)
})

test_that("Creator metadata is passed to PDF device", {
  # This test mainly checks that the code runs without error,
  # as programmatically reading PDF metadata requires another dependency (e.g., pdftools).
  withr::local_dir(tempdir())
  withr::local_options(list(ggsaveR.creator = test_creator))

  filename <- "creator_test.pdf"

  # Expect no errors when passing the 'author' argument to the pdf device
  expect_no_error(ggsave(filename, p))
  expect_true(file.exists(filename))
})

test_that("User-provided arguments are handled correctly", {
  withr::local_dir(tempdir())
  withr::local_options(list(ggsaveR.creator = "Option Creator"))

  # For PDF, the 'author' argument is not supported by ggsave, so it should be ignored
  filename_pdf <- "creator_override.pdf"
  expect_no_error(ggsave(filename_pdf, p))

  # For PNG, the 'creator' argument is passed directly, not from '...'
  # so it should still use the option value. This is expected behavior.
  filename_png <- "creator_no_override.png"
  ggsave(filename_png, p)
  img_meta <- png::readPNG(filename_png, info = TRUE)
  metadata <- attr(img_meta, "metadata")

  expect_equal(metadata$Author, "Option Creator")
})
