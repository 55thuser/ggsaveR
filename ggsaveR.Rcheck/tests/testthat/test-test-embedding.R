test_that("Data embedding works for PNG files", {
  withr::local_dir(tempdir())
  withr::local_options(list(
    ggsaveR.embed_data = TRUE,
    # Test with all metadata components
    ggsaveR.embed_metadata = c("plot", "data", "session_info", "call")
  ))

  filename <- "embed_test.png"

  # Capture the message
  expect_message(
    ggsave(filename, p),
    "Embedded reproducibility data into embed_test.png"
  )

  # Read the data back
  reloaded_data <- read_ggsaveR_data(filename)

  expect_true(is.list(reloaded_data))
  expect_equal(names(reloaded_data), c("ggsaveR_version", "plot_object", "plot_data", "session_info", "plot_call"))

  # Verify contents
  expect_s3_class(reloaded_data$plot_object, "ggplot")
  expect_equal(reloaded_data$plot_data, mtcars)
  expect_true("R.version" %in% names(reloaded_data$session_info))
  expect_equal(reloaded_data$plot_call, "p")
})

test_that("Call embedding captures piped/nested calls", {
  withr::local_dir(tempdir())
  withr::local_options(list(ggsaveR.embed_data = TRUE))

  filename <- "embed_call_test.png"

  # Use a nested call
  ggsave(filename, plot = ggplot2::ggplot(iris, ggplot2::aes(Sepal.Length, Petal.Length)) + ggplot2::geom_point())

  reloaded_data <- read_ggsaveR_data(filename)
  expect_match(reloaded_data$plot_call, "ggplot2::ggplot(iris", fixed = TRUE)
})

test_that("embed_metadata option is respected", {
  withr::local_dir(tempdir())
  withr::local_options(list(
    ggsaveR.embed_data = TRUE,
    # Only embed the plot and the call
    ggsaveR.embed_metadata = c("plot", "call")
  ))

  filename <- "embed_subset_test.png"
  ggsave(filename, p)

  reloaded_data <- read_ggsaveR_data(filename)

  # Check that only the requested components are present
  expect_equal(names(reloaded_data), c("ggsaveR_version", "plot_object", "plot_call"))
  expect_null(reloaded_data$plot_data)
  expect_null(reloaded_data$session_info)
})

test_that("Embedding is skipped for non-PNG files", {
  withr::local_dir(tempdir())
  withr::local_options(list(ggsaveR.embed_data = TRUE))

  filename_pdf <- "no_embed.pdf"

  # Should not print an embedding message (but may show ggplot2::ggsave messages)
  # We'll capture the message and check it doesn't contain embedding text
  captured_message <- capture_messages(ggsave(filename_pdf, p))
  expect_false(any(grepl("Embedded reproducibility data", captured_message)))

  # Ensure the file was created
  expect_true(file.exists(filename_pdf))

  # read_ggsaveR_data should fail for non-PNGs
  expect_error(
    read_ggsaveR_data(filename_pdf),
    "Data embedding is only supported for PNG files."
  )
})
