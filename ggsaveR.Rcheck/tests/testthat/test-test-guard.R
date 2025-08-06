library(testthat)
library(ggplot2)
library(ggsaveR) # Load ggsaveR after ggplot2 to ensure ggsave is masked correctly

# A simple plot object for use in all tests
p <- ggplot2::ggplot(data.frame(x = 1, y = 1), ggplot2::aes(x, y)) +
  ggplot2::geom_point()

test_that("guard = TRUE bypasses all ggsaveR enhancements", {
  withr::local_dir(tempdir())

  # --- Test Case 1: `guard` should ignore the `ggsaveR.formats` option ---

  # Set up ggsaveR options that should be ignored
  withr::local_options(list(ggsaveR.formats = list(
    list(device = "png"),
    list(device = "pdf")
  )))

  # Call ggsave with `guard = TRUE`. It should only save a single SVG file.
  filename_svg <- "guard_test.svg"
  ggsave(filename_svg, p, guard = TRUE)

  # Assert that the specified file was created
  expect_true(file.exists(filename_svg))

  # Assert that the files from the `ggsaveR.formats` option were NOT created
  expect_false(file.exists("guard_test.png"))
  expect_false(file.exists("guard_test.pdf"))

  # --- Test Case 2: `guard` should ignore `ggsaveR.overwrite_action = "unique"` ---

  withr::local_options(list(
    # Set overwrite action to "unique"
    ggsaveR.overwrite_action = "unique",
    # Clear the formats option to isolate this test
    ggsaveR.formats = NULL
  ))

  filename_unique <- "guard_unique_test.png"

  # Save the same file twice with `guard = TRUE`
  ggsave(filename_unique, p, guard = TRUE)
  ggsave(filename_unique, p, guard = TRUE)

  # Assert that the original file exists
  expect_true(file.exists(filename_unique))

  # Assert that NO unique-ified file was created, because `guard` should
  # trigger ggplot2's default behavior, which is to overwrite.
  expect_false(file.exists("guard_unique_test-1.png"))


  # --- Test Case 3: `guard` should ignore `ggsaveR.overwrite_action = "stop"` ---

  withr::local_options(list(
    # Set overwrite action to "stop"
    ggsaveR.overwrite_action = "stop",
    # Clear the formats option to isolate this test
    ggsaveR.formats = NULL
  ))

  filename_stop <- "guard_stop_test.png"

  # Save the file once
  ggsave(filename_stop, p, guard = TRUE)

  # The second call with `guard = TRUE` should NOT throw an error; it should
  # bypass the "stop" action and simply overwrite the file.
  expect_no_error(
    ggsave(filename_stop, p, guard = TRUE)
  )
})
