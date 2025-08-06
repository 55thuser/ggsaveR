# A standard plot object to use in all tests
p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) +
  ggplot2::geom_point()

# A standard creator name for tests
test_creator <- "Test Author, ggsaveR"
