# ggsave.R

#' @importFrom rlang call_args `%||%`
#' @importFrom ggplot2 ggsave last_plot
#' @importFrom tools file_path_sans_ext file_ext
#' @importFrom ragg agg_png
#' @importFrom png writePNG
#' @importFrom base64enc base64encode
#' @importFrom utils packageVersion sessionInfo
#' @importFrom grDevices dev.cur dev.off

# You will need to add ragg, png, and base64enc to your DESCRIPTION file:
# Imports:
#   ragg,
#   png,
#   base64enc

# unique_filename function is defined in utils.R

#' Saves a PNG with embedded R metadata using native R packages
#'
#' This function replaces the default ggsave behavior for PNGs. It renders
#' the plot using the {ragg} device, prepares metadata (standard and custom),
#' and writes the final PNG with embedded text chunks using the {png} package.
#' @noRd
save_png_with_data <- function(filename, plot, plot_call_str, creator, embed_data, ...) {

  # --- 1. Prepare metadata payload ---
  text_chunks <- list()

  # Add standard, human-readable metadata
  if (!is.null(creator)) {
    text_chunks$Author <- creator
  }
  text_chunks$Description <- paste("Plot generated from R code:", plot_call_str)
  text_chunks$Software <- paste("R", getRversion(), "using ggsaveR", packageVersion("ggsaveR"))
  text_chunks$CreationTime <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")

  # Add custom, machine-readable data blob for reproducibility
  if (isTRUE(embed_data)) {
    # Get options for what to embed
    embed_opts <- getOption("ggsaveR.embed_metadata", c("plot", "data", "session_info", "call"))

    repro_data <- list()

    if ("plot" %in% embed_opts) {
      repro_data$plot_object <- plot
    }
    if ("data" %in% embed_opts && !is.null(plot$data)) {
      repro_data$plot_data <- plot$data
    }
    if ("session_info" %in% embed_opts) {
      repro_data$session_info <- sessionInfo()
    }
    if ("call" %in% embed_opts) {
      repro_data$plot_call <- plot_call_str
    }

    # Add version info only if we have other data to embed
    if (length(repro_data) > 0) {
      # Create a new list with ggsaveR_version first to match test expectations
      final_repro_data <- list(ggsaveR_version = packageVersion("ggsaveR"))
      final_repro_data <- c(final_repro_data, repro_data)
      repro_data <- final_repro_data
      message("Embedded reproducibility data into ", basename(filename))
    }

    # Pipeline: Serialize R object -> Compress -> Base64 Encode for tEXt chunk
    serialized_data <- serialize(repro_data, NULL)
    compressed_data <- memCompress(serialized_data, type = "gzip")
    encoded_data <- base64enc::base64encode(compressed_data)

    # Use a custom keyword. "Raw" is sometimes used, but a custom one is safer.
    text_chunks$ggsaveR_data <- encoded_data
  }

  # --- 2. Render the plot to a raster array in memory ---
  # We pass ggsave args like width, height, dpi, etc., to ragg::agg_png.
  ggsave_args <- list(...)

  # Use a temporary file for ragg rendering since it doesn't support direct memory output
  tmp_ragg_file <- tempfile(fileext = ".png")

  render_args <- list(
    filename = tmp_ragg_file,
    width = ggsave_args$width %||% 7,
    height = ggsave_args$height %||% 7,
    units = ggsave_args$units %||% "in",
    background = ggsave_args$bg %||% "white",
    res = ggsave_args$dpi %||% 300 # ragg uses `res` for dpi
  )

  # Capture the plot rendered by ragg
  captured_plot <- tryCatch({
    ragg::agg_png(
      filename = render_args$filename,
      width = render_args$width,
      height = render_args$height,
      units = render_args$units,
      res = render_args$res,
      background = render_args$background
    )
    print(plot)
    dev.off()

    # Read the temporary file and clean up
    raster_data <- png::readPNG(tmp_ragg_file)
    unlink(tmp_ragg_file)
    raster_data
  }, error = function(e) {
    # Ensure device is closed on error
    if (dev.cur() != 1) dev.off()
    # Clean up temp file
    if (file.exists(tmp_ragg_file)) unlink(tmp_ragg_file)
    stop("Failed to render plot in memory using {ragg}: ", e$message, call. = FALSE)
  })

  # --- 3. Write the final PNG with the plot image and metadata ---
  tryCatch({
    png::writePNG(image = captured_plot, target = filename, metadata = text_chunks)
    message("Successfully saved '", filename, "' with metadata.")
  }, error = function(e) {
    stop("Failed to write final PNG file '", filename, "': ", e$message, call. = FALSE)
  })
}


# --- Your main ggsave function ---

#' Save a ggplot (or other grid object) with ggsaveR enhancements.
#'
#' `ggsave()` is a convenient function for saving a plot. It defaults to
#' saving the last plot that you displayed, and it uses the file extension to
#' figure out which graphics device to use. `ggsaveR` extends this behavior
#' with several new features controlled by R options, such as saving to
#' multiple formats at once, embedding reproducibility data in PNGs, and
#' advanced file overwrite controls.
#'
#' @section ggsaveR Enhancements:
#' \itemize{
#'   \item **Multiple Formats**: Set the `ggsaveR.formats` option to a list of
#'     output configurations to save to multiple files at once (e.g., PNG, PDF,
#'     and SVG).
#'   \item **Data Embedding**: Set `ggsaveR.embed_data = TRUE` to embed the
#'     plot object, data, session info, and the generating call into the PNG
#'     file. This data can be retrieved with `read_ggsaveR_data()`.
#'   \item **Overwrite Control**: Set the `ggsaveR.overwrite_action` option to
#'     `"overwrite"` (default), `"stop"` (to error if the file exists), or
#'     `"unique"` (to save to a new file like `plot-1.png`).
#'   \item **Creator Metadata**: Set the `ggsaveR.creator` option to add an
#'     author/creator field to the file's metadata.
#' }
#'
#' @param filename File name to create on disk.
#' @param plot Plot to save, defaults to the last plot displayed.
#' @param device Device to use. See \code{\link[ggplot2]{ggsave}} for details. When using the `ggsaveR.formats` option, this argument is ignored.
#' @param guard A logical flag. If `TRUE`, `ggsaveR`'s enhancements are
#'   bypassed, and the call is forwarded directly to `ggplot2::ggsave()`.
#' @param ... Other arguments passed on to the graphics device function, such as `width`, `height`, `units`, or `dpi`.
#' @return Invisibly returns a character vector of the file paths created.
#' @export
ggsave <- function(filename, plot = last_plot(), device = NULL, ..., guard = FALSE) {

  if (isTRUE(guard)) {
    # Get the original call as a list of arguments
    call <- match.call()
    # Change the function to be called to ggplot2's ggsave
    call[[1L]] <- quote(ggplot2::ggsave)
    # Remove the 'guard' argument so it is not passed to ggplot2::ggsave
    call$guard <- NULL
    # Evaluate the modified call in the parent environment, where the
    # arguments (e.g., plot objects, filenames) are defined.
    return(eval.parent(call))
  }

  plot_arg <- substitute(plot)
  plot_call_str <- paste(deparse(plot_arg), collapse = "\n")

  formats <- getOption("ggsaveR.formats", NULL)
  overwrite_action <- getOption("ggsaveR.overwrite_action", "overwrite")
  embed_data <- getOption("ggsaveR.embed_data", FALSE)
  creator <- getOption("ggsaveR.creator", NULL)

  user_args <- list(...)
  saved_files <- character()
  base_filename <- tools::file_path_sans_ext(filename)

  # Filter out arguments that shouldn't be passed to ggplot2::ggsave
  filter_ggplot2_args <- function(args) {
    # List of arguments that are specific to ggsaveR and shouldn't be passed to ggplot2::ggsave
    # Also filter out arguments that we explicitly pass to avoid conflicts
    ggsaveR_args <- c("embed_data", "creator", "guard", "device", "filename", "plot")
    args[!names(args) %in% ggsaveR_args]
  }

  # Function to handle a single save operation
  # This avoids code duplication between the two main branches
  process_single_save <- function(dev, current_filename, args) {

    # Handle data embedding for PNGs vs. other formats
    is_png <- tolower(dev) == "png"

    if (is_png && (embed_data || !is.null(creator))) {
      # Use our special native PNG handler
      do.call(
        save_png_with_data,
        c(list(filename = current_filename, plot = plot, plot_call_str = plot_call_str,
               creator = creator, embed_data = embed_data), args)
      )
    } else {
      # Fallback to standard ggsave for other formats (e.g., PDF, SVG)
      # or for PNGs when no metadata is being added.

      # Filter out ggsaveR-specific arguments
      filtered_args <- filter_ggplot2_args(args)

      # Note: Most devices don't support metadata like 'author'. For PDF, postscript, etc.
      # we can't inject creator metadata through ggplot2::ggsave device arguments.
      # This is a limitation of the underlying graphics devices.

      do.call(ggplot2::ggsave, c(list(filename = current_filename, plot = plot, device = dev), filtered_args))
    }
  }

  if (is.list(formats) && !is.data.frame(formats)) {
    # --- A. NEW BEHAVIOR: Use ggsaveR.formats option ---
    for (fmt_config in formats) {
      dev <- fmt_config$device
      if (is.null(dev)) {
        warning("A format in `ggsaveR.formats` is missing a 'device'. Skipping.", call. = FALSE)
        next
      }
      current_filename <- paste0(base_filename, ".", dev)
      if (file.exists(current_filename)) {
        if (overwrite_action == "stop") stop("File '", current_filename, "' already exists.", call. = FALSE)
        if (overwrite_action == "unique") current_filename <- unique_filename(current_filename)
      }

      final_args <- c(user_args, fmt_config)
      process_single_save(dev, current_filename, final_args)
      saved_files <- c(saved_files, current_filename)
    }
  } else {
    # --- B. FALLBACK BEHAVIOR: Use `device` argument ---
    devices <- device %||% tolower(tools::file_ext(filename))
    if (length(devices) == 0 || all(devices == "")) {
      stop("Cannot determine device. Please specify `device`.", call. = FALSE)
    }

    for (dev in devices) {
      current_filename <- paste0(base_filename, ".", dev)
      if (file.exists(current_filename)) {
        if (overwrite_action == "stop") stop("File '", current_filename, "' already exists.", call. = FALSE)
        if (overwrite_action == "unique") current_filename <- unique_filename(current_filename)
      }

      process_single_save(dev, current_filename, user_args)
      saved_files <- c(saved_files, current_filename)
    }
  }

  invisible(saved_files)
}
