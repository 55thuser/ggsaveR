#' Generate a unique filename to avoid overwriting
#'
#' @param path The full path to the file.
#' @return A unique file path.
#' @noRd
unique_filename <- function(path) {
  if (!file.exists(path)) {
    return(path)
  }

  dir <- dirname(path)
  filename <- tools::file_path_sans_ext(basename(path))
  ext <- tools::file_ext(path)

  i <- 1
  new_path <- file.path(dir, paste0(filename, "-", i, ".", ext))

  while (file.exists(new_path)) {
    i <- i + 1
    new_path <- file.path(dir, paste0(filename, "-", i, ".", ext))
  }

  message("File exists. Saving to ", basename(new_path), " instead.")
  return(new_path)
}


#' Save a ggplot to PNG with embedded data
#'
#' This function handles the special case of saving a PNG with embedded
#' reproducibility data.
#'
#' @inheritParams ggplot2::ggsave
#' @param filename The final filename for the PNG.
#' @param plot_call_str A string representing the call used to generate the plot.
#' @param creator A string for the author/creator metadata field.
#' @return Invisibly returns the filename.
#' @noRd
save_png_with_data <- function(filename, plot, plot_call_str = NULL, creator = NULL, ...) {

  # 1. Create the plot in memory (same as before)
  tmp <- tempfile(fileext = ".png")
  # Pass ... down to the internal ggsave call (for width, height, dpi, etc.)
  args <- list(filename = tmp, plot = plot, ...)
  do.call(ggplot2::ggsave, args)
  img_data <- png::readPNG(tmp, native = FALSE)
  unlink(tmp)

  # 2. Prepare the metadata for embedding
  metadata_to_embed <- list()

  # Get options for what to embed
  embed_opts <- getOption("ggsaveR.embed_metadata", c("plot", "data", "session_info", "call"))

  if ("plot" %in% embed_opts) {
    metadata_to_embed$plot_object <- plot
  }
  if ("data" %in% embed_opts && !is.null(plot$data)) {
    metadata_to_embed$plot_data <- plot$data
    message("Note: Raw data will be embedded in this png file. Make sure you understand the risk of this.")
  }
  if ("session_info" %in% embed_opts) {
    metadata_to_embed$session_info <- sessionInfo()
  }
  # Add the plot call to the metadata if requested
  if ("call" %in% embed_opts && !is.null(plot_call_str)) {
    metadata_to_embed$plot_call <- plot_call_str
  }

  # 3. Serialize and encode the metadata (same as before)
  if (length(metadata_to_embed) > 0) {
    serialized_data <- serialize(metadata_to_embed, NULL)
    encoded_data <- base64enc::base64encode(serialized_data)
    png_metadata <- list(ggsaveR_data = encoded_data)
    message("Embedded reproducibility data into ", basename(filename))
  }

  # --- Handle standard metadata fields ---
  if (!is.null(creator)) {
    png_metadata$Author <- creator
  }

  # 3. Write the final PNG with the embedded data
  if (length(png_metadata) > 0) {
    png::writePNG(img_data, target = filename, metadata = png_metadata)
  } else {
    # If for some reason this was called with nothing to do, just write the file
    png::writePNG(img_data, target = filename)
  }

  invisible(filename)
}


#' Parse device specification for multiple formats and dimensions
#'
#' This function handles both the legacy vector format (c("png", "pdf")) and 
#' the new list format with dimensions.
#'
#' @param device Either a character vector of devices or a list of format specifications
#' @param filename The base filename to use
#' @return A list of format specifications, each containing device, filename, and dimension info
#' @noRd
parse_device_specification <- function(device, filename) {
  
  # If device is not specified, infer from filename extension
  if (is.null(device)) {
    device <- tolower(tools::file_ext(filename))
    if (device == "") {
      stop("Cannot determine device from filename with no extension.", call. = FALSE)
    }
  }
  
  # Base filename without extension
  base_filename <- tools::file_path_sans_ext(filename)
  
  # Case 1: List format (new nested format)
  if (is.list(device) && !is.character(device)) {
    format_specs <- list()
    
    for (i in seq_along(device)) {
      spec <- device[[i]]
      
      # Validate required fields
      if (is.null(spec$filetype)) {
        stop("Format specification ", i, " is missing 'filetype' field.", call. = FALSE)
      }
      
      # Validate supported file formats
      supported_formats <- c("png", "pdf", "svg", "ps", "eps", "jpg", "jpeg", "tiff", "tif", "bmp")
      if (!tolower(spec$filetype) %in% supported_formats) {
        stop("Unsupported file format: ", spec$filetype, ". Supported formats: ", 
             paste(supported_formats, collapse = ", "), call. = FALSE)
      }
      
      # Generate filename with dimensions if specified
      dimension_suffix <- ""
      if (!is.null(spec$width) && !is.null(spec$height)) {
        dimension_suffix <- paste0("_", spec$width, "x", spec$height)
        if (!is.null(spec$units)) {
          dimension_suffix <- paste0(dimension_suffix, spec$units)
        }
      }
      
      current_filename <- paste0(base_filename, dimension_suffix, ".", tolower(spec$filetype))
      
      # Map jpeg to jpg for device name (ggplot2 uses "jpeg" device name)
      device_name <- if (tolower(spec$filetype) %in% c("jpg", "jpeg")) "jpeg" else tolower(spec$filetype)
      # Map tif to tiff for device name  
      device_name <- if (tolower(spec$filetype) == "tif") "tiff" else device_name
      
      format_specs[[i]] <- list(
        device = device_name,
        filename = current_filename,
        width = spec$width,
        height = spec$height,
        units = spec$units,
        dpi = spec$dpi
      )
    }
    
    return(format_specs)
  }
  
  # Case 2: Character vector format (legacy support)
  if (is.character(device)) {
    devices <- device
    format_specs <- list()
    
    for (i in seq_along(devices)) {
      dev <- devices[i]
      
      # Validate supported file formats for legacy format too
      supported_formats <- c("png", "pdf", "svg", "ps", "eps", "jpg", "jpeg", "tiff", "tif", "bmp")
      if (!tolower(dev) %in% supported_formats) {
        stop("Unsupported file format: ", dev, ". Supported formats: ", 
             paste(supported_formats, collapse = ", "), call. = FALSE)
      }
      
      current_filename <- paste0(base_filename, ".", dev)
      
      # Map file extensions to ggplot2 device names
      device_name <- if (tolower(dev) %in% c("jpg", "jpeg")) "jpeg" else tolower(dev)
      device_name <- if (tolower(dev) == "tif") "tiff" else device_name
      
      format_specs[[i]] <- list(
        device = device_name,
        filename = current_filename,
        width = NULL,
        height = NULL,
        units = NULL,
        dpi = NULL
      )
    }
    
    return(format_specs)
  }
  
  # Case 3: Single device (convert to vector format)
  if (length(device) == 1) {
    return(parse_device_specification(as.character(device), filename))
  }
  
  stop("Invalid device specification. Must be a character vector or a list of format specifications.", call. = FALSE)
}
