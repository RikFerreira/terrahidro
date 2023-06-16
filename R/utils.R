#' Update workflow
#'
#' @param layer Layer in workflow
#' @param path Path of the layer in workflow
#'
#' @return TRUE if it was possible to update the workflow, FALSE otherwise.
update_workflow <- function(layer, path) {
    if(pkg.env$workflow[which(pkg.env$workflow$layer == layer), "layer"] == layer) {
        pkg.env$workflow[which(pkg.env$workflow$layer == layer), "path"] <- path

        return(TRUE)
    } else {
        return(FALSE)
    }
}

run_th_command <- function(command, input_args, output) {
    cli::cli_inform("Running {.var {command}} command")

    command_return <- suppressWarnings(
        system(
            paste("th", command, input_args, output),
            intern = TRUE,
            ignore.stdout = FALSE, ignore.stderr = FALSE,
        ),
    )

    invisible(command_return)
}

check_input <- function(input, name, keep_input) {
    if(grepl("\\.tif$|\\.tiff$", input)) { # If input is a TIFF file
        if(!file.exists(input)) cli::cli_abort("File {input} did not found!")

        if(keep_input) {
            cli::cli_inform("Storing original input for later use...")
            temp_input <- terra::rast(input)

            input_file <- paste0(pkg.env$temp_working_dir, "input_", name, ".tif")
            terra::writeRaster(temp_input, input_file, overwrite = TRUE)
        } else {
            input_file <- input
        }
    } else if(methods::is(input, "SpatRaster")) { # If input is a terra::SpatRaster file
        if(!keep_input) cli::cli_warn("keep_input = FALSE option ignored!")

        cli::cli_inform("Storing original input for later use...")

        input_file <- paste0(pkg.env$temp_working_dir, "input_", name, ".tif")
        terra::writeRaster(input, input_file, overwrite = TRUE)
    } else if(is.null(input)) { # If input should be recovered from workflow
        if(!keep_input) cli::cli_warn("keep_input = FALSE option ignored!")

        cli::cli_inform("Recovering input from workflow...")

        input_file <- pkg.env$workflow[which(pkg.env$workflow$layer == "keep_input"), "path"]

        if(is.na(input_file)) cli::cli_abort("File {.var {input}} isn't in workflow!")
    } else { # Invalid input
        cli::cli_abort("Either the input is not a TIFF file or it isn't a {.cls {class(terra::rast())}} object")
    }

    return(input_file)
}
