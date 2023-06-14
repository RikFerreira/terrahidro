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
    message(sprintf("Running %s command", command))

    command_return <- suppressWarnings(
        system(
            paste("th", command, input_args, output),
            intern = TRUE,
            ignore.stdout = FALSE, ignore.stderr = FALSE,
        ),
    )

    invisible(command_return)
}
