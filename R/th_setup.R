#' Initial setup to terrahidro package
#'
#' @export
pkg.env <- new.env()

th_setup <- function() {
    pkg.env$temp_working_dir <- paste0(tempdir(), "/terrahidro/")

    if(!dir.exists(pkg.env$temp_working_dir)) {
        dir.create(pkg.env$temp_working_dir)

        message("Temporary working directory created!")
        message(paste("Directory:", pkg.env$temp_working_dir))
    } else {
        message("Temporary working directory already exists!")
        message(paste("Directory:", pkg.env$temp_working_dir))
    }

    pkg.env$workflow <- data.frame(
        layer = c(
            "input_dem",
            "dem_carve", "dem_carvev"
        ),
        path = c("")
    )
}
