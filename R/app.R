#' @name mct_shinyapp
#' @author Eduardo Jr
#' @title Interactive Interface to Analysis MCMC objects
#' @description Interactive interface implemented in shiny to analysis
#'     MCMC objects (class \code{\link[coda]{mcmc}} or
#'     \code{\link[coda]{mcmc.list}}).
#' @param object An object of class \code{\link[coda]{mcmc}} or
#'     \code{\link[coda]{mcmc.list}}.
#' @param ... Others arguments passed to \code{\link[shiny]{runApp}}.
#' @return NULL. This function opens the web browser with the
#'     interactive shiny interface.
#' @importFrom shiny runApp
#' @export
#'
mct_shinyapp <- function(object, ...) {
    cls <- class(object)
    if (!cls %in% c("mcmc", "mcmc.list")) {
        stop("The object must be of class 'mcmc' or 'mcmc.list'")
    }
    dir <- system.file("mcmc_app", package = "mcmctools")
    .GlobalEnv$object <- object
    shiny::runApp(dir, ...)
    # return(NULL)
}
