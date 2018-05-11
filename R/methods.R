#-----------------------------------------------------------------------
#' @name mct_select
#' @author Eduardo Jr
#' @title Select Variables of a MCMC object
#' @description Select variables (columns) of a \code{\link[coda]{mcmc}}
#'     and \code{\link[coda]{mcmc.list}} objects.
#' @param object An object of class \code{\link[coda]{mcmc}} or
#'     \code{\link[coda]{mcmc.list}}.
#' @param variable The variables to select. Can be the column positions
#'     (atomic integer) or column names (atomic character).
#' @param ... Dots argument, to pass the variables separately.
#' @return The input object \code{\link[coda]{mcmc}} or
#'     \code{\link[coda]{mcmc.list}} with the selected columns.
#' @examples
#'
#' data("line", package = "coda")
#' str(line)
#' str(mct_select(line, "alpha"))
#' str(mct_select(line, "alpha", "beta"))
#' str(mct_select(line[[1]], "sigma"))
#'
#' @export
mct_select <- function(object, variable, ...) {
  UseMethod("mct_select")
}

#' @rdname mct_select
#' @export
mct_select.default <-  function(object, variable, ...) {
    dots <- unlist(list(...))
    if (class(dots) != class(variable) & !is.null(dots)) {
        stop("Variables must all be the same class (integer or character)")
    }
    variable <- c(variable, dots)
    if (is.numeric(variable)) {
        out <- object[, variable, drop = FALSE]
    }
    else if (is.character(variable)) {
        sel <- gsub("\\[.+\\]", replacement = "",
                    coda::varnames(object))
        ind <- unlist(lapply(variable,
                             function(x) which(sel %in% x)))
        out <- object[, ind, drop = FALSE]
    }
    else {
        stop("`variable` must be atomic integer or character")
    }
    return(out)
}

#' @rdname mct_select
#' @export
mct_select.mcmc <- mct_select.default

#' @rdname mct_select
#' @export
mct_select.mcmc.list <- mct_select.default

#-----------------------------------------------------------------------
#' @name mct_thin
#' @author Eduardo Jr
#' @title Thin the Chains
#' @description Thin the chain(s) of \code{\link[coda]{mcmc}} or
#'     \code{\link[coda]{mcmc.list}} object.
#' @param object An object of class \code{\link[coda]{mcmc}} or
#'     \code{\link[coda]{mcmc.list}}.
#' @param thin The thining
#' @return The input object \code{\link[coda]{mcmc}} or
#'     \code{\link[coda]{mcmc.list}} thinned.
#' @examples
#'
#' data("line", package = "coda")
#' str(line)
#' str(mct_thin(line, thin = 5))
#' str(mct_thin(line[[1L]], thin = 10))
#'
#' @export
mct_thin <- function(object, thin) {
  UseMethod("mct_thin")
}

#' @rdname mct_thin
#' @export
mct_thin.mcmc <- function(object, thin) {
    mcpar <- mcpar(object)
    if (mcpar[3L] == 1L) mcpar[3L] <- 0L
    index <- seq(from = thin,
                 to = coda::niter(object),
                 by = thin)
    out <- coda::mcmc(data = object[index, , drop = FALSE],
                      end = mcpar[2L],
                      thin = mcpar[3L] + thin)
    return(out)
}

#' @rdname mct_thin
#' @export
mct_thin.mcmc.list <- function(object, thin) {
    out <- lapply(object, mct_thin.mcmc, thin = thin)
    out <- coda::as.mcmc.list(out)
    return(out)
}

#-----------------------------------------------------------------------
#' @name mct_burnin
#' @author Eduardo Jr
#' @title Burn Initials Values of the Chains
#' @description Burn initials values of the chain(s) of
#'     \code{\link[coda]{mcmc}} or \code{\link[coda]{mcmc.list}}
#'     objects.
#' @param object An object of class \code{\link[coda]{mcmc}} or
#'     \code{\link[coda]{mcmc.list}}.
#' @param burnin The number of samples to burn.
#' @return The input object \code{\link[coda]{mcmc}} or
#'     \code{\link[coda]{mcmc.list}} with initials samples burned.
#' @examples
#'
#' data("line", package = "coda")
#' str(line)
#' str(mct_burnin(line, burnin = 5))
#' str(mct_burnin(line[[1L]], burnin = 100))
#'
#' @export
mct_burnin <- function(object, burnin) {
  UseMethod("mct_burnin")
}

#' @rdname mct_burnin
#' @export
mct_burnin.mcmc <- function(object, burnin) {
    mcpar <- coda::mcpar(object)
    out <- object[-(1:burnin), , drop = FALSE]
    out <- coda::mcmc(data = out,
                      end = mcpar[2L],
                      thin = mcpar[3L])
    return(out)
}

#' @rdname mct_burnin
#' @export
mct_burnin.mcmc.list <- function(object, burnin) {
    out <- lapply(object, mct_burnin.mcmc, burnin = burnin)
    out <- coda::as.mcmc.list(out)
    return(out)
}

#-----------------------------------------------------------------------
#' @name mct_merge
#' @author Eduardo Jr
#' @title Merge Multiples Chains
#' @description Merge the chains of a \code{\link[coda]{mcmc.list}}
#'     object.
#' @param object An object of class \code{\link[coda]{mcmc.list}}.
#' @return An object \code{\link[coda]{mcmc}}.
#' @examples
#'
#' data("line", package = "coda")
#' str(line)
#' str(mct_merge(line))
#'
#' @export
mct_merge <- function(object) {
  UseMethod("mct_merge")
}

#' @rdname mct_merge
#' @export
mct_merge.mcmc.list <- function(object) {
    coda::as.mcmc(do.call(rbind, object))
}
