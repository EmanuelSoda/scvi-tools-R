#' @importFrom memoise memoise
.onAttach <- function(libname, pkgname) {
    scvi <<- memoise::memoise(load_scvi)
    sc <<- memoise::memoise(load_sc)
    #sceasy <<- memoise::memoise(load_sceasy)
}



load_sceasy <- function() {
    tryCatch({
        find.package("sceasy")
    },
    error = function(e){
        message(paste0("It seams that sceasy is not installed. \n",
                       "You can install it using: \n",
                       'devtools::install_github("cellgeni/sceasy")'))
    }
    )
}

#' Load scvi-tools Python Package
#' @importFrom reticulate py_discover_config import
load_scvi <- function() {
    py_config <- try(
        reticulate::py_discover_config(required_module = "scvi")
    )

    delay_load = list(on_load = check_pyscvi_version #,
                      #on_error = failed_pyphate_import
                      )
    # load
    scvi <- try(
        reticulate::import("scvi", delay_load = delay_load)
    ) #delay_load = delay_load

    return(scvi)
}


#' Check that the current scvi-tools version in Python is up to date.
#'
#' @importFrom utils packageVersion
#' @importFrom reticulate py_config
#' @export
check_pyscvi_version <- function() {
    pyversion <- strsplit(scvi()$`__version__`, '\\.')[[1]]
    rversion <- strsplit(as.character(packageVersion("scvi")), '\\.')[[1]]
    major_version <- as.integer(rversion[1])
    minor_version <- as.integer(rversion[2])
    if (as.integer(pyversion[1]) < major_version) {
        warning(paste0("Python scvi version ", scvi()$`__version__`,
                       " is out of date (recommended: ",
                       major_version, ".", minor_version,
                       "). Please update with pip ",
                       "(e.g. ", reticulate::py_config()$python,
                       " -m pip install --upgrade scvi-tools)"))
        return(FALSE)
    } else if (as.integer(pyversion[2]) < minor_version) {
        warning(paste0("Python scvi version ", pyphate()$`__version__`,
                       " is out of date (recommended: ",
                       major_version, ".", minor_version,
                       "). Consider updating with pip ",
                       "(e.g. ", reticulate::py_config()$python,
                       " -m pip install --upgrade scvi-tools)."))
        return(FALSE)
    }
    return(TRUE)
}

#' Install scvi-tools Python Package
#'
#' Install scvi-tools Python package into a virtualenv or conda env.
#'
#' On Linux and OS X the "virtualenv" method will be used by default
#' ("conda" will be used if virtualenv isn't available). On Windows,
#' the "conda" method is always used.
#'
#' @param envname Name of environment to install packages into
#' @param method Installation method. By default, "auto" automatically finds
#' a method that will work in the local environment. Change the default to
#' force a specific installation method. Note that the "virtualenv" method
#' is not available on Windows.
#' @param conda Path to conda executable (or "auto" to find conda using the PATH
#'  and other conventional install locations).
#' @param pip Install from pip, if possible.
#' @param ... Additional arguments passed to conda_install() or
#' virtualenv_install().
#' @importFrom reticulate py_install
#'
#' @export
install_scvi <- function(envname = "r-reticulate", method = "auto",
                          conda = "auto", pip=TRUE, ...) {
    tryCatch({
        message(paste0("Attempting to install scvi-tools ",
                       "Python package with reticulate"))
        reticulate::py_install("scvi-tools",
                               envname = envname, method = method,
                               conda = conda, pip=pip, ...
        )
        message("Install complete. Please restart R and try again.")
    },
    error = function(e) {
        stop(paste0(
            "Cannot locate scvi-tools Python package, ",
            "please install through pip ",
            "(e.g. ", reticulate::py_config()$python,
            " -m pip install --user scvi-tools) and then restart R."
        ))
    }
    )
}



#' Load scanpy Python Package
load_sc <- function() {
    py_config <- try(
        reticulate::py_discover_config(required_module = "sc")
    )

    delay_load = list(on_load = check_pysc_version #,
                      #on_error = failed_pyphate_import
    )
    # load
    sc <- try(
        reticulate::import("scanpy", delay_load = delay_load)
    ) #delay_load = delay_load

    return(sc)
}


#' Check that the current scvi-tools version in Python is up to date.
#'
#' @importFrom utils packageVersion
#' @export
check_pysc_version <- function() {
    pyversion <- strsplit(sc()$`__version__`, '\\.')[[1]]
    rversion <- strsplit(as.character(packageVersion("scanpy")), '\\.')[[1]]
    major_version <- as.integer(rversion[1])
    minor_version <- as.integer(rversion[2])
    if (as.integer(pyversion[1]) < major_version) {
        warning(paste0("Python scanpy version ", sc()$`__version__`,
                       " is out of date (recommended: ",
                       major_version, ".", minor_version,
                       "). Please update with pip ",
                       "(e.g. ", reticulate::py_config()$python,
                       " -m pip install --upgrade scanpy)"))
        return(FALSE)
    } else if (as.integer(pyversion[2]) < minor_version) {
        warning(paste0("Python scanpy version ", pyphate()$`__version__`,
                       " is out of date (recommended: ",
                       major_version, ".", minor_version,
                       "). Consider updating with pip ",
                       "(e.g. ", reticulate::py_config()$python,
                       " -m pip install --upgrade scanpy)."))
        return(FALSE)
    }
    return(TRUE)
}

#' Install scanpy Python Package
#'
#' Install sscanpy Python package into a virtualenv or conda env.
#'
#' On Linux and OS X the "virtualenv" method will be used by default
#' ("conda" will be used if virtualenv isn't available). On Windows,
#' the "conda" method is always used.
#'
#' @param envname Name of environment to install packages into
#' @param method Installation method. By default, "auto" automatically finds
#' a method that will work in the local environment. Change the default to
#' force a specific installation method. Note that the "virtualenv" method
#' is not available on Windows.
#' @param conda Path to conda executable (or "auto" to find conda using the PATH
#'  and other conventional install locations).
#' @param pip Install from pip, if possible.
#' @param ... Additional arguments passed to conda_install() or
#' virtualenv_install().
#'
#' @export
install_scvi <- function(envname = "r-reticulate", method = "auto",
                         conda = "auto", pip=TRUE, ...) {
    tryCatch({
        message(paste0("Attempting to install scanpy ",
                       "Python package with reticulate"))
        reticulate::py_install("scanpy",
                               envname = envname, method = method,
                               conda = conda, pip=pip, ...
        )
        message("Install complete. Please restart R and try again.")
    },
    error = function(e) {
        stop(paste0(
            "Cannot locate scanpy Python package, ",
            "please install through pip ",
            "(e.g. ", reticulate::py_config()$python,
            " -m pip install --user scanpy) and then restart R."
        ))
    }
    )
}

