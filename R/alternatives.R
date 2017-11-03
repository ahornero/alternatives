#' library0
#'
#' \code{library0} is an alternative to \link{library}, which also installs the package if needed.
#'
#' This is an alternative function to \link{library}, which pretends to be an easier way
#' to attach and install a package in the same step.
#'
#' @param package the name of the package, only given as a name.
#'
#' @examples
#' library0(rgdal)
#' library0('rgdal')
#' library0(username/packagename)
#'
library0 <- function(package) {
  # Check if the package is installed
  is.installed <- function(package) {
    is.element(package, installed.packages()[,1])
  }

  package.expr <- substitute(package)

  switch(class(package.expr),
         name = {
           # package given as name e.g. library0(rgdal)
           package.name <- deparse(package.expr)
         },
         call = {
           # package given as call e.g. library0(username/packagename)
           package.name <- deparse(package.expr)
         },
         {
           # package given as name e.g. library0('rgdal')
           package.name <- package.expr
         }
  )

  if (grepl('/', package.name)) {
    # GitHub Package
    # Todo ...
  } else {

    if (!is.installed(package.name)) {
      suppressPackageStartupMessages(install.packages(package.name))
    }

    library(package.name, character.only = T)
  }
}

#' cat0
#'
#' \code{cat0} is an alternative to \link{cat}
#'
#' This is indeed the same as \code{cat(..., sep = '')}
#'
#' @param ... R objects.
#'
#' @examples
#' cat0('string', 'withoutspaces')
#'
cat0 <- function(...) {
  cat(..., sep = '')
}

#' catn
#'
#' \code{catn} is an alternative to \link{cat}
#'
#' This is indeed the same as \code{\link{cat0}} but with a new line
#'
#' @param ... R objects.
#'
#' @examples
#' catn('string', 'withoutspaces', 'andanewline')
#'
catn <- function(...) {
  cat(..., '\n', sep = '')
}

#' stop0
#'
#' \code{stop0} is an alternative to \link{stop}
#'
#' This is indeed the same as \code{\link{stop}} but quietly, very
#' useful when included in the middle of a source file and you want
#' to stop it without a warning message.
#'
#' @examples
#' stop0()
stop0 <- function() {
  opt <- options(show.error.messages = FALSE)
  on.exit(options(opt))
  stop()
}

#' last
#'
#' \code{last} is based on \link{tail}
#'
#' It returns the last element of an array or dataframe
#'
#' @examples
#' last(c(1,2,3))
last <- function(x) { tail(x, n = 1) }

# install.packages("roxygen2")
# roxygen2::roxygenise()
# devtools::document()
