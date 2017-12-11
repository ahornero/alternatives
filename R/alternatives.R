#' library0
#'
#' \code{library0} is an alternative to \link{library}, which also installs the package if needed.
#' It is also compatible with GitHub package without using additional libraries.
#'
#' @details This is an alternative function to \link{library}, which pretends to be an easier way
#' to attach and install a package in the same step. You can indicate the package as a string or
#' directly by the name. It also works with GitHub packages through the install-github.me service,
#' which is based on \code{remotes}
#'
#' @param package the name of the package, given as a name or character string. It also supports
#' the GitHub package name for both installing and loading.
#' @param force.update Optional. Logical. It indicates if the package should be updated even if is installed.
#'
#' @import utils
#' @export
#' @examples
#' \dontrun{
#' # Load and install (if so) CRAN and GitHub R packages, and automatically as character strings or not
#' library0("ggplot2")
#' library0(ggplot2)
#' library0(hadley/devtools)
#' }
library0 <- function(package, force.update = FALSE) {

  is.installed <- function(package) {
    is.element(package, utils::installed.packages()[,1])
  }

  detach.package <- function(package)
  {
    item <- paste("package", package, sep = ":")
    while (item %in% search()) {
      detach(item, unload = TRUE, character.only = TRUE)
    }
  }

  # Original library function from R Core, just to avoid warnings from R CMD check results
  library.internal <- function(package, pos = 2, lib.loc = NULL, character.only = FALSE, logical.return = FALSE, warn.conflicts = TRUE, quietly = FALSE, verbose = getOption("verbose")) {
    testRversion <- function(pkgInfo, pkgname, pkgpath) {
      if (is.null(built <- pkgInfo$Built))
        stop(gettextf("package %s has not been installed properly\n",
                      sQuote(pkgname)), call. = FALSE, domain = NA)
      R_version_built_under <- as.numeric_version(built$R)
      if (R_version_built_under < "3.0.0")
        stop(gettextf("package %s was built before R 3.0.0: please re-install it",
                      sQuote(pkgname)), call. = FALSE, domain = NA)
      current <- getRversion()
      if (length(Rdeps <- pkgInfo$Rdepends2)) {
        for (dep in Rdeps) if (length(dep) > 1L) {
          target <- dep$version
          res <- if (is.character(target)) {
            do.call(dep$op, list(as.numeric(R.version[["svn rev"]]),
                                 as.numeric(sub("^r", "", dep$version))))
          }
          else {
            do.call(dep$op, list(current, as.numeric_version(target)))
          }
          if (!res)
            stop(gettextf("This is R %s, package %s needs %s %s",
                          current, sQuote(pkgname), dep$op, target),
                 call. = FALSE, domain = NA)
        }
      }
      if (R_version_built_under > current)
        warning(gettextf("package %s was built under R version %s",
                         sQuote(pkgname), as.character(built$R)), call. = FALSE,
                domain = NA)
      platform <- built$Platform
      r_arch <- .Platform$r_arch
      if (.Platform$OS.type == "unix") {
        if (!nzchar(r_arch) && grepl("\\w", platform) &&
            !testPlatformEquivalence(platform, R.version$platform))
          stop(gettextf("package %s was built for %s",
                        sQuote(pkgname), platform), call. = FALSE,
               domain = NA)
      }
      else {
        if (nzchar(platform) && !grepl("mingw", platform))
          stop(gettextf("package %s was built for %s",
                        sQuote(pkgname), platform), call. = FALSE,
               domain = NA)
      }
      if (nzchar(r_arch) && file.exists(file.path(pkgpath,
                                                  "libs")) && !file.exists(file.path(pkgpath, "libs",
                                                                                     r_arch)))
        stop(gettextf("package %s is not installed for 'arch = %s'",
                      sQuote(pkgname), r_arch), call. = FALSE, domain = NA)
    }
    checkNoGenerics <- function(env, pkg) {
      nenv <- env
      ns <- .getNamespace(as.name(pkg))
      if (!is.null(ns))
        nenv <- asNamespace(ns)
      if (exists(".noGenerics", envir = nenv, inherits = FALSE))
        TRUE
      else {
        !any(startsWith(names(env), ".__T"))
      }
    }
    checkConflicts <- function(package, pkgname, pkgpath, nogenerics,
                               env) {
      dont.mind <- c("last.dump", "last.warning", ".Last.value",
                     ".Random.seed", ".Last.lib", ".onDetach", ".packageName",
                     ".noGenerics", ".required", ".no_S3_generics", ".Depends",
                     ".requireCachedGenerics")
      sp <- search()
      lib.pos <- which(sp == pkgname)
      ob <- names(as.environment(lib.pos))
      if (!nogenerics) {
        these <- ob[startsWith(ob, ".__T__")]
        gen <- gsub(".__T__(.*):([^:]+)", "\\1", these)
        from <- gsub(".__T__(.*):([^:]+)", "\\2", these)
        gen <- gen[from != package]
        ob <- ob[!(ob %in% gen)]
      }
      fst <- TRUE
      ipos <- seq_along(sp)[-c(lib.pos, match(c("Autoloads",
                                                "CheckExEnv"), sp, 0L))]
      for (i in ipos) {
        obj.same <- match(names(as.environment(i)), ob,
                          nomatch = 0L)
        if (any(obj.same > 0)) {
          same <- ob[obj.same]
          same <- same[!(same %in% dont.mind)]
          Classobjs <- which(startsWith(same, ".__"))
          if (length(Classobjs))
            same <- same[-Classobjs]
          same.isFn <- function(where) vapply(same, exists,
                                              NA, where = where, mode = "function", inherits = FALSE)
          same <- same[same.isFn(i) == same.isFn(lib.pos)]
          not.Ident <- function(ch, TRAFO = identity,
                                ...) vapply(ch, function(.) !identical(TRAFO(get(.,
                                                                                 i)), TRAFO(get(., lib.pos)), ...), NA)
          if (length(same))
            same <- same[not.Ident(same)]
          if (length(same) && identical(sp[i], "package:base"))
            same <- same[not.Ident(same, ignore.environment = TRUE)]
          if (length(same)) {
            if (fst) {
              fst <- FALSE
              packageStartupMessage(gettextf("\nAttaching package: %s\n",
                                             sQuote(package)), domain = NA)
            }
            msg <- .maskedMsg(sort(same), pkg = sQuote(sp[i]),
                              by = i < lib.pos)
            packageStartupMessage(msg, domain = NA)
          }
        }
      }
    }
    if (verbose && quietly)
      message("'verbose' and 'quietly' are both true; being verbose then ..")
    if (!missing(package)) {
      if (is.null(lib.loc))
        lib.loc <- .libPaths()
      lib.loc <- lib.loc[dir.exists(lib.loc)]
      if (!character.only)
        package <- as.character(substitute(package))
      if (length(package) != 1L)
        stop("'package' must be of length 1")
      if (is.na(package) || (package == ""))
        stop("invalid package name")
      pkgname <- paste0("package:", package)
      newpackage <- is.na(match(pkgname, search()))
      if (newpackage) {
        pkgpath <- find.package(package, lib.loc, quiet = TRUE,
                                verbose = verbose)
        if (length(pkgpath) == 0L) {
          txt <- if (length(lib.loc))
            gettextf("there is no package called %s",
                     sQuote(package))
          else gettext("no library trees found in 'lib.loc'")
          if (logical.return) {
            warning(txt, domain = NA)
            return(FALSE)
          }
          else stop(txt, domain = NA)
        }
        which.lib.loc <- normalizePath(dirname(pkgpath),
                                       "/", TRUE)
        pfile <- system.file("Meta", "package.rds", package = package,
                             lib.loc = which.lib.loc)
        if (!nzchar(pfile))
          stop(gettextf("%s is not a valid installed package",
                        sQuote(package)), domain = NA)
        pkgInfo <- readRDS(pfile)
        testRversion(pkgInfo, package, pkgpath)
        ffile <- system.file("Meta", "features.rds", package = package,
                             lib.loc = which.lib.loc)
        features <- if (file.exists(ffile))
          readRDS(ffile)
        else NULL
        if (is.character(pos)) {
          npos <- match(pos, search())
          if (is.na(npos)) {
            warning(gettextf("%s not found on search path, using pos = 2",
                             sQuote(pos)), domain = NA)
            pos <- 2
          }
          else pos <- npos
        }
        .getRequiredPackages2(pkgInfo, quietly = quietly)
        deps <- unique(names(pkgInfo$Depends))
        if (packageHasNamespace(package, which.lib.loc)) {
          if (isNamespaceLoaded(package)) {
            newversion <- as.numeric_version(pkgInfo$DESCRIPTION["Version"])
            oldversion <- as.numeric_version(getNamespaceVersion(package))
            if (newversion != oldversion) {
              res <- tryCatch(unloadNamespace(package),
                              error = function(e) {
                                P <- if (!is.null(cc <- conditionCall(e)))
                                  paste("Error in", deparse(cc)[1L],
                                        ": ")
                                else "Error : "
                                stop(gettextf("Package %s version %s cannot be unloaded:\n %s",
                                              sQuote(package), oldversion, paste0(P,
                                                                                  conditionMessage(e), "\n")), domain = NA)
                              })
            }
          }
          tt <- tryCatch({
            attr(package, "LibPath") <- which.lib.loc
            ns <- loadNamespace(package, lib.loc)
            env <- attachNamespace(ns, pos = pos, deps)
          }, error = function(e) {
            P <- if (!is.null(cc <- conditionCall(e)))
              paste(" in", deparse(cc)[1L])
            else ""
            msg <- gettextf("package or namespace load failed for %s%s:\n %s",
                            sQuote(package), P, conditionMessage(e))
            if (logical.return)
              message(paste("Error:", msg), domain = NA)
            else stop(msg, call. = FALSE, domain = NA)
          })
          if (logical.return && is.null(tt))
            return(FALSE)
          attr(package, "LibPath") <- NULL
          {
            on.exit(detach(pos = pos))
            nogenerics <- !.isMethodsDispatchOn() || checkNoGenerics(env,
                                                                     package)
            if (warn.conflicts && !exists(".conflicts.OK",
                                          envir = env, inherits = FALSE))
              checkConflicts(package, pkgname, pkgpath,
                             nogenerics, ns)
            on.exit()
            if (logical.return)
              return(TRUE)
            else return(invisible(.packages()))
          }
        }
        else stop(gettextf("package %s does not have a namespace and should be re-installed",
                           sQuote(package)), domain = NA)
      }
      if (verbose && !newpackage)
        warning(gettextf("package %s already present in search()",
                         sQuote(package)), domain = NA)
    }
    else {
      if (is.null(lib.loc))
        lib.loc <- .libPaths()
      db <- matrix(character(), nrow = 0L, ncol = 3L)
      nopkgs <- character()
      for (lib in lib.loc) {
        a <- .packages(all.available = TRUE, lib.loc = lib)
        for (i in sort(a)) {
          file <- system.file("Meta", "package.rds", package = i,
                              lib.loc = lib)
          title <- if (nzchar(file)) {
            txt <- readRDS(file)
            if (is.list(txt))
              txt <- txt$DESCRIPTION
            if ("Encoding" %in% names(txt)) {
              to <- if (Sys.getlocale("LC_CTYPE") == "C")
                "ASCII//TRANSLIT"
              else ""
              tmp <- try(iconv(txt, txt["Encoding"], to,
                               "?"))
              if (!inherits(tmp, "try-error"))
                txt <- tmp
              else warning("'DESCRIPTION' has an 'Encoding' field and re-encoding is not possible",
                           call. = FALSE)
            }
            txt["Title"]
          }
          else NA
          if (is.na(title))
            title <- " ** No title available ** "
          db <- rbind(db, cbind(i, lib, title))
        }
        if (length(a) == 0L)
          nopkgs <- c(nopkgs, lib)
      }
      dimnames(db) <- list(NULL, c("Package", "LibPath", "Title"))
      if (length(nopkgs) && !missing(lib.loc)) {
        pkglist <- paste(sQuote(nopkgs), collapse = ", ")
        msg <- sprintf(ngettext(length(nopkgs), "library %s contains no packages",
                                "libraries %s contain no packages"), pkglist)
        warning(msg, domain = NA)
      }
      y <- list(header = NULL, results = db, footer = NULL)
      class(y) <- "libraryIQR"
      return(y)
    }
    if (logical.return)
      TRUE
    else invisible(.packages())
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
    package.name.gh <- unlist(strsplit(x = package.name, split = '/'))[2]

    if (force.update) {
      detach.package(package.name.gh)
      remove.packages(package.name.gh)
    }

    if (!is.installed(package.name.gh)) {
      source(paste0("https://install-github.me/", package.name))
    }

    library.internal(package.name.gh, character.only = T)

  } else {
    # CRAN Package
    if (force.update) {
      detach.package(package.name)
      remove.packages(package.name)
    }

    if (!is.installed(package.name)) {
      utils::install.packages(package.name)
    }

    library.internal(package.name, character.only = T)
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
#' @export
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
#' @export
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
#' @export
#'
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
#' @param x data.frame or array
#'
#' @export
#'
#' @examples
#' last(c(1,2,3))
last <- function(x) { utils::tail(x, n = 1) }

# roxygen2::roxygenise()
# devtools::document()
