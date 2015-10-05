#' Pepa tells you
#'
#' Explain an R object in plain English if she knows about it
#' @param x An R object.
#' @author Raul Eyzaguirre.
#' @details It uses a set of templates to explain R objects in plain English.
#' It aims to produce automatic reports for some standard statistical procedures, most
#' of them included in the \code{st4gi} package.
#' @return It returns an automatic report about the selected R object.
#' @examples
#' # Pepa tells you something about a data frame:
#' pty(pjpz09)
#' @export

pty <- function(x) {
  if (class(x) == "data.frame") pty_df(x) else
    if (class(x) == "numeric") pty_nt(x) else
      print("I am sorry, this object is not in my list")
}

# for data.frames

pty_df <- function(x) {
  rmarkdown::render("inst/df.Rmd", params = list(x = x))
  browseURL("inst/df.html")
}

# for numeric traits

pty_nt <- function(x) {
  rmarkdown::render("inst/nt.Rmd", params = list(x = x))
  browseURL("inst/nt.html")
}
