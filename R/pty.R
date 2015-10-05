#' Pepa tells you
#'
#' Explain an R object in plain English if she knows about it
#' @param x An R object.
#' @author Raul Eyzaguirre.
#' @details It uses a set of templates to explain R objects in plain English.
#' It aims to produce automatic reports for some standard statistical procedures, most
#' of them included in the \code{st4gi} package.
#' @return It returns an automatic report about the selected R object.
#' @export

pty <- function(x) {
  if (class(x) == "data.frame") pty_df(x) else
    print("I am sorry, this object is not in my list")
}

pty_df <- function(x) {
  rmarkdown::render("inst/df.Rmd", params = list(x = x))
}
