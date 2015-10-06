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
  classlist <- c("data.frame", "numeric")
  if (class(x) %in% classlist == FALSE) pty_na(x)
  if (class(x) == "data.frame") pty_df(x)
  if (class(x) == "numeric") pty_nt(x)
  if (class(x) == "aov") pty_aov(x)
}

dirfiles <- system.file(package = "pepa")

# Not available yet

pty_na <- function(x) {
  fileRmd <- paste(dirfiles, "/na.Rmd", sep = "")
  fileURL <- paste(dirfiles, "/na.html", sep = "")
  rmarkdown::render(fileRmd, params = list(x = x))
  browseURL(fileURL)
}

# for data.frames

pty_df <- function(x) {
  fileRmd <- paste(dirfiles, "/df.Rmd", sep = "")
  fileURL <- paste(dirfiles, "/df.html", sep = "")
  rmarkdown::render(fileRmd, params = list(x = x))
  browseURL(fileURL)
}

# for numeric traits

pty_nt <- function(x) {
  fileRmd <- paste(dirfiles, "/nt.Rmd", sep = "")
  fileURL <- paste(dirfiles, "/nt.html", sep = "")
  rmarkdown::render(fileRmd, params = list(x = x))
  browseURL(fileURL)
}

# for linear models with aov

pty_aov <- function(x) {
  fileRmd <- paste(dirfiles, "/aov.Rmd", sep = "")
  fileURL <- paste(dirfiles, "/aov.html", sep = "")
  rmarkdown::render(fileRmd, params = list(x = x))
  browseURL(fileURL)
}
