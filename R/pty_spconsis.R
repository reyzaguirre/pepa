#' Pepa tells you about the consistency of your data
#'
#' It checks your data for inconsistencies.
#' @param data The name of the data frame.
#' @param plot.size Plot size in square meters.
#' @param f Factor for extreme values detection. See details.
#' @param author Author.
#' @author Raul Eyzaguirre.
#' @details The data frame must use the labels (lower or upper case) specified in
#' the function \code{spconsis} of package \code{st4gi}. Type \code{?spconsis} to
#' see the list.
#' Extreme values are detected using the interquartile range.
#' The rule is to detect any value out of the interval
#' \eqn{[Q_1 - f \times IQR; Q_3 + f \times IQR]}. By default \code{f = 3}.
#' @return It returns a list of all rows with some kind of inconsistency and
#' all rows with outliers.
#' @examples
#' pty.spconsis(pjpz09, 4.5)
#' @export

pty.spconsis <- function(data, plot.size, f = 3, author = "International Potato Center") {

  dirfiles <- system.file(package = "pepa")
  fileRmd <- paste(dirfiles, "/spconsis.Rmd", sep = "")
  fileURL <- paste(dirfiles, "/spconsis.html", sep = "")

  rmarkdown::render(fileRmd, params = list(data = data,
                                           plot.size = plot.size,
                                           f = f,
                                           author = author))
  browseURL(fileURL)
}
