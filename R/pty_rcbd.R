#' Pepa tells you about RCBD
#'
#' Explain a RCBD fitted model in plain English
#' @param trait The trait to analize.
#' @param treat The treatments.
#' @param rep The replications.
#' @param data The name of the data frame.
#' @param maxp Maximum allowed proportion of missing values to estimate, default is 10\%.
#' @param author Author.
#' @author Raul Eyzaguirre.
#' @details It fits a linear model for a RCBD and explains the results.
#' It also checks the assumptions.
#' @return It returns an automatic report about the RCBD fitted model.
#' @examples
#' pty_rcbd("trw", "geno", "rep", pjpz09)
#' @export

pty_rcbd <- function(trait, treat, rep, data, maxp = 0.1,
                     author = "International Potato Center") {

  dirfiles <- system.file(package = "pepa")
  fileRmd <- paste(dirfiles, "/rcbd.Rmd", sep = "")
  fileURL <- paste(dirfiles, "/rcbd.html", sep = "")

  rmarkdown::render(fileRmd, params = list(trait = trait,
                                           treat = treat,
                                           rep = rep,
                                           data = data,
                                           maxp = maxp,
                                           author = author))
  browseURL(fileURL)
}
