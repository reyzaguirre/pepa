#' Automatic report for an augmented block design (ABD)
#'
#' Produces an automatic report for selected traits in an experiment with an ABD.
#' @param traits The traits to analize.
#' @param treat The treatments.
#' @param rep The replications.
#' @param data The name of the data frame.
#' @param author Author.
#' @author Raul Eyzaguirre.
#' @details It fits a linear model for an ABD and explains the results.
#'
#' The Tukey HSD method for pairwise differences is applied.
#' Assumptions of the model are evaluated with residual plots.
#'
#' @return It returns an explanation about the ABD fitted model.
#' @examples
#' # A subset that looks like an ABD
#' temp <- pjpz09[c(1, 2, 9, 10, 13, 14, 27, 29, 31, 33, 35, 37, 40, 42, 44, 46, 48, 50, 203, 204), ]
#' repo.abd(c("trw", "vw", "crw"), "geno", "rep", temp)
#'
#' # With some missing values
#' temp[c(1, 2, 3, 4), "trw"] <- NA
#' temp[c(1, 2, 3), "vw"] <- NA
#' temp[c(1, 10, 15), "crw"] <- NA
#' repo.abd(c("nocr", "trw", "vw", "crw"), "geno", "rep", temp)
#' @export

repo.abd <- function(traits, treat, rep, data, author = "International Potato Center") {

  dirfiles <- system.file(package = "pepa")
  fileRmd <- paste(dirfiles, "/rmd/abd.Rmd", sep = "")
  fileURL <- paste(dirfiles, "/rmd/abd.html", sep = "")

  rmarkdown::render(fileRmd, params = list(traits = traits,
                                           treat = treat,
                                           rep = rep,
                                           data = data,
                                           author = author))
  browseURL(fileURL)
}
