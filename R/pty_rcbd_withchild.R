#' Pepa tells you about RCBD
#'
#' Explain a RCBD fitted model in plain English
#' @param traits The traits to analize.
#' @param treat The treatments.
#' @param rep The replications.
#' @param data The name of the data frame.
#' @param maxp Maximum allowed proportion of missing values to estimate, default is 10\%.
#' @param author Author.
#' @author Raul Eyzaguirre.
#' @details It fits a linear model for a RCBD and explains the results.
#' It also checks the assumptions.
#' @return It returns an explanation about the RCBD fitted model.
#' @examples
#' pty.rcbd.withchild(c("trw", "vw"), "geno", "rep", pjpz09)
#' @export

pty.rcbd.withchild <- function(traits, treat, rep, data, maxp = 0.1,
                               author = "International Potato Center") {

  dirfiles <- system.file(package = "pepa")
  fileRmd <- paste(dirfiles, "/rmd/rcbd_withchild.Rmd", sep = "")
  fileURL <- paste(dirfiles, "/rmd/rcbd_withchild.html", sep = "")

  rmarkdown::render(fileRmd, params = list(traits = traits,
                                           treat = treat,
                                           rep = rep,
                                           data = data,
                                           maxp = maxp,
                                           author = author))
  browseURL(fileURL)
}
