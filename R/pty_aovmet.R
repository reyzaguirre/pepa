#' Pepa tells you about RCBD
#'
#' Explain a RCBD fitted model in plain English
#' @param trait The trait to analize.
#' @param treat The treatments.
#' @param block The blocks.
#' @param data The name of the data frame.
#' @param author Author.
#' @author Raul Eyzaguirre.
#' @details It fits a linear model for a RCBD and explains the results.
#' It also checks the assumptions.
#' @return It returns an automatic report about the RCBD fitted model.
#' @examples
#' pty_aovmet("trw", "geno", "rep", pjpz09)
#' @export

pty_aovmet <- function(trait, treat, block, data, author = "International Potato Center") {

  dirfiles <- system.file(package = "pepa")
  fileRmd <- paste(dirfiles, "/aovmet.Rmd", sep = "")
  fileURL <- paste(dirfiles, "/aovmet.html", sep = "")

  rmarkdown::render(fileRmd, params = list(trait = trait,
                                           treat = treat,
                                           block = block,
                                           data = data,
                                           author = author))
  browseURL(fileURL)
}
