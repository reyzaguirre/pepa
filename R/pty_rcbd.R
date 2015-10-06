#' Pepa tells you about RCBD
#'
#' Explain a RCBD fitted model in plain English
#' @param trait The trait to analize.
#' @param treat The treatments.
#' @param block The blocks.
#' @param data The name of the data frame.
#' @author Raul Eyzaguirre.
#' @details It fits a linear model for a RCBD and explains the results.
#' It also checks the assumptions.
#' @return It returns an automatic report about the RCBD fitted model.
#' @examples
#' pty_rcbd("trw", "geno", "rep", pjpz09)
#' @export

pty_rcbd <- function(trait, treat, block, data) {

  dirfiles <- system.file(package = "pepa")
  fileRmd <- paste(dirfiles, "/aov_rcbd.Rmd", sep = "")
  fileURL <- paste(dirfiles, "/aov_rcbd.html", sep = "")

  rmarkdown::render(fileRmd, params = list(trait = trait,
                                           treat = treat,
                                           block = block,
                                           data = data))
  browseURL(fileURL)
}
