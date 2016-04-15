#' Authomatic report for a RCBD
#'
#' Produces an authomatic report for selected traits in an experiment with a RCBD.
#' @param traits The traits to analize.
#' @param treat The treatments.
#' @param rep The replications.
#' @param data The name of the data frame.
#' @param maxp Maximum allowed proportion of missing values to estimate, default is 10\%.
#' @param author Author.
#' @author Raul Eyzaguirre.
#' @details It fits a linear model for a RCBD and explains the results.
#'
#' Under the assumption of fixed effects an ANOVA table is computed with missing values
#' estimated up to a specified percentage (10\% by default). If the ANOVA results in
#' a significant value for treatments then the Tukey HSD method for pairwise difference
#' is applied. Assumptions of the model are illustrated with residual plots.
#'
#' Under the assumption of random effects the model is estimated using REML and the
#' variance components are shown. Missing values are not estimated in this case.
#' @return It returns an explanation about the RCBD fitted model.
#' @examples
#' repo.rcbd(c("trw", "vw", "crw"), "geno", "rep", pjpz09)
#'
#' # With a small data set
#' temp <- pjpz09[1:18, ]
#' repo.rcbd(c("trw", "vw", "crw"), "geno", "rep", temp)
#' @export

repo.rcbd <- function(traits, treat, rep, data, maxp = 0.1,
                      author = "International Potato Center") {

  dirfiles <- system.file(package = "pepa")
  fileRmd <- paste(dirfiles, "/rmd/rcbd.Rmd", sep = "")
  fileURL <- paste(dirfiles, "/rmd/rcbd.html", sep = "")

  rmarkdown::render(fileRmd, params = list(traits = traits,
                                           treat = treat,
                                           rep = rep,
                                           data = data,
                                           maxp = maxp,
                                           author = author))
  browseURL(fileURL)
}
