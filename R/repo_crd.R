#' Authomatic report for a CRD
#'
#' Produces an authomatic report for selected traits in an experiment with a CRD.
#' @param traits The traits to analize.
#' @param treat The treatments.
#' @param data The name of the data frame.
#' @param maxp Maximum allowed proportion of missing values to estimate, default is 10\%.
#' @param author Author.
#' @author Raul Eyzaguirre.
#' @details It fits a linear model for a CRD and explains the results.
#'
#' Under the assumption of fixed effects an ANOVA table is computed. If the ANOVA
#' results in a significant value then the Tukey HSD method for pairwise differences
#' is applied. Assumptions of the model are evaluated with residual plots.
#'
#' Under the assumption of random effects the model is estimated using REML and the
#' variance components are shown.
#' @return It returns an explanation about the CRD fitted model.
#' @examples
#' repo.crd(c("trw", "vw", "crw"), "geno", pjpz09)
#'
#' # With a small data set
#' temp <- pjpz09[1:18, ]
#' repo.crd(c("trw", "vw", "crw"), "geno", temp)
#' @export

repo.crd <- function(traits, treat, data, maxp = 0.1,
                     author = "International Potato Center") {

  dirfiles <- system.file(package = "pepa")
  fileRmd <- paste(dirfiles, "/rmd/crd.Rmd", sep = "")
  fileURL <- paste(dirfiles, "/rmd/crd.html", sep = "")

  rmarkdown::render(fileRmd, params = list(traits = traits,
                                           treat = treat,
                                           data = data,
                                           maxp = maxp,
                                           author = author))
  browseURL(fileURL)
}
