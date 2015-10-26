#' Authomatic report for a MET with a RCBD
#'
#' Produces an authomatic report for a set of selected traits in a multi environment
#' trial (MET) with a RCBD in each environment.
#' @param traits The list of traits to include.
#' @param geno The genotypes.
#' @param env The environments.
#' @param rep The replications.
#' @param data The name of the data frame containing the data.
#' @param maxp Maximum allowed proportion of missing values to estimate, default is 10\%.
#' @param author Author.
#' @author Raul Eyzaguirre.
#' @details It fits a linear model for a MET with a RCBD for each trait.
#' If data is unbalanced, missing values are estimated up to an specified maximum
#' proportion, 10\% by default. Genotypes and environments are considered as fixed
#' factors while the blocks are considered as random and nested into the environments.
#' @return It returns an automatic report about the MET with a RCBD fitted model.
#' @examples
#' repo.met(c("rytha", "bc", "dm"), "geno", "loc", "rep", spg)
#' @export

repo.met <- function(traits, geno, env, rep, data, maxp = 0.1,
                     author = "International Potato Center") {

  dirfiles <- system.file(package = "pepa")
  fileRmd <- paste(dirfiles, "/met.Rmd", sep = "")
  fileURL <- paste(dirfiles, "/met.html", sep = "")

  rmarkdown::render(fileRmd, params = list(traits = traits,
                                           geno = geno,
                                           env = env,
                                           rep = rep,
                                           data = data,
                                           maxp = maxp,
                                           author = author))
  browseURL(fileURL)
}
