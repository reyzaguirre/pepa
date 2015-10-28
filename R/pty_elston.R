#' Pepa tells you about the Elston index
#'
#' Explain the results of the Elston index in plain English.
#' @param traits List of traits.
#' @param geno The genotypes.
#' @param env The environments.
#' @param rep The replications. Must be defined if \code{model = "gxe"}.
#' @param data The name of the data frame containing the data.
#' @param means The genotypic means to compute the index, \code{"single"}
#' or \code{"fitted"}. The default is \code{"single"}. See details for more information.
#' @param model Type of model to fit means if \code{means = "fitted"}, \code{"gxe"} for
#' a model with gxe interaction or \code{"g+e"} for a model without interaction.
#' The default is \code{"gxe"}. See details for more information.
#' @param lb Lower bound. \code{1} for \eqn{k = min(x)} and \code{2} for
#' \eqn{k = (n \times min(x) - max(x)) / (n - 1)}
#' @param author Author.
#' @author Raul Eyzaguirre.
#' @details The Elston index is a weight free index.
#'
#' If \code{means = "fitted"} and \code{model = "gxe"} then the arguments \code{env} and
#' \code{rep} must be specified.
#' If \code{means = "fitted"} and \code{model = "g+e"} then only the argument \code{env}
#' must be specified.
#' If \code{means = "single"} and \code{env} and \code{rep} are specified, then
#' single arithmetic means are computed over the replications for each genotype
#' at each environment and then for each genotype over environments. In any other case
#' single arithmetic means are computed over all the observations for each genotype.
#' @return It returns an explanation about the Elston index.
#' @examples
#' pty.elston(c("rytha", "bc", "dm", "star", "nocr"), "geno", data = spg)
#' @export

pty.elston <- function(traits, geno, env = NULL, rep = NULL, data, means = "single",
                       model = "gxe", lb = 1, author = "International Potato Center") {

  dirfiles <- system.file(package = "pepa")
  fileRmd <- paste(dirfiles, "/elston.Rmd", sep = "")
  fileURL <- paste(dirfiles, "/elston.html", sep = "")

  rmarkdown::render(fileRmd, params = list(traits = traits,
                                           geno = geno,
                                           env = env,
                                           rep = rep,
                                           data = data,
                                           means = means,
                                           model = model,
                                           lb = lb,
                                           author = author))
  browseURL(fileURL)
}
