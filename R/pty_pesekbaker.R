#' Pepa tells you about the Pesek-Baker index
#'
#' Explain the results of the Pesek-Baker index in plain English.
#' @param traits List of traits.
#' @param geno The genotypes.
#' @param env The environments.
#' @param rep The replications. Must be defined if \code{model = "gxe"}.
#' @param data The name of the data frame containing the data.
#' @param means The genotypic means to compute the index, \code{"single"}
#' or \code{"fitted"}. The default is \code{"single"}. See details for more information.
#' @param model Type of model, \code{"gxe"} for a model with gxe interaction or \code{"g+e"}
#' for a model without interaction. The default is \code{"gxe"}. See details for more information.
#' @param dgg Desired genetic gains. The default is one standard deviation for each trait.
#' @param units Units for dgg, \code{"actual"} or \code{"sdu"}. See details for more information.
#' @param sf Selected fraction. The default is 0.1.
#' @param author Author.
#' @author Raul Eyzaguirre.
#' @details The Pesek-Baker is an index where relative economic weights have been replaced
#' by desired gains.
#'
#' By default a model with components for genotypes, environments, genotypes by environments
#' interaction and replications nested into environments is fitted (\code{model = "gxe"}).
#' If \code{model = "g+e"} then a model with components for genotypes
#' and environments is fitted, and in this case the gxe variance includes the gxe plus the
#' error variance. Response to selection is only computed when \code{model = "gxe"}.
#'
#' If \code{means = "fitted"} then the model specified in \code{model} is used to fit
#' the means of the genotypes. Otherwise single arithmetic means are computed over the
#' replications for each genotype at each environment and then for each genotype over environments.
#'
#' If \code{dgg} is not specified, the standard deviations of the traits are used.
#' It means that the desired genetic gains are equal to one standard deviation for
#' each trait. \code{dgg} can be specified in actual units (\code{units = "actual"}) or in
#' standard deviations (\code{units = "sdu"}), defaults to \code{"sdu"}. For example,
#' if you have a trait which is expressed in kilograms and with a standard deviation of
#' 5 kilograms, typing \code{dgg = 2} means a desired genetic gain of 2 standard deviations
#' that corresponds to 10 kilograms. If you type \code{dgg = 2} and \code{units = "actual"}
#' then this means a desired genetic gain of 2 kilograms. If \code{dgg = NULL} then the
#' desired genetic gain will be one standard deviation, no matter if \code{units} is set
#' as \code{"actual"} or \code{"sdu"}.
#' @return It returns an explanation about the Pesek-Baker index.
#' @examples
#' pty.pesekbaker(c("rytha", "bc", "dm", "star", "nocr"), "geno", "loc", "rep", spg)
#' @export

pty.pesekbaker <- function(traits, geno, env, rep = NULL, data, means = "single",
                           model = "gxe", dgg = NULL, units = "sdu", sf = 0.1,
                           author = "International Potato Center") {

  dirfiles <- system.file(package = "pepa")
  fileRmd <- paste(dirfiles, "/pesekbaker.Rmd", sep = "")
  fileURL <- paste(dirfiles, "/pesekbaker.html", sep = "")

  rmarkdown::render(fileRmd, params = list(traits = traits,
                                           geno = geno,
                                           env = env,
                                           rep = rep,
                                           data = data,
                                           means = means,
                                           model = model,
                                           dgg = dgg,
                                           units = units,
                                           sf = sf,
                                           author = author))
  browseURL(fileURL)
}
