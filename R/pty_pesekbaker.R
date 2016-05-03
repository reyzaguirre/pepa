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
#' @details Type \code{?pesekbaker} for additional details.
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
