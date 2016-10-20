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
#' @param dgg Desired genetic gains. The default is one standard deviation for each trait.
#' @param units Units for dgg, \code{"actual"} or \code{"sdu"}. See details for more information.
#' @param sf Selected fraction. The default is 0.1.
#' @param author Author.
#' @param format The output file format for the report, \code{"html"} by default.
#' Other options are \code{"word"} and \code{"pdf"}.
#' @author Raul Eyzaguirre.
#' @details Type \code{?pesekbaker} for additional details.
#' @return It returns an explanation about the Pesek-Baker index.
#' @examples
#' pty.pesekbaker(c("rytha", "bc", "dm", "star", "nocr"), "geno", "loc", "rep", spg)
#' @import st4gi
#' @importFrom utils browseURL
#' @export

pty.pesekbaker <- function(traits, geno, env, rep = NULL, data, means = "single",
                           dgg = NULL, units = "sdu", sf = 0.1,
                           author = "International Potato Center",
                           format = c("html", "word", "pdf")) {

  format <- paste(match.arg(format), "_document", sep = "")
  dirfiles <- system.file(package = "pepa")

  fileRmd <- paste(dirfiles, "/pesekbaker.Rmd", sep = "")
  fileURL <- paste(dirfiles, "/pesekbaker.html", sep = "")
  fileDOCX <- paste(dirfiles, "/pesekbaker.docx", sep = "")
  filePDF <- paste(dirfiles, "/pesekbaker.pdf", sep = "")

  rmarkdown::render(fileRmd, output_format = format,
                    params = list(traits = traits,
                                  geno = geno,
                                  env = env,
                                  rep = rep,
                                  data = data,
                                  means = means,
                                  dgg = dgg,
                                  units = units,
                                  sf = sf,
                                  author = author))

  if(format == "html_document") try(browseURL(fileURL))
  if(format == "word_document") try(shell.exec(fileDOCX))
  if(format == "pdf_document")  try(shell.exec(filePDF))
}
