#' Pepa tells you about the Pesek-Baker index
#'
#' Explain the results of the Pesek-Baker index in plain English.
#' @param traits List of traits.
#' @param geno The genotypes.
#' @param env The environments.
#' @param rep The replications. Must be defined if \code{model = "gxe"}.
#' @param dfr The name of the data frame containing the data.
#' @param means The genotypic means to compute the index, \code{"single"} or
#' \code{"fitted"}. The default is \code{"single"}. See details for more information.
#' @param dgg Desired genetic gains. The default is one standard deviation for each trait.
#' @param units Units for dgg, \code{"actual"} or \code{"sdu"}.
#' See details for more information.
#' @param sf Selected fraction. The default is 0.1.
#' @param author Report author.
#' @param format The output file format for the report, \code{"html"} by default.
#' Other options are \code{"word"} and \code{"pdf"}.
#' @details Type \code{?pesekbaker} for additional details.
#' @return It returns an explanation about the Pesek-Baker index.
#' @author Raul Eyzaguirre.
#' @examples
#' pty.pesekbaker(c("rytha", "bc", "dm", "star", "nocr"), "geno", "loc", "rep", spg)
#' @import st4gi
#' @importFrom utils browseURL
#' @export

pty.pesekbaker <- function(traits, geno, env, rep = NULL, dfr, means = "single",
                           dgg = NULL, units = "sdu", sf = 0.1,
                           author = "International Potato Center",
                           format = c("html", "word", "pdf")) {

  format <- paste0(match.arg(format), "_document")
  dirfiles <- system.file(package = "pepa")

  fileRmd <- paste0(dirfiles, "/rmd/pesekbaker.Rmd")
  fileURL <- paste0(dirfiles, "/rmd/pesekbaker.html")
  fileDOCX <- paste0(dirfiles, "/rmd/pesekbaker.docx")
  filePDF <- paste0(dirfiles, "/rmd/pesekbaker.pdf")

  rmarkdown::render(fileRmd, output_format = format,
                    params = list(traits = traits,
                                  geno = geno,
                                  env = env,
                                  rep = rep,
                                  dfr = dfr,
                                  means = means,
                                  dgg = dgg,
                                  units = units,
                                  sf = sf,
                                  author = author))

  if(format == "html_document")
    try(browseURL(fileURL))

  if(format == "word_document")
    try(system(paste("open", fileDOCX)))

  if(format == "pdf_document")
    try(system(paste("open", filePDF)))

}
