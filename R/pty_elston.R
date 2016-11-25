#' Pepa tells you about the Elston index
#'
#' Explain the results of the Elston index in plain English.
#' @param traits List of traits.
#' @param geno The genotypes.
#' @param env The environments.
#' @param rep The replications.
#' @param data The name of the data frame containing the data.
#' @param means The genotypic means to compute the index, \code{"single"}
#' or \code{"fitted"}. The default is \code{"single"}. See details for more information.
#' @param lb Lower bound. \code{1} for \eqn{k = min(x)} and \code{2} for
#' \eqn{k = (n \times min(x) - max(x)) / (n - 1)}
#' @param author Author.
#' @param format The output file format for the report, \code{"html"} by default.
#' Other options are \code{"word"} and \code{"pdf"}.
#' @author Raul Eyzaguirre.
#' @details Type \code{?elston} for additional details.
#' @return It returns an explanation about the Elston index.
#' @examples
#' pty.elston(c("rytha", "bc", "dm", "star", "nocr"), "geno", data = spg)
#' @import st4gi
#' @importFrom utils browseURL
#' @export

pty.elston <- function(traits, geno, env = NULL, rep = NULL, data,
                       means = "single", lb = 1,
                       author = "International Potato Center",
                       format = c("html", "word", "pdf")) {

  format <- paste(match.arg(format), "_document", sep = "")
  dirfiles <- system.file(package = "pepa")

  fileRmd <- paste(dirfiles, "/rmd/elston.Rmd", sep = "")
  fileURL <- paste(dirfiles, "/rmd/elston.html", sep = "")
  fileDOCX <- paste(dirfiles, "/rmd/elston.docx", sep = "")
  filePDF <- paste(dirfiles, "/rmd/elston.pdf", sep = "")

  rmarkdown::render(fileRmd, output_format = format,
                    params = list(traits = traits,
                                  geno = geno,
                                  env = env,
                                  rep = rep,
                                  data = data,
                                  means = means,
                                  lb = lb,
                                  author = author))

  if(format == "html_document") try(browseURL(fileURL))
  if(format == "word_document") try(system(paste("open", fileDOCX)))
  if(format == "pdf_document")  try(system(paste("open", filePDF)))
}
