#' Pepa tells you about a MET with a RCBD
#'
#' Explain a fitted model for a multi environment trial (MET) with a RCBD
#' in each environment in plain English.
#' @param trait The trait to analize.
#' @param geno The genotypes.
#' @param env The environments.
#' @param rep The replications.
#' @param data The name of the data frame containing the data.
#' @param maxp Maximum allowed proportion of missing values to estimate, default is 10\%.
#' @param author Author.
#' @param format The output file format for the report, \code{"html"} by default.
#' Other options are \code{"word"} and \code{"pdf"}.
#' @author Raul Eyzaguirre.
#' @details It fits a linear model for a MET with a RCBD and explains the results.
#' If data is unbalanced, missing values are estimated up to an specified maximum
#' proportion, 10\% by default. Genotypes and environments are considered as fixed
#' factors while the blocks are considered as random and nested into the environments.
#' @return It returns an explanation about the MET with a RCBD fitted model.
#' @examples
#' pty.aovmet("y", "geno", "env", "rep", met8x12)
#' @import st4gi
#' @importFrom utils browseURL
#' @export

pty.aovmet <- function(trait, geno, env, rep, data, maxp = 0.1,
                       author = "International Potato Center",
                       format = c("html", "word", "pdf")) {

  format <- paste(match.arg(format), "_document", sep = "")
  dirfiles <- system.file(package = "pepa")

  fileRmd <- paste(dirfiles, "/aovmet.Rmd", sep = "")
  fileURL <- paste(dirfiles, "/aovmet.html", sep = "")
  fileDOCX <- paste(dirfiles, "/aovmet.docx", sep = "")
  filePDF <- paste(dirfiles, "/aovmet.pdf", sep = "")

  rmarkdown::render(fileRmd, output_format = format,
                    params = list(trait = trait,
                                  geno = geno,
                                  env = env,
                                  rep = rep,
                                  data = data,
                                  maxp = maxp,
                                  author = author))

  if(format == "html_document") try(browseURL(fileURL))
  if(format == "word_document") try(shell.exec(fileDOCX))
  if(format == "pdf_document")  try(shell.exec(filePDF))
}
