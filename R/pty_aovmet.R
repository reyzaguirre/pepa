#' Pepa tells you about a MET with a RCBD
#'
#' Explain a fitted model for a multi environment trial (MET) with a RCBD
#' in each environment in plain English.
#' @param y The variable to analize.
#' @param geno The genotypes.
#' @param env The environments.
#' @param rep The replications.
#' @param dfr The name of the data frame containing the data.
#' @param maxp Maximum allowed proportion of missing values to estimate, default is 10\%.
#' @param author Report author.
#' @param format The output file format for the report, \code{"html"} by default.
#' Other options are \code{"word"} and \code{"pdf"}.
#' @details It fits a linear model for a MET with a RCBD and explains the results.
#' If data is unbalanced, missing values are estimated up to an specified maximum
#' proportion, 10\% by default. Genotypes and environments are considered as fixed
#' factors while the blocks are considered as random and nested into the environments.
#' @return It returns an explanation about the MET with a RCBD fitted model.
#' @author Raul Eyzaguirre.
#' @examples
#' pty.aovmet("y", "geno", "env", "rep", met8x12)
#' @import st4gi
#' @importFrom utils browseURL
#' @export

pty.aovmet <- function(y, geno, env, rep, dfr, maxp = 0.1,
                       author = "International Potato Center",
                       format = c("html", "word", "pdf")) {

  format <- paste0(match.arg(format), "_document")
  dirfiles <- system.file(package = "pepa")

  fileRmd <- paste0(dirfiles, "/rmd/aovmet.Rmd")
  fileURL <- paste0(dirfiles, "/rmd/aovmet.html")
  fileDOCX <- paste0(dirfiles, "/rmd/aovmet.docx")
  filePDF <- paste0(dirfiles, "/rmd/aovmet.pdf")

  rmarkdown::render(fileRmd, output_format = format,
                    params = list(y = y,
                                  geno = geno,
                                  env = env,
                                  rep = rep,
                                  dfr = dfr,
                                  maxp = maxp,
                                  author = author))

  if(format == "html_document")
    try(browseURL(fileURL))

  if(format == "word_document")
    try(system(paste("open", fileDOCX)))

  if(format == "pdf_document")
    try(system(paste("open", filePDF)))

}
