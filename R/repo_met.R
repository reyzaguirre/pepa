#' Automatic report for a MET with a RCBD
#'
#' Produces an automatic report for selected traits in a multi environment
#' trial (MET) with a RCBD in each environment.
#' @param traits The traits to analize.
#' @param geno The genotypes.
#' @param env The environments.
#' @param rep The replications.
#' @param data The name of the data frame containing the data.
#' @param maxp Maximum allowed proportion of missing values to estimate, default is 10\%.
#' @param author Author.
#' @param format The output file format for the report, \code{"html"} by default.
#' Other options are \code{"word"} and \code{"pdf"}.
#' @author Raul Eyzaguirre.
#' @details It fits a linear model for a MET with a RCBD for the selected trait.
#' If data is unbalanced, missing values are estimated up to an specified maximum
#' proportion, 10\% by default. Genotypes and environments are considered as fixed
#' factors while the blocks are considered as random and nested into the environments
#' for ANOVA. For variance components estimation all the factors
#' are treated as random.
#' @return It returns an automatic report about the MET with a RCBD fitted model.
#' @examples
#' repo.met(c("rytha", "fytha"), "geno", "env", "rep", megaclones)
#' @export

repo.met <- function(traits, geno, env, rep, data, maxp = 0.1,
                     author = "International Potato Center",
                     format = c("html", "word", "pdf")) {

  format <- paste(match.arg(format), "_document", sep = "")

  dirfiles <- system.file(package = "pepa")
  fileRmd <- paste(dirfiles, "/rmd/met.Rmd", sep = "")
  fileURL <- paste(dirfiles, "/rmd/met.html", sep = "")
  fileDOCX <- paste(dirfiles, "/rmd/met.docx", sep = "")
  filePDF <- paste(dirfiles, "/rmd/met.pdf", sep = "")

  rmarkdown::render(fileRmd, output_format = format,
                    params = list(traits = traits,
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
