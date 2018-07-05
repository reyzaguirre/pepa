#' Automatic report for a factorial experiment
#'
#' Produces an automatic report for selected traits in a factorial experiment with
#' a CRD or RCBD.
#' @param traits The traits to analize.
#' @param factors The factors (up to 3).
#' @param rep The replications or blocks.
#' @param design The statistical design, \code{crd} or \code{rcbd}.
#' @param data The name of the data frame containing the data.
#' @param maxp Maximum allowed proportion of missing values to estimate, default is 10\%.
#' @param title The title.
#' @param subtitle The subtitle.
#' @param author Author.
#' @param format The output file format for the report, \code{"html"} by default.
#' Other options are \code{"word"} and \code{"pdf"}.
#' @author Raul Eyzaguirre.
#' @details It fits a linear model for a factorial experiment with a CRD or RCBD for
#' the selected traits. If data is unbalanced, missing values are estimated up to an
#' specified maximum proportion, 10\% by default. All factors are considered as fixed.
#' @return It returns an automatic report for the factorial experiment with a CRD or
#' RCBD fitted model.
#' @examples
#' repo.f(c("asc.dw", "asc.fw"), c("geno", "treat"), "rep", "crd", asc)
#' @import agricolae
#' @importFrom utils browseURL
#' @export

repo.f <- function(traits, factors, rep, design, data, maxp = 0.1,
                   title = "Automatic report for a factorial experiment",
                   subtitle = NULL,
                   author = "International Potato Center",
                   format = c("html", "word", "pdf")) {

  format <- paste(match.arg(format), "_document", sep = "")
  dirfiles <- system.file(package = "pepa")

  if (design == "crd") {
    fileRmd <- paste(dirfiles, "/rmd/2fcrd.Rmd", sep = "")
    fileURL <- paste(dirfiles, "/rmd/2fcrd.html", sep = "")
    fileDOCX <- paste(dirfiles, "/rmd/2fcrd.docx", sep = "")
    filePDF <- paste(dirfiles, "/rmd/2fcrd.pdf", sep = "")
  }
  if (design == "rcbd") {
    fileRmd <- paste(dirfiles, "/rmd/2frcbd.Rmd", sep = "")
    fileURL <- paste(dirfiles, "/rmd/2frcbd.html", sep = "")
    fileDOCX <- paste(dirfiles, "/rmd/2frcbd.docx", sep = "")
    filePDF <- paste(dirfiles, "/rmd/2frcbd.pdf", sep = "")
  }

  rmarkdown::render(fileRmd, output_format = format,
                    params = list(traits = traits,
                                  factors = factors,
                                  rep = rep,
                                  design = design,
                                  data = data,
                                  maxp = maxp,
                                  title = title,
                                  subtitle = subtitle,
                                  author = author))

  if (format == "html_document") try(browseURL(fileURL))
  if (format == "word_document") try(system(paste("open", fileDOCX)))
  if (format == "pdf_document")  try(system(paste("open", filePDF)))
}
