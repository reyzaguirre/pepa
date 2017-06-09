#' Automatic report for a North Carolina genetic design.
#'
#' Produces an automatic report for a North Carolina I, II or III genetic design analysis.
#' @param traits The traits to analize.
#' @param set The set.
#' @param male The male.
#' @param female The female.
#' @param progeny The progeny (only for Carolina I design).
#' @param rep The replication.
#' @param model 1, 2 or 3.
#' @param data The data frame.
#' @param title The title.
#' @param subtitle The subtitle.
#' @param author Author.
#' @param format The output file format for the report, \code{"html"} by default.
#' Other options are \code{"word"} and \code{"pdf"}.
#' @author Raul Eyzaguirre.
#' @details It fits a linear model to data from a North Carolina I, II or III
#' genetic design.
#' @return It returns an ANOVA and related quantities.
#' @examples
#' library(agricolae)
#' data(DC)
#' carolina1 <- DC$carolina1
#' repo.nc("yield", "set", "male", "female", "progenie", "rep", 1, carolina1)
#' @import agricolae
#' @importFrom utils browseURL
#' @export

repo.nc <- function(traits, set, male, female, progeny, rep, model = NULL, data,
                    title = "Automatic report for a North Carolina genetic design",
                    subtitle = NULL,
                    author = "International Potato Center",
                    format = c("html", "word", "pdf")) {

  format <- paste(match.arg(format), "_document", sep = "")
  dirfiles <- system.file(package = "pepa")

  fileRmd <- paste(dirfiles, "/rmd/nc.Rmd", sep = "")
  fileURL <- paste(dirfiles, "/rmd/nc.html", sep = "")
  fileDOCX <- paste(dirfiles, "/rmd/nc.docx", sep = "")
  filePDF <- paste(dirfiles, "/rmd/nc.pdf", sep = "")

  rmarkdown::render(fileRmd, output_format = format,
                    params = list(traits = traits,
                                  set = set,
                                  male = male,
                                  female = female,
                                  progeny = progeny,
                                  rep = rep,
                                  model = model,
                                  data = data,
                                  title = title,
                                  subtitle = subtitle,
                                  author = author))

  if(format == "html_document") try(browseURL(fileURL))
  if(format == "word_document") try(shell.exec(fileDOCX))
  if(format == "pdf_document")  try(shell.exec(filePDF))
}
