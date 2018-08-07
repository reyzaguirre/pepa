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
#' @param dfr The data frame.
#' @param title Report title.
#' @param subtitle Report subtitle.
#' @param author Report author.
#' @param format The output file format for the report, \code{"html"} by default.
#' Other options are \code{"word"} and \code{"pdf"}.
#' @param server Logical. If \code{"FALSE"} works with local machines.
#' Otherwise works in server environments.
#' @param server_dir_name If \code{"server = TRUE"}, this is the directory name in the server.
#' @param server_file_name If \code{"server = TRUE"}, this is the file name in the server.
#' @return It returns an ANOVA and related quantities.
#' @author Raul Eyzaguirre.
#' @examples
#' library(agricolae)
#' data(DC)
#' carolina1 <- DC$carolina1
#' repo.nc("yield", "set", "male", "female", "progenie", "rep", 1, carolina1)
#' carolina2 <- DC$carolina2
#' carolina2 <- carolina2[carolina2$Loc == 1, c(2, 5, 4, 3, 7, 8)]
#' repo.nc(c("yield", "tuber"), "set", "male", "female", NULL, "rep", 2, carolina2)
#' carolina3 <- DC$carolina3
#' repo.nc("yield", "set", "male", "female", NULL, "rep", 3, carolina3)
#' @import agricolae
#' @importFrom utils browseURL
#' @export

repo.nc <- function(traits, set, male, female, progeny = NULL, rep, model, dfr,
                    title = NULL, subtitle = NULL,
                    author = "International Potato Center",
                    format = c("html", "word", "pdf"),
                    server = FALSE,
                    server_dir_name = "directory",
                    server_file_name = "filename") {

  if (model == 1)
    title <- "Automatic report for a North Carolina I genetic design"
  if (model == 2)
    title <- "Automatic report for a North Carolina II genetic design"
  if (model == 3)
    title <- "Automatic report for a North Carolina III genetic design"

  format <- paste0(match.arg(format), "_document")
  dirfiles <- system.file(package = "pepa")

  if (!server) {

    fileRmd <- paste0(dirfiles, "/rmd/nc.Rmd")
    fileURL <- paste0(dirfiles, "/rmd/nc.html")
    fileDOCX <- paste0(dirfiles, "/rmd/nc.docx")
    filePDF <- paste0(dirfiles, "/rmd/nc.pdf")

  } else {

    dirfiles <- server_dir_name

    # Only Markdown and Word files

    fileRmd <- paste0(dirfiles, "nc.Rmd")
    fileRmd_server_name <- paste0(dirfiles, server_file_name, ".Rmd")
    fileDOCX <- paste0(dirfiles, "nc.docx")
    fileDOCX_server_name <- paste0(dirfiles, server_file_name, ".docx")

  }

  rmarkdown::render(fileRmd, output_format = format,
                    params = list(traits = traits,
                                  set = set,
                                  male = male,
                                  female = female,
                                  progeny = progeny,
                                  rep = rep,
                                  model = model,
                                  dfr = dfr,
                                  title = title,
                                  subtitle = subtitle,
                                  author = author))

  if (!server) {

    if (format == "html_document")
      try(browseURL(fileURL))

    if (format == "word_document")
      try(system(paste("open", fileDOCX)))

    if (format == "pdf_document")
      try(system(paste("open", filePDF)))

  } else {

    file.copy(fileDOCX, fileDOCX_server_name, overwrite = TRUE)

  }

}
