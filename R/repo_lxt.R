#' Automatic report for a Line x Tester genetic design.
#'
#' Produces an automatic report for a Line x Tester genetic design analysis.
#' @param dfr The name of the data frame.
#' @param vars The variables to analize.
#' @param line The lines.
#' @param tester The testers.
#' @param rep The replications.
#' @param title Report title.
#' @param subtitle Report subtitle.
#' @param author Report author.
#' @param format The output file format for the report, \code{"html"} by default.
#' Other options are \code{"word"} and \code{"pdf"}.
#' @param server Logical. If \code{"FALSE"} works with local machines.
#' Otherwise works in server environments.
#' @param server_dir_name If \code{"server = TRUE"}, this is the directory name in the server.
#' @param server_file_name If \code{"server = TRUE"}, this is the file name in the server.
#' @return It returns an automatic report for a Line by Tester genetic design.
#' @author Raul Eyzaguirre.
#' @examples
#' repo.lxt(lxt, "yield", "line", "tester", "rep")
#' @importFrom utils browseURL
#' @export

repo.lxt <- function(dfr, vars, line, tester, rep,
                     title = "Automatic report for a Line x Tester genetic design",
                     subtitle = NULL,
                     author = "International Potato Center",
                     format = c("html", "word", "pdf"),
                     server = FALSE,
                     server_dir_name = "directory",
                     server_file_name = "filename") {

  format <- paste0(match.arg(format), "_document")
  dirfiles <- system.file(package = "pepa")

  if (!server) {

    fileRmd <- paste0(dirfiles, "/rmd/lxt.Rmd")
    fileURL <- paste0(dirfiles, "/rmd/lxt.html")
    fileDOCX <- paste0(dirfiles, "/rmd/lxt.docx")
    filePDF <- paste0(dirfiles, "/rmd/lxt.pdf")

  } else {

    dirfiles <- server_dir_name

    # Only Markdown and Word files

    fileRmd <- paste0(dirfiles, "lxt.Rmd")
    fileRmd_server_name <- paste0(dirfiles, server_file_name, ".Rmd")
    fileDOCX <- paste0(dirfiles, "lxt.docx")
    fileDOCX_server_name <- paste0(dirfiles, server_file_name, ".docx")

  }

  rmarkdown::render(fileRmd, output_format = format,
                    params = list(dfr = dfr,
                                  vars = vars,
                                  line = line,
                                  tester = tester,
                                  rep = rep,
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
