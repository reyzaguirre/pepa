#' Report for PVS - Global summary
#'
#' Produce a report for PVS data global summary sheet.
#' @param traits A list of traits to be included in the report.
#' @param dfr A data frame with data for a pvs global summary form.
#' @param title Report title.
#' @param subtitle Report subtitle.
#' @param author Report author.
#' @param format The output file format for the report, \code{"html"} by default.
#' Other options are \code{"word"} and \code{"pdf"}.
#' @param server Logical. If \code{"FALSE"} works with local machines.
#' Otherwise works in server environments.
#' @param server_dir_name If \code{"server = TRUE"}, this is the directory name in the server.
#' @param server_file_name If \code{"server = TRUE"}, this is the file name in the server.
#' @details It produces a report for global summary sheet of the Mother & Baby methodology
#' implemented by CIP.
#' @return It returns an automatic report.
#' @author Raul Eyzaguirre.
#' @examples
#' traits <- c("NMTP_Mean_mother", "TNTP_Mean_mother", "PPH_Mean_mother", "TTYA_Mean_mother")
#' repo.pvssg(traits, pvssg)
#' @import ggplot2
#' @importFrom factoextra fviz_pca
#' @importFrom magrittr %>%
#' @importFrom purrr map
#' @export

repo.pvssg <- function(traits,
                       dfr,
                       title = "Automatic report for PVS",
                       subtitle = "Global summary data",
                       author = "International Potato Center",
                       format = c("html", "word", "pdf"),
                       server = FALSE,
                       server_dir_name = "directory",
                       server_file_name = "filename") {

  format <- paste0(match.arg(format), "_document")
  dirfiles <- system.file(package = "pepa")

  if (!server) {

    fileRmd <- paste0(dirfiles, "/rmd/pvssg.Rmd")
    fileURL <- paste0(dirfiles, "/rmd/pvssg.html")
    fileDOCX <- paste0(dirfiles, "/rmd/pvssg.docx")
    filePDF <- paste0(dirfiles, "/rmd/pvssg.pdf")

  } else {

    dirfiles <- server_dir_name

    # Only Markdown and Word files

    fileRmd <- paste0(dirfiles, "pvssg.Rmd")
    fileRmd_server_name <- paste0(dirfiles, server_file_name, ".Rmd")
    fileDOCX <- paste0(dirfiles, "pvssg.docx")
    fileDOCX_server_name <- paste0(dirfiles, server_file_name, ".docx")

  }

  rmarkdown::render(fileRmd, output_format = format,
                    params = list(traits = traits,
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
