#' Report for PVS
#'
#' Produce standard reports for PVS data
#' @param data A data frame with data for a pvs form.
#' @param form Form number.
#' @param title The title.
#' @param subtitle The subtitle.
#' @param author Author.
#' @param format The output file format for the report, \code{"html"} by default.
#' Other options are \code{"word"} and \code{"pdf"}.
#' @param server Logical. If \code{"FALSE"} works with local machines.
#' Otherwise works in server environments.
#' @param server_dir_name If \code{"server = TRUE"}, this is the directory name in the server.
#' @param server_file_name If \code{"server = TRUE"}, this is the file name in the server.
#' @author Raul Eyzaguirre.
#' @details It produces a report for different forms of the Mother & Baby methodology
#' implemented by CIP.
#' @return It returns automatic reports for the different forms on a PVS book.
#' @examples
#' repo.pvs(pvsf1, 1)
#' repo.pvs(pvsf2, 2)
#' repo.pvs(pvsf3, 3)
#' repo.pvs(pvsf6, 6)
#' repo.pvs(pvsf7, 7)
#' repo.pvs(pvsf9, 9)
#' @import ggplot2
#' @importFrom factoextra fviz_pca
#' @export

repo.pvs <- function(data, form,
                     title = "Automatic report for PVS",
                     subtitle = NULL,
                     author = "International Potato Center",
                     format = c("html", "word", "pdf"),
                     server = FALSE,
                     server_dir_name = "directory",
                     server_file_name = "filename") {

  format <- paste0(match.arg(format), "_document")
  dirfiles <- system.file(package = "pepa")

  if (form == 1) {
    subtitle <- "Identification of selection criteria for genotypes"
    fn <- "pvs1"
  }
  if (form == 2) {
    subtitle <- "Best genotypes at flowering"
    fn <- "pvs23"
  }
  if (form == 3) {
    subtitle <- "Best genotypes at harvest"
    fn <- "pvs23"
  }
  if (form == 6) {
    subtitle <- "Organoleptic analysis at mother trial"
    fn <- "pvs67"
  }
  if (form == 7) {
    subtitle <- "Organoleptic analysis at baby trials"
    fn <- "pvs67"
  }
  if (form == 9) {
    subtitle <- "Best genotypes at post-harvest"
    fn <- "pvs9"
  }

  if (!server) {

    fileRmd <- paste0(dirfiles, "/rmd/", fn, ".Rmd")
    fileURL <- paste0(dirfiles, "/rmd/", fn, ".html")
    fileDOCX <- paste0(dirfiles, "/rmd/", fn, ".docx")
    filePDF <- paste0(dirfiles, "/rmd/", fn, ".pdf")

    } else {

    dirfiles <- server_dir_name

    # Only Markdown and Word files

    fileRmd <- paste0(dirfiles, fn, ".Rmd")
    fileRmd_server_name <- paste0(dirfiles, server_file_name, ".Rmd")
    fileDOCX <- paste0(dirfiles, fn, ".docx")
    fileDOCX_server_name <- paste0(dirfiles, server_file_name, ".docx")

  }

  rmarkdown::render(fileRmd, output_format = format,
                    params = list(data = data,
                                  form = form,
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
