#' Report for repeated measures data
#'
#' Produce a ggplot2 graph for data over time.
#' @param response Response variable.
#' @param treat The treatments.
#' @param eu The experimental unit. Must be defined in case of subsamples.
#' @param dap Days after planting measuring time.
#' @param dfr The name of the data frame.
#' @param average If \code{"TRUE"}, it computes average over experimental units
#' (for subsampling data).
#' @param se If \code{"TRUE"}, standard errors are ploted.
#' @param title Report title.
#' @param subtitle Report subtitle.
#' @param author Report author.
#' @param format The output file format for the report, \code{"html"} by default.
#' Other options are \code{"word"} and \code{"pdf"}.
#' @param server Logical. If \code{"FALSE"} works with local machines.
#' Otherwise works in server environments.
#' @param server_dir_name If \code{"server = TRUE"}, this is the directory name in the server.
#' @param server_file_name If \code{"server = TRUE"}, this is the file name in the server.
#' @return It returns a ggplot2 graph for repeated measures data.
#' @author Raul Eyzaguirre.
#' @examples
#' repo.trend("yield", "treat", "plot", "date", trend.data)
#' repo.trend("yield", "treat", "plot", "date", trend.data, FALSE)
#' repo.trend("yield", "treat", "plot", "date", trend.data, FALSE, FALSE)
#' @import ggplot2
#' @export

repo.trend <- function(response, treat, eu, dap, dfr, average = TRUE, se = TRUE,
                       title = "Automatic report for repeated measures data",
                       subtitle = "Dispersion plot with a fitted smoothing function",
                       author = "International Potato Center",
                       format = c("html", "word", "pdf"),
                       server = FALSE,
                       server_dir_name = "directory",
                       server_file_name = "filename") {

  format <- paste0(match.arg(format), "_document")
  dirfiles <- system.file(package = "pepa")

  if (!server) {

    fileRmd <- paste0(dirfiles, "/rmd/trend_graph.Rmd")
    fileURL <- paste0(dirfiles, "/rmd/trend_graph.html")
    fileDOCX <- paste0(dirfiles, "/rmd/trend_graph.docx")
    filePDF <- paste0(dirfiles, "/rmd/trend_graph.pdf")

  } else {

    dirfiles <- server_dir_name

    # Only Markdown and Word files

    fileRmd <- paste0(dirfiles, "trend_graph.Rmd")
    fileRmd_server_name <- paste0(dirfiles, server_file_name, ".Rmd")
    fileDOCX <- paste0(dirfiles, "trend_graph.docx")
    fileDOCX_server_name <- paste0(dirfiles, server_file_name, ".docx")

  }

  rmarkdown::render(fileRmd, output_format = format,
                    params = list(response = response,
                                  treat = treat,
                                  eu = eu,
                                  dap = dap,
                                  dfr = dfr,
                                  average = average,
                                  se = se,
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
