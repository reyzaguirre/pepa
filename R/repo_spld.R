#' Automatic report for a split plot design
#'
#' Produces an automatic report for selected traits in an experiment
#' with a split-plot or a split-split-plot design.
#' @param traits The traits to analize.
#' @param mpf The main plots factor.
#' @param spf The sub plots factor.
#' @param sspf The sub sub plot factor (if any).
#' @param rep The replications.
#' @param dfr The name of the data frame.
#' @param title Report title.
#' @param subtitle Report subtitle.
#' @param author Report author.
#' @param format The output file format for the report, \code{"html"} by default.
#' Other options are \code{"word"} and \code{"pdf"}.
#' @param server Logical. If \code{"FALSE"} works with local machines.
#' Otherwise works in server environments.
#' @param server_dir_name If \code{"server = TRUE"}, this is the directory name in the server.
#' @param server_file_name If \code{"server = TRUE"}, this is the file name in the server.
#' @details It fits a linear model for a split-plot or a split-split-plot design and explains
#' the results.
#' @return It returns an explanation about the split-plot or a split-split-plot design
#' fitted model.
#' @author Raul Eyzaguirre.
#' @examples
#' # Create designs
#' mpf <- paste0("mpf", 1:2)
#' spf <- paste0("spf", 1:3)
#' sspf <- paste0("sspf", 1:4)
#' d1 <- cr.spld(c("mpf", "spf"), list(mpf, spf), 3)$book
#' d2 <- cr.spld(c("mpf", "spf", "sspf"), list(mpf, spf, sspf), 2)$book
#' # Some random data
#' d1$yield <- rnorm(18, 10)
#' d2$yield <- rnorm(48, 10)
#' # Fit models
#' repo.spld("yield", "mpf", "spf", NULL, "block", d1)
#' repo.spld("yield", "mpf", "spf", "sspf", "block", d2)
#' @import st4gi
#' @importFrom utils browseURL
#' @export

repo.spld <- function(traits, mpf, spf, sspf = NULL, rep, dfr,
                      title = NULL,
                      subtitle = NULL,
                      author = "International Potato Center",
                      format = c("html", "word", "pdf"),
                      server = FALSE,
                      server_dir_name = "directory",
                      server_file_name = "filename") {

  format <- paste0(match.arg(format), "_document")
  dirfiles <- system.file(package = "pepa")

  # Select names for files

  if (is.null(sspf)) {
    fn <- "spld"
    if (is.null(title))
      title <- "Automatic report for a split-plot design"
  } else {
    fn <- "sspld"
    if (is.null(title))
      title <- "Automatic report for a split-split-plot design"
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
                    params = list(traits = traits,
                                  mpf = mpf,
                                  spf = spf,
                                  sspf = sspf,
                                  rep = rep,
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
