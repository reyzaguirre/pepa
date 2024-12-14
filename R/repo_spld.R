#' Automatic report for a split plot design
#'
#' Produces an automatic report for selected variables in an experiment
#' with a split-plot or a split-split-plot design.
#' @param dfr The name of the data frame.
#' @param vars The variables to analize.
#' @param mpf The main plots factor.
#' @param spf The sub plots factor.
#' @param sspf The sub sub plot factor (if any).
#' @param rep The replications.
#' @param pe Logical. If \code{"pe = TRUE"} multiple comparison tests for principal effects
#' are included even if interaction is significat, only in the case of 2 factors.
#' Default to \code{"pe = FALSE"}.
#' @param title Report title.
#' @param subtitle Report subtitle.
#' @param author Report author.
#' @param format The output file format for the report, \code{"html"} by default.
#' Other options are \code{"word"} and \code{"pdf"}.
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
#' repo.spld(d1, "yield", "mpf", "spf", NULL, "block")
#' repo.spld(d2, "yield", "mpf", "spf", "sspf", "block")
#' @import st4gi
#' @importFrom utils browseURL
#' @export

repo.spld <- function(dfr, vars, mpf, spf, sspf = NULL, rep, pe = FALSE,
                      title = NULL,
                      subtitle = NULL,
                      author = "International Potato Center",
                      format = c("html", "word", "pdf")) {

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

  fileRmd <- paste0(dirfiles, "/rmd/", fn, ".Rmd")
  fileURL <- paste0(dirfiles, "/rmd/", fn, ".html")
  fileDOCX <- paste0(dirfiles, "/rmd/", fn, ".docx")
  filePDF <- paste0(dirfiles, "/rmd/", fn, ".pdf")

  rmarkdown::render(fileRmd, output_format = format,
                    params = list(dfr = dfr,
                                  vars = vars,
                                  mpf = mpf,
                                  spf = spf,
                                  sspf = sspf,
                                  rep = rep,
                                  pe = pe,
                                  title = title,
                                  subtitle = subtitle,
                                  author = author))

  if (format == "html_document")
    try(browseURL(fileURL))

  if (format == "word_document")
    try(system(paste("open", fileDOCX)))

  if (format == "pdf_document")
    try(system(paste("open", filePDF)))

}
