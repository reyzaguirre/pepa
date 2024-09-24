#' Automatic report for a factorial experiment
#'
#' Produces an automatic report for selected variables in a factorial experiment with
#' a CRD or RCBD.
#' @param vars The variables to analize.
#' @param factors The factors.
#' @param rep The replications or blocks, \code{NULL} for a CRD.
#' @param dfr The name of the data frame containing the data.
#' @param maxp Maximum allowed proportion of missing values to estimate, default is 10\%.
#' @param pe Logical. If \code{"pe = TRUE"} multiple comparison tests for principal effects
#' are included even if interaction is significat, only in the case of 2 factors.
#' Default to \code{"pe = FALSE"}.
#' @param se Logical. If \code{"se = TRUE"} multiple comparison tests for simple effects
#' are included even if interaction is not significat, only in the case of 2 factors.
#' Default to \code{"se = FALSE"}.
#' @param title Report title.
#' @param subtitle Report subtitle.
#' @param author Report author.
#' @param format The output file format for the report, \code{"html"} by default.
#' Other options are \code{"word"} and \code{"pdf"}.
#' @details It fits a linear model for a factorial experiment with a CRD or RCBD for
#' the selected variables. If data is unbalanced, missing values are estimated up to an
#' specified maximum proportion, 10\% by default. All factors are considered as fixed.
#' @return It returns an automatic report for the factorial experiment with a CRD or
#' RCBD fitted model.
#' @author Raul Eyzaguirre.
#' @examples
#' ## Example 1: Two factors factorial
#' repo.f(c("asc.dw", "asc.fw"), c("geno", "treat"), NULL, asc)
#'
#' ## Example 2: Three factors factorial
#' # Create design with 3 factors
#' fnames <- c('A', 'B', 'C')
#' flevels <- list(c('a1', 'a2'), c('b1', 'b2'), c('c1', 'c2', 'c3'))
#' temp <- cr.f(fnames, flevels, 'rcbd', 2, 10)$book
#' # Simulate random data
#' temp$y <- rnorm(24)
#' # Run report
#' repo.f('y', c('A', 'B', 'C'), 'block', temp)
#' @importFrom agricolae LSD.test HSD.test
#' @importFrom utils browseURL
#' @export

repo.f <- function(vars, factors, rep, dfr, maxp = 0.1,
                   pe = FALSE, se = FALSE,
                   title = "Automatic report for a factorial experiment",
                   subtitle = NULL,
                   author = "International Potato Center",
                   format = c("html", "word", "pdf")) {

  format <- paste0(match.arg(format), "_document")
  dirfiles <- system.file(package = "pepa")

  # Number of factors

  nf <- length(factors)

  # Select names for files

  if (is.null(rep) & nf == 2)
    fn <- "2fcrd"
  if (!is.null(rep) & nf == 2)
    fn <- "2frcbd"
  if (nf > 2)
    fn <- "factorial"

  fileRmd <- paste0(dirfiles, "/rmd/", fn, ".Rmd")
  fileURL <- paste0(dirfiles, "/rmd/", fn, ".html")
  fileDOCX <- paste0(dirfiles, "/rmd/", fn, ".docx")
  filePDF <- paste0(dirfiles, "/rmd/", fn, ".pdf")

  rmarkdown::render(fileRmd, output_format = format,
                    params = list(vars = vars,
                                  factors = factors,
                                  rep = rep,
                                  dfr = dfr,
                                  maxp = maxp,
                                  pe = pe,
                                  se = se,
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
