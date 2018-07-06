#' Automatic report for a factorial experiment
#'
#' Produces an automatic report for selected traits in a factorial experiment with
#' a CRD or RCBD.
#' @param traits The traits to analize.
#' @param factors The factors.
#' @param rep The replications or blocks.
#' @param design The statistical design, \code{crd} or \code{rcbd}.
#' @param data The name of the data frame containing the data.
#' @param maxp Maximum allowed proportion of missing values to estimate, default is 10\%.
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
                   format = c("html", "word", "pdf"),
                   server = FALSE,
                   server_dir_name = "directory",
                   server_file_name = "filename") {

  format <- paste0(match.arg(format), "_document")
  dirfiles <- system.file(package = "pepa")

  if (design == "crd")
    fn <- "2fcrd"
  if (design == "rcbd")
    fn <- "2frcbd"

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
                                  factors = factors,
                                  rep = rep,
                                  design = design,
                                  data = data,
                                  maxp = maxp,
                                  title = title,
                                  subtitle = subtitle,
                                  author = author))

  if (!server) {

    if (format == "html_document") try(browseURL(fileURL))
    if (format == "word_document") try(system(paste("open", fileDOCX)))
    if (format == "pdf_document") try(system(paste("open", filePDF)))

  } else {

    file.copy(fileDOCX, fileDOCX_server_name, overwrite = TRUE)

  }

}
