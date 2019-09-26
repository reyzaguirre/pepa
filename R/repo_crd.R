#' Automatic report for a Completely Randomized Design (CRD)
#'
#' Produces an automatic report for selected traits in an experiment with a CRD.
#' @param traits The traits to analize.
#' @param trt The treatments.
#' @param trt.lab The labels for treatments.
#' @param eu The experimental unit. Must be defined in case of subsamples.
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
#' @details It fits a linear model for a CRD and explains the results.
#'
#' Under the assumption of fixed effects an ANOVA table is computed.
#' In case of subsampling the ANOVA table is computed over the means for each
#' experimental unit, and a second ANOVA table is computed considering subsampling.
#' If the ANOVA results in a significant value then the Tukey HSD method for pairwise
#' differences is applied. Assumptions of the model are evaluated with residual plots.
#'
#' Under the assumption of random effects the model is estimated using REML and the
#' variance components are shown.
#' @return It returns an explanation about the CRD fitted model.
#' @author Raul Eyzaguirre.
#' @examples
#' repo.crd(c("trw", "vw"), "geno", "genotype", dfr = pjpz09)
#' # With a small data set
#' temp <- pjpz09[1:18, ]
#' repo.crd(c("trw", "vw", "crw"), "geno", "genotypes", dfr = temp)
#' @import st4gi
#' @importFrom utils browseURL
#' @export

repo.crd <- function(traits, trt, trt.lab = "treatment", eu = NULL, dfr,
                     title = "Automatic report for a Completely Randomized Design (CRD)",
                     subtitle = NULL,
                     author = "International Potato Center",
                     format = c("html", "word", "pdf"),
                     server = FALSE,
                     server_dir_name = "directory",
                     server_file_name = "filename") {

  format <- paste0(match.arg(format), "_document")
  dirfiles <- system.file(package = "pepa")

  if (!server) {

    fileRmd <- paste0(dirfiles, "/rmd/crd.Rmd")
    fileURL <- paste0(dirfiles, "/rmd/crd.html")
    fileDOCX <- paste0(dirfiles, "/rmd/crd.docx")
    filePDF <- paste0(dirfiles, "/rmd/crd.pdf")

  } else {

    dirfiles <- server_dir_name

    # Only Markdown and Word files

    fileRmd <- paste0(dirfiles, "crd.Rmd")
    fileRmd_server_name <- paste0(dirfiles, server_file_name, ".Rmd")
    fileDOCX <- paste0(dirfiles, "crd.docx")
    fileDOCX_server_name <- paste0(dirfiles, server_file_name, ".docx")

  }

  rmarkdown::render(fileRmd, output_format = format,
                    params = list(traits = traits,
                                  trt = trt,
                                  trt.lab = trt.lab,
                                  eu = eu,
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
