#' Automatic report for an alpha (0,1) design.
#'
#' Produces an automatic report for selected traits in an experiment with an
#' alpha (0,1) design.
#' @param traits The traits to analize.
#' @param geno The genotypes.
#' @param rep The replications.
#' @param block The blocks.
#' @param k The size of the blocks.
#' @param data The name of the data frame.
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
#' @details It fits a linear model for an alpha (0,1) design and explains the results.
#' An alpha (0,1) design is an incomplete block design that is resolvable.
#' In a resolvable design the incomplete blocks group together in complete replications.
#' @return It returns an explanation about the alpha (0,1) design fitted model.
#' @examples
#' repo.a01d("yield", "geno", "rep", "block", k = 3, a01data)
#' @import st4gi
#' @importFrom utils browseURL
#' @export

repo.a01d <- function(traits, geno, rep, block, k, data,
                      title = "Automatic report for an alpha (0,1) design",
                      subtitle = NULL,
                      author = "International Potato Center",
                      format = c("html", "word", "pdf"),
                      server = FALSE,
                      server_dir_name = "directory",
                      server_file_name = "filename") {

  format <- paste0(match.arg(format), "_document")
  dirfiles <- system.file(package = "pepa")

  if (!server) {

    fileRmd <- paste0(dirfiles, "/rmd/a01d.Rmd")
    fileURL <- paste0(dirfiles, "/rmd/a01d.html")
    fileDOCX <- paste0(dirfiles, "/rmd/a01d.docx")
    filePDF <- paste0(dirfiles, "/rmd/a01d.pdf")

  } else {

    dirfiles <- server_dir_name

    # Only Markdown and Word files

    fileRmd <- paste0(dirfiles, "a01d.Rmd")
    fileRmd_server_name <- paste0(dirfiles, server_file_name, ".Rmd")
    fileDOCX <- paste0(dirfiles, "a01d.docx")
    fileDOCX_server_name <- paste0(dirfiles, server_file_name, ".docx")

  }

  rmarkdown::render(fileRmd, output_format = format,
                    params = list(traits = traits,
                                  geno = geno,
                                  rep = rep,
                                  block = block,
                                  k = k,
                                  data = data,
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
