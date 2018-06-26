#' Automatic report for a 2-factor factorial
#'
#' Produces an automatic report for selected traits in a 2-factor factorial with
#' a CRD or RCBD.
#' @param traits The traits to analize.
#' @param A Factor A.
#' @param B Factor B.
#' @param rep The replications or blocks.
#' @param design The statistical design, \code{crd} or \code{rcbd}.
#' @param data The name of the data frame containing the data.
#' @param maxp Maximum allowed proportion of missing values to estimate, default is 10\%.
#' @param title The title.
#' @param subtitle The subtitle.
#' @param author Author.
#' @param format The output file format for the report, \code{"html"} by default.
#' Other options are \code{"word"} and \code{"pdf"}.
#' @param server logical
#' @param server_dir_name directory name in the server
#' @param server_file_name  file name in the server, without extensions
#' @author Raul Eyzaguirre.
#' @details It fits a linear model for a 2-factor factorial with a CRD or RCBD for
#' the selected trait. If data is unbalanced, missing values are estimated up to an
#' specified maximum proportion, 10\% by default. Both factors are considered as fixed.
#' @return It returns an automatic report about the 2-factor factorial with a CRD or
#' RCBD fitted model.
#' @examples
#' repo.2f(c("asc.dw", "asc.fw"), "geno", "treat", "rep", "crd", asc)
#' @import agricolae
#' @importFrom utils browseURL
#' @export

repo.2f <- function(traits, A, B, rep, design, data, maxp = 0.1,
                     title = "Automatic report for a 2-factor factorial",
                     subtitle = NULL,
                     author = "International Potato Center",
                     format = c("html", "word", "pdf"),
                     server = FALSE,
                     server_dir_name = "foo",
                     server_file_name = "foo2"
                    ) {

  format <- paste(match.arg(format), "_document", sep = "")
  dirfiles <- system.file(package = "pepa")
  server<- server


  if (design == "crd") {

    if(!server){
      fileRmd <- paste(dirfiles, "/rmd/2fcrd.Rmd", sep = "")
      fileURL <- paste(dirfiles, "/rmd/2fcrd.html", sep = "")
      fileDOCX <- paste(dirfiles, "/rmd/2fcrd.docx", sep = "")
      filePDF <- paste(dirfiles, "/rmd/2fcrd.pdf", sep = "")
    } else {

      dirfiles <-  server_dir_name
      fileRmd <-  paste0(dirfiles, "2fcrd.Rmd")
      fileRmd_server_name <-  paste0(dirfiles,  server_file_name, ".Rmd") #r.arias
      fileDOCX <-  paste0(dirfiles, "2fcrd.docx")
      fileDOCX_server_name <- paste0(dirfiles, server_file_name , ".docx")
    }
}


  if (design == "rcbd") {

    if(!server){
      fileRmd <- paste(dirfiles, "/rmd/2frcbd.Rmd", sep = "")
      fileURL <- paste(dirfiles, "/rmd/2frcbd.html", sep = "")
      fileDOCX <- paste(dirfiles, "/rmd/2frcbd.docx", sep = "")
      filePDF <- paste(dirfiles, "/rmd/2frcbd.pdf", sep = "")
    } else {
      dirfiles <-  server_dir_name
      fileRmd <-  paste0(dirfiles, "2frcbd.Rmd")
      fileRmd_server_name <-  paste0(dirfiles,  server_file_name, ".Rmd") #r.arias
      fileDOCX <-  paste0(dirfiles, "2frcbd.docx")
      fileDOCX_server_name <- paste0(dirfiles, server_file_name , ".docx")
    }
  }

  rmarkdown::render(fileRmd, output_format = format,
                    params = list(traits = traits,
                                  A = A,
                                  B = B,
                                  rep = rep,
                                  design = design,
                                  data = data,
                                  maxp = maxp,
                                  title = title,
                                  subtitle = subtitle,
                                  author = author))

  if(server){
    file.copy(fileDOCX, fileDOCX_server_name, overwrite = TRUE)
  }

  if(!server){
    if(format == "html_document") try(browseURL(fileURL))
    if(format == "word_document") try(system(paste("open", fileDOCX)))
    if(format == "pdf_document")  try(system(paste("open", filePDF)))
  }
}
