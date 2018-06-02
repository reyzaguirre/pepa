#' Automatic report for an augmented block design (ABD)
#'
#' Produces an automatic report for selected traits in an experiment with an ABD.
#' @param traits The traits to analize.
#' @param geno The genotypes.
#' @param rep The replications.
#' @param data The name of the data frame.
#' @param title The title.
#' @param subtitle The subtitle.
#' @param author Author.
#' @param format The output file format for the report, \code{"html"} by default.
#' Other options are \code{"word"} and \code{"pdf"}.
#' @param server logical
#' @param server_dir_name directory name in the server
#' @param server_file_name  file name in the server, without extensions
#' @author Raul Eyzaguirre.
#' @details It fits a linear model for an ABD and explains the results.
#' @return It returns an explanation about the ABD fitted model.
#' @examples
#' # A subset that looks like an ABD
#' temp <- pjpz09[c(1, 2, 9, 10, 13, 14, 27, 29, 31, 33, 35, 37, 40, 42, 44, 46, 48, 50, 203, 204), ]
#' repo.abd(c("trw", "vw"), "geno", "rep", temp)
#'
#' # With some missing values
#' temp[c(1, 2, 3), "trw"] <- NA
#' temp[c(1, 10, 15), "vw"] <- NA
#' repo.abd(c("nocr", "trw", "vw"), "geno", "rep", temp)
#' @import st4gi
#' @importFrom utils browseURL
#' @export

repo.abd <- function(traits, geno, rep, data,
                     title = "Automatic report for an Augmented Block Design (ABD)",
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

  if(!server){

  fileRmd <- paste(dirfiles, "/rmd/abd.Rmd", sep = "")
  fileURL <- paste(dirfiles, "/rmd/abd.html", sep = "")
  fileDOCX <- paste(dirfiles, "/rmd/abd.docx", sep = "")
  filePDF <- paste(dirfiles, "/rmd/abd.pdf", sep = "")

  } else{

    dirfiles <-  server_dir_name

    fileRmd <-  paste0(dirfiles, "abd.Rmd")   #paste(dirfiles, "/rmd/crd.Rmd", sep = "")
    fileRmd_server_name <-  paste0(dirfiles,  server_file_name, ".Rmd") #r.arias

    #fileURL <- paste(dirfiles, "/rmd/crd.html", sep = "")
    fileDOCX <-  paste0(dirfiles, "abd.docx")
    fileDOCX_server_name <- paste0(dirfiles, server_file_name , ".docx")  #paste(dirfiles, "/rmd/crd.docx", sep = "")
    #filePDF <- paste(dirfiles, "/rmd/crd.pdf", sep = "")

  }

  rmarkdown::render(fileRmd, output_format = format,
                    params = list(traits = traits,
                                  geno = geno,
                                  rep = rep,
                                  data = data,
                                  title = title,
                                  subtitle = subtitle,
                                  author = author))

  if(server){
    file.copy(fileDOCX, fileDOCX_server_name, overwrite = TRUE)
  }
  if(!server){

    if(format == "html_document") try(browseURL(fileURL))
    if(format == "word_document") try(shell.exec(fileDOCX))
    if(format == "pdf_document")  try(shell.exec(filePDF))
  }
}
