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
#' @param server logical
#' @param server_dir_name directory name in the server
#' @param server_file_name  file name in the server, without extensions
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
                      server_dir_name = "foo",
                      server_file_name = "foo2"

                      ) {

  format <- paste(match.arg(format), "_document", sep = "")
  dirfiles <- system.file(package = "pepa")

  server<- server

  if(!server){


  fileRmd <- paste(dirfiles, "/rmd/a01d.Rmd", sep = "")
  fileURL <- paste(dirfiles, "/rmd/a01d.html", sep = "")
  fileDOCX <- paste(dirfiles, "/rmd/a01d.docx", sep = "")
  filePDF <- paste(dirfiles, "/rmd/a01d.pdf", sep = "")
  }else{

    dirfiles <-  server_dir_name

    fileRmd <-  paste0(dirfiles, "a01d.Rmd")   #paste(dirfiles, "/rmd/crd.Rmd", sep = "")
    fileRmd_server_name <-  paste0(dirfiles,  server_file_name, ".Rmd") #r.arias

    #fileURL <- paste(dirfiles, "/rmd/crd.html", sep = "")
    fileDOCX <-  paste0(dirfiles, "a01d.docx")
    fileDOCX_server_name <- paste0(dirfiles, server_file_name , ".docx")  #paste(dirfiles, "/rmd/crd.docx", sep = "")
    #filePDF <- paste(dirfiles, "/rmd/crd.pdf", sep = "")

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

  if(server){
    file.copy(fileDOCX, fileDOCX_server_name, overwrite = TRUE)
  }
  if(!server){
    if(format == "html_document") try(browseURL(fileURL))
    if(format == "word_document") try(shell.exec(fileDOCX))
    if(format == "pdf_document")  try(shell.exec(filePDF))
  }
}
