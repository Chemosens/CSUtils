#' @title Save a ggplot.
#' @description Save a ggplot to an external file.
#' @param gg Object of class ggplot
#' @param format Format of file: "", "html", eps", "ps", "tex" (pictex), "pdf", "jpeg", "tiff", "png", "bmp", "svg" or "wmf" (windows only)
#' @param fileName File name.
#' @importFrom RCurl base64Encode
#' @importFrom stringr str_replace_all str_replace
#' @importFrom ggplot2 ggsave
#' @importFrom  grDevices png dev.off
#' @importFrom htmltools HTML
#' @return Object of class ggplot or kable
gSavePlot=function(gg, format, fileName) {

  # if (print ==  TRUE) {
  #   if (interactive == TRUE) {
  #     plotly::ggplotly(gg)
  #   } else {
  #     #X11()
  #     print(gg)
  #   }
  # }

  if (is.null(gg)==FALSE & format != "") {
    if (fileName == "") {
      fileName=str_replace_all(cleanString(str_replace(Sys.time(),"CET",""))," ","")
    }

    encodeGraphic <- function(g) {
      png(tf1 <- tempfile(fileext = ".png"))
      print(gg)
     dev.off()
      txt <- base64Encode(readBin(tf1, "raw", file.info(tf1)[1, "size"]), "txt")
      myImage <- HTML(sprintf('<img src="data:image/png;base64,%s">', txt))
    }


    fullFileName=paste(fileName,".",format,sep="")
    if (format != "html") {
      ggplot2::ggsave(fullFileName,gg,device=format)
      return (gg)
    } else {
      hg <- encodeGraphic(gg)
      #forHTML <- list(htmltools::h1("My header"), htmltools::p("Lead-in text about the graph"), hg)
      html <- list(hg)
      htmltools::save_html(html, fullFileName)
      return (html)
    }

  }


}