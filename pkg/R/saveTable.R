saveTable <-
function (x, file, HTML=FALSE) {
  if(HTML) {
    require(R2HTML)
    target <- HTMLInitFile(getwd(),filename=file)
    HTML("<br>Table X.<br>",file=target)
    HTML(x, file=target)
    HTMLEndFile()
  } else {
    require(rtf)
    if(length(grep(".rtf", file)) == 0 & length(grep(".doc", file)) == 0) {
      file <- paste(file, ".rtf", sep="")
    } else if (length(grep(".docx", file)) > 0) {
      file <- paste(file, ".rtf", sep="")
    }
    output <- file
    rtf <- RTF(output, font.size=12)
    addParagraph(rtf,"Table X.")
    addTable(rtf, x, row.names=TRUE, NA.string="")
    done(rtf)
  }
  
  if(rownames(x)[1] == c("Configural")) {
    #Post-editing
    rtf.code <- readLines(file)
    
    rtf.code <- gsub("paperw12240", "paperw16840", rtf.code)
    rtf.code <- gsub("paperh15840", "paperh11907", rtf.code)
    rtf.code <- gsub("clwWidth424", "clwWidth600", rtf.code)
    rtf.code <- gsub("clwWidth761", "clwWidth1000", rtf.code)
    rtf.code <- gsub("clwWidth536", "clwWidth700", rtf.code)
    rtf.code <- gsub("clwWidth874", "clwWidth1100", rtf.code)
    
    writeLines(rtf.code, file)
      
  }
}
