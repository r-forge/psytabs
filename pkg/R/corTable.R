corTable <- function(data, use = "pairwise", method = "pearson", round = 2, significance = NA, sd=FALSE) {
  
  require(psych)
  corr.test.out <- corr.test(data, use = use, method = method) # calculate correlation
  corr.test.out.r <- (data.frame(round(corr.test.out$r, round))) # get correlation coefs
  corr.test.out.p <- (data.frame(round(corr.test.out$p, 3))) # get p-values
  
  pasteStars <- function(x, y) paste(x, y, sep="")
  pastePval <- function(x, y) paste(x, " (", y, ")", sep="")
  
  if("stars" %in% significance && "p-values" %in% significance) {
    star.table <- data.frame(ifelse(corr.test.out.p < .001, "***", ifelse(corr.test.out.p < .01, "** ", ifelse(corr.test.out.p < .05, "* ", "")))) # generate significance stars  
    significance.table <- data.frame(mapply(pastePval, star.table, corr.test.out.p)) 
    cor.table <- mapply(pasteStars, corr.test.out.r, significance.table)
  } else if("stars" %in% significance) {
    significance.table <- data.frame(ifelse(corr.test.out.p < .001, "***", ifelse(corr.test.out.p < .01, "** ", ifelse(corr.test.out.p < .05, "* ", "")))) # generate significance stars  
    cor.table <- mapply(pasteStars, corr.test.out.r, significance.table)
  } else if("p-values" %in% significance){
    cor.table <- mapply(pastePval, corr.test.out.r, corr.test.out.p)
  } else {
    cor.table <- corr.test.out.r
  }
  
  rownames(cor.table) <- colnames(cor.table)
  
  cor.table[upper.tri(cor.table)] <- NA
  cor.table <- as.matrix(cor.table)
  diag(cor.table) <- NA
  
  if(sd) {
    sd2 <- function(x) sd(x, na.rm = TRUE)
    diag(cor.table) <- paste("(", round(sapply(data, sd2), 2), ")", sep="")
  }
  cor.table <- gsub("(0)", "(< 0.001)", cor.table, fixed=T)
  cor.table
}