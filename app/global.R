## Load in the Data
thyroid <- read.csv("www/data/thyroid.csv", header = TRUE,
                    stringsAsFactors = FALSE)

validation.tab <- function(x){
  if (!is.null(x)) {
    data.col <- x + 1
    var.name <- names(thyroid)[data.col]
    var.class <- class(thyroid[[data.col]])
    cur.var <- thyroid[,var.name]
    
    unique.values <- c(na.omit(unique(cur.var)))
    table.output <- matrix(0, nrow = length(unique.values), ncol = 3)
    colnames(table.output) <- c("Values", "Count", "%")
    table.output <- as.data.frame(table.output)
    table.output[,1] <- unique.values
    for (i in 1:length(unique.values)) {
      table.output[i,2] <- length(which(cur.var == unique.values[i]))
    }
    table.output[,3] <- round(table.output[,2]*100/nrow(thyroid), digits = 2)
    table.output <- table.output[order(table.output$Values),]
    return(table.output)
  }
}