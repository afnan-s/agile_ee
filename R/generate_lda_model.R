args <- commandArgs(trailingOnly=TRUE)
if (length(args) != 1) {
  stop("Exactly one argument must be supplied: path to data directory.", call.=FALSE)
}

data_path <- args[1]
#data_path <- "../SPDataset-PorruFilter"
source("ee_clust.r")

data <- get_data(data_path, "train")
cat("Train Corpus Dimensions: ", dim(data), "\n")
valid <- get_data(data_path, "valid")
cat("Validation Corpus Dimensions: ", dim(valid), "\n")

data$text <- paste(data$title, data$description, sep = " ")
valid$text <- paste(valid$title, valid$description, sep = " ")

#Generate model:
lda(as.matrix(data$text), as.matrix(valid$text))