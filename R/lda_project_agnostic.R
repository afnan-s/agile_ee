args <- commandArgs(trailingOnly=TRUE)
if (length(args) != 2) {
  stop("Exactly two arguments must be supplied: path to data directory and path to LDA model (.rda).", call.=FALSE)
}

source("ee_clust.r")

## Project-Agnostic

#We have lda model and distance already:

# load("../saved_data2/lda_2002.rda")             #lda_model
# load("../saved_data2/distance_lda_2002.rda")    #distance
data_path <- args[1]
load(args[2]) #lda_model

#data_path <- "../SPDataset-PorruFilter"

## Project-Agnostic:
    data <- get_data(data_path, "train")
    cat("Train Corpus Dimensions: ", dim(data), "\n")
    valid <- get_data(data_path, "valid")
    cat("Validation Corpus Dimensions: ", dim(valid), "\n")
    test <- get_data(data_path, "test")
    cat("Testing Corpus Dimensions: ", dim(test), "\n")
    #Append title and description into one column:
    data$text <- paste(data$title, data$description, sep = " ")
    valid$text <- paste(valid$title, valid$description, sep = " ")
    test$text <- paste(test$title, test$description, sep = " ")

    cat("Fitting LDA model to training, testing and validation sets..")

    dtm <- get_dtm_lda(as.matrix(data$text), as.matrix(valid$text), as.matrix(test$text), lda_model = lda_model)


    #Carry out clustering. The code is in 'ee_cluster.r'

    data$labels <- cluster_h(data, test, valid, dtm,
                FE = "LDA",
                verbose = T,
                ev = "sil", ###TUNE###
                lda_model = lda_model)

    #Find statistics per cluster:
    results <- validate(data = data, test = test, dtm.train = dtm$train, dtm.test = dtm$test)$results

    ae.sp.closest <- abs(results$sp - results$closest.sp)
    cat("\nStory Point - Absolute Error when matching with closest point:\n")
    cat("\nMean of Absolute Error: ", mean(ae.sp.closest))
    cat("\nMedian of Absolute Error: ", median(ae.sp.closest), "\n")

    ae.effort.closest <- abs(results$effort.t - results$closest.effort.t)
    cat("\nEffort Time - Absolute Error when matching with closest point:\n")
    cat(summary(ae.effort.closest))
    cat("\nMean of Absolute Error: ", mean(ae.effort.closest))
    cat("\nMedian of Absolute Error: ", median(ae.effort.closest), "\n")

    ae.resolution.t <- abs(results$resolution.t - results$closest.resolution.t)
    cat("\nResolution Time - Absolute Error when matching with closest point:\n")
    cat(summary(ae.resolution.t))
    cat("\nMean of Absolute Error: ", mean(ae.resolution.t))
    cat("\nMedian of Absolute Error: ", median(ae.resolution.t), "\n")

    ae.inprogress <- abs(results$inprogress - results$closest.inprogress)
    cat("\nIn Progress Time - Absolute Error when matching with closest point:\n")
    cat(summary(ae.inprogress))
    cat("\nMean of Absolute Error: ", mean(ae.inprogress))
    cat("\nMedian of Absolute Error: ", median(ae.inprogress), "\n")

    ae.sp.cluster.mean <- abs(results$sp - results$mean.cluster.sp)
    cat("\nStory Point - Absolute Error when matching with cluster mean:\n")
    cat(summary(ae.sp.cluster.mean))
    cat("\nMean of Absolute Error: ", mean(ae.sp.cluster.mean))
    cat("\nMedian of Absolute Error: ", median(ae.sp.cluster.mean), "\n")

    ae.effort.cluster.mean <- abs(results$effort.t - results$mean.cluster.effort.t)
    cat("\nEffort Time - Absolute Error when matching with cluster mean:\n")
    cat(summary(ae.effort.cluster.mean))
    cat("\nMean of Absolute Error: ", mean(ae.effort.cluster.mean))
    cat("\nMedian of Absolute Error: ", median(ae.effort.cluster.mean), "\n")

    ae.resolution.cluster.mean <- abs(results$resolution.t - results$mean.cluster.resolution.t)
    cat("\nResolution Time - Absolute Error when matching with cluster mean:\n")
    cat(summary(ae.resolution.cluster.mean))
    cat("\nMean of Absolute Error: ", mean(ae.resolution.cluster.mean))
    cat("\nMedian of Absolute Error: ", median(ae.resolution.cluster.mean), "\n")

    ae.inprogress.cluster.mean <- abs(results$inprogress - results$mean.cluster.inprogress)
    cat("\nIn Progress Time - Absolute Error when matching with cluster mean:\n")
    cat(summary(ae.inprogress.cluster.mean))
    cat("\nMean of Absolute Error: ", mean(ae.inprogress.cluster.mean))
    cat("\nMedian of Absolute Error: ", median(ae.inprogress.cluster.mean), "\n")

    ae.sp.cluster.median <- abs(results$sp - results$median.cluster.sp)
    cat("\nStory Point - Absolute Error when matching with cluster median:\n")
    cat(summary(ae.sp.cluster.median))
    cat("\nMean of Absolute Error: ", mean(ae.sp.cluster.median))
    cat("\nMedian of Absolute Error: ", median(ae.sp.cluster.median), "\n")

    ae.effort.cluster.median <- abs(results$effort.t - results$median.cluster.effort.t)
    cat("\nEffort Time - Absolute Error when matching with cluster median:\n")
    cat(summary(ae.effort.cluster.median))
    cat("\nMean of Absolute Error: ", mean(ae.effort.cluster.median))
    cat("\nMedian of Absolute Error: ", median(ae.effort.cluster.median), "\n")

    ae.resolution.cluster.median <- abs(results$resolution.t - results$median.cluster.resolution.t)
    cat("\nResolution Time - Absolute Error when matching with cluster median:\n")
    cat(summary(ae.resolution.cluster.median))
    cat("\nMean of Absolute Error: ", mean(ae.resolution.cluster.median))
    cat("\nMedian of Absolute Error: ", median(ae.resolution.cluster.median), "\n")

    ae.inprogress.cluster.median <- abs(results$inprogress - results$median.cluster.inprogress)
    cat("\nIn Progress Time - Absolute Error when matching with cluster median:\n")
    cat(summary(ae.inprogress.cluster.median))
    cat("\nMean of Absolute Error: ", mean(ae.inprogress.cluster.median))
    cat("\nMedian of Absolute Error: ", median(ae.inprogress.cluster.median), "\n")

    cat("\n########################################################################\n")

