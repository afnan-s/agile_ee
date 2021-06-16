# args <- commandArgs(trailingOnly=TRUE)
# if (length(args) != 2) {
#   stop("Exactly two arguments must be supplied: path to data directory and path to LDA model (.rda).", call.=FALSE)
# }

load("results/lda_2265.rda")             #lda_model
# load("../saved_data2/distance_lda_2002.rda")    #distance
# data_path <- args[1]
# load(args[2]) #lda_model


data_path <- "/Users/vali/OneDrive - University College London/Thesis/Task 4-Clustering/Project/agile_ee/data/"

source("R/ee_clust.r")

#load("../saved_data2/lda_2002.rda")             #lda_model

# Get a list of project names:
# projects <- get_project_names(data_path)

projects <- c('CLOV')

output <- matrix(NA, nrow = 0, ncol = 25)
  
# Loop through all projects:
for (project_name in projects) {
    cat(paste("\n############################# PROJECT", project_name, "#############################\n", sep=" "))
    cat(paste("Now working on Project ", project_name, "...\n", sep=""))
    data <- get_data(data_path, "train", project_name)
    cat("Train Corpus Dimensions: ", dim(data), "\n")
    valid <- get_data(data_path, "valid", project_name)
    cat("Validation Corpus Dimensions: ", dim(valid), "\n")
    test <- get_data(data_path, "test", project_name)
    cat("Testing Corpus Dimensions: ", dim(test), "\n")
    #Append title and description into one column:
    # data$text <- paste(data$title, data$description, sep = " ")
    # valid$text<- paste(valid$title, valid$description, sep = " ")
    # test$text <- paste(test$title, test$description, sep = " ")

    # preparing the extra features (extra features include binary indicators for Issue Type, Components, and title+description length, and 1-hot matrix of the occurance )
    data.extra <- subset(data, select = -c(issuekey, storypoint, in.progress.time, effort.time, resolution.time, text, project))
    data.rows  <- dim(data.extra)[1]
    data.extra <- matrix(data = unlist(data.extra), nrow = data.rows, byrow = F)
    
    stopifnot(sum(is.na(as.numeric(unlist(data.extra))))==0)

    valid.extra<- subset(valid, select = -c(issuekey, storypoint, in.progress.time, effort.time, resolution.time, text, project))
    valid.rows <- dim(valid.extra)[1]
    valid.extra<- matrix(data = unlist(valid.extra), nrow = valid.rows, byrow = F)
    
    stopifnot(sum(is.na(as.numeric(unlist(valid.extra))))==0)

    test.extra <- subset(test, select = -c(issuekey, storypoint, in.progress.time, effort.time, resolution.time, text, project))
    test.rows  <- dim(test.extra)[1]
    test.extra <- matrix(data = unlist(test.extra), nrow = test.rows, byrow = F)
    
    stopifnot(sum(is.na(as.numeric(unlist(test.extra))))==0)

    # cat("Getting document-term matrix from tf-idf vsm for training, testing and validation sets..\n")
    # dtm_tfidf <- get_dtm_tfidf(as.matrix(data$text), as.matrix(test$text), as.matrix(valid$text))
    cat("Fitting LDA model to training, testing and validation sets..\n")
    dtm_lda   <- get_dtm_lda(as.matrix(data$text), as.matrix(valid$text), as.matrix(test$text), lda_model = lda_model)
    
    dtm = list()
    
    # This is when we use LDA + TFIDF + extra features
    # dtm$train = cbind(dtm_lda$train, dtm_tfidf$train, data.extra)
    # dtm$valid = cbind(dtm_lda$valid, dtm_tfidf$valid, valid.extra)
    # dtm$test = cbind(dtm_lda$test, dtm_tfidf$test, test.extra)
    
    # This is when we use LDA + extra features
    dtm$train = cbind(dtm_lda$train, data.extra)
    dtm$valid = cbind(dtm_lda$valid,valid.extra)
    dtm$test = cbind(dtm_lda$test, test.extra)
    
    stopifnot(dim(dtm$train)[2]==dim(dtm$valid)[2],dim(dtm$valid)[2]==dim(dtm$test)[2]) 
    
    #Carry out clustering. The code is in 'ee_cluster.r'
    data$labels <- cluster_h(data, test, valid, dtm,
                FE = "LDA-Extra",
                verbose = T,
                project_name = project_name,
                ev = "MdAE",
                lda_model = lda_model)

    #Find statistics per cluster:
    results <- validate(data = data, test = test, dtm.train = dtm$train, dtm.test = dtm$test)$results

    ae.sp.closest <- abs(results$sp - results$closest.sp)
    cat("\nStory Point - Absolute Error when matching with closest point:\n")
    cat(summary(ae.sp.closest))
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
    
    output <- rbind(output,
                    c(project_name,
                      mean(ae.sp.closest), median(ae.sp.closest),
                      mean(ae.effort.closest), median(ae.effort.closest),
                      mean(ae.resolution.t), median(ae.resolution.t),
                      mean(ae.inprogress), median(ae.inprogress),
                      
                      mean(ae.sp.cluster.mean), median(ae.sp.cluster.mean),
                      mean(ae.effort.cluster.mean), median(ae.effort.cluster.mean),
                      mean(ae.resolution.cluster.mean), median(ae.resolution.cluster.mean),
                      mean(ae.inprogress.cluster.mean), median(ae.inprogress.cluster.mean),
                      
                      mean(ae.sp.cluster.median), median(ae.sp.cluster.median),
                      mean(ae.effort.cluster.median), median(ae.effort.cluster.median),
                      mean(ae.resolution.cluster.median), median(ae.resolution.cluster.median),
                      mean(ae.inprogress.cluster.median), median(ae.inprogress.cluster.median)
                    ))
    

} #End loop through projects.
colnames(output) <- c("Project",
  "SP-ClosestP-MAE", "SP-ClosestP-MdAE",
  "ET-ClosestP-MAE", "ET-ClosestP-MdAE",
  "RT-ClosestP-MAE", "RT-ClosestP-MdAE",
  "PT-ClosestP-MAE", "PT-ClosestP-MdAE",
  
  "SP-ClustMean-MAE", "SP-ClustMean-MdAE",
  "ET-ClustMean-MAE", "ET-ClustMean-MdAE",
  "RT-ClustMean-MAE", "RT-ClustMean-MdAE",
  "PT-ClustMean-MAE", "PT-ClustMean-MdAE",
  
  "SP-ClustMedian-MAE", "SP-ClustMedian-MdAE",
  "ET-ClustMedian-MAE", "ET-ClustMedian-MdAE",
  "RT-ClustMedian-MAE", "RT-ClustMedian-MdAE",
  "PT-ClustMedian-MAE", "PT-ClustMedian-MdAE"
)

write.csv(output, file = "ClsutringResult.csv", row.names = F)