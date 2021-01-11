source("ee_clust.r")

## Project-Agnostic

#We have lda model and distance already:

load("../saved_data2/lda_2002.rda")             #lda_model



# ## Project-Agnostic:
# load("../saved_data2/distance_lda_2002.rda")    #distance
# data <- get_data("../SPDataset-PorruFilter", "train")
# test <- get_data("../SPDataset-PorruFilter", "valid")
# #Append title and description into one column:
# data$text <- paste(data$title, data$description, sep = " ")
# test$text <- paste(test$title, test$description, sep = " ")
# data$labels <- cluster_h(data,
#                         FE = "LDA",
#                         verbose = T,
#                         test = test,
#                         lda_model = lda_model,
#                         distance = distance,
#                         ev = "sil")

# #Find statistics per cluster:
# results <- validate(data = data, type = "test", lda_model = lda_model, FE = "LDA")$results




# Project-Specific:
# Get a list of project names:
projects <- get_project_names("../SPDataset-PorruFilter")

# Loop through all projects:
for (project_name in projects) {
    cat(paste("Now working on Project ", project_name, "...\n", sep=""))
    data <- get_data("../SPDataset-PorruFilter", "train", project_name)
    cat("Train Corpus Dimensions: ", dim(data), "\n")
    test <- get_data("../SPDataset-PorruFilter", "valid", project_name)
    cat("Validation Corpus Dimensions: ", dim(test), "\n")
    #Append title and description into one column:
    data$text <- paste(data$title, data$description, sep = " ")
    test$text <- paste(test$title, test$description, sep = " ")
    #Carry out clustering. The code is in 'ee_cluster.r'
    data$labels <- cluster_h(data,
                FE = "LDA",
                verbose = T, 
                project_name = project_name, 
                ev = "sil", 
                test = test, 
                lda_model = lda_model)

    #Find statistics per cluster:
    results <- validate(data = data, project_name = project_name, FE = "LDA", type = "test", lda_model = lda_model)$results



    cat(paste("\n############################# PROJECT", project_name, "#############################\n", sep=" "))

    ae.sp.closest <- abs(results$sp - results$closest.sp)
    cat("\nStory Point - Absolute Error when matching with closest point:\n")
    cat(summary(ae.sp.closest))

    ae.effort.closest <- abs(results$effort.t - results$closest.effort.t)
    cat("\nEffort Time - Absolute Error when matching with closest point:\n")
    cat(summary(ae.effort.closest))

    ae.resolution.t <- abs(results$resolution.t - results$closest.resolution.t)
    cat("\nResolution Time - Absolute Error when matching with closest point:\n")
    cat(summary(ae.resolution.t))

    ae.inprogress <- abs(results$inprogress - results$closest.inprogress)
    cat("\nIn Progress Time - Absolute Error when matching with closest point:\n")
    cat(summary(ae.inprogress))

    ae.sp.cluster.mean <- abs(results$sp - results$mean.cluster.sp)
    cat("\nStory Point - Absolute Error when matching with cluster mean:\n")
    cat(summary(ae.sp.cluster.mean))

    ae.effort.cluster.mean <- abs(results$effort.t - results$mean.cluster.effort.t)
    cat("\nEffort Time - Absolute Error when matching with cluster mean:\n")
    cat(summary(ae.effort.cluster.mean))

    ae.resolution.cluster.mean <- abs(results$resolution.t - results$mean.cluster.resolution.t)
    cat("\nResolution Time - Absolute Error when matching with cluster mean:\n")
    cat(summary(ae.resolution.cluster.mean))

    ae.inprogress.cluster.mean <- abs(results$inprogress - results$mean.cluster.inprogress)
    cat("\nIn Progress Time - Absolute Error when matching with cluster mean:\n")
    cat(summary(ae.inprogress.cluster.mean))

    ae.sp.cluster.median <- abs(results$sp - results$median.cluster.sp)
    cat("\nStory Point - Absolute Error when matching with cluster median:\n")
    cat(summary(ae.sp.cluster.median))

    ae.effort.cluster.median <- abs(results$effort.t - results$median.cluster.effort.t)
    cat("\nEffort Time - Absolute Error when matching with cluster median:\n")
    cat(summary(ae.effort.cluster.median))

    ae.resolution.cluster.median <- abs(results$resolution.t - results$median.cluster.resolution.t)
    cat("\nResolution Time - Absolute Error when matching with cluster median:\n")
    cat(summary(ae.resolution.cluster.median))

    ae.inprogress.cluster.median <- abs(results$inprogress - results$median.cluster.inprogress)
    cat("\nIn Progress Time - Absolute Error when matching with cluster median:\n")
    cat(summary(ae.inprogress.cluster.median))

    cat("\n########################################################################\n")

} #End loop through projects.

