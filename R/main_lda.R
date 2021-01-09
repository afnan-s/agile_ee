source("ee_clust.r")

## Project-Agnostic

#We have lda model and distance already:
load("../saved_data2/distance_lda_2002.rda")    #distance
load("../saved_data2/lda_2002.rda")             #lda_model

data <- get_data("../SPDataset-PorruFilter", "train")
test <- get_data("../SPDataset-PorruFilter", "valid")
#Append title and description into one column:
data$text <- paste(data$title, data$description, sep = " ")
test$text <- paste(test$title, test$description, sep = " ")
data$labels <- cluster_h(data,
                        FE = "LDA",
                        verbose = T,
                        test = test,
                        lda_model = lda_model,
                        distance = distance)

#Find statistics per cluster:
results <- validate(data = data, type = "test", lda_model = lda_model)


ae.sp.closest <- abs(results$sp - results$closest.sp)
cat("Story Point - Absolute Error when matching with closest point:")
summary(ae.sp.closest)

ae.effort.closest <- abs(results$effort.t - results$closest.effort.t)
cat("Effort Time - Absolute Error when matching with closest point:")
summary(ae.effort.closest)

ae.resolution.t <- abs(results$resolution.t - results$closest.resolution.t)
cat("Resolution Time - Absolute Error when matching with closest point:")
summary(ae.resolution.t)

ae.inprogress <- abs(results$inprogress - results$closest.inprogress)
cat("In Progress Time - Absolute Error when matching with closest point:")
summary(ae.inprogress)

ae.sp.cluster.mean <- abs(results$sp - results$mean.cluster.sp)
cat("Story Point - Absolute Error when matching with cluster mean:")
summary(ae.sp.cluster.mean)

ae.effort.cluster.mean <- abs(results$effort.t - results$mean.cluster.effort.t)
cat("Effort Time - Absolute Error when matching with cluster mean:")
summary(ae.effort.cluster.mean)

ae.resolution.cluster.mean <- abs(results$resolution.t - results$mean.cluster.resolution.t)
cat("Resolution Time - Absolute Error when matching with cluster mean:")
summary(ae.resolution.cluster.mean)

ae.inprogress.cluster.mean <- abs(results$inprogress - results$mean.cluster.inprogress)
cat("In Progress Time - Absolute Error when matching with cluster mean:")
summary(ae.inprogress.cluster.mean)

ae.sp.cluster.median <- abs(results$sp - results$median.cluster.sp)
cat("Story Point - Absolute Error when matching with cluster median:")
summary(ae.sp.cluster.median)

ae.effort.cluster.median <- abs(results$effort.t - results$median.cluster.effort.t)
cat("Effort Time - Absolute Error when matching with cluster median:")
summary(ae.effort.cluster.median)

ae.resolution.cluster.median <- abs(results$resolution.t - results$median.cluster.resolution.t)
cat("Resolution Time - Absolute Error when matching with cluster median:")
summary(ae.resolution.cluster.median)

ae.inprogress.cluster.median <- abs(results$inprogress - results$median.cluster.inprogress)
cat("In Progress Time - Absolute Error when matching with cluster median:")
summary(ae.inprogress.cluster.median)




## Project-Specific:
# project_name <- "MESOS"
# data <- get_data("../SPDataset-PorruFilter", "train", project_name)
# test <- get_data("../SPDataset-PorruFilter", "valid", project_name)
# #Append title and description into one column:
# data$text <- paste(data$title, data$description, sep = " ")
# test$text <- paste(test$title, test$description, sep = " ")
# #Carry out clustering. The code is in 'ee_cluster.r'
# data$labels <- cluster_h(data,
#             FE = "LDA",
#             verbose = T, 
#             project_name = project_name, 
#             ev = "MAE", 
#             test = test)

#Find statistics per cluster:
#validate(data = data, project_name = project_name, type = "test")
