###The code below is intended for REPL mode###

source("ee_clust.r")

#Display a list of projects:
# data <- get_data("../SPDataset-PorruFilter", "train")
# unique(data$project)

#Iteratively go through this code for each project from previous list. 

project_name <- "MESOS"
data <- get_data("../SPDataset-PorruFilter", "train", project_name)

#Append title and description into one column:
data$text <- paste(data$title, data$description, sep = " ")
#Carry out clustering. The code is in 'ee_cluster.r'
data$labels <- cluster_h(data,
            FE = "TFIDF",
            verbose = T, project_name = project_name, ev = "MdAE")

#Find statistics per cluster:
validate(data = data, project_name = project_name, type = "test")
# means <- aggregate(list(sp = data$storypoint, effort = data$effort.time, resolution = data$resolution.time, inprogress = data$in.progress.time),
#     list(label = data$label),
#     mean)

# medians <- aggregate(list(sp = data$storypoint, effort = data$effort.time, resolution = data$resolution.time, inprogress = data$in.progress.time),
#     list(label = data$label),
#     median)

# #Now test!
# test <- get_data("../SPDataset-PorruFilter", "test", project_name)
# test$text <- paste(test$title, test$description, sep = " ")

# #calculate dtm for both
# #Note: need to recalculate DTM so vocabulary would include
# #both sets.
# training_text <- as.matrix(data$text)
# testing_text <- as.matrix(test$text)

# train_size <- dim(training_text)[1]
# test_size <- dim(testing_text)[1]

# combined <- rbind(training_text, testing_text)
# stopifnot(dim(combined)[1] == train_size + test_size)

# dtm <- vsm(combined)$dtm

# #To generate word cloud:
# library(wordcloud)
# freq = data.frame(sort(colSums(as.matrix(dtm)), decreasing=TRUE))
# wordcloud(rownames(freq), freq[,1], max.words=50, colors=brewer.pal(1, "Dark2"))

# #Now separate the two dtms:
# dtm.train <- dtm[1:train_size, ]
# dtm.test <- dtm[-(1:train_size),]

# stopifnot(dtm.train$nrow == train_size)
# stopifnot(dtm.test$nrow == test_size)

# #Calculate the distance between the two:
# distance <- skmeans_xdist(dtm.test, dtm.train)

# #Now, for each row, find the index of the issue
# #that has the minimum distance value to the current one:
# closest <- apply(distance, 1, which.min)
# stopifnot(length(closest) == test_size)
# closest.labels <- data$labels[closest]

# #Now, construct a dataframe that, for each issue (row)
# #contains the sp, effort.t, resolution.t, closest.sp, closest.effort.t,
# #closest.resolution.t, cluster.sp, cluster.resolution.t, cluster.effort.t
# results <- data.frame(closest = closest,
#         sp = test$storypoint,
#         effort.t = test$effort.time,
#         resolution.t = test$resolution.time,
#         inprogress = test$in.progress.time,
#         closest.sp = data$storypoint[closest],
#         closest.effort.t = data$effort.time[closest],
#         closest.resolution.t = data$resolution.time[closest],
#         closest.inprogress = data$in.progress.time[closest],
#         mean.cluster.sp = means$sp[closest.labels],
#         mean.cluster.effort.t = means$effort[closest.labels],
#         mean.cluster.resolution.t = means$resolution[closest.labels],
#         mean.cluster.inprogress = means$inprogress[closest.labels],
#         median.cluster.sp = medians$sp[closest.labels],
#         median.cluster.effort.t = medians$effort[closest.labels],
#         median.cluster.resolution.t = medians$resolution[closest.labels],
#         median.cluster.inprogress = medians$inprogress[closest.labels]
#         )


# ae.sp.cluster.median <- abs(results$sp - results$median.cluster.sp)
# summary(ae.sp.cluster.median)

# #calculate absolute error:
# ae.sp.closest <- abs(results$sp - results$closest.sp)
# summary(ae.sp.closest)
# ae.effort.closest <- abs(results$effort.t - results$closest.effort.t)
# summary(ae.effort.closest)
# ae.resolution.t <- abs(results$resolution.t - results$closest.resolution.t)
# summary(ae.resolution.t)
# ae.inprogress <- abs(results$inprogress - results$closest.inprogress)
# summary(ae.inprogress)

# ae.sp.cluster.mean <- abs(results$sp - results$mean.cluster.sp)
# summary(ae.sp.cluster.mean)
# ae.effort.cluster.mean <- abs(results$effort.t - results$mean.cluster.effort.t)
# summary(ae.effort.cluster.mean)
# ae.resolution.cluster.mean <- abs(results$resolution.t - results$mean.cluster.resolution.t)
# summary(ae.resolution.cluster.mean)
# ae.inprogress.cluster.mean <- abs(results$inprogress - results$mean.cluster.inprogress)
# summary(ae.inprogress.cluster.mean)


# ae.effort.cluster.median <- abs(results$effort.t - results$median.cluster.effort.t)
# summary(ae.effort.cluster.median)
# ae.resolution.cluster.median <- abs(results$resolution.t - results$median.cluster.resolution.t)
# summary(ae.resolution.cluster.median)
# ae.inprogress.cluster.median <- abs(results$inprogress - results$median.cluster.inprogress)
# summary(ae.inprogress.cluster.median)