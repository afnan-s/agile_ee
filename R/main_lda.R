source("ee_clust.r")

# data <- get_data("../SPDataset-PorruFilter", "train")
# test <- get_data("../SPDataset-PorruFilter", "valid")
#Append title and description into one column:
# data$text <- paste(data$title, data$description, sep = " ")
# test$text <- paste(test$title, test$description, sep = " ")
# cluster_h(data,
            # FE = "LDA",
            # verbose = T,
            # test = test)


project_name <- "MESOS"
data <- get_data("../SPDataset-PorruFilter", "train", project_name)
test <- get_data("../SPDataset-PorruFilter", "valid", project_name)
#Append title and description into one column:
data$text <- paste(data$title, data$description, sep = " ")
test$text <- paste(test$title, test$description, sep = " ")
#Carry out clustering. The code is in 'ee_cluster.r'
data$labels <- cluster_h(data,
            FE = "LDA",
            verbose = T, 
            project_name = project_name, 
            ev = "MAE", 
            test = test)

#Find statistics per cluster:
validate(data = data, project_name = project_name, type = "test")
