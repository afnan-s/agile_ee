############ INCLUDES##############
library(stringr)
library(RTextTools)
library(topicmodels)
library(SnowballC)
library(stringr)
library(tm)
library(cluster)
library(skmeans)
library(lsa)
# library(wordcloud2)
# library(slam)
##################################
extendedstopwords <- c("a", "aaaaa", "aaaaaa", "aaaaaaa", "aaaaaaaa", "aaaaaaaaaa", "about", "above", "across", "after", "again", "against", "all", "almost", "alone", "along", "already", "also", "although", "always", "am", "among", "an", "and", "another", "any", "anybody", "anyone", "anything", "anywhere", "are",  "aren't", "around", "as", "ask", "asked", "asking", "asks", "at", "away", "b", "back", "be", "became", "because", "become", "becomes", "been", "before", "began", "behind", "being", "beings", "below", "best", "better", "between", "big", "both", "but", "by", "c", "came", "can", "cannot", "can't", "case", "cases", "certain", "certainly", "clear", "clearly", "come", "could", "couldn't", "d", "did", "didn't", "differ", "different", "differently", "do", "does", "doesn't", "doing", "done", "don't", "down", "downed", "downing", "downs", "during", "e", "each", "early", "either", "end", "ended", "ending", "ends", "enough", "even", "evenly", "ever", "every", "everybody", "everyone", "everything", "everywhere", "f", "face", "faces", "fact", "facts", "far", "felt", "few", "find", "finds", "first", "for", "four", "from", "full", "fully", "further", "furthered", "furthering", "furthers", "g", "gave", "general", "generally", "get", "gets", "give", "given", "gives", "go", "going", "good", "goods", "got", "great", "greater", "greatest", "group", "grouped", "grouping", "groups", "h", "had", "hadn't", "has", "hasn't", "have", "haven't", "having", "he", "he'd", "he'll", "her", "here", "here's", "hers", "herself", "he's", "high", "higher", "highest", "him", "himself", "his", "how", "however", "how's", "i", "i'd", "if", "i'll", "i'm", "important", "in", "interest", "interested", "interesting", "interests", "into", "is", "isn't", "it", "its", "it's", "itself", "i've", "j", "just", "k", "keep", "keeps", "kind", "knew", "know", "known", "knows", "l", "large", "largely", "last", "later", "latest", "least", "less", "let", "lets", "let's", "like", "likely", "long", "longer", "longest", "m", "made", "make", "making", "man", "many", "may", "me", "member", "members", "men", "might", "more", "most", "mostly", "mr", "mrs", "much", "must", "mustn't", "my", "myself", "n", "necessary", "need", "needed", "needing", "needs", "never", "new", "newer", "newest", "next", "no", "nobody", "non", "noone", "nor", "not", "nothing", "now", "nowhere", "number", "numbers", "o", "of", "off", "often", "old", "older", "oldest", "on", "once", "one", "only", "open", "opened", "opening", "opens", "or", "order", "ordered", "ordering", "orders", "other", "others", "ought", "our", "ours", "ourselves", "out", "over", "own", "p", "part", "parted", "parting", "parts", "per", "perhaps", "place", "places", "point", "pointed", "pointing", "possible", "q", "quite", "r", "rather", "really", "right",  "s", "said", "same", "saw", "say", "says", "see", "seem", "seemed", "seeming", "seems", "sees",  "shall", "shan't", "she", "she'd", "she'll", "she's", "should", "shouldn't",  "since", "small",  "so", "some", "somebody", "someone", "something", "somewhere", "state", "states", "still", "such", "sure", "t", "take", "taken", "than", "that", "that's", "the", "their", "theirs", "them", "themselves", "then", "there", "therefore", "there's", "these", "they", "they'd", "they'll", "they're", "they've", "thing", "things", "think", "thinks", "this", "those", "though", "thought", "thoughts", "three", "through", "thus", "to", "today", "together", "too", "took", "toward",  "two", "u", "under", "until", "up", "upon", "us", "use", "used", "uses", "v", "very","via", "w", "want", "wanted", "wanting", "wants", "was", "wasn't", "way", "ways", "we", "we'd", "well", "we'll",  "went", "were", "we're", "weren't", "we've", "what", "what's", "when", "when's", "where", "where's", "whether", "which", "while", "who", "whole", "whom", "who's", "whose", "why", "why's", "will", "with", "within", "without", "won't", "work", "worked", "working", "works", "would", "wouldn't", "x", "y", "yes", "yet", "you", "you'd", "you'll", "your", "you're", "yours", "yourself", "yourselves", "you've", "z")
extendedstopwords <- c(extendedstopwords, gsub("'", "", grep("'", extendedstopwords, value = T)))
#Specify where to save all generated data (intermediate)
data.prefix <- "../saved_data3/"

# Get all data in the passed folder
# All data returned as DF
# A column showing the project name is column 'project'
# path: path to a folder containing data
# type: type of data (train, valid, test)
# returns: a dataframe containing the data
get_data <- function(path, type, project = NULL) {
    oldwd <- getwd()
    setwd(path)
    for (file in list.files(path)) {
        project_name <- substr(
            file,
            0,
            unlist(gregexpr("-", file)) - 1
        )
        data_type <- substr(
            file,
            unlist(gregexpr("-", file)) + 1,
            unlist(gregexpr(".csv", file)) - 1
        )

        # Check if current file is of the required type (train, valid, test)
        if (data_type != type) next
        # Check if current file belongs to the required project,
        # otherwise, skip. 
        if ( ! is.null(project) && project_name != project) next

        temp <- read.csv(file, header = TRUE, stringsAsFactors = FALSE)
        temp <- cbind(temp, project = project_name)
        if (!exists("dataset")) {
            dataset <- temp
        }
        else {
            dataset <- rbind(dataset, temp)
        }
        rm(temp)
    }
    setwd(oldwd)
    return(dataset)
}

# Given a path to a folder, retrieve a list of unique project names in dataset:
get_project_names <- function(path){
    oldwd <- getwd()
    setwd(path)
    dataset <- list()
    for (file in list.files(path)) {
        project_name <- substr(
            file,
            0,
            unlist(gregexpr("-", file)) - 1
        )

        dataset <- rbind(dataset, project_name)
    }
    setwd(oldwd)
    return(unique(dataset))
}

# function to cluster the data
# Data: array or matrix of issue with all their columns (needed for validation)
# Distance: Distance matrix (as.dist())
# Method: Hierarchical clustering agglomeration method
# can be: "ward.D", "ward.D2", "single", "complete", "average",
# "mcquitty", "median" or "centroid".
# ev: The evaluation method based on which to select best k. Can be either 'sil'
# for silhouette or 'MAE' based on minimum mean absolute error, or 'MdAE'. 
# when selecting 'MAE' or 'MdAE', a validation set must be passed. 
# Returns:
# object:  The text list with added column containing assigned cluster number
#          The cluster silhouettes
cluster_h <- function(data, test, valid, dtm, FE = "TFIDF", distance = NULL, verbose = F, 
                    method = "ward.D2", ev = "sil", project_name = NULL, lda_model = NULL)
{
    if(is.null(project_name))
        project_name <- "All_projects"


    dataset_size <- dim(dtm$train)[1]
    vocabulary_size <- dim(dtm$train)[2]
    
    if (verbose) {
        cat("Corpus Dimensions: ", dim(dtm$train), "\n")
    }
    #If distance matrix was not passed, calculate distance (using cosine)
    #This is a slow step, if distance to be calculated more than once,
    #calculate it beforehand and pass it. Or if you need other than cosine. 
    if (is.null(distance)) {
        if (verbose)
            cat("Calculating distance matrix..", "\n")
        start.time <- Sys.time()
        distance <- as.dist(skmeans_xdist(dtm$train))
        # cat(summary(distance), "\n")
        end.time <- Sys.time()
        time.taken <- end.time - start.time
        file_name <- paste(data.prefix, project_name, "_distance_", FE, ".rda", sep = "")
        save(distance, file = file_name)
        cat("Distance matrix saved to ", file_name, "\n")
        if (verbose == T)
            cat("Time taken to calculate distance matrix: ", time.taken, "\n")
    }

    #Hierarchical Clustering:
    start.time <- Sys.time()
    dendrogram <- hclust(distance, method = method)
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    time.taken
    if (verbose == T)
        cat("Time taken to cluster: ", time.taken, "\n")
    file_name <- paste(data.prefix, project_name, "_dendrogram_", FE, ".rda", sep = "")
    save(dendrogram, file = file_name)
    cat("Dendrogram saved to ", file_name, "\n")
    # Loop through dendrogram, finding the k that produceds the maximum silhouette
    eval_gran <- data.frame()
    #Define the sequence by which the loop iterates (no need to test at every point)
    step <- trunc(dataset_size * 0.1)
    ks <- seq(3, dataset_size-step, by = step)
    #zooming-in based on the results of the first initial looping:
    #ks <- seq(7000, 8000, by = 50)
    #ks <- seq(7200, 7300, by = 5)
    if (verbose)
        cat("Looping through dendrogram to plot silhouette scores..", "\n")
    for (i in ks) {
        # Cut the tree at level i:
        current <- cutree(dendrogram, k = i)

        # Calculate the evaluative measure:
        sil <- summary(silhouette(current, distance))$avg.width
        data$labels <- current
        #Call validate (but on validation set not test set)
        evals <- validate(data = data, test = valid, dtm.train = dtm$train, dtm.test = dtm$valid)$mae_mdae

        # Combine cluster number and cost together, write to df
        eval_gran <- rbind(eval_gran, cbind(i, sil, evals[1], evals[2]))
        if (verbose)
            cat("\rDone loop : ", i)
    }
    names(eval_gran) <- c("granularity", "silhouette", "MAE", "MdAE")
    file_name <- paste(data.prefix, project_name,"_gran_", FE, ".rda", sep = "")
    save(eval_gran, file = file_name)
    cat("\nGranularity evaluation table is saved to ", file_name, "\n")
    # Plot:
    file_name <- paste(data.prefix, project_name, "_gran_plot_", FE, ".pdf", sep = "")
    pdf(file_name)
    matplot(eval_gran$granularity, 
            cbind(eval_gran$silhouette, eval_gran$MAE, eval_gran$MdAE), 
            type = c("b"),
            pch=1,
            col = 1:4,
            xlab = "Number of Clusters",
            ylab = "Evaluation Metrics: Silhouette, MAE and MdAE",
            main = paste("Cluster Quality for ", project_name)) #plot
    legend("right", legend = c("Silhouette", "MAE", "MdAE"), col=1:4, pch=1) 
    dev.off()
    cat("Plot successfully generated to", file_name, "\n")

    if (verbose) {
        if(ev == "sil"){
            k <- eval_gran[which.max(eval_gran$silhouette), ]
            cat("\nBest K is ", k$granularity, " Producing ", ev, " of ", k$silhouette, "\n")
        }
        else if (ev == "MAE") {
           k <- eval_gran[which.min(eval_gran$MAE), ]
           cat("\nBest K is ", k$granularity, " Producing ", ev, " of ", k$MAE, "\n")
        }
        else if (ev == "MdAE"){
            k <- eval_gran[which.min(eval_gran$MdAE), ]
            cat("\nBest K is ", k$granularity, " Producing ", ev, " of ", k$MdAE, "\n")
        }
        
        
    }

    return(cutree(dendrogram, k = k$granularity))

}

#Function that builds the vector space, using the specified weighting
vsm <- function(data, weighting = weightTf, verbose = T) {
    data <- apply(data, 1, purify)

    #TODO: Preprocessing for code

    dtm.control <- list(
        tolower = T,
        removePunctuation = T,
        removeNumbers = T,
        stopwords = c(stopwords("english"), extendedstopwords),
        stemming = T,
        wordLengths = c(3, Inf),
        weighting = weighting
    )
    corp <- Corpus(VectorSource(as.vector(data)))
    dtm <- DocumentTermMatrix(corp, control = dtm.control)



    #removeSparseTerms(dtm, 0.95)

    # freq = data.frame(sort(colSums(as.matrix(dtm)), decreasing=TRUE))
    # wordcloud(rownames(freq), freq[,1], max.words=100, colors=brewer.pal(1, "Dark2"))

    
    if (verbose)
    cat("Corpus Dimensions, before cleaning: ", dim(dtm), "\n")
    # exclude terms that occur less than 2 times
    # idx <- colSums(dtmm) > 1
    # dtm <- dtm[, idx]

    # # throw out any empty documents
    # idx <- rowSums(dtmm) > 0
    # dtm <- dtm[idx, ]
    
    #Select Vocabulary based on TFIDF. 
    #Source: https://cran.r-project.org/web/packages/topicmodels/vignettes/topicmodels.pdf

    # ##To Save WordCloud 
    # library(webshot)
    # library(htmlwidgets)
    # freq <- data.frame(sort(colSums(as.matrix(dtm)), decreasing=TRUE))
    # input <- data.frame(rownames(freq), freq[,1])
    # hw <- wordcloud2(data=input, size=1.5, color='random-dark')
    # saveWidget(hw,"1.html",selfcontained = F)
    # webshot::webshot("1.html","1.png",vwidth = 1992, vheight = 1744, delay =3)

    term_tfidf <- tapply(dtm$v/row_sums(dtm)[dtm$i], dtm$j, mean) * log2(nDocs(dtm)/col_sums(dtm > 0))
    if(verbose){
        cat("TfIdf statistics:\n")
        print(summary(term_tfidf))
        cat("\n")
    }
    
    dtm <- dtm[,term_tfidf >= 0.15]
    dtm <- dtm[row_sums(dtm) > 0,]
    # removeSparseTerms(dtm, 0.95)

    dtmm <- as.matrix(dtm)




    if (verbose)
    cat("Corpus Dimensions, after cleaning: ", dim(dtm), "\n")

    termFreqs <- colSums(dtmm)
    stopifnot(!any(termFreqs == 0))
    docLens <- rowSums(dtmm)
    stopifnot(!any(docLens == 0))
    return(list(data = data, dtm = dtm, dtmm = dtmm,
                term_freq = termFreqs, doc_lengths = docLens))

}

#Function that builds a vector space out of LDA topics
# K is number of topics (if known), leave null to calculate the K that produces 
# the least perplexity.
# if K is null, need to send validation data as well.
lda <- function(data, valid = NULL, k = NULL) {
    cat("Generating LDA Model..\n")
    data <- vsm(data)
    valid <- vsm(valid)
    if (is.null(k)) {
        k_res <- find_best_k(data$dtm, valid$dtm)
        cat("Best K: ", k_res[1,1], " prodcuced perplexity: ", k_res[1,2], "\n")
        k <- k_res[1,1]
    }
    start.time <- Sys.time()
    lda_model <- LDA(data$dtm, k, method = "Gibbs",
                    control = list(alpha = 1 / k,
                    delta = 0.1,
                    burnin = 50, iter = 500, keep = 50, #####TUNE#####
                    verbose = 100))
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    time.taken
    cat("Time taken to generate final LDA Model: ", time.taken, "\n")
    file_name <- paste(data.prefix, "lda_", k, ".rda", sep = "")
    save(lda_model, file = file_name)
    cat("Final LDA model saved to ", file_name, "\n")
    p <- perplexity(lda_model, valid$dtm)
    cat("The perplexity of this model is ", p, "\n")


    #Extract the topics:
    #d_g <- topics(lda_model, 4, 0.1)
    

    #Now build the final DTM for clustering:
    # dtm <- create_matrix(d_g,
    #                  stemWords = FALSE,
    #                  removeStopwords = FALSE,
    #                  minWordLength = 1,
    #                  removePunctuation = TRUE,
    #                  weighting = tm::weightTfIdf
    #                  )
    # dtmm <- as.matrix(dtm)
    # cat("Corpus Dimensions, before cleaning: ", dim(dtm), "\n")
    # # exclude topics that occur less than 2 times
    # idx <- colSums(dtmm) > 2
    # dtm <- dtm[, idx]

    # # throw out any empty documents
    # idx <- rowSums(dtmm) > 0
    # dtm <- dtm[idx, ]
    # cat("Corpus Dimensions, after cleaning: ", dim(dtm), "\n")

    # termFreqs <- colSums(dtmm)
    # stopifnot(!any(termFreqs == 0))
    # docLens <- rowSums(dtmm)
    # stopifnot(!any(docLens == 0))

    # return(list(data = data, dtm = dtm, dtmm = dtmm,
    #             term_freq = termFreqs, doc_lengths = docLens))



    return(lda_model)

}

find_best_k <- function(training, test) {
    start.time <- Sys.time()
    ks <- seq(2, 3000, by = 250) #####TUNE#####
    models <- lapply(ks, function(k) LDA(training, k, method = "Gibbs",
                        control = list(alpha = 1/k, delta = 0.1,
                        burnin = 50, iter = 300, keep = 50, #####TUNE#####
                        verbose = 10)))
    end.time <- Sys.time()
    time.taken <- end.time - start.time
    cat("Time taken to generate LDA models: ", time.taken, "\n")
    # file_name <- paste(data.prefix, "LDA_models.rda", sep = "")
    # save(models, file = file_name)
    # cat("Generated LDA models saved to ", file_name, "\n")
    perps <- sapply(models, perplexity, test)
    pdf(paste(data.prefix,"perplexity_graph.pdf", sep=""))
    plot(ks, perps, xlab = "Number of topics", ylab = "Perplexity")
    dev.off()

    h <- cbind(ks, perps)
    h <- as.data.frame(h)
    k <- h[which.min(h$perps), ]

    return(k)

}


#Function used internally to replace all non-alphanumeric characters.
purify <- function(x) str_replace_all(x, "[^[:alnum:]]", " ")

#Define a class to hold clustering solutions:
setClass(Class = "Solution",
         slots = c(original.data = "data.frame",
         centers = "data.frame",
         dendrogram = "list", #???
         sil_gran = "data.frame", #silhouette at different levels of granularity
         silhouette = "ANY",
         calinski_harabasz = "numeric",
         c_index = "numeric",
         dunn_index = "numeric",
         runtime = "ANY",
         call = "ANY"))


# Data: must have the cluster labels included. 
# Now only validates based on SP. 
validate <- function(data, test, dtm.train, dtm.test){
    #Find statistics per cluster:
    means <- aggregate(list(sp = data$storypoint, effort = data$effort.time, resolution = data$resolution.time, inprogress = data$in.progress.time),
        list(label = data$label),
        mean)

    medians <- aggregate(list(sp = data$storypoint, effort = data$effort.time, resolution = data$resolution.time, inprogress = data$in.progress.time),
        list(label = data$label),
        median)

    #To generate word cloud:
    # library(wordcloud)
    # freq = data.frame(sort(colSums(as.matrix(dtm)), decreasing=TRUE))
    # wordcloud(rownames(freq), freq[,1], max.words=50, colors=brewer.pal(1, "Dark2"))



    #Calculate the distance between the two:
    distance <- skmeans_xdist(dtm.test, dtm.train)

    #Now, for each row, find the index of the issue
    #that has the minimum distance value to the current one:
    closest <- apply(distance, 1, which.min)
    closest.labels <- data$labels[closest]

    #Now, construct a dataframe that, for each issue (row)
    #contains the sp, effort.t, resolution.t, closest.sp, closest.effort.t,
    #closest.resolution.t, cluster.sp, cluster.resolution.t, cluster.effort.t
    results <- data.frame(closest = closest,
            sp = test$storypoint,
            effort.t = test$effort.time,
            resolution.t = test$resolution.time,
            inprogress = test$in.progress.time,
            closest.sp = data$storypoint[closest],
            closest.effort.t = data$effort.time[closest],
            closest.resolution.t = data$resolution.time[closest],
            closest.inprogress = data$in.progress.time[closest],
            mean.cluster.sp = means$sp[closest.labels],
            mean.cluster.effort.t = means$effort[closest.labels],
            mean.cluster.resolution.t = means$resolution[closest.labels],
            mean.cluster.inprogress = means$inprogress[closest.labels],
            median.cluster.sp = medians$sp[closest.labels],
            median.cluster.effort.t = medians$effort[closest.labels],
            median.cluster.resolution.t = medians$resolution[closest.labels],
            median.cluster.inprogress = medians$inprogress[closest.labels]
            )


     ae.sp.cluster.median <- abs(results$sp - results$median.cluster.sp)
     mae <- mean(ae.sp.cluster.median)
     mdae <- median(ae.sp.cluster.median)


    #  ae.sp.cluster.mean <- abs(results$sp - results$mean.cluster.sp)
    #  mae <- mean(ae.sp.cluster.mean)
    #  mdae <- median(ae.sp.cluster.mean)
    
    # return (c(mae, mdae))
    return(list(results = results, mae_mdae = c(mae, mdae)))
}

get_dtm_lda <- function(training_text, validation_text, testing_text, lda_model) {

    dtm.train <- vsm(training_text, verbose = F)$dtmm
    dtm.train <- posterior(lda_model, dtm.train)$topics
    
    dtm.valid <- vsm(validation_text, verbose = F)$dtmm
    dtm.valid <- posterior(lda_model, dtm.valid)$topics
    
    dtm.test <- vsm(testing_text, verbose = F)$dtmm
    dtm.test <- posterior(lda_model, dtm.test)$topics
   
    return(list(train = dtm.train, valid = dtm.valid, test = dtm.test))
}

get_dtm_tfidf <- function(training_text, testing_text) {

    #Note: need to recalculate DTM so vocabulary would include
    #both sets.

    combined <- rbind(training_text, testing_text)
    stopifnot(dim(combined)[1] == train_size + test_size)
    dtm <- vsm(combined, verbose = F)$dtm

    #Now separate the two dtms:
    dtm.train <- dtm[1:train_size, ]
    dtm.test <- dtm[-(1:train_size),]

    stopifnot(dtm.train$nrow == train_size)
    stopifnot(dtm.test$nrow == test_size)
    
    return(list(train = dtm.train, test = dtm.test))
}