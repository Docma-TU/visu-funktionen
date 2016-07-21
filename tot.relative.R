#' Plotting Topics over Time relative to Corpus
#' 
#' Creates a pdf including a plot for each topic. For each topic the share of
#' words per month would be plotted. Shares can be calculated on subcorpus or corpus level.
#' 
#' @param topics Numbers of the topics to be plotted. Defaults to all topics.
#' @param x LDA result object.
#' @param ldaID Character vector including IDs of the texts.
#' @param meta Specify to analyze subcorpus. The meta data for the texts.
#' @param corpus Specify to analyze entiere corous. The data used for normalization.
#' @param file Name of the pdf file.
#' @param pages Should the topics be plotted on separate pages (true) or on one page (false). Defaults to true.
#' @param Tnames Vector with labels for the topics.
#' @param smooth How much the output should be smoothed. Set to 0 to disable smoothing.
#' @return A pdf.
#' @author Lars Koppers (<koppers@@statistik.tu-dortmund.de>)
#' @keywords ~kwd1 ~kwd2
#' @examples ##
#' @export tot.relative
tot.relative <- function(topics = 1:nrow(x$document_sums), x, ldaID, meta = NULL, corpus = NULL,
                         file, pages=TRUE, Tnames = top.topic.words(x$topics,1), smooth = 0.05){
    #check if arguments are properly specified
    if((is.null(meta) & is.null(corpus))|(!is.null(meta) & !is.null(corpus))){
        stop("Please specify either 'meta' for analysis on subcorpus level or 'corpus' to compare values to entire corpus")
    }
    
    #create data frame. rows: documents, columns: topics
    tmp <- data.frame(t(x$document_sums))
    
    #get dates for all documents to be visualized
    if(!is.null(meta)) tmpdate <- meta$datum[match(ldaID, meta$id)]
    if(!is.null(corpus)) tmpdate <- corpus$meta$datum[match(ldaID, corpus$meta$id)]
    #round to months
    tmpdate <- floor_date(tmpdate, "month")
    
    ### Prepare normalization data ###
    if(!is.null(meta)){
        (cat("Calculate monthly sums in subcorpus for normalization..\n"))
        #calculate row sums: word count for each document
        normsums <- apply(tmp, 1, sum)
        #sum row sums to months
        normsums <- aggregate(normsums, by = list(date = tmpdate), FUN = sum)
    }
    if(!is.null(corpus)){
        (cat("Calculate monthly sums in corpus for normalization..\n"))
        #get dates of every document in the corpus
        normdates <- corpus$meta$datum[match(names(corpus$text), corpus$meta$id)]
        normdates <- floor_date(normdates, "month")
        #count words for every document
        normsums <- sapply(corpus$text, function(x) length(x))
        #sum words to months
        normsums <- aggregate(normsums, by = list(date = normdates), FUN = sum)
        #tidy up
        rm(normdates)
    }
    #sum document-levels values to months
    tmp <- aggregate(tmp, by = list(date = tmpdate), FUN = sum)

    ### Normalize data ###
    normsums <- normsums[match(tmp$date, normsums$date),]
    tmp[,2:length(tmp)] <- apply(tmp[,2:length(tmp)],2,function(y) y/normsums$x)
    
    #convert dataframe to tidy data format for ggplot
    tmp <- cbind(expand.grid(tmp$date, colnames(tmp)[2:length(tmp)]), unlist(tmp[,2:length(tmp)]))
    names(tmp) <- c("date", "topic","docsum")
    tmp <- tmp[grepl(paste0(paste(topics, collapse = "$|"),"$"), tmp$topic),]
    tmp <- tmp[with(tmp, order(date, topic)), ]
    
    #adjust topic names to those given in argument
    levels(tmp$topic)[1:length(levels(tmp$topic)) %in% topics] <- Tnames
    
    #plot limits: round to next 5 years
    roundyear <- 5*round(year(range(tmpdate))/5)
    roundyear <- as.Date(paste0(roundyear, "-01-01"))
    
    #plotting
    (cat("Plotting..\n"))
    if(pages){
        pdf(file, width = 12)
        topicnr <- 0
        for(i in levels(tmp$topic)){
            topicnr <- topicnr + 1
            p <- ggplot(tmp[tmp$topic == i,], aes(x = date, y = docsum)) + {
                if(smooth == 0) geom_line(colour = "black")
                else stat_smooth(span = smooth, se = FALSE, size = 0.5, colour = "black")  } +
                scale_x_date(expand = c(0.05, 0), limits = roundyear) +
                theme(panel.background = element_rect(fill = '#e2e8ed', colour = '#e2e8ed'),
                      axis.ticks = element_blank(),
                      axis.text.x = element_text(angle = -330, hjust = 1)) + {
                          if(all(Tnames == top.topic.words(x$topics,1))) ggtitle(paste("Topic Nr.", topicnr, "/ Top Topic Word:", i))
                          else ggtitle(paste0("Topic Nr. ", topicnr, ": ",i)) } +
                xlab('') + ylab('Share of Topic in Subcorpus')
            print(p)
        }
        dev.off()
    }
    if(!pages){
        pdf(file, width = 12)
        p <- ggplot(tmp, aes(x = date, y = docsum, colour = topic)) + {
            if(smooth == 0) geom_line(colour = "black")
            else stat_smooth(span = smooth, se = FALSE, size = 0.5)  } +
            scale_x_date(expand = c(0.05, 0), limits = roundyear) +
            scale_colour_discrete(name="Topic") +
            theme(panel.background = element_rect(fill = '#e2e8ed', colour = '#e2e8ed'),
                  axis.ticks = element_blank(),
                  axis.text.x = element_text(angle = -330, hjust = 1)) +
            ggtitle(paste("Share of Topics:", paste0(paste(topics, Tnames, sep = ": "), collapse = ", "), "in Subcorpus")) +
            xlab('') + ylab('Share in Subcorpus')
        print(p)
        dev.off()
    }
}
