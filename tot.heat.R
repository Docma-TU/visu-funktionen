#' Plotting Topics over Time relative to Corpus
#' 
#' Creates a pdf including a heat map. For each topic, the heat map shows the deviation of 
#' its current share from its mean share. Shares can be calculated on subcorpus or corpus level.
#' Shares can be calculated in absolute deviation from the mean or relative to the mean of the topic to account for different topic strengths.
#' 
#' @param topics Numeric vector containing the numbers of the topics to be plotted. Defaults to all topics.
#' @param x LDA result object.
#' @param ldaID Character vector containing IDs of the texts.
#' @param meta Optional. Specify to analyze subcorpus. The meta data for the texts. One of meta or corpus has to be specified.
#' @param corpus Optional. Specify to analyze entire copous. The data used for normalization. One of meta or corpus has to be specified.
#' @param norm Logical. Should the values be normalized by the mean topic share to account for differently sized topics? Defaults to FALSE.
#' @param file Character vector containing the path and name for the pdf output file.
#' @param Tnames Character vector with labels for the topics.
#' @param date_breaks Which years should be shown on the x axis. Can be one of "1 year","5 years" or "10 years".
#' @return A pdf.
#' @author Kira Schacht (<kira.schacht@@tu-dortmund.de>)
#' @keywords ~kwd1 ~kwd2
#' @examples ##
#' @export tot.heat

tot.heat <- function(x, topics = 1:nrow(x$document_sums), ldaID, meta = NULL, corpus = NULL,
                     norm = FALSE, file, Tnames = top.topic.words(x$topics,1), date_breaks = "1 year"){
      #check if arguments are properly specified
      if((is.null(meta) & is.null(corpus))|(!is.null(meta) & !is.null(corpus))){
            stop("Please specify either 'meta' for analysis on subcorpus level or 'corpus' to compare values to entire corpus")
      }
      
      #create data frame. rows: documents, columns: topics
      tmp <- data.frame(t(x$document_sums))
      
      #get dates for all documents to be visualized
      if(!is.null(meta)) tmpdate <- meta$datum[match(ldaID, meta$id)]
      if(!is.null(corpus)) tmpdate <- corpus$meta$datum[match(ldaID, corpus$meta$id)]
      #round to years
      tmpdate <- floor_date(tmpdate, "year")
      
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
      #cell values are now shares in document x of topic y
      
      #filter for topics to be plotted
      tmp <- tmp[, c(1,topics+1)]
      
      #get mean for each topic over entire time: column means
      tmeans <- apply(tmp[2:length(tmp)], 2, mean)
      #calculate absolute distance to mean. normalize distance with mean if specified
      for(i in 1:nrow(tmp)){
            tmp[i,2:length(tmp)] <- tmp[i,2:length(tmp)] - tmeans
            if(norm == TRUE){
                  tmp[i,2:length(tmp)] <- tmp[i,2:length(tmp)] / tmeans
            }
      }

      #set breaks for x axis labels
      if(date_breaks =="1 year")   breaks <- as.character(unique(tmpdate))
      if(date_breaks =="5 years"){
            breaks <- as.character(unique(tmpdate))
            breaks[!grepl("^[0-9]{3}[05]-", unique(tmpdate))] <- ""
      }
      if(date_breaks == "10 years"){
            breaks <- as.character(unique(tmpdate))
            breaks[!grepl("^[0-9]{3}0-", unique(tmpdate))] <- ""
      }

      #plot heat map
      (cat("Plotting..\n"))

      pdf(file, width = 56/(3+(0.1*nrow(x$topics))))
      heatmap.2(t(as.matrix(tmp[-1])), Colv = NA, dendrogram = 'row',
                #enable legend, disable histogram and deviation traces
                trace = 'none', density.info = 'none', key = T,
                #colours
                col=colorRampPalette(c("#0571b0", "#ffffff","#ca0020"))(50),
                #settings for legend
                keysize=1, key.par=list(mar=c(3,0,3,7), bty="n", fg="white"), key.xlab = NA,
                #layout: title, then legend, then dendrogram and heat map
                lmat=rbind(c(0,3,3,3), c(0,5,4,5),c(2,1,1,1)), lhei=c(0.1,0.18,0.72), lwid=c(0.15,0.1,0.65,0.1),
                #separate heat map cells with white space
                rowsep = 1:(ncol(tmp)-1), colsep = 1:nrow(tmp),
                #configure labels for heat map
                labRow = Tnames, labCol = breaks, margins = c(8,12),
                cexRow = 1.2, cexCol = 1.2, srtCol = 45,
                main = ifelse(norm == T, "Normalized Deviation of Topic Shares from Mean Topic Share","Absolute Deviation of Topic Shares from Mean Topic Share"))
      dev.off()
}