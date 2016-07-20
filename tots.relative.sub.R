'
install.packages("/media/kira/TOSHIBA EXT/DoCMA/tmT/0.1/tmT_0.1.tar.gz", repos=NULL)
library(tmT) # Laden des tmT Pakets
setwd("/media/kira/TOSHIBA EXT/DoCMA") # Pfad anpassen.
load("LDA-Sozialismus/Sozlda-k10i20b70s24601.Rdata")
load("Spiegel/Spiegel-meta.Rdata")
tots.relative.sub(x = result, ldaID = ldaID, meta = meta,
file = "LDA-Sozialismus/tot2.pdf", smooth = 0.1)
'
#tot.relative.sub returns a pdf document with topic over time curves
#for each topic, normalizing by the number of words in the subcorpus for each month.

# topics: Zu plottende Themen
# x:      LDA result object
# ldaID:  Character vector including IDs of the texts.
# meta:   The meta data for the texts.
# file:   Name of the pdf file.
# Tnames: Label for the topics
# smooth: How much the output should be smoothed. Set to 0 for no smoothing.

#dependencies: ggplot2, magrittr
library("magrittr")

tots.relative.sub <- function(topics = 1:nrow(x$document_sums), x, ldaID, meta, file, Tnames = top.topic.words(x$topics,1), smooth = 0.05, ...){
      #create data frame. rows: documents, columns: topics
      tmp <- data.frame(t(x$document_sums))
      
      #calculate row sums: word count for each document
      tsums <- apply(tmp, 1, sum)
      #get dates for all documents to be visualized
      tmpdate <- meta$datum[match(ldaID, meta$id)]
      #round to months
      tmpdate <- floor_date(tmpdate, "month")
      
      #sum row sums and document-levels values to months
      tmp <- aggregate(tmp, by = list(date = tmpdate), FUN = sum)
      tsums <- aggregate(tsums, by = list(tmpdate), FUN = sum)[,2]
      
      #normalize by tsums (word counts for each month)
      tmp[,2:length(tmp)] <- apply(tmp[,2:length(tmp)],2,function(x) x/tsums)
      
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
      pdf(file, width = 12)
      p <- ggplot(tmp, aes(x = date, y = docsum, colour = topic)) + {
        if(smooth == 0) geom_line(colour = "black")
        else stat_smooth(span = 0.05, se = FALSE, size = 0.5)  } +
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
