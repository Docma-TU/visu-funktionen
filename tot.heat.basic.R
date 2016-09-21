#option: absolute differenz normieren mit mittelwert des topicanteils über gesamte zeit
'
install.packages("/media/kira/TOSHIBA EXT/DoCMA/tmT/0.1/tmT_0.1.tar.gz", repos=NULL, dependencies=T)
library(tmT) # Laden des tmT Pakets
setwd("/media/kira/TOSHIBA EXT/DoCMA") # Pfad anpassen.
load("LDA-Sozialismus/Sozlda-k10i20b70s24601.Rdata")
load("Spiegel/Spiegel-meta.Rdata")
tot.heat.sub(topics = c(1:5,8), x = result, ldaID = ldaID, meta = meta,
file = "LDA-Sozialismus/tot-heat-test.pdf",
Tnames = row.names(mtcars)[1:10],
date_breaks = "1 year")
'
#dependencies: gplots
#' Plotting Topics over Time relative to Corpus
#' 
#' Creates a pdf including a heat map. For each topic, the heat map shows the deviation of 
#' its current share from its mean share. Shares can be calculated on subcorpus or corpus level.
#' Shares can be calculated in absolute deviation from the mean or relative to the mean of the topic to account for different topic strengths.
#' 
#' @param topics Numbers of the topics to be plotted. Defaults to all topics.
#' @param x LDA result object.
#' @param ldaID Character vector including IDs of the texts.
#' @param meta Specify to analyze subcorpus. The meta data for the texts.
#' @param corpus Specify to analyze entiere corous. The data used for normalization.
#' @param file Name of the pdf file.
#' @param pages Should the topics be plotted on separate pages (true) or on one page (false). Defaults to true.
#' @param Tnames Vector with labels for the topics.
#' @param date_breaks Which years should be shown on the x axis. Can be one of "1 year","5 years" or "10 years".
#' @return A pdf.
#' @author Lars Koppers (<koppers@@statistik.tu-dortmund.de>)
#' @keywords ~kwd1 ~kwd2
#' @examples ##
#' @export tot.heat
tot.heat <- function(topics = 1:nrow(x$document_sums), x, ldaID, meta = NULL, corpus = NULL,
                     file, Tnames = top.topic.words(x$topics,1), date_breaks = "1 year"){
      
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
      
      #filter for topics to be plotted
      tmp <- tmp[,c(TRUE,grepl(paste0(paste(topics, collapse = "$|"),"$"), colnames(tmp)[2:length(tmp)]))]
      
      '
      #dendrogramm berechnen
      (cat("Determine distances between topics..\n"))
      dd <- as.matrix(t(tmp[,2:length(tmp)]))
      dd <- as.dendrogram(hclust(dist(dd)))
      ord <- order.dendrogram(dd)
      ddata <- dendro_data(dd)
      '
      #get mean for each topic over entire time
      tmeans <- apply(tmp[2:length(tmp)], 2, mean)
      #calculate absolute distance to mean
      for(i in 1:nrow(tmp)){
            tmp[i,2:length(tmp)] <- tmp[i,2:length(tmp)] - tmeans
      }
      
      #breaks bestimmen
      if(date_breaks =="1 year")   breaks <- as.character(unique(tmpdate))
      if(date_breaks =="5 years"){
            breaks <- as.character(unique(tmpdate))
            breaks[!grepl("^[0-9]{3}[05]-", unique(tmpdate))] <- ""
      }
      if(date_breaks == "10 years"){
            breaks <- as.character(unique(tmpdate))
            breaks[!grepl("^[0-9]{3}0-", unique(tmpdate))] <- ""
      }

      #plotten
      (cat("Plotten..\n"))
      ?rainbow
      pdf("xy.pdf", width = 56/(3+(0.1*nrow(x$topics))))
      heatmap.2(t(as.matrix(tmp[-1])), Colv = NA, dendrogram = 'row',
                #legende an, histogramm und abweichungslinien aus
                trace = 'none', density.info = 'none', key = T,
                #farbe
                col=colorRampPalette(c("#0571b0", "#ffffff","#ca0020"))(50),
                #einstellungen für legende
                keysize=1, key.par=list(mar=c(3,0,3,7), bty="n", fg="white"), key.title = NA, key.xlab = NA,
                #layout: titel, dann legende, dann dendro und heatmap
                lmat=rbind(c(0,3,3,3), c(0,5,4,5),c(2,1,1,1)), lhei=c(0.1,0.18,0.72), lwid=c(0.15,0.1,0.65,0.1),
                #zellen der heatmap durch white space trennen
                rowsep = 1:(ncol(tmp)-1), colsep = 1:nrow(tmp),
                #labels der heat map konfigurieren
                labRow = Tnames, labCol = breaks, margins = c(8,12),
                cexRow = 1.2, cexCol = 1.2, srtCol = 45,
                main = "Abweichung der Topics von ihrem Durchschnittsanteil")
      dev.off()


      
      pdf(file, width = 56/(3+(0.1*nrow(x$topics))))
      dev.off()
      
}