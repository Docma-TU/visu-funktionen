#option: absolute differenz normieren mit mittelwert des topicanteils über gesamte zeit

'
install.packages("/media/kira/TOSHIBA EXT/DoCMA/tmT/0.1/tmT_0.1.tar.gz", repos=NULL, dependencies=T)
library(tmT) # Laden des tmT Pakets
setwd("/media/kira/TOSHIBA EXT/DoCMA") # Pfad anpassen.
load("LDA-Sozialismus/Sozlda-k10i20b70s24601.Rdata")
load("Spiegel/Spiegel-meta.Rdata")
library(ggplot2)
tot.heat.sub(topics = c(1:5,8), x = result, ldaID = ldaID, meta = meta,
             file = "LDA-Sozialismus/tot-heat-test.pdf",
             Tnames = row.names(mtcars)[1:10],
             date_breaks = "1 year")
'
#dependencies: ggplot2, ggdendro
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
    
    #dendrogramm berechnen
    (cat("Determine distances between topics..\n"))
    dd <- as.matrix(t(tmp[,2:length(tmp)]))
    dd <- as.dendrogram(hclust(dist(dd)))
    ord <- order.dendrogram(dd)
    ddata <- dendro_data(dd)
    
    #get mean for each topic over entire time
    tmeans <- apply(tmp[2:length(tmp)], 2, mean)
    #calculate absolute distance to mean
    for(i in 1:nrow(tmp)){
        tmp[i,2:length(tmp)] <- tmp[i,2:length(tmp)] - tmeans
    }
    
    #convert dataframe to tidy data format for ggplot
    tmp <- cbind(expand.grid(tmp$date, colnames(tmp)[2:length(tmp)]), unlist(tmp[,2:length(tmp)]))
    names(tmp) <- c("date", "topic","docsum")
    tmp <- tmp[with(tmp, order(date, topic)), ]
    
    #adjust topic names to those given in argument
    levels(tmp$topic)[1:length(levels(tmp$topic)) %in% topics] <- Tnames
    #adjust factor levels to have the same order as the dendrogram
    tmp$topic <- factor(tmp$topic, levels(tmp$topic)[ord])
    
    ### Set up a blank theme
    theme_none <- theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank()
    )
    
    #breaks bestimmen
    if(date_breaks =="1 year")   breaks <- unique(tmpdate)
    if(date_breaks =="5 years")  breaks <- as.Date(paste0(unique(5*round(year(tmpdate)/5)), "-01-01"))
    if(date_breaks =="10 years") breaks <- as.Date(paste0(unique(10*round(year(tmpdate)/10)), "-01-01"))
    
    #plotten
    (cat("Plotten..\n"))
    
    pdf(file, width = 56/(3+(0.1*nrow(x$topics))))
    
    #Tile plot
    p1 <- ggplot(tmp, aes(date, topic)) +
        geom_tile(aes(fill=docsum), colour = "white") +
        scale_fill_gradient2(low = "#0571b0", mid = "white",
                             high = "#ca0020", midpoint = 0) +
        theme_light() +
        labs(x = "", y = "") +
        scale_x_date(expand = c(0, 0), breaks = breaks, date_labels = "%Y") +
        scale_y_discrete(expand = c(0, 0)) +
        guides(fill=guide_colourbar(title=NULL)) +
        theme(legend.text = element_text(size = 100/(4+(0.1*nrow(x$topics)))),
              axis.ticks.y = element_blank(),
              axis.text.x = element_text(size = 65/(4+(0.1*nrow(x$topics))), hjust = 0, angle = 300),
              axis.text.y = element_text(size = 65/(4+(0.1*nrow(x$topics)))),
              plot.margin = unit(c(0,0,0,0),"cm")
        ) +
        coord_fixed(ratio = 400)
    
    # Dendrogram
    if(nrow(x$topics) == 10) topm <- 0
    else topm <- -0.125*log(nrow(x$topics))
    p2 <- ggplot(segment(ddata)) + 
        geom_segment(aes(x=x, y=y, xend=xend, yend=yend)) + 
        theme_none + theme(plot.margin = unit(c(topm,-0.6,topm-0.1,0),"cm")) +
        coord_flip() + scale_y_reverse()
    
    #Plots in einem gtable zusammenfügen
    g2 <- ggplotGrob(p2)
    g <- ggplotGrob(p1) %>%
        gtable_add_cols(unit(3,"cm"), pos = 0) %>%
        gtable_add_grob(g2, t = 2, l=1, b=3, r=1) %>%
        gtable_add_rows(unit(3,"cm"), pos = 0) %>%
        gtable_add_grob(grid.text("Abweichung der Topics von ihrem Durchschnittsanteil", just = c("center", "top"),
                                  gp = gpar(fontsize = 160/(7+(0.1*nrow(x$topics))))),
                        t=1, l=1, r=7)
    grid.draw(g)
    
    dev.off()
    
}