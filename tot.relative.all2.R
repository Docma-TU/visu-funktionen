'library(tmT) # Laden des tmT Pakets
setwd("L:\\DoCMA\\Spiegel") # Pfad anpassen.

load("Spiegel-Art.Rdata")
load("../LDA-Sozialismus/Sozlda-k10i20b70s24601.Rdata")


tot.relative.all(x = result, ldaID = ldaID, corpus = SpiegelArt,
file = "../LDA-Sozialismus/tot_all.pdf",
Tnames = 1:10, smooth = 0.1)'
#tot.relative.all returns a pdf document with topic over time curves
#for each topic, normalizing by the number of words in the entire corpus for each month.

# k:        Topic range to be analyzed. default is all appropriate files in given directory.
#           Files will be analyzed in the order they have in the folder.
# dir:      Directory where the LDA results are stored.
# corpus:   Clear corpus (X-Art) to extract monthly sums and metadata from.
# file:     Name of the pdf file the output should be saved to.
# Tnames:   A list containing labels for the topics. Each list entry corresponds to one LDA result,
#           in the order they have in the folder.
# smooth:   How much the output should be smoothed. Set to 0 for no smoothing.

tot.relative.all <- function(dir, k=unique(as.numeric(sub(".*?k(.*?)i.*", "\\1", f))),
                             corpus, dest, Tnames, smooth = 0.05, ...){
      
      #pakete laden
      install.required <- function(required.packages) {
            for(x in required.packages) require(x,character.only = T)
      }
      install.required(c("reshape2","plyr","ggplot2"))
      
      ####normierung vorbereiten####
      (cat("Monatssummen im Gesamtcorpus zum normieren berechnen..\n"))
      #daten jedes artikels in art
      normdates <- corpus$meta$datum[match(names(corpus$text), corpus$meta$id)]
      normdates <- round_date(normdates, "month")
      #worte jedes artikels zählen
      normsums <- sapply(corpus$text, function(x) length(x))
      #worte pro monat aufsummieren
      normsums <- aggregate(normsums, by = list(date = normdates), FUN = sum)
      
      #LDA-Dateien finden
      f <- list.files(dir)
      f <- f[grep(paste(paste0("k",k,"i"),collapse="|"),f)]
      f <- f[grep(".Rdata",f)]
      
      #Labels schon gegeben?
      autolabel <- missing(Tnames)
      if(autolabel) Tnames <- list(); length(Tnames) <-length(f)
for(n in 1:length(f)){
      (cat("LDA k=",k[n],":\n",sep=""))
      load(paste(dir,f[n],sep="/"))
      #####Datensatz zum visualisieren vorbereiten####
      (cat("Datensatz zum visualisieren vorbereiten..\n"))
      if(autolabel) Tnames[[n]] = top.topic.words(result$topics,1)
      tmp <- data.frame(t(result$document_sums))
      names(tmp) <- Tnames[[n]]
      
      #datum aller dokumente, die visualisiert werden sollen:
      tmpdate <- corpus$meta$datum[match(ldaID, corpus$meta$id)]
      #auf monate runden:
      tmpdate <- round_date(tmpdate, "month")
      #einzelwerte für die monate aufsummieren
      tmp <- aggregate(tmp, by = list(date = tmpdate), FUN = sum)
      
      #normieren mit normsums
      normsums <- normsums[match(tmp$date, normsums$date),]
      tmp[,2:length(tmp)] <- apply(tmp[,2:length(tmp)],2,function(y) y/normsums$x)
      
      #datensatz für ggplot nach tidy data prinzip aufbereiten
      tmp <- tmp %>% gather(topic, docsum, 2:length(tmp), factor_key = TRUE) %>%
        filter(grepl(paste(topics, collapse = "$|"), topic)) %>%
        arrange(date, topic)
      
      #limits für den plot: auf nächste 5 jahre gerundet
      roundyear <- 5*round(year(range(tmpdate))/5)
      roundyear <- as.Date(paste0(roundyear, "-01-01"))
      
      #plotten
      (cat("Plotten..\n"))
      pdf(paste0(dest,"/tot-all-k",k[n],".pdf"), width = 12)
      
      for(i in Tnames[[n]]){
            
            p <- ggplot(tmp[tmp$topic == i,], aes(x = date, y = docsum)) + {
                  if(smooth == 0) geom_line(colour = "black")
                  else stat_smooth(span = 0.05, se = FALSE, size = 0.5, colour = "black")  } +
                  scale_x_date(expand = c(0.05, 0), limits = roundyear) +
                  theme(panel.background = element_rect(fill = '#e2e8ed', colour = '#e2e8ed'),
                        axis.ticks = element_blank(),
                        axis.text.x = element_text(angle = -330, hjust = 1)) + {
                  if(all(Tnames == top.topic.words(x$topics,1))) ggtitle(paste("Topic Nr.", topicnr, "/ Top topic word:", i))
                  else ggtitle(paste0("Topic Nr. ", topicnr, ": ",i)) } +
                  xlab('') + ylab('Anteil des Topics am Gesamtcorpus')
            print(p)
      }
      dev.off()
}
}