'library(tmT) # Laden des tmT Pakets
setwd("/media/kira/TOSHIBA EXT/DoCMA") # Pfad anpassen.

load("Spiegel-Art.Rdata")
load("../LDA-Sozialismus/Sozlda-k10i20b70s24601.Rdata")


tots.relative.all(x = result, ldaID = ldaID, corpus = SpiegelArt,
file = "../LDA-Sozialismus/tot_all.pdf",
Tnames = 1:10, smooth = 0.1)'
#tot.relative.all returns a pdf document with topic over time curves
#for each topic, normalizing by the number of words in the entire corpus for each month.

# topics: Zu plottende Themen
# x: LDA result object
# ldaID: Character vector including IDs of the texts.
# corpus: Clear corpus (X-Art) to extract monthly sums and metadata from.
# file: Name of the pdf file.
# Tnames: Label for the topics
# smooth: How much the output should be smoothed. Set to 0 for no smoothing.

tots.relative.all <- function(topics = 1:nrow(x$document_sums), x, ldaID, corpus, file, Tnames = top.topic.words(x$topics,1), smooth = 0.05, ...){
  
  #pakete laden
  install.required <- function(required.packages) {
    'new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
    if(!length(new.packages)){
    opt <- options(show.error.messages=FALSE)
    on.exit(options(opt))
    stop()
    }
    inst <- readline(paste("Do you want to install required packages:", new.packages, "[y|n]: "))
    if(inst == "y") install.packages(new.packages)
    else stop("Required packages not installed")'
    for(x in required.packages) require(x,character.only = T)
  }
  install.required(c("tidyr","dplyr","ggplot2"))
  
  ####normierung vorbereiten####
  (cat("Monatssummen im Gesamtcorpus zum normieren berechnen..\n"))
  #daten jedes artikels in art
  normdates <- corpus$meta$datum[match(names(corpus$text), corpus$meta$id)]
  normdates <- round_date(normdates, "month")
  #worte jedes artikels zählen
  normsums <- sapply(corpus$text, function(x) length(x))
  #worte pro monat aufsummieren
  normsums <- aggregate(normsums, by = list(date = normdates), FUN = sum)
  
  #####Datensatz zum visualisieren vorbereiten####
  (cat("Datensatz zum visualisieren vorbereiten..\n"))
  
  tmp <- data.frame(t(x$document_sums))

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
  #topic names anpassen
  levels(tmp$topic)[1:length(levels(tmp$topic)) %in% topics] <- Tnames
  
  #limits für den plot: auf nächste 5 jahre gerundet
  roundyear <- 5*round(year(range(tmpdate))/5)
  roundyear <- as.Date(paste0(roundyear, "-01-01"))
  
  #plotten
  (cat("Plotten..\n"))
  pdf(file, width = 12)
    p <- ggplot(tmp, aes(x = date, y = docsum, colour = topic)) + {
      if(smooth == 0) geom_line(colour = "black")
      else stat_smooth(span = 0.05, se = FALSE, size = 0.5, colour = "black")  } +
      scale_x_date(expand = c(0.05, 0), limits = roundyear) +
      scale_colour_discrete(name="Topics") +
      theme(panel.background = element_rect(fill = '#e2e8ed', colour = '#e2e8ed'),
            axis.ticks = element_blank(),
            axis.text.x = element_text(angle = -330, hjust = 1)) + 
      ggtitle(paste("Anteile der Topics", paste0(paste(topics, Tnames, sep = ": "), collapse = ", "), "am Subcorpus")) +
      xlab('') + ylab('Anteil des Topics am Gesamtcorpus')
    print(p)
  dev.off()
  
  }