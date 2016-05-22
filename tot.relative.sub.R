'library(tmT) # Laden des tmT Pakets
setwd("L:\\DoCMA") # Pfad anpassen.
load("LDA-Sozialismus/Sozlda-k10i20b70s24601.Rdata")
load("Spiegel/Spiegel-meta.Rdata")
tot.relative.sub(x = result, ldaID = ldaID, meta = meta,
                 file = "LDA-Sozialismus/tot.pdf", Tnames = letters[1:10], smooth = 0.1)
'
#tot.relative.sub returns a pdf document with topic over time curves
#for each topic, normalizing by the number of words in the subcorpus for each month.

# x: LDA result object
# ldaID: Character vector including IDs of the texts.
# meta: The meta data for the texts.
# file: Name of the pdf file.
# Tnames: Label for the topics
# smooth: How much the output should be smoothed. Set to 0 for no smoothing.

tot.relative.sub <- function(x, ldaID, meta, file, Tnames = top.topic.words(x$topics,1), smooth = 0.05, ...){

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
      install.required(c("reshape2","plyr","ggplot2"))
      
#data frame erstellen, jedes dokument eine zeile
tmp <- data.frame(t(x$document_sums))
names(tmp) <- Tnames

#zeilensummen berechnen (für jedes dokument anzahl wörter)
tsums <- apply(tmp, 1, sum)

#datum aller dokumente, die visualisiert werden sollen:
tmpdate <- meta$datum[match(ldaID, meta$id)]
#auf monate runden:
tmpdate <- round_date(tmpdate, "month")

#zeilensummen und einzelwerte für die monate aufsummieren
tmp <- aggregate(tmp, by = list(date = tmpdate), FUN = sum)
tsums <- aggregate(tsums, by = list(tmpdate), FUN = sum)[,2]


#normieren mit tsums
tmp[,2:length(tmp)] <- apply(tmp[,2:length(tmp)],2,function(x) x/tsums)

#datensatz für ggplot nach tidy data prinzip aufbereiten
tmp <- reshape2::melt(tmp, id = "date", variable.name = "topic", value.name = "docsum")
tmp <- plyr::arrange(tmp, date, topic)

#limits für den plot: auf nächste 5 jahre gerundet
roundyear <- 5*round(year(range(tmpdate))/5)
roundyear <- as.Date(paste0(roundyear, "-01-01"))

#plotten
pdf(file, width = 12)

for(i in levels(tmp$topic)){

p <- ggplot(tmp[tmp$topic == i,], aes(x = date, y = docsum)) + {
      if(smooth == 0) geom_line(colour = "black")
      else stat_smooth(span = 0.05, se = FALSE, size = 0.5, colour = "black")  } +
      scale_x_date(expand = c(0.05, 0), limits = roundyear) +
      theme(panel.background = element_rect(fill = '#e2e8ed', colour = '#e2e8ed'),
            axis.ticks = element_blank(),
            axis.text.x = element_text(angle = -330, hjust = 1)) + {
      if(all(Tnames == top.topic.words(x$topics,1))) ggtitle(paste("Top topic word:", i))
      else ggtitle(i) } +
      xlab('') + ylab('Anteil des Topics am Subcorpus')
print(p)

}
dev.off()

}