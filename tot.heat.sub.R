library(tmT) # Laden des tmT Pakets
setwd("L:\\DoCMA") # Pfad anpassen.

load("LDA-Sozialismus/Sozlda-k10i20b70s24601.Rdata")
load("Spiegel/Spiegel-meta.Rdata")

tot.heat.sub(x = result, ldaID = ldaID, meta = meta,
             file = "LDA-Sozialismus/tot-heat.pdf",
             Tnames = row.names(mtcars)[1:10])

#tot.heat.sub returns a pdf document with a heat map showing the deviations
#of each topic from its mean share in all topics,
#normalizing by the number of words in the subcorpus for each month.

# x: LDA result object
# ldaID: Character vector including IDs of the texts.
# meta: The meta data for the texts.
# file: Name of the pdf file.
# Tnames: Label for the topics
# smooth: How much the output should be smoothed. Set to 0 for no smoothing.

tot.heat.sub <- function(x, ldaID, meta, file, Tnames = 1:10, ...){

#pakete laden
require("reshape2")
require("dplyr")
require("ggplot2")
require("ggdendro")
require("gtable")
require("grid")

#data frame erstellen, jedes dokument eine zeile
tmp <- data.frame(t(x$document_sums))
names(tmp) <- Tnames

#zeilensummen berechnen (für jedes dokument anzahl wörter)
tsums <- apply(tmp, 1, sum)

#datum aller dokumente, die visualisiert werden sollen, auf monate runden:
tmpdate <- meta$datum[match(ldaID, meta$id)] %>%
      round_date("year")

(cat("Normieren, Distanz der Themen bestimmen..\n"))
#zeilensummen und einzelwerte für die monate aufsummieren
tmp <- aggregate(tmp, by = list(date = tmpdate), FUN = sum)
tsums <- aggregate(tsums, by = list(tmpdate), FUN = sum)[,2]

#normieren mit tsums
tmp[,2:length(tmp)] <- apply(tmp[,2:length(tmp)],2,function(x) x/tsums)

#dendrogramm berechnen
dd <- t(tmp[,2:length(tmp)]) %>% as.matrix() %>%
      dist() %>% hclust() %>% as.dendrogram()
ord <- order.dendrogram(dd)
ddata <- dendro_data(dd)

#mean für jedes topic berechnen
tmeans <- apply(tmp[2:length(tmp)], 2, mean)

#differenz zum mean ausrechnen
for(i in 1:nrow(tmp)){
      tmp[i,2:length(tmp)] <- tmp[i,2:length(tmp)] - tmeans
}

(cat("Datensatz zum visualisieren vorbereiten..\n"))
#datensatz für ggplot nach tidy data prinzip aufbereiten
tmp <- reshape2::melt(tmp, id = "date", variable.name = "topic", value.name = "docsum")
tmp <- plyr::arrange(tmp, date, topic)
#factor levels anpassen,
#sodass sie die gleiche reihenfolge haben wie im dendrogramm
tmp$topic <- factor(tmp$topic, levels(tmp$topic)[ord])

### Set up a blank theme
theme_none <- theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.title.x = element_text(colour=NA),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.line = element_blank(),
      axis.ticks = element_blank()
)

#plotten
(cat("Plotten..\n"))
pdf(file, width = 14)

#Tile plot
p1 <- ggplot(tmp, aes(date, topic)) +
      geom_tile(aes(fill=docsum), colour = "white") +
      scale_fill_gradient2(low = "#0571b0", mid = "white",
                           high = "#ca0020", midpoint = 0) +
      theme_light(base_size = 20) +
      labs(x = "", y = "") +
      scale_x_date(expand = c(0, 0), date_breaks = "1 year", date_labels = "%Y") +
      scale_y_discrete(expand = c(0, 0)) +
      guides(fill=guide_colourbar(title=NULL)) +
      theme(axis.ticks = element_blank(),
            axis.text.x = element_text(size = 16,
                                       angle = 300, hjust = 0),
            axis.text.y = element_text(size = 16)
      ) +
      coord_fixed(ratio = 400)

# Dendrogram
p2 <- ggplot(segment(ddata)) + 
      geom_segment(aes(x=x, y=y, xend=xend, yend=yend)) + 
      theme_none + theme(axis.title.x=element_blank(),
                         plot.margin = unit(c(-0.1,-1,-0.1,0),"cm")) +
      coord_flip() + scale_y_reverse()

#Plots in einem gtable zusammenfügen
g2 <- ggplotGrob(p2)
g <- ggplotGrob(p1) %>%
      gtable_add_cols(unit(3,"cm"), pos = 0) %>%
      gtable_add_grob(g2, t = 2, l=1, b=3, r=1)
grid.draw(g)

#Überschrift
grid.text("Abweichung der Topics von ihrem Durchschnittsanteil", x=0.5,y=0.9,
          just = c("center", "top"), gp = gpar(fontsize = 20))

dev.off()

}