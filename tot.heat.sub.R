'library(tmT) # Laden des tmT Pakets
setwd("L:\\DoCMA") # Pfad anpassen.

load("LDA-Sozialismus/Sozlda-k10i20b70s24601.Rdata")
load("Spiegel/Spiegel-meta.Rdata")

tot.heat.sub(x = result, ldaID = ldaID, meta = meta,
             file = "LDA-Sozialismus/tot-heat.pdf",
             Tnames = row.names(mtcars)[1:10],
             date_breaks = "1 year")
'
#tot.heat.sub returns a pdf document with a heat map showing the deviations
#of each topic from its mean share in all topics,
#normalizing by the number of words in the subcorpus for each month.

# x: LDA result object
# ldaID: Character vector including IDs of the texts.
# meta: The meta data for the texts.
# file: Name of the pdf file.
# Tnames: Label for the topics
# date_breaks: Which years should be shown on the x axis. Either "1 year","5 years" or "10 years".

tot.heat.sub <- function(x, ldaID, meta, file, Tnames = top.topic.words(x$topics,1), date_breaks = "1 year", ...){

#pakete laden
      install.required <- function(required.packages) {
            for(x in required.packages) require(x,character.only = T)
      }
install.required(c("reshape2","dplyr","ggplot2","ggdendro","gtable","grid"))

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