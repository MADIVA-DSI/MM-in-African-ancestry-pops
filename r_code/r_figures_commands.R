#!/usr/bin/env R

library(tidyverse)
library(tidyr)

if(!require(rworldmap)){
    install.packages("rworldmap", dependencies=TRUE)
    library(rworldmap)
}

if(!require(ggplot2)){
    install.packages("ggplot2", dependencies=TRUE)
    library(ggplot2)
}

if(!require(dplyr)){
    install.packages("dplyr", dependencies=TRUE)
    library(dplyr)
}

if(!require(gridExtra)){
    install.packages("gridExtra", dependencies=TRUE)
    library(gridExtra)
}

if(!require(grid)){
    install.packages("grid", dependencies=TRUE)
    library(grid)
}

if(!require(lattice)){
    install.packages("lattice", dependencies=TRUE)
    library(lattice)
}

if(!require(UpSetR)){
    install.packages("UpSetR",dependencies=TRUE)
    library(UpSetR)
}

if(!require(pacman)){
    install.packages("pacman",dependencies=TRUE)
    library(pacman)
}

if(!require(RColorBrewer)){
    install.packages("RColorBrewer",dependencies=TRUE)
    library(RColorBrewer)
}

if(!require(ggrepel)){
    install.packages("ggrepel",dependencies=TRUE)
    library(ggrepel)
}

if(!require(ggplotify)){
    install.packages("ggplotify",dependencies=TRUE)
    library(ggplotify)
}

if(!require(ggpubr)){
    install.packages("ggpubr",dependencies=TRUE)
    library(ggpubr)
}

## if(!require(ComplexUpset)){
##     install.packages("ComplexUpset")
##     library(ComplexUpset)
## }


## ====== DATA CLEANING AND PREPROCESSING 
setwd('/Users/phele/Documents/projects/madiva/review')
data <- read.csv('/Users/phele/Documents/projects/madiva/review/the_data.tsv', sep="|", header=TRUE)
data <- data[-which(data$Authors == "Ake et al."),]

publications <- data.frame(data[,c("Authors", "Year","Geographic.region","Location","African.population","Sample.size")])
colnames(publications) <- c("Authors", "Year","Region","Location","Population","Sample.Size")

publications$Year <- factor(publications$Year)
publications$Region <- factor(publications$Region)
publications$Location <- factor(publications$Location)
publications$Population <- factor(publications$Population, levels=c('Both', 'Diaspora', 'Continental'))

publications$Sample.Size <- str_replace(publications$Sample.Size, " ","")
publications$Sample.Size <- as.numeric(publications$Sample.Size)
head(publications)

## ====== FIGURE 3 - PLOT THE PUBLICATION DATA
pub.summary <- data.frame(table(publications$Population))
colnames(pub.summary) <- c("Population","Publications")
pub.summary <- pub.summary %>% mutate(Percentage = round(Publications/sum(Publications)*100), digits=1)
pub.summary <- pub.summary %>% mutate(Year = "2010-2022")

## GET LEGEND FROM FIGURE
get_legend<-function(plot){
    tmp <- ggplot_gtable(ggplot_build(plot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
}
## FIGURE 3A
figure.3a <- ggplot(data=pub.summary, aes(x=Year, y=Publications, fill=Population)) +
    geom_col(color='white', width=0.5) +
    geom_text(aes(label = paste0(Percentage, "%")), position = position_stack(vjust = 0.5), size=2.5, color="white") + ## 1.75 > 2.5
    scale_fill_manual(breaks = c("Diaspora", "Continental", "Both"), values=c("#0000A7","#C1272D", "#008176")) +
    scale_y_continuous(limits=c(0,250),
                       expand=c(0,2.5),
                       breaks=seq(0,250,by=5),
                       labels = c(0, rep("",4), 25, rep("",4), 50, rep("",4), 75, rep("",4), 100, rep("",4), 125, rep("",4), 
                                  150, rep("",4), 175, rep("",4), 200, rep("",4), 225, rep("",4), 250)) +
    xlab("Year") +
    ylab("Publications") +
    ggtitle("A") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title = element_text(size=10),
          axis.text = element_text(size=8),
          plot.title = element_text(size=8, face = "bold"),
          plot.margin = unit(c(0.5, 0.25, 0.25, 0.25), "cm"),
          legend.key = element_blank(),
          legend.position = "bottom",
          legend.box = "vertical",
          legend.justification = c("center", "center"),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 8,face = 'bold'),
          legend.key.size = unit(0.5, "cm"),
          legend.margin = margin(t = 0, unit='cm'))
## FIGURE 3B
figure.3b <- ggplot(data=publications, aes(x=Year, fill=Population)) +
    geom_bar(position='stack', aes(y=after_stat(count)), width=0.5,color='white') + ## position=position_dodge(.7)) +
    scale_fill_manual(breaks = c("Diaspora", "Continental", "Both"), values=c("#0000A7","#C1272D", "#008176")) +
    geom_text(stat='count', aes(label=after_stat(count)), position = position_stack(vjust = 0.5), size=2.5, color="white") + ## 1.75 > 2.5
    ## geom_text(stat='count', aes(label=after_stat(count), group=Year), vjust= -0.5, size=1.75, color="black") +
    scale_y_continuous(limits=c(0,35),
                       expand=c(0,0.5),
                       breaks=seq(0,35,by=1),
                       labels = c(0, rep("",4), 5, rep("",4), 10, rep("",4), 15, rep("",4), 20, rep("",4), 25, rep("",4), 30, rep("",4), 35)) +
    xlab("Year") +
    ylab("Publications") +
    ggtitle("B") +
    theme_bw() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title = element_text(size=10),
          axis.text = element_text(size=8),
          plot.title = element_text(size=8, face = 'bold'),
          plot.margin = unit(c(0.5, 0.25, 0.25, 0.25), "cm"),
          legend.key = element_blank(),
          legend.position = "bottom",
          legend.box = "vertical",
          legend.justification = c("center", "center"),
          legend.text = element_text(size = 6),
          legend.title = element_text(size = 7,face = 'bold'),
          legend.key.size = unit(0.25, "cm"),
          legend.margin = margin(t = 0, unit='cm'))
## PLOT BOTH FIGURES IN A SINGLE PDF
pdf('figure_3.pdf', width=8, height=5, onefile=FALSE)
lay <- rbind(c(1,2),3)
mylegend <- get_legend(figure.3a)
grid.arrange(figure.3a + theme(legend.position="none"),
             figure.3b + theme(legend.position="none"),
             layout_matrix = lay,
             widths = c(1,5),
             heights = c(1,0.05),
             mylegend
             )
dev.off()

## ===== FIGURE 4 - PLOT THE WORLD MAP SHOWING PUBLICATIONS
by.country <- as.data.frame(table(publications[which(!(publications$Location == "Multiple" & !(publications$Region == "Africa" | publications$Region == "Africa, West"))),]$Location))
colnames(by.country) <- c("Country","Publications")
by.country$Population <- publications$Population[match(by.country$Country, publications$Location)]
## by.country$Location <- publications$Location[match(by.country$Country, publications$Location)]
by.country$Region <- publications$Region[match(by.country$Country, publications$Location)]
## by.country[-13,] <- as.data.frame(factor("Africa"),as.integer(5),factor("Continental"),factor("Africa"),factor("Africa"))

by.country$Country <- as.character(by.country$Country)
by.country$Population <- as.character(by.country$Population)
by.country$Country[by.country$Country == "Multiple"] <- "Africa"
by.country$Country[by.country$Country == "USA"] <- "United States of America"
by.country$Country[by.country$Country == "UK"] <- "United Kingdom"
by.country$Country[by.country$Country == "Tanzania"] <- "United Republic of Tanzania"
by.country$Population[by.country$Population == "Both"] <- "Continental"
by.country$Country <- factor(by.country$Country)
by.country$Population <- factor(by.country$Population)
by.country
str(by.country)

by.country$GEO3_loc <- countryRegions$GEO3[match(by.country$Country, countryRegions$ADMIN)]
by.country$GEO3_loc[by.country$Country %in% c("Malawi", "Zambia", "Zimbabwe", "United Republic of Tanzania")] <- "Eastern Africa"
by.country$GEO3_loc[by.country$Country == "United States of America"] <- "North America"
by.country$GEO3_loc[by.country$GEO3_loc == "North Africa"] <- "Northern Africa"
by.country$GEO3_loc <- factor(by.country$GEO3_loc)

by.country$ISO3_code <- countryRegions$ISO3[match(by.country$Country, countryRegions$ADMIN)]
by.country$ISO3_code <- factor(by.country$ISO3_code)
by.country
str(by.country)

## countryRegions[countryRegions$ADMIN %in% by.country$Country,]
## countryRegions$[match(by.country$Country, countryRegions$ADMIN)]
## colnames(countryRegions)
## colnames(countryExData)
## countryRegions[,1:2]
## countryExData[,1:2]
## map_regions <- countryExData[,1:4]
## map_regions <- map_regions[map_regions$Country %in% by.country$Country,]
## by.country$GEO_subregion <- map_regions$GEO_subregion[match(by.country$Country, map_regions$Country)]
## by.country$ISO3V10 <- map_regions$ISO3V10[match(by.country$Country, map_regions$Country)]
## country2Region(by.country, nameDataColumn="Publications", nameJoinColumn="ISO3V10", regionType="GBD", joinCode="ISO3", FUN="sum")

## FIGURE 4A - WORLD MAP
## country2Region(by.country
##               ,nameDataColumn="Publications"
##               ,joinCode="ISO3"
##               ,nameJoinColumn="ISO3V10"
##               ,regionType="GEO3"
##               ,FUN="sum")
## mapByRegion(by.country
##            ,nameDataColumn="Publications"
##            ,joinCode="ISO3"
##            ,nameJoinColumn="ISO3V10"pub.map <- joinCountryData2Map(by.country[-13,], joinCode = "ISO3", nameJoinColumn = "ISO3")
##            ,regionType="GEO3"
##            ,FUN="sum")
## dev.off()

### === BUBBLED COUNTRIES

region_cols <- brewer.pal(n = 8, name = "Dark2")
## draw_map <- function(by.country) {
##     function() {

mapDevice(device = "pdf", width = 8, height = 4, file = "figure_4a.pdf", rows = 1, columns=1, mai = c(0.25,0.25,0.25,0.25))
pub.map <- joinCountryData2Map(by.country[-13,], joinCode = "ISO3", nameJoinColumn = "ISO3_code")
mapParams <- mapBubbles(pub.map,
                        nameZSize="Publications",
                        nameZColour="GEO3_loc",
                        colourPalette=region_cols,
                        catMethod="categorical",
                        numCats=8,
                        ylim=c(-40,90),
                        aspect="variable",
                        addLegend = FALSE,
                        legendBg = "white",
                        legendTitle = "Publications",
                        legendHoriz = TRUE,
                        addColourLegend = FALSE,
                        symbolSize = 1,
                        plotZeroVals = FALSE,
                        lwd = 0.25,
                        lwdSymbols = 0.5,
                        borderCol="black")
## do.call(addMapLegendBoxes, c(mapParams))
## }
## }
## draw_map(by.country)
dev.off()

## ### === COLOURED COUNTRIES
## mapDevice(device = "pdf", width = 8, height = 4, file = "figure_4a.pdf",
##           rows = 1,
##           columns=1,
##           mai = c(0.25,0,0,0))
## ## draw_map <- function(by.country) {
## ##     function() {
##         pub.map <- joinCountryData2Map(by.country, joinCode = "NAME", nameJoinColumn = "Country")
##         mapParams <- mapCountryData(pub.map,
##                                     nameColumnToPlot="Publications",
##                                     catMethod = "pretty",
##                                     numCats=20,
##                                     missingCountryCol = gray(0.9),
##                                     mapTitle='',
##                                     ylim=c(-60,90),
##                                     aspect="variable", 
##                                     colourPalette=c("#008176", "#0000A7","#C1272D"),
##                                     addLegend='FALSE',
##                                     borderCol="black",
##                                     lwd=0.5)
##         do.call(addMapLegend, c(mapParams,
##                                 legendLabels="all",
##                                 labelFontSize=0.5,
##                                 legendWidth=0.5,
##                                 legendShrink=0.2375,
##                                 tcl=-0.25,
##                                 legendIntervals="data",
##                                 legendMar=1))
##         grid.text("0                 20                40               60                80             100", x = 0.5, y = 0.025, gp=gpar(fontsize=5))
## ##     }
## ## }
## ## draw_map(by.country)
## dev.off()

## FIGURE 4B - PIE CHARTS
continental <- by.country[(by.country$Population == "Continental"),]
continental <- continental %>% group_by(GEO3_loc) %>% summarise(Publications = sum(Publications))
continental <- continental %>% mutate(Percentage = round(Publications/sum(Publications)*100, digits=0))
continental <- as.data.frame(continental)
continental$GEO3_loc <- as.character(continental$GEO3_loc)
continental$GEO3_loc <- continental$GEO3_loc %>% replace_na("Africa - Multi Region")
continental$GEO3_loc <- factor(continental$GEO3_loc)
continental

diaspora <- by.country[(by.country$Population == "Diaspora"),]
diaspora <- diaspora %>% group_by(GEO3_loc) %>% summarise(Publications = sum(Publications))
diaspora <- diaspora %>% mutate(Percentage = round(Publications/sum(Publications)*100, digits=0))
diaspora <- as.data.frame(diaspora)
diaspora

all <- by.country %>% group_by(GEO3_loc) %>% summarise(Publications = sum(Publications))
all <- all %>% mutate(Percentage = round(Publications/sum(Publications)*100, digits=2))
all <- as.data.frame(all)
all$GEO3_loc <- as.character(all$GEO3_loc)
all$GEO3_loc <- all$GEO3_loc %>% replace_na("Africa - Multi Region")
all$GEO3_loc <- factor(all$GEO3_loc)
all

region_cols <- append(region_cols, "#0000A7")
reg <- all$GEO3_loc

##
all <- all %>% mutate(GEO3_loc = fct_reorder(GEO3_loc, desc(Percentage)))
figure.4b.all <- ggplot(all, aes(x="", y=Percentage, fill=GEO3_loc)) +
    geom_bar(stat="identity", width=1, color="white") +
    scale_fill_manual(breaks = reg,
                      values = region_cols) +
    coord_polar("y", start=0) +
    theme_void() +
    theme(legend.key = element_blank(),
          legend.position = "bottom",
          legend.box = "vertical",
          legend.justification = c("center", "center"),
          legend.text = element_text(size = 8),
          legend.title = element_text(size = 8,face = 'bold'),
          legend.key.size = unit(0.5, "cm"),
          legend.margin = margin(t = 0, unit='cm'),
          plot.margin = unit(c(0.5, 0.25, 0.25, 0.25), "cm")) +
    labs(fill = "Geographical Region")
##
## pdf("figure_4b_test.pdf", width=8, height=5, onefile=FALSE)
## figure.4b.all
## dev.off()

## 
diaspora <- diaspora %>% mutate(GEO3_loc = fct_reorder(GEO3_loc, desc(Percentage))) #%>% mutate(ypos = cumsum(Percentage) - Percentage/2 )
figure.4b.diaspora <- ggplot(diaspora, aes(x="", y=Percentage, fill=GEO3_loc)) +
    geom_bar(stat="identity", width=1, color="white") +
    scale_fill_manual(breaks = reg,
                      values = region_cols) +
    coord_polar("y", start=0) +
    theme_void() +
    ggtitle("Diaspora") +
    geom_label_repel(aes(label = paste0(GEO3_loc," (",Percentage,"%)")), position = position_stack(vjust = .5), color = "white", size=3.5,face = "bold") +
    theme(plot.title = element_text(size=10, face = "bold",hjust = 0.5,vjust = -10),
          plot.margin = unit(c(-0.5, -0.5, -0.5, -0.5), "cm"))
##
continental <- continental %>% mutate(GEO3_loc = fct_reorder(GEO3_loc, desc(Percentage))) # %>% mutate(ypos = cumsum(Percentage) - Percentage/2 )
figure.4b.continental<- ggplot(continental, aes(x="", y=Percentage, fill=GEO3_loc)) +
    geom_bar(stat="identity", width=1, color="white") +
    scale_fill_manual(breaks = reg,
                      values = region_cols) +
    coord_polar("y", start=0) +
    ggtitle("Continental") +
    theme_void() +
    geom_label_repel(aes(label = paste0(GEO3_loc," (",Percentage,"%)")), position = position_stack(vjust = .5), color = "white", size=3.5,face = "bold") +
    theme(plot.title = element_text(size=10, face = "bold",hjust = 0.5,vjust = -10),
          plot.margin = unit(c(-0.5, -0.5, -0.5, 0.5), "cm"))

##
pdf("figure_4b.pdf", width=8, height=8, onefile=FALSE)
lay <- rbind(c(1,2),3)
mylegend <- get_legend(figure.4b.all)
pies <- grid.arrange(figure.4b.diaspora + theme(legend.position="none"),
                     figure.4b.continental + theme(legend.position="none"),
                     layout_matrix = lay,
                     widths = c(10,11),
                     heights = c(10,1),
                     mylegend
                     )
cowplot::plot_grid(NULL,pies,nrow=2,labels = c("A", "B"), rel_heights = c(0.75,1), rel_widths = c(1,1))
dev.off()

## ========== FIGURE 5 - UPSET PLOTS
data$Disease.clusters.upSet.plot_included <- str_replace(data$Disease.clusters.upSet.plot_included,"Muscoskeletal","Musculoskeletal")
upset.data <- data.frame(data[,c("Authors", "Year","Disease.clusters.upSet.plot_included","African.population")])

## GET UNIQUE DISEASE NAMES
diseases <- c()
for (row in 1:nrow(upset.data)) {
    the.list <- as.vector(strsplit(upset.data[row, "Disease.clusters.upSet.plot_included"], split = " \\+ "))
    diseases <- c(diseases, the.list)
    print(the.list)
}
diseases <- unique(unlist(diseases))                
diseases <- diseases[!is.na(diseases)]
diseases

## CREATE NEW DATA FRAME TO INCLUDE THE DISEASE COLUMNS
disease.data <- data.frame(upset.data$Authors, upset.data$Year, upset.data$African.population)
colnames(disease.data) <- c("Authors", "Year","African.population")
for (item in 1:length(diseases)) {
    disease.data[paste0(diseases[item])] <- NA
}

## FILL IN THE MATCHING
for (row in 1:nrow(upset.data)) {
    the.list <- as.vector(strsplit(upset.data[row, "Disease.clusters.upSet.plot_included"], split = " \\+ "))
    the.list <- unlist(the.list)
    print(the.list)
    for (item in 1:length(the.list)) {
        if (the.list[item] %in% colnames(disease.data[,4:14])) {
            disease.data[row,the.list[item]] <- 1
            print(the.list[item])
        }
    }
}

disease.data[is.na(disease.data)] <- 0
disease.data <- disease.data[ , !names(disease.data) %in% c("unknown","Unclear")]

upset.continental <- disease.data[(disease.data$African.population == "Continental"),]
upset.continental.plot <- upset(upset.continental,
                                nintersects=54,keep.order=T,sets=sort(colnames(upset.continental[,4:12])),
                                mainbar.y.label="Intersection Size of Diseases Combinations",
                                sets.x.label="Diseases", nsets=10, empty.intersections=TRUE,
                                order.by="freq", sets.bar.color="#008176",
                                matrix.dot.alpha=0.5, mb.ratio=c(0.7, 0.3), text.scale=1.2,
                                set_size.scale_max=100, mainbar.y.max=18, main.bar.color="#0000A7")
#upset.continental.plot <- upset.continental.plot + grid.text("Continental", x = 0.65, y = 0.95, gp=gpar(fontsize=9, fontface = "bold"))
upset.diaspora <- disease.data[(disease.data$African.population == "Diaspora"),]
upset.diaspora.plot <- upset(upset.diaspora,
                             nintersects=49,keep.order=T,sets=sort(colnames(upset.diaspora[,4:12])),
                             mainbar.y.label="Intersection Size of Diseases Combinations",
                             sets.x.label="Diseases", nsets=10, empty.intersections=FALSE,
                             order.by="freq", sets.bar.color="#008176",
                             matrix.dot.alpha=0.5, mb.ratio=c(0.7, 0.3), text.scale=1.2,
                             set_size.scale_max=100, mainbar.y.max=18, main.bar.color="#0000A7")
pdf("figure_5.pdf", width=10, height=10, onefile=FALSE)
#main <- as.ggplot(uu$Main_bar) #+ theme(plot.margin = unit(c(0,0,0,0), "cm"))
#sizes <- as.ggplot(uu$Sizes) #+ theme(plot.margin = unit(c(0,0,-20,-20), "cm"))
##matrix <- (as.ggplot(uu$Matrix) + theme(plot.margin = unit(c(-10,0,0,0), "cm")))
##
main.c <- upset.continental.plot$Main_bar #+ theme(plot.margin = unit(c(0,0,0,0), "cm"))
sizes.c <- upset.continental.plot$Sizes  #+ theme(plot.margin = unit(c(0,0,0,0), "cm"))
matrix.c <- upset.continental.plot$Matrix #+ theme(plot.margin = unit(c(0,0,0,0), "cm"))
##
main.d <- upset.diaspora.plot$Main_bar #+ theme(plot.margin = unit(c(0,0,0,0), "cm"))
sizes.d <- upset.diaspora.plot$Sizes  #+ theme(plot.margin = unit(c(0,0,0,0), "cm"))
matrix.d <- upset.diaspora.plot$Matrix #+ theme(plot.margin = unit(c(0,0,0,0), "cm"))
##
up.c <- cowplot::plot_grid(NULL,main.c,
                           NULL,NULL,
                           sizes.c,matrix.c,
                           nrow=3, ncol=2, align='hv', rel_heights = c(6.1,-0.9,3.9), rel_widths = c(2,8),scale = 1)
up.d <- cowplot::plot_grid(NULL,main.d,
                           NULL,NULL,
                           sizes.d,matrix.d,
                           nrow=3, ncol=2, align='hv', rel_heights = c(6.1,-0.9,3.9), rel_widths = c(2,8),scale = 1)
ggarrange(up.c, up.d, ncol = 1, labels = c("A", "B"),hjust = -6) +
    theme(plot.margin = margin(0,0,0,-1.75, "cm"))
dev.off()

## ##  grid.text("Diaspora", x = 0.65, y = 0.95, gp=gpar(fontsize=9, fontface = "bold")))
## pdf("figure_5.pdf", width=10, height=10, onefile=FALSE)
## ## ggarrange(as.ggplot(upset.continental.plot),
## ##           as.ggplot(upset.diaspora.plot),
## ##           labels = c("A", "B"),
## ##           nrow=2, ncol=1,
## ##           heights = c(2, 2),
## ##           widths = c(2, 2))
## #          hjust = -15) +
## #    theme(plot.margin = margin(0,0,0,-3, "cm"))
## cowplot::plot_grid(as.ggplot(upset.continental.plot), as.grob(upset.diaspora.plot), ncol=1, nrow=2)
## #grid.arrange(upset.continental.plot, upset.diaspora.plot, ncol=1, nrow=2)
## dev.off()

the.data.c <- as.data.frame(upset.continental[,4:13] %>% group_by_all() %>% count)
the.data.d <- as.data.frame(upset.diaspora[,4:13] %>% group_by_all() %>% count)
table(publications$Population) 
upset_stats <- merge(as.data.frame(colSums(upset.continental[,4:12])),
                     as.data.frame(colSums(upset.diaspora[,4:12])),
                     by="row.names",all=TRUE)
colnames(upset_stats) <- c("Disease", "Continental","Diaspora")
upset_stats


## EXCEL SPREADSHEET
write.table(data, file="publications_data.csv",row.names=FALSE,col.names=TRUE,sep="|")
write.table(by.country, file="country_data.csv",row.names=FALSE,col.names=TRUE,sep="|")
write.table(disease.data, file="upset_data.csv",row.names=FALSE,col.names=TRUE,sep="|")


summary(publications[which(publications$Population == "Diaspora"),][,6])
summary(publications[which(publications$Population == "Continental"),][,6])
summary(publications[which(publications$Population == "Both"),][,6])
