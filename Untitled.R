
library(ggplot2)
library(dplyr)
library(DT)
library(tools)
library(RColorBrewer)
library(readxl)
library(reshape2)
library(tidyr)
library (scales)
library(latticeExtra)
library(cowplot)
library(PieBarViz)
library(knitr)
library(kableExtra)
library(stats)
library(DT)
library(MASS)
library(ggparallel)
library(GGally)



red <- read.csv("Exercise Notebooks/PDF NOTEBOOKS/winequality-red.csv", sep = ";")
white <- read.csv("Exercise Notebooks/PDF NOTEBOOKS/winequality-white.csv", sep = ";")


red['color'] <- "red"
white['color'] <- "white"

wineDataSet <- rbind(white, red)
wineDataSet <- wineDataSet[sample(nrow(wineDataSet)),]

ggparcoord(wineDataSet, columns = c(1:7, 10:12), groupColumn = 'color', scale = 'uniminmax', alphaLines = 0.2)

hypermthylation1 <- read_excel("hypermthylation.xlsx")

hypermthylation <- gather(hypermthylation1, genes, value, 2:ncol(hypermthylation1))

head(hypermthylation)

#3D Visualisation
cloud(value ~ as.factor(TumorTypes) + as.factor(genes), 
      hypermthylation, panel.3d.cloud=panel.3dbars,
      xbase=0.3,  
      ybase=0.3, 
      zlab = NULL,
      groups = genes,
      col.facet = c("blue", "yellow", "purple", "orange", "green", "purple", "pink"),
      scales=list(arrows=FALSE, col=1), xlab = NULL, ylab = NULL, main = NULL,
      par.box = list(col = NA), lcol=NULL
)

hypermthylation1$TumorTypes
hypermthylation1[is.na(hypermthylation1)] <- -0.05
head(hypermthylation1)

#Option One - Parallel Coordinates 
ggparcoord(hypermthylation1,  columns = 2:ncol(hypermthylation1), groupColumn = 'TumorTypes', scale = 'globalminmax')
#Option Two - Faceted Bar Chart
ggplot(hypermthylation, aes(x = genes, y = value, fill = TumorTypes)) + 
  geom_bar(position = "dodge", stat = "identity") + facet_grid(rows = vars(TumorTypes)) 

ggplot(hypermthylation, aes(x = genes, y = value, fill = TumorTypes)) + 
  geom_bar(position = "dodge", stat = "identity")
#Option Three - Facetted Scatterplot
ggplot(hypermthylation, aes(x = TumorTypes, y = value, col = TumorTypes)) + 
  geom_point() + facet_grid(rows = vars(genes)) 
#Option Four - Dot Plot 
ggplot(hypermthylation, aes(x = TumorTypes, y = genes, size = value, col = value)) + 
  geom_point() 

GHG_Concentration <- read_excel("GHG_Concentration.xlsx")

GHG_Concentration <- gather(GHG_Concentration, type, GHG_concentration, 2:ncol(GHG_Concentration))

head(GHG_Concentration)

cloud(GHG_concentration ~ as.factor(X__1) + as.factor(type), 
      GHG_Concentration, panel.3d.cloud=panel.3dbars,
      xbase=0.3,  
      ybase=0.3, 
      zlab = NULL,
      groups = type,
      col.facet =as.factor(GHG_Concentration$type),
      scales=list(arrows=FALSE, col=1), xlab = NULL, ylab = NULL, main = NULL,
      par.box = list(col = NA), lcol=NULL
)

air_quality <- read.csv("Exercise Notebooks/PDF NOTEBOOKS/Air_Quality.csv")
air_quality <-dplyr:: filter(air_quality, year_description == 'Annual    2009-2010')

#3D Vizualisation 
cloud(data_valuemessage ~ as.factor(geo_entity_id) + as.factor(name), 
      air_quality, panel.3d.cloud=panel.3dbars,
      xbase=0.3,  
      ybase=0.3, 
      zlab = NULL,
      groups = name,
      col.facet =as.factor(air_quality$name),
      scales=list(arrows=FALSE, col=1, x=list(rot=90)), xlab = NULL, ylab = NULL, main = NULL,
      par.box = list(col = NA), lcol=NULL, 
      par.settings = list(axis.text = list(cex = 0.4))
)


air_quality <- air_quality[,-c(1,2,4,5,6,8)]
air_quality
air_quality2 <- dcast(air_quality,name~geo_entity_name, value.var = "data_valuemessage")
air_quality2
ggparcoord(air_quality2,  columns = 2:ncol(air_quality2), groupColumn = 'name', scale = 'globalminmax')


air_quality3 <- dcast(air_quality,geo_entity_name~name, value.var = "data_valuemessage")
air_quality3
ggparcoord(air_quality3,  columns = 2:ncol(air_quality3), groupColumn = 'geo_entity_name', scale = 'globalminmax')

#Option Two - Faceted Bar Chart
ggplot(air_quality, aes(x = geo_entity_name, y = data_valuemessage, fill = name)) + 
  geom_bar(position = "dodge", stat = "identity") +         
  theme(
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1)
  )

ggplot(air_quality, aes(x = geo_entity_name, y = data_valuemessage, fill = name)) + 
  geom_bar(position = "dodge", stat = "identity") + facet_grid(rows = vars(name))  +    
  theme(
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1),
    strip.text.y = element_text(size = 6)
  )


# #Option Three - Facetted Scatterplot
ggplot(air_quality, aes(x = geo_entity_name, y = data_valuemessage, col = name)) +
  geom_point() + facet_grid(rows = vars(name)) + 
  theme(
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1),
    strip.text.y = element_text(size = 6)
  )
# #Option Four - Dot Plot 
ggplot(air_quality, aes(x = geo_entity_name, y = name, size = data_valuemessage)) +
  geom_point(col = "blue") +  theme(
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1),
    strip.text.y = element_text(size = 6)
  )
