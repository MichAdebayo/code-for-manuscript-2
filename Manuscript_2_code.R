##**********************************************************************************************************************************##
## Written by Michael B. Adebayo, PhD Student in de Garidel-Thoron lab, (2019 – 2023)                                               ##
## Centre européen de recherche et d'enseignement de géosciences de l'environnement (CEREGE), Aix-en-Provence                       ##
## Aix-Marseille University, France                                                                                                 ##
## Code related to figures in Thesis Chapter 3 (Chapter to be submitted as a technical note to Biogeosciences Journal)              ##
##**********************************************************************************************************************************##

##***********************************************************************************************************************************##
## Manuscript Title: Evaluation of the generalized application of convolutional neural networks (CNN) to different dataset sources   ##
##***********************************************************************************************************************************##

##**************************
## Load Packages -----------
##**************************

library("ggplot2")

##****************************************************
## Figure 5 (Pacific vs Human Recognition) -----------
##****************************************************

Pacific_cnn <- ggplot(Pacific_CNN_vs_Human, # specify data object
                      aes(x = Human, y = Machine)) + # specify columns for x and y axes 
  geom_point(aes(color = Species, shape = Species), # add color and shape by species
             size = 8) + # set point size
  geom_smooth(method = "lm", # fit linear model
              se=F, # do not show confidence interval
              size = .5, # set size of regression line
              color = 'black') + # set regression line color
  scale_color_manual(aesthetics = "colour", # specify the color representing each species manually
                     values = c("#DC050C", "#67c675", "#72190E", "#6a70d7", "#6aa13f",
                                "#ce9534", "#6d8bd6", "#cd6d3b", "#33d4d1", "#c9417e",
                                "#47bb8a", "#a54190", "#4a6e24", "#ca86ce", "#af9e4d", 
                                "#802657", "#72190E", "#dd5858", "#42150A", "#b44555")) + 
  scale_shape_manual(values = c(1, 5, 7, 8, 9, 11, 12, 13, 14, 15, # specify the shape representing each species manually
                                16, 17, 18, 19, 20, 21, 22, 23, 3, 24)) + 
  theme_bw() + # set background to white
  theme(axis.line  = element_line(colour = "black", size = 0), # set x and y axis line color
        panel.border = element_rect(colour = "black", fill = NA, size = 1), # format panel border color and set border size
        panel.grid.minor = element_blank(), # remove minor grid lines in figure
        panel.grid.major = element_blank()) + # remove major grid lines in figure
  theme(axis.text = element_text(size = 20, colour = "black"), # set axis text size and color
        axis.title = element_text(size=22, colour = "black")) + # set axis title text size and color
  theme(legend.text = element_text(face = "italic", size = 18)) + # set legend text format and size
  guides(shape = guide_legend(override.aes = list(size = 8))) + # override the initial size of the shapes shown in the legend section and specify new size for the shapes
  theme(legend.title = element_text(size = 19)) + # set legend title text size
  annotate("text", x = 2500, y = 14000, size = 7.5, # specify position and size of text to be annotated
           label = "italic(ρ)==0.94*','~italic(p)", parse = TRUE) + # add the text to be annotated
  annotate("text", x = 5300, y = 14000, size = 7.5, # specify position and size of second text to be annotated
           label = "< 2.2e-16") + # add the text to be annotated
  labs(y = expression("Pacific CNN"), x = expression("Human Experts")) # add titles for x and y axes

## Export Figure as JPG

jpeg("~/Desktop/Figure 5a.jpg",width = 6700, height = 6000, units = "px",res = 600, bg = "white")
Pacific_cnn # save the new figure object in jpeg format
dev.off()

##********************************************
## Indian CNN vs Human Recognition -----------
##********************************************

Indian_cnn <- ggplot(Indian_CNN_vs_Human, # specify data object
                     aes(x = Human, y = Machine)) + # specify columns for x and y axes 
  geom_point(aes(color = Species, shape = Species), # add color and shape by species
             size = 8) + # set point size
  geom_smooth(method="lm", # fit linear model
              se=F, # do not show confidence interval
              size = .5, # set size of regression line
              color = 'black') + # set regression line color
  scale_color_manual(aesthetics = "colour", # specify the color representing each species manually
                     values = c("#DC050C", "#67c675", "#72190E", "#6a70d7", "#6aa13f",
                                "#ce9534", "#6d8bd6", "#cd6d3b", "#33d4d1", "#c9417e",
                                "#47bb8a", "#a54190", "#4a6e24", "#ca86ce", "#af9e4d", 
                                "#802657", "#72190E", "#dd5858", "#42150A", "#b44555")) + 
  scale_shape_manual(values = c(1, 5, 7, 8, 9, 11, 12, 13, 14, 15, # specify the shape representing each species manually
                                16, 17, 18, 19, 20, 21, 22, 23, 3, 24)) + 
  theme_bw() + # set background to white
  theme(axis.line  = element_line(colour = "black",size=0), # set x and y axis line color
        panel.border = element_rect(colour = "black", fill = NA, size = 1), # format panel border color and set border size
        panel.grid.minor = element_blank(), # remove minor grid lines in figure
        panel.grid.major = element_blank()) + # remove major grid lines in figure
  theme(axis.text = element_text(size = 20, colour = "black"), # set axis text size and color
        axis.title = element_text(size = 22, colour = "black")) + # set axis title text size and color
  theme(legend.text = element_text(face = "italic", size = 18)) + # set legend text format and size
  guides(shape = guide_legend(override.aes = list(size = 8))) + # override the initial size of the shapes shown in the legend section and specify new size for the shapes
  theme(legend.title = element_text(size = 19)) + # set legend title text size
  annotate("text", x = 2500, y = 14000, size = 7.5, # specify position and size of text to be annotated
           label = "italic(ρ)==0.99*','~italic(p)", parse = TRUE) + # add the text to be annotated
  annotate("text", x = 5700, y = 14000, size = 7.5, # specify position and size of the second text to be annotated
           label = "< 2.2e-16") + # add the second text to be annotated
  labs(y = expression("Indian CNN"), x = expression("Human Experts")) # add titles for x and y axes

## Export Figure as JPG

jpeg("~/Desktop/Figure 5b.jpg",width = 6700, height = 6000, units = "px", res = 600, bg = "white")
Indian_cnn # save the new figure object in jpeg format
dev.off()

#******************************
#Chord Diagram (Pacific)-------
#******************************
#*

# Libraries

library(tidyverse)
library(viridis)
library(patchwork)
library(hrbrthemes)
library(circlize)

-----
  
  #colors <- c(G. bulloides = "#a4843c",
  #G. glutinata = "#5b388b",
  #G. siphonifera = "#57c46c",
  #T. sacculifer = "#c973c6",
  #G. elongatus = "#98b342",
  # G. ruber = "#6d80d8",
  # G. rubescens = #d1a13a",
  # G. tenella = "#8d2c66",
  # G. menardii = "#5eb676",
  # G. tumida = "#d76092"
# G. ungulata = "#43c9b0",
# N.dutertrei = #cf4f42",
# N. incompta = "#5f8938",
# O. universa = "#be4a5b",
# P. obliquiloculata = "#ce7239",
# Fragments = "#98432a")

# color palette 1

colork <- c("#a4843c", "#5b388b", "#57c46c","#c973c6",
            "#98b342", "#6d80d8", "#d1a13a","#8d2c66",
            "#5eb676","#d76092", "#43c9b0", "#cf4f42",
            "#5f8938", "#be4a5b","#ce7239","#98432a")

# color palette 2

colorgrd <- colorRampPalette(list("#856705", "#d3d3d3","#113069"))(17)

# color palette 3

mycolor <- viridis(17, alpha = 1, begin = 0, end = 1, option = "D")
mycolor <- mycolor[sample(1:17)]

# color palette 4

cols <- hcl.colors(17, "Temps")

# Open plot save graphics
png("~/Desktop/Chord_pacific25102022.png",width=6700,height=6000,units="px",res=600,bg="white")

#Set circular layout parameters

circos.clear()
circos.par(start.degree = 165, gap.degree = 4, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
par(cex = 1)

# Base plot

chordDiagram(chord_test_pacific, 
             grid.col = colork, 
             col = colorgrd,
             big.gap = 20,
             transparency = 0.25,
             link.sort = TRUE, 
             link.largest.ontop = TRUE,
             directional = 1,
             direction.type = c("arrows"),
             link.arr.type = "big.arrow",
             annotationTrack = c("grid", "axis"), 
             annotationTrackHeight = c(0.05, 0.1),
             scale = FALSE,
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(chord_test_pacific))))))

# we go back to the first track and customize sector labels
circos.trackPlotRegion(
  track.index = 1,
  panel.fun = function(x, y) {
    circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
                facing = "clockwise", cex = 1.4, font = 3, niceFacing = TRUE, adj = c(-0.255, 0.5))
    
  }, bg.border = NA) # here set bg.border to NA is important

dev.off()



#******************************
#Chord Diagram (Indian)-------
#******************************
#*

# Set color palette 1

colorsecInd <- c("#a4843c", "#5b388b", "#57c46c","#c973c6",
                 "#98b342", "#6d80d8", "#d1a13a","#8d2c66",
                 "#5eb676","#d76092", "#43c9b0", "#cf4f42",
                 "#5f8938", "#be4a5b","#ce7239","#98432a")

# Set color palette 2

colorsecInd2 <- colorRampPalette(list("#93a24e", "#d3d3d3","#ba4d4c"))(17)

# color palette 3

mycolor <- viridis(17, alpha = 1, begin = 0, end = 1, option = "D")
mycolor <- mycolor[sample(1:17)]

# color palette 4

cols <- hcl.colors(17, "Temps")

# Open plot save graphics
png("~/Desktop/Chord_indian2510222.png",width=6700,height=6000,units="px",res=600,bg="white")

#Set circular layout parameters

circos.clear()
circos.par(start.degree = 170, gap.degree = 4, track.margin = c(-0.1, 0.1), points.overflow.warning = FALSE)
par(cex = 1)

# Base plot

chordDiagram(chord_test_Indian, 
             grid.col = colorsecInd, 
             col = colorsecInd2,
             big.gap = 20,
             transparency = 0.25,
             link.sort = TRUE, 
             link.largest.ontop = TRUE,
             directional = 1,
             direction.type = c("arrows"),
             link.arr.type = "big.arrow",
             annotationTrack = c("grid", "axis"), 
             annotationTrackHeight = c(0.05, 0.1),
             scale = FALSE,
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(chord_test_Indian))))))

# we go back to the first track and customize sector labels

circos.trackPlotRegion(
  track.index = 1,
  panel.fun = function(x, y) {
    circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
                facing = "clockwise", cex = 1.4, font = 3, niceFacing = TRUE, adj = c(-0.255, 0.5))
    
  }, bg.border = NA) # here set bg.border to NA is important

dev.off()

--------
  
  ggplot(size_fire_explor, aes(x = Age)) +
  geom_line(aes(y = R), color = "red") +
  geom_line(aes(y = B), color = "blue") +
  geom_line(aes(y = G), color = "green")


ggplot(size_fire_explor, aes(x = Age)) +
  geom_rect(aes(xmin = 1, xmax = 30, 
                ymin = 0, ymax = 5),
            color = "#2C77BF", 
            fill = alpha("#2C77BF", .5)) +
  
  ggplot(size_fire_explor) + 
  geom_tile(aes(x = Age, y = R, fill = R)) +
  scale_y_continuous(limits=c(-0.3,0.2),breaks=1) +
  scale_fill_gradient2(low = 'blue', mid = 'white', high = 'red', midpoint = -0.05) +
  theme_minimal()

expl <- ggplot(size_fire_explor, aes(x = Age)) + 
  geom_line(aes(y = R), color = "red")


expl + geom_rect(aes(xmin = 2, xmax = 43, ymin = -0.35, ymax = -0.31), color = size_fire_explor$B) +
  gradient_color(aes(color = "red"))

##**********************************************************************************************************************************##
## End of Script --------
##**********************************************************************************************************************************##

