#*********************
#Funnel Chart---------
#*********************

# Top 5 misidentified species by the Pacific CNN 

fig1 <- plot_ly() 
fig1 <- fig1 %>%
            marker = list(color = c("#3B528B", "#3B528B", "#3B528B", "#3B528B", "#3B528B"),
                          line = list(width = c(1,1,1,1,1), color = c("white", "white", "white", "white", "white"))))


fig1 <- fig1 %>%
  layout(yaxis = list(categoryorder = "trace", size = 18, visible = FALSE),annotations = list("<i></i>")) 
fig1


# Top 5 misidentified species by the Indian CNN 

fig2 <- plot_ly() 
fig2 <- fig2 %>%
  add_trace(type = "funnel",
            y = c("<i>G. elongatus</i>", "<i>G. ungulata</i>","<i>G. tenella</i>", "<i>O. Universa</i>", "<i>G. tumida</i>"),
            x = c(90, 55, 32, 32, 30),
            textposition = "inside",
            textfont = list(family = "Old Standard TT, Arial", size = 24, color = "white"),
            textinfo = "label+value",
            opacity = 0.90,
            connector = list(line = list(color = "white", width = 3)),
            marker = list(color = c("#b94b75", "#b94b75", "#b94b75", "#b94b75", "#b94b75"),
                          line = list(width = c(1,1,1,1,1), color = c("white", "white", "white", "white", "white"))))

fig2 <- fig2 %>%
  layout(yaxis = list(categoryorder = "trace", size = 18, visible = FALSE)) 
fig2 


#**************************************
#Pacific vs Human Recognition----------
#**************************************
#*

Pacific_cnn <- ggplot(Pacific_CNN_vs_Human,
                      aes(x = Human, y = Machine)) + 
  geom_point(aes(color = Species, shape = Species), size = 8) + 
  geom_smooth(method="lm",se=F, size = .5, color = 'black') + 
  scale_color_manual(aesthetics = "colour", values = c("#DC050C","#67c675", #72190E,  "#6a70d7", "#6aa13f",
                                                       "#ce9534","#6d8bd6","#cd6d3b", "#33d4d1","#c9417e",
                                                       "#47bb8a", "#a54190", "#4a6e24","#ca86ce", "#af9e4d", 
                                                       "#802657", "#72190E","#dd5858","#42150A", "#b44555")) + 
  scale_shape_manual(values = c(1,5,7,8,9,11,12,13,14,15,16,17,18,19,20,21,22,23,3,24)) + 
  theme_bw() + 
  theme(axis.line  = element_line(colour = "black",size=0), 
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + 
  theme(axis.text=element_text(size = 20, colour = "black"),
        axis.title=element_text(size=22, colour = "black")) +
  theme(legend.text = element_text(face = "italic", size = 18)) + 
  guides(shape = guide_legend(override.aes = list(size = 8))) + 
  theme(legend.title = element_text(size = 19)) + 
  annotate("text", x = 2500, y = 14000, size = 7.5, 
           label = "italic(ρ)==0.94*','~italic(p)", parse = TRUE) +
  annotate("text", x = 5300, y = 14000, size = 7.5, 
           label = "< 2.2e-16") +
  labs(y = expression("Pacific CNN"), x = expression("Human Experts"))

#Export Figure as JPG

jpeg("~/Desktop/Pacific_cnn_vs_human_05102022.jpg",width=6700,height=6000,units="px",res=600,bg="white")
Pacific_cnn
dev.off()

#**************************************
#Indian CNN vs Human Recognition-------
#**************************************
#*

Indian_cnn <- ggplot(Indian_CNN_vs_Human,
                     aes(x = Human, y = Machine)) + 
  geom_point(aes(color = Species, shape = Species), size = 8) + 
  geom_smooth(method="lm",se=F, size = .5, color = 'black') + 
  scale_color_manual(aesthetics = "colour", values = c("#DC050C","#67c675", #72190E,  "#6a70d7", "#6aa13f",
                                                       "#ce9534","#6d8bd6","#cd6d3b", "#33d4d1","#c9417e",
                                                       "#47bb8a", "#a54190", "#4a6e24","#ca86ce", "#af9e4d", 
                                                       "#802657", "#72190E","#dd5858","#42150A", "#b44555")) + 
  scale_shape_manual(values = c(1,5,7,8,9,11,12,13,14,15,16,17,18,19,20,21,22,23,3,24)) + 
  theme_bw() + 
  theme(axis.line  = element_line(colour = "black",size=0), 
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        panel.grid.minor = element_blank(), panel.grid.major = element_blank()) + 
  theme(axis.text=element_text(size = 20, colour = "black"),
        axis.title=element_text(size=22, colour = "black")) +
  theme(legend.text = element_text(face = "italic", size = 18)) + 
  guides(shape = guide_legend(override.aes = list(size = 8))) + 
  theme(legend.title = element_text(size = 19)) + 
  annotate("text", x = 2500, y = 14000, size = 7.5, 
           label = "italic(ρ)==0.99*','~italic(p)", parse = TRUE) +
  annotate("text", x = 5700, y = 14000, size = 7.5, 
           label = "< 2.2e-16") +
  labs(y = expression("Indian CNN"), x = expression("Human Experts"))

#Export Figure as JPG

jpeg("~/Desktop/Indian_cnn_vs_human_05102022.jpg",width=6700,height=6000,units="px",res=600,bg="white")
Indian_cnn
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



