#*********************
#Funnel Chart---------
#*********************

# Top 5 misidentified species by the Pacific CNN 

fig1 <- plot_ly() 
fig1 <- fig1 %>%
  add_trace(type = "funnel",
            y = c("<i>P. obliquiloculata</i>", "<i>N. incompta</i>","<i>G. tenella</i>", "<i>G. elongatus</i>", "<i>G. bulloides</i>"),
            x = c(234, 64, 64, 56, 24),
            textposition = "inside",
            textfont = list(family = "Old Standard TT, Arial", size = 24, color = "white", face = "italic"),
            textinfo = "label+percent total",
            opacity = 0.90,
            connector = list(line = list(color = "white", width = 3)),
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




