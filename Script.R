library(ggplot2)
library(ggtext)
library(ggeasy)
library(tidyverse)
library(palmerpenguins)
library(ggtext)
library(glue)


# My data sample
df <- data.frame(x=1:3, Type = c("CH4     (Metano)", 
                                 "N2O     (Óxido nitrógeno)",
                                 "CO2     (Dióxido carbono)"), 
                 myColour = c("#009E73", "#D55E00", "#0072B2"), 
                 Amount = c(12.5, 12.1, 29.14))

# Create factor to order by amount value
df$Type <- factor(df$Type, levels = df[order(df$Amount), "Type"])

Value2 = c("CH<sub>4</sub>", "N<sub>2</sub>O", "CO<sub>2</sub>")


library(grid)
library(png)
library(ggimage)
img <- readPNG("CO2.png", FALSE)
g <- rasterGrob(img, x = unit(0.9, "npc"),y = unit(0.82, "npc"), width = unit(0.2, "npc"))

imgg <- readPNG("CH4.png", FALSE)
gg <- rasterGrob(imgg, x = unit(0.7, "npc"),y = unit(0.5, "npc"), width = unit(0.2, "npc"))

imggg <- readPNG("N2O.png", FALSE)
ggg <- rasterGrob(imggg, x = unit(0.6, "npc"),y = unit(0.2, "npc"), width = unit(0.2, "npc"))

# MAKE BAR
Final=ggplot(df, aes(Type, Amount, fill = Type, color = myColour)) +
  geom_bar(stat = 'identity', position = 'dodge', show.legend = FALSE, width = .85, 
           colour = 'lightgrey', fill = df$myColour) + 
  theme_bw()+
  theme(axis.text.y = element_text(colour = df$myColour, size = 14, face = 'bold'))+
  coord_flip()+
  scale_y_continuous(breaks = waiver(), limits = c(0,40)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width =8), name="Emissiones gas")+
  labs(y="Concentracion (ppm)",
       X="Concentracion")+
  geom_text(data=df, aes(label = Amount , y = 5), vjust = -0.3, size = 3.5, show.legend = F, color="black") +
  geom_richtext(data= df, aes(label = Value2, hjust=-0.1), size=4, show.legend = F, color="black")+
  annotation_custom(g)+
  annotation_custom(gg)+
  annotation_custom(ggg)


Final


ggsave(plot=Final,"Grafica.png",units = "cm",width = 20, #alto
       height = 20, #ancho
       dpi=1200)

