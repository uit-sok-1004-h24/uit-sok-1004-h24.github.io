# SOK-1004 Forelesning 11

# Norges valg mellom fritid og konsum



# rydd

rm(list=ls())

# last inn pakken
library(tidyverse)

#lag aksen for tegningen

axes_1 <- ggplot()+
  labs(x=expression("Fritid"), y=expression("Konsum"))+
  theme(axis.title = element_text(size = 15),
        plot.title = element_text(size = 15),
        panel.background = element_blank(), # hvit bakgrunn
        axis.line = element_line(colour = "black"), # sett inn akselinjer
        axis.title.x = element_text(hjust = 1),
        axis.title.y = element_text(angle = 0, vjust = 1, hjust=1))+ # juster labels på aksene  
  scale_x_continuous(limits = c(15, 25), expand = c(0, 0))+
  scale_y_continuous(limits = c(0, 200), expand = c(0, 0)) # begrense aksene
# og sikre at akselinjene møttes i (0,0).

axes_1

# nyttefunkjsonen som brukes U=F^.783K^.15 (Cobb-Douglas)
# F=fritid, K=konsum

# vi angir noen indifferenskurver

I_0 <- function(F) (21.802^6.67)/F^(5.22) # nyttenivå 21.802
I_1 <- function(F) (20^6.67)/F^(5.22) # nytte=20


figur_1 <- axes_1 + labs(title="Norges tilpasning, 2021")+
  stat_function(
    fun=I_0,
    mapping = aes()
  ) +
  stat_function(
    fun=I_1,
    mapping = aes()
  ) +
  annotate("text",x=24,y=60, label=expression(u[0]))+
  annotate("text",x=24,y=40, label=expression(u[1]))

figur_1

# vi angir budsjettlinjen

# Vi må regne ut fra oppgitt info

# Det er 24*365 timer pr år
# Av disse jobber man 1408, dvs fritid 7352 timer
# Fritid er 7352/24=20.14 timer pr dag

# Husholdningsinntekten er 47687 USD
# Lønn per time arbeid blir 47687/1408=33.87 USD
# Konsum per dag er 47687/365=130.65 USD

# Budsjettbetingelsen per dag er K = 33.87*(24-F)
 

buds_0 <- function(F) 33.87*(24-F)

figur_2 <- figur_1+
  stat_function(
    fun=buds_0,
    mapping = aes()
  )

figur_2

# Vi ser at optimal tilpasning er der indifferenskurven er tangent til budsjettlinjen
# verdiene F=20.14 og K=130.65

figur_3 <- figur_2 +
  geom_segment(aes(x=20.14, y=0, xend=20.14, yend=130.65), linetype="dashed")+
  geom_segment(aes(x=15, y=130.65, xend=20.14, yend=130.65), linetype="dashed")+
  annotate("text",x=20.5,y=133, label="Norge")+
  geom_point(aes(x = 20.14, y = 130.65), color = "red", size = 2)

figur_3
