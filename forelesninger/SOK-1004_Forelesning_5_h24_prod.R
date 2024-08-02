# SOK-1004 Forelesning 5

# Produskjon i Norge 

# Data fra SSB, tabell 09171
# Produksjon i basisverdi. Faste 2021-priser (mill. kr)
# Basisverdi er prisen selgeren mottar for varen, uten mva og avgifter med evt subsidier.


# rydd

rm(list=ls())

# last inn pakken
library(tidyverse)



# URL (csv fil)
url <- "https://raw.githubusercontent.com/uit-sok-1004-h24/uit-sok-1004-h24.github.io/main/data/prod_f5.csv"

# last inn fila

df <- read.csv(url, header = FALSE)
#df <- read.csv("prod_f5.csv", header = FALSE)

# endre kolonnenavn

df <- df %>%
  rename(Næring = V1, Verdi = V2)

################
#  OPPGAVE    #
################

# 1 Bruk geom_bar() i ggplot 2 for å lage en stolpediagram av produksjonsverdier for hver næring
# 2 Er det lett å tolke denne visualiseringen? Hva kan gjøres for å forbedre den?

