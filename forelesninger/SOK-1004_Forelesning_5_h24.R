# SOK-1004 Forelesning 5

# Sammenheng mellom lønnsvekst og arbeidsledighet i Norge, 1971-2023

# Data fra SSB, tabell 09786 og 08517

# rydd

rm(list=ls())

# last inn pakken
library(tidyverse)



# URL (csv fil)
url <- "https://raw.githubusercontent.com/uit-sok-1004-h24/uit-sok-1004-h24.github.io/main/data/lonn_arbled_f5.csv"

# last inn fila

df <- read.csv(url)

