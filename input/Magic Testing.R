library(scryr)
library(tidyverse)
setwd("~/R/Magic the Virgining")
cards = scry_bulk_file(name = 'Oracle Cards')

dominaria_cards = cards %>%
  filter(set == 'dmu') %>%
  select(c(name, 
           set, 
           colors, 
           cmc, 
           color_identity, 
           color_indicator, 
           colors, 
           keywords, 
           loyalty, 
           mana_cost, 
           power, 
           produced_mana, 
           toughness,
           type_line,
           rarity))

