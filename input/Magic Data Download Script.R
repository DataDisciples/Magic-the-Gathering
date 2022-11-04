#### Magic the Virgining ####

# Load required libraries
library(scryr)
library(tidyverse)
library(httr)
library(XML)
library(jsonlite)
library(data.table)
library(magick)

# Set working directory
setwd("~/R/Magic the Virgining/Magic-the-Gathering")

# Download bulk card data
cards = 
  scry_bulk_file(
    name = 
      'Default Cards'
  )

# uppercasing set names for matching later
cards$set = 
  toupper(cards$set) 

#only want to use english cards, removing columns that contain lists that probably aren't too useful, and also unnesting the relevant columns
english_cards = 
  cards %>% 
  filter(
    lang == 'en'
  ) %>%
  select(!c(color_identity,
            all_parts,
            preview,
            produced_mana,
            multiverse_ids,
            artist_ids,
            finishes,
            color_indicator,
            frame_effects,
            card_faces,
            promo_types)) %>% 
  unnest(
    c(legalities,
      image_uris,
      prices,
      related_uris), 
    keep_empty = TRUE
  )

#Time to unnest all of the colors, keywords, and anything else we need
card_colors = english_cards %>%
  unnest(
    colors,
    keep_empty = T
  ) %>%
  group_by(id) %>%
  summarise(colors_pulled = paste0(colors, collapse = ','))

english_cards$colors = card_colors$colors_pulled

card_keywords = english_cards %>% 
  unnest(keywords,
         keep_empty = T) %>%
  group_by(id) %>%
  summarise(keywords_pulled = paste0(keywords,
                                     collapse = ','))

english_cards$keywords = card_keywords$keywords_pulled

card_games = english_cards %>%
  unnest(games,
         keep_empty = T) %>%
  group_by(id) %>%
  summarise(games_pulled = paste0(games,
                                  collapse = ','))

english_cards$games = card_games$games_pulled

#Split the data into two files. One that contains core and expansion data, and one that contains all other release types
english_core_expansion_cards = english_cards %>%
  filter(set_type == 'expansion' | set_type == 'core')

english_other_cards = english_cards %>%
  filter(set_type != 'expansion' & set_type != 'core')

# Write bulk card data to local drive
fwrite(
  english_core_expansion_cards, 
  '~/R/Magic the Virgining/Magic-the-Gathering/input/magic-default-cards-english.csv', 
  row.names = F
)

fwrite(english_other_cards,
       '~/R/Magic the Virgining/Magic-the-Gathering/input/magic-not-core-or-expansion-cards.csv',
       row.names = F)
