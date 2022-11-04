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
setwd("~/R/Magic the Virgining")

# Download bulk card data
cards = 
  scry_bulk_file(
    name = 
      'Default Cards'
  )

# uppercasing set names for matching later
cards$set = 
  toupper(cards$set) 

#only want to use english cards, cards that aren't reprints, and run the distinct function to clear up any doubles of any cards
english_cards = 
  cards %>% 
  filter(
    lang == 'en',
    reprint == F
  ) %>%
  distinct(name,
           .keep_all = T) %>%
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
            promo_types))

english_cards = english_cards %>% 
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
  )

card_colors = card_colors %>%
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

# Write bulk card data to local drive ######STILL NEED TO SET THIS UP!!!!!!!!######
fwrite(
  english_cards, 
  '~/R/Magic the Virgining/input/magic-default-cards-english.csv', 
  row.names = F
)

#pulling in data from wikipedia so we have a working list of sets that are either core or expansion sets (no commander only or unsets)
url = 
  'https://en.wikipedia.org/wiki/List_of_Magic:_The_Gathering_sets'

# Get API call response
response = 
  GET(
    url
  )

response = 
  content(
    response, 
    'text'
  )

response = 
  readHTMLTable(
    response
  )

#table of core sets
core_table = 
  response[[1]]

names(
  core_table
) = 
  c(
    'Set', 
    'Set Symbol', 
    'Set Code', 
    'Pre-Release Date', 
    'Release Date', 
    'Size', 
    '1',
    '2',
    '3',
    '4',
    '5',
    '6'
  )

core_table = 
  core_table[-1:-2, ]

# table of expansion sets (no such thing as core sets after 2021 btw)
expansion_table = 
  response[[2]]

names(expansion_table) = 
  c(
    'Set', 
    'Set Symbol', 
    'Set Code', 
    'Pre-Release Date', 
    'Release Date', 
    'Size', 
    '1',
    '2',
    '3',
    '4',
    '5',
    '6',
    '7'
  )

expansion_table = 
  expansion_table[-1:-3, ]

expansion_table = 
  expansion_table %>%
  filter(
    !is.na(`Set Code`) & `Set Code` != 'unrevealed')

expansion_table$`Set Code` = 
  substr(expansion_table$`Set Code`,1,3)

#making a master set list for reference
master_set_list = 
  append(
    core_table$`Set Code`, 
    expansion_table$`Set Code`
  )

#reducing english card dataset to only cards that are in the master set (no unsets or commander sets)
core_and_expansion_cards = 
  english_cards[english_cards$set %in% master_set_list, ]

#create csv for core and expansion cards 
#STILL NOT WORKING BECAUSE NEED TO REMOVE VECTORS FROM COLUMNS####
fwrite(
  core_and_expansion_cards,
  '~/R/Magic the Virgining/input/core-and-expansion-cards-english.csv',
  row.names = F
)

#create text length column
core_and_expansion_cards$text_length = 
  nchar(
    core_and_expansion_cards$oracle_text
  )

#outputting file for animation
core_and_expansion_cards = core_and_expansion_cards %>%
  select(c(name,
           rarity,
           oracle_text,
           set,
           released_at,
           large,
           reprint))

fwrite(core_and_expansion_cards,
       '~/R/Magic the Virgining/input/magic-cards-input.csv',
       row.names = F)
