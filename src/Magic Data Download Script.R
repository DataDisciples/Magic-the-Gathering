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

#only want to use english cards
english_cards = 
  cards %>% 
  filter(
    lang == 'en'
  )

df = english_cards %>%
  select(
    name,
    rarity,
    oracle_text,
    set,
    released_at,
    large
  )

fwrite(df, 'magic-cards-input.csv', row.names = F)

colnames(english_cards)

english_cards = english_cards %>% 
  unnest(
    c(legalities, image_uris, prices, related_uris), 
    keep_empty = TRUE
)



dim(english_cards)
english_cards
head(english_cards)


# Write bulk card data to local drive
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

#export master_set_list
fwrite(
  master_set_list, 
  '~/R/Magic the Virgining/input/master-set-list.csv', 
  row.names = F
)

#reducing english card dataset to only cards that are in the master set (no unsets or commander sets)
core_and_expansion_cards = 
  english_cards[english_cards$set %in% master_set_list, ]

#create csv for core and expansion cards
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

#create group by set/released at and average the text length
core_and_expansion_cards_text_length_group = 
  core_and_expansion_cards %>%
  filter(
    !is.na(text_length)
  ) %>%
  group_by(
    set, 
    rarity
  ) %>%
  summarise(
    avg_text_length = 
      mean(
        text_length
      ),
    released_at = 
      min(
        released_at
      )
  )

#combine set and release date for easy displaying in animated plot
core_and_expansion_cards_text_length_group$released_at = 
  paste(core_and_expansion_cards_text_length_group$released_at,
        'Set:',
        core_and_expansion_cards_text_length_group$set)

#following code is the adjusted bar chart race code
text_length_formatted <- core_and_expansion_cards_text_length_group %>%
  group_by(released_at) %>% # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-avg_text_length),
         avg_text_length_rel = avg_text_length/avg_text_length[rank==1],
         avg_text_length_lbl = paste0(" ",round(avg_text_length))) %>%
  group_by(rarity) %>%
  filter(rank <=10) %>%
  ungroup()

#create csv for animated bar chart
fwrite(
  text_length_formatted,
  '~/R/Magic the Virgining/input/text-length-animation.csv',
  row.names = F
)