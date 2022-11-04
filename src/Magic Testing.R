#### Magic the Virgining ####

# Load required libraries
library(scryr)
library(tidyverse)
library(httr)
library(XML)
library(jsonlite)
library(data.table)
library(magick)
library(lubridate)
library(gganimate)

# Set working directory
setwd("~/R/Magic the Virgining/input")


# Read in bulk card data from local file
cards = 
  fread(
    '~/R/Magic the Virgining/input/magic-cards-input.csv'
    )

#pull in core set data
core_sets = fread(
  'core-sets-wiki.csv'
)

#need to remove this code later but for right now using this to fix issues with core set data
names(
  core_sets
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

core_sets = 
  core_sets[-1:-2, ]

#pull in expansion set data
expansion_sets = fread(
  'expansion-sets-wiki.csv'
)

#remove this code later but for now need it to fix some issues w the data
names(expansion_sets) = 
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

expansion_sets = 
  expansion_sets[-1:-3, ]

expansion_sets = 
  expansion_sets %>%
  filter(
    !is.na(`Set Code`) & `Set Code` != 'unrevealed')

expansion_sets$`Set Code` = 
  substr(expansion_sets$`Set Code`,1,3)

#making a master set list for reference
master_set_list = 
  append(
    core_sets$`Set Code`, 
    expansion_sets$`Set Code`
    )

#reducing card dataset to only cards that are in the master set (no unsets or commander sets)
core_and_expansion_cards = 
  cards[cards$set %in% master_set_list, ]

core_and_expansion_cards$text_length = 
  nchar(
    core_and_expansion_cards$oracle_text
    )
######DONT NEED THIS BUT MAYBE USEFUL SOME OTHER TIME#######
#create group by set/released at and average the text length
#core_and_expansion_cards_text_length_group = 
#  core_and_expansion_cards %>%
#  filter(
#    !is.na(text_length)
#    ) %>%
#  group_by(
#    set, 
#    rarity
#    ) %>%
#  summarise(
#    avg_text_length = 
#      mean(
#        text_length
#        ),
#    released_at = 
#      min(
#        released_at
#        )
#    )
#
#combine set and release date for easy displaying in animated plot
#core_and_expansion_cards_text_length_group$released_at = 
#  paste(core_and_expansion_cards_text_length_group$released_at,
#                                                               'Set:',
#                                                               core_and_expansion_cards_text_length_group$set)


#code to reformat the data in a way that works with the bar chart race
empty_df = core_and_expansion_cards[0, ]
unique_release_dates_double = ymd(unique(core_and_expansion_cards$released_at))
unique_release_dates_char = as.character(unique_release_dates)

for (i in 1:length(unique_release_dates_double)) {
  loop_frame = core_and_expansion_cards %>%
    filter(released_at <= unique_release_dates_double[i]) %>%
    mutate(rank = rank(-text_length)) %>%
    filter(rank <= 10)
  
  loop_frame$date_of_ranking = unique_release_dates_char[i]
  
  empty_df = rbind(empty_df, loop_frame, fill = TRUE)
}




#FOLLOWING CODE IS NO LONGER NECESSARY BC ABOVE CODE DOES THE JOB
##following code is the adjusted bar chart race code AND CURRENTLY DOESN'T WORK. NEEDS TO BE ADJUSTED IN A FEW WAYS TO WORK
#core_expansion_formatted = core_and_expansion_cards %>%
#  group_by(released_at) %>%
#  mutate(rank = rank(-text_length),
#         text_length_rel = text_length/text_length[rank == 1],
#         text_length_lbl = paste0(' ', round(text_length))) %>%
#  group_by(name) %>%
#  filter(rank <= 10) %>%
#  ungroup()





staticplot = ggplot(empty_df, aes(rank, group = name,
                                               fill = as.factor(name), color = as.factor(name))) +
  geom_tile(aes(y = text_length/2,
                height = text_length,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(name, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=text_length,label = text_length, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))

anim = staticplot + transition_states(date_of_ranking, transition_length = 4, state_length = 1) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Release Date: {closest_state}',
       subtitle  =  "Top 10 Countries",
       caption  = "GDP in Billions USD | Data Source: World Bank Data")

animate(anim, 2400, fps = 20,  width = 1200, height = 1000,
        renderer = gifski_renderer("gganim.gif"))
