#=================================================================================
# global variables accessible by both ui.R and server.R
# steven wong, february 2016
#=================================================================================
source('project_library.R')
load('GDP.RData')

#---------------------------------------------------------------------------------
# date range
date_min <- min(GDP_Ranked$DATE)
date_max <- max(GDP_Ranked$DATE)

#---------------------------------------------------------------------------------
# unique values

# prices
prices <- unique(GDP_Ranked$PRICES)

# broad level NAICS codes
NAICS <- unique(unlist(GDP_Ranked %>% filter(Position == 2) %>% select(NAICS)))
