#################################
#### Lab FDI and Trade Data
#### May 20, 2026 
#### Kyle Handley 
#### Version 1.2
#### Edited by Vincent Alulu
#### Added: Top 10 US Export Destinations 2015 vs 2025
#################################

rm(list = ls())

library(censusapi)
library(tidyverse)

#Note: I delted parts not required and commented out the imports section

## Part 3: Census Trade Data ----

# YOU MUST SPEND SOME TIME READING MANUAL HERE #

#Trade data details https://www.census.gov/data/developers/data-sets/international-trade.html



#naics basis
#imports_naics<-getCensus(
  #name = "timeseries/intltrade/imports/naics", # this says where to look there are dozens of options
  #vars = c("GEN_VAL_MO"), #this is the variable you want: here General Imports by Month
 # time = "from 2016",
  #CTY_CODE="1220", #this is Canada, Census uses it's own 4 character codes
 #CTY_CODE="2010", #this is China
  #show_call = TRUE #useful to check or for replication later on different system
#)
#head(imports_naics)

# note there is also a cumulative import and expor value with suffix YR
#imports_naics<-getCensus(
  #name = "timeseries/intltrade/imports/naics",
  #vars = c("GEN_VAL_YR","GEN_VAL_MO","YEAR"),
  #time = "from 2023",
  #CTY_CODE="1220",
  #CTY_CODE="2010"
#)
 

#to really save time, we want the cumulative value, for the month of December
#GEN_VAL_YR is cumulative imports for consumption by month
#the end of the year value for this is annual total, month=12
#imports_naics<-getCensus(
 # name = "timeseries/intltrade/imports/naics",
  #vars = c("GEN_VAL_YR","YEAR"),
  #time = "from 2016",
  #CTY_CODE="1220",
  #CTY_CODE="2010",
 # MONTH = "12", ## this setting here only gets us year end values #
#)

## I want a make a graph of the top 10 import partners for any given year

## so we will use the method above, but we want all countries
## we also need to screen out some regional codes again
## We also want to get the country names because the numericacodes 
# are not meaningful to non-specialists

#imports_cty_yr<-getCensus(
 # name = "timeseries/intltrade/imports/naics",
  #vars = c("GEN_VAL_YR","YEAR","CTY_CODE","CTY_NAME"),
 # time = "from 2000",
 # MONTH = "12",
 # show_call = TRUE
#)


#head(imports_cty_yr)


# Get all export destinations by country and year
exports_cty_yr <- getCensus(
  name  = "timeseries/intltrade/exports/naics",
  vars  = c("ALL_VAL_YR", "YEAR", "CTY_CODE", "CTY_NAME"),
  time  = "from 2000",
  MONTH = "12",  # December = full year cumulative total
  show_call = TRUE
)

head(exports_cty_yr)


# Filter out regional/aggregation codes — same logic as professor used for imports
exports_cty_yr_clean <- exports_cty_yr %>%
  filter(!(substr(CTY_CODE, 1, 1) == "0" | 
             substr(CTY_CODE, 2, 2) == "X" | 
             substr(CTY_CODE, 1, 1) == "-")) %>%
  mutate(
    ALL_VAL_YR = as.numeric(ALL_VAL_YR) / 1000000000,  # convert to billions
    YEAR = as.numeric(YEAR)
  )

# Get top 10 by year
top10_exports <- exports_cty_yr_clean %>%
  group_by(YEAR) %>%
  slice_max(order_by = ALL_VAL_YR, n = 10, with_ties = FALSE) %>%
  arrange(YEAR, desc(ALL_VAL_YR)) %>%
  group_by(YEAR) %>%
  mutate(rank = row_number()) %>%
  ungroup()

# Check we have 2015 and 2025 data
top10_exports %>% 
  filter(YEAR %in% c(2015, 2025)) %>%
  count(YEAR)
# Filter for 2015 and 2025 only
plot_data <- top10_exports %>%
  filter(YEAR %in% c(2015, 2025))

# Graph: top 10 export destinations for 2015 and 2025
p <- ggplot(plot_data, aes(x = reorder(CTY_NAME, ALL_VAL_YR), 
                           y = ALL_VAL_YR, 
                           fill = factor(YEAR))) +
  geom_col(position = "dodge") +
  coord_flip() +
  scale_fill_brewer(palette = "Paired") +
  labs(
    title    = "Top 10 U.S. Export Destinations: 2015 vs 2025",
    subtitle = "Annual export value in billions of USD",
    x        = NULL,
    y        = "Export Value (billions USD)",
    fill     = "Year"
  ) +
  theme_minimal(base_size = 13)

print(p)

ggsave("top10_us_exports.png", plot = p, width = 10, height = 6, dpi = 150)
cat("Plot saved as top10_us_exports.png\n")

