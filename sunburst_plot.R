#  Create a sunburst plot with population for years 2009-2018
library(tidyverse)
library(plotly)
library(jsonlite)
library(httr)

# for loop to read data for each year from 2009-2018
all_years <- NULL
years <- c("2009", "2010", "2011", "2012", "2013", "2014","2015", "2016", "2017", "2018")
for (i in seq_len(10)) {
  url <- paste0("https://api.census.gov/data/", years[i], "/acs/acs5/")
  all_years <- bind_rows(
    all_years,
    
    GET(url,
        query = list(
          get = "NAME,B01001_001E,B01001_002E,B01001_026E,B01001A_001E,B01001A_002E,B01001A_017E,B01001B_001E,B01001B_002E,B01001B_017E,B01001D_001E,B01001D_002E,B01001D_017E",
          `for` = "state:*"
        )) %>%
      content(as = "text") %>%
      fromJSON(simplifyMatrix = TRUE) %>%
      as.data.frame() %>%
      slice(-1)
    
  )
}
all_years
dim(all_years)

names(all_years) <- c("state", "total", "total_male", "total_female", 
                      "total_white", "total_white_male", "total_white_female", 
                      "total_black", "total_black_male", "total_black_female",
                      "total_asian", "total_asian_male", "total_asian_female", "state_number")

# add year column to "all_years" data frame
year <- c(rep(2009,52),rep(2010,52),rep(2011,52),rep(2012,52),rep(2013,52),rep(2014,52),
          rep(2015,52),rep(2016,52),rep(2017,52),rep(2018,52))
all_years <- all_years %>% mutate(year = year)


total_pop_gender <- all_years %>% select(state, total, total_male, total_female, year)
total_pop_race <- all_years %>% select(state, total,total_white, total_black, total_asian, year)
total_pop_by_race_and_gender <- all_years %>% select(-"state_number")
total_pop <- all_years %>% select(state, total, year) %>% mutate(total=as.numeric(as.character(total)))


# add information about region to data frame
west <- c("Alaska","Arizona","California","Colorado","Hawaii","Idaho","Montana","Nevada","New Mexico",
          "Oregon","Utah","Washington","Wyoming") %>% sort() %>% as.factor()
midwest <- c("Illinois","Indiana","Iowa","Kansas","Michigan","Missouri","Minnesota","Nebraska",
             "North Dakota","Ohio","South Dakota","Wisconsin") %>% sort() %>% as.factor()
south <- c("Alabama","Arkansas","Delaware","Florida","Georgia","Kentucky","Louisiana","Maryland",
           "Mississippi","Oklahoma","North Carolina","South Carolina","Tennessee","Texas","Virginia",
           "West Virginia", "Puerto Rico") %>% sort() %>% as.factor()
northeast <- c("Connecticut","Maine","New Hampshire","Massachusetts","New Jersey","New York",
               "Pennsylvania","Rhode Island","Vermont","District of Columbia") %>% sort()
all_states <- all_years %>% pull(state) %>% unique() %>%  sort() %>% as.factor()

west_df <- data.frame(state=west) %>% mutate(region = "West")
midwest_df <- data.frame(state=midwest) %>% mutate(region = "Midwest")
south_df <- data.frame(state=south) %>% mutate(region = "South")
northeast_df <- data.frame(state=northeast) %>% mutate(region="Northeast") 
regions_df <- rbind(west_df, midwest_df, south_df, northeast_df) %>% mutate(region = as.factor(region))

# add region to total_pop dataframe
total_pop_by_region <- inner_join(total_pop, regions_df, by="state")
total_pop_by_region$state <- as.factor(total_pop_by_region$state)

# create sunburst plot for years 2009 - 2018
names(total_pop_by_region)[2] <- "pop"

bind_rows(
  total_pop_by_region %>%
    group_by(year) %>% 
    select(children = state, parents = region, pop = pop, yr=year),
  total_pop_by_region %>%
    group_by(region,year) %>%
    summarize(pop = sum(pop)) %>%
    transmute(children = region, parents = "United States", pop = pop, yr=year),
  total_pop_by_region %>%
    group_by(year) %>% 
    summarize(pop = sum(pop)) %>%
    transmute(children = "United States", parents = "", pop = pop, yr=year)
) %>%
  plot_ly(
    ids = ~children, 
    labels = ~children, 
    parents = ~parents, 
    values = ~pop, 
    type = "sunburst", 
    branchvalues = "total",
    frame = ~yr)
