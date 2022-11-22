library(tidyverse)
library(maps)
source("../source/a4-helpers.R")
library(R.utils)
#Our Data frame
jail <- read.csv("../data/incarceration_trends.csv")

## Test queries ----
#----------------------------------------------------------------------------#
# Simple queries for basic testing
#----------------------------------------------------------------------------#
# Return a simple string
test_query1 <- function() {
  return("Hello world")
}

# Return a vector of numbers
test_query2 <- function(num = 6) {
  v <- seq(1:num)
  return(v)
}

## Section 2  ----
#----------------------------------------------------------------------------#

# Filter jail data for most recent year. We want to filter out missing data
# for prison and jail populations for white and black people
jail_recent <- filter(jail, !is.na(black_prison_pop)) %>%

  #Filtering missing entries
  filter(!is.na(white_prison_pop)) %>%
  filter(!is.na(black_jail_pop)) %>%
  filter(!is.na(white_jail_pop)) %>%
  filter(!is.na(black_pop_15to64)) %>%
  filter(!is.na(white_pop_15to64)) %>%

  #Filter for max year given the previous missing values are filtered
  filter(year == max(year)) %>%
  mutate(
    black_percent =
      (black_jail_pop + black_prison_pop) / black_pop_15to64 * 100,
    white_percent =
      (white_jail_pop + white_prison_pop) / white_pop_15to64 * 100
  ) %>%
  mutate(location = paste0(county_name, ", ", state))
most_recent_year <- max(jail$year)

# Value List
values <- list()
# Value One a for average percent of black prisoners per black people between 15
# and 64 years old in the most recent year
values$avg_black_perc <- round(jail_recent %>% summarize(
  black_percent = mean(black_percent, na.rm = TRUE)
) %>%
  pull(black_percent), digits = 2)

# Value One b for where the highest percent of black prisoners per
# black population is located, and its value
values$highest_black_perc <- round(filter(jail_recent, black_percent ==
  max(black_percent, na.rm = TRUE)) %>%
  pull(black_percent), digits = 2)
values$highest_black_perc_where <- filter(jail_recent, black_percent ==
  max(black_percent, na.rm = TRUE)) %>%
  pull(location)

# Value Two a for average percent of white prisoners per white people between 15
# and 64 years old in the most recent year
values$avg_white_perc <- round(jail_recent %>% summarize(
  white_percent = mean(white_percent, na.rm = TRUE)
) %>%
  pull(white_percent), digits = 2)

# Value Two b for where the highest percent of white prisoners per
# white population is located, and its value
values$highest_white_perc <- round(filter(jail_recent, white_percent ==
  max(white_percent, na.rm = TRUE)) %>%
  pull(white_percent), digits = 2)
values$highest_white_perc_where <- filter(jail_recent, white_percent ==
  max(white_percent, na.rm = TRUE)) %>%
  pull(location)
# Value Three a for the scale comparison between the two percents; we divide
# the black percent with the white percent to get the ratio between the two
values$ratio <- round(values$avg_black_perc / values$avg_white_perc, digits = 2)

# Value Three b for the difference in percentage between the counties with the
# highest percentage of black inmates per population and highest percentage of
# white inmates per population.
values$highest_diff <- values$highest_black_perc - values$highest_white_perc

#----------------------------------------------------------------------------#

## Section 3  ----
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# The following functions will be used to create a bar chart of the total jail
# population in the US by year.
#----------------------------------------------------------------------------#
# This function makes a data frame suitable for creating the visualization
get_year_jail_pop <- function() {

  #First filter missing total jail population entries
  jail_viz_data <- filter(jail, !is.na(total_jail_pop)) %>%

    #group by year
    group_by(year) %>%

    #Add populations
    summarize(total_jail_pop = sum(total_jail_pop))
  return(jail_viz_data)
}

# This function makes the actual chart for the total jail population in the US
# by year
plot_jail_pop_for_us <- function() {
  jail_pop_bar_chart <- ggplot(
    data = get_year_jail_pop(),
    aes(x = year, y = total_jail_pop)
  ) +
    geom_col() +
    scale_y_continuous(labels = scales::comma) +
    xlab("Year") +
    ylab("Total Jail Population") +
    labs(
      subtitle =
        "Increase of Jail Populations in the U.S. (1970-2018)",
      caption = "Figure 1. Chart of the total jail populations between the years 1970 to 2018.
      This shows an increase of the jail population by almost four times in the span of almost five decades."
    )

  return(jail_pop_bar_chart)
}


## Section 4  ----
#----------------------------------------------------------------------------#
# Growth of Prison Population by State
# The following functions help make a visualization for the growth of the
# jail populations by state
#----------------------------------------------------------------------------#
#This function returns us a data frame of
#the jail population given a vector of states we are interested in.
get_jail_pop_by_states <- function(states) {
  jail_state_pop_data <- filter(jail, !is.na(total_jail_pop)) %>%

                        #%in% tells if first argument is contained within
                        #second argument.
                  #Source: https://www.programmingr.com/tutorial/in-operator/
                        filter(state %in% states) %>%

                        #Group by year
                        group_by(year, state) %>%

                        #Sum the total jail population by year and state
                        summarize(total_jail_pop = sum(total_jail_pop))
                        return(jail_state_pop_data)
}

#This function produces the chart for the total jail populations
#given the states of interests
plot_jail_pop_by_states <- function(states) {
  jail_state_pop_chart <- ggplot(
    data = get_jail_pop_by_states(states),
    aes(x = year, y = total_jail_pop)) +
    geom_col() +
    scale_y_continuous(labels = scales::comma) +
    facet_wrap(~state) +
    xlab("Year") +
    ylab("Total Jail Population") +
    labs(
      subtitle =
"Increase of Jail Populations in the states of NY, CA, TX, and FL in the years of 1970 to 2018",
caption = "Figure 2. Chart of the total jail populations between the years 1970 to 2018 by state."
    )
  return(jail_state_pop_chart)
}

## Section 5  ----
#----------------------------------------------------------------------------#
# In this section, the following functions serve to ultimately create a
# visualization that compares the proportion of black inmates relative
# to the black population between 15 to 64 year olds vs the same proportion for
# white inmates as of the most recent date available with both data.
# We also want to see these two variables have a similar or different
# relationship depending on the population of a county (very small, small,
# medium, large, very large).
#
#----------------------------------------------------------------------------#

#Function for creating the data frame that filters missing proportion values,
#and make a distinction for the population of a county.
get_jail_race_data <- function() {
  jail_race_data <- filter(jail, !is.na(white_prison_pop)) %>%

    #Keep filtering missing values for our variables of interest
    filter(!is.na(black_prison_pop)) %>%
    filter(!is.na(white_jail_pop)) %>%
    filter(!is.na(black_jail_pop)) %>%

    #Make new variables
    mutate(
      white_inmate_pop = white_jail_pop + white_prison_pop,
      black_inmate_pop = black_jail_pop + black_prison_pop
    ) %>%
    mutate(location = paste0(county_name, ", ", state)) %>%

    #Create the type of county based on their population. I'll explain in the
    #paragraph why I chose to do a "logarithmic scale" of county populations.
    mutate(population_type = ifelse(total_pop < 25000, "County Pop: < 25,000",
          ifelse(25000 <= total_pop & total_pop < 100000, "County Pop: ≥ 25,000 & < 100K",
          "County Pop: has ≥ 100,000 people")))
  return(jail_race_data)
}

#Function for creating the visualization for the black inmates.
#We will make another plot for the white inmates below this chart.
plot_jail_black_county <- function() {
  jail_black_plot <- ggplot(
    data = get_jail_race_data(),
    aes(x = year, y = black_inmate_pop)
  ) +
    geom_point() +
    scale_y_continuous(labels = scales::comma) +
    facet_wrap(~population_type) +
    xlab("Year") +
ylab("Black Inmate Population") +
    labs(
      subtitle =
        "Relationship between Year and Population of Black Inmates, Grouped by County Population",
caption = "Figure 3a. Chart of the relationship between Year and the Population of Black Inmates. "
    )
  return(jail_black_plot)
}

#Function for creating the visualization for the white inmates.
plot_jail_white_county <- function() {
  jail_white_plot <- ggplot(
    data = get_jail_race_data(),
    aes(x = year, y = white_inmate_pop)
  ) +
    geom_point() +
    scale_y_continuous(labels = scales::comma, limits = c(0, 30000)) +
    facet_wrap(~population_type) +
    xlab("Year") +
    ylab("White Inmate Population") +
    labs(
      subtitle =
        "Relationship between Year and Population of White Inmates, Grouped by County Population",
      caption = "Figure 3b. Chart of the relationship between Year and the Population of White Inmates. "
    )
  return(jail_white_plot)
}

## Section 6  ----
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Load data frames for mapping:

#Map codes give us the abbreviation for states. We need them because
#map_data("county") doesn't have them, but our jail data only has abbreviations.
#So before getting our dataframes, let's make this function to get state codes!
get_codes <- function() {
  map_codes <- read.csv("../source/state_names_and_codes.csv") %>%
    rename(state = State)
  map_codes$state <- str_to_lower(map_codes$state)
  return(map_codes)
}

#We'll need county long and lat data
county_locations <- map_data("county") %>%
  rename(state = region, county = subregion) %>%
  inner_join(get_codes(), by = "state") %>%
  group_by(state, county) %>%
  summarize(long = mean(long), lat = mean(lat), Code) %>%
  unique()


#Dataframe for latinx jail percentages per county
get_county_map_data_frame <- function() {

  #Filter our datafraame for joining map data
  jail_county_map_data <- jail %>% mutate(county = str_to_lower(

    #We also need to str_replace Louisiana since they don't use Counties
    str_replace(str_replace(county_name, " County", ""), " Parish", ""))) %>%
    rename(Code = state) %>%

    #Filter missing data for our variables of interest

    #Note: States like Rhode Island DO NOT have any data for latinx_jail_pop
    filter(!is.na(latinx_jail_pop)) %>%
    filter(!is.na(latinx_pop_15to64)) %>%

    #I found the isZero function from:
    #https://www.rdocumentation.org/packages/R.utils/versions/2.7.0/topics/isZero
    filter(!isZero(latinx_pop_15to64)) %>%
    #Mutate for the latinx proportion variable
    mutate(latinx_jail_perc = latinx_jail_pop / latinx_pop_15to64 * 100) %>%

    #Now join with our county locations.
    inner_join(county_locations, by = c("county", "Code")) %>%

    #filter for max year given our previous operations
    filter(year == max(year))
  return(jail_county_map_data)
}

#Dataframe for latinx jail percentages per state
get_state_map_data_frame <- function() {

  #Use the filters given to us previously
  jail_state_map_data <- get_county_map_data_frame() %>%
    group_by(state) %>%
    summarize(latinx_jail_perc = mean(latinx_jail_perc, na.rm = TRUE),
              year, region, division) %>%
          unique() %>%

          #map_data actually refers to states as regions, so
          #we must rename our actual regions to actual_region
          rename(actual_region = region, region = state) %>%
                      select(actual_region, region, division, latinx_jail_perc) %>%
                      unique() %>%
                      inner_join(map_data("state"))
  return(jail_state_map_data)
}


#Make the Visualizations:

#Function for showing latinx jail percentages by county
get_county_map_chart <- function() {
  #We call the data wrangling function in geom_polygon
  map_chart <- ggplot(map_data("state")) +
    geom_polygon(mapping = aes(long, lat, group = group),
                 color = "white",
                 size = .1) +
    geom_point(data = get_county_map_data_frame(),
               aes(x = long, y = lat,
                   color = region, fill = region,
                   size = latinx_jail_perc),
#I learned about pch and color around the circle,
#which gives a black border around each circle,
#from: https://stackoverflow.com/questions/10437442/place-a-border-around-points
               pch = 21, color = "black") +
    coord_map() +

  #As required, we use a minimalist theme here. I will use the
  #minimalist theme as mentioned in the textbook
  theme_bw() +
    theme(
      axis.line = element_blank(), # remove axis lines
      axis.text = element_blank(), # remove axis labels
      axis.ticks = element_blank(), # remove axis ticks
      axis.title = element_blank(), # remove axis titles
      plot.background = element_blank(), # remove gray background
      panel.grid.major = element_blank(), # remove major grid lines
      panel.grid.minor = element_blank(), # remove minor grid lines
      panel.border = element_blank() # remove border around plot
    ) +
labs(subtitle = "Percent of Latinx Jail Inmates to Latinx Population",
     size = "% of Latinx Jail Inmates to Latinx Population
              Ages 15 to 64",
     fill = "Region",
     caption = "Figure 4. Map of the US with each county containing the percentage of 
     Latinx jail inmates relative to the Latinx Population in the county ages 15 to 64."
         )
  return(map_chart)
}

#Function for showing latinx jail percentages by state
get_state_map_chart <- function() {
  map_chart <- ggplot(get_state_map_data_frame()) +

      coord_map() +
      geom_polygon(mapping = aes(x = long, y = lat, group = group,
                                 color = actual_region,
                                 fill = latinx_jail_perc),
                                  size = 1.25) +
    scale_fill_continuous(low = "white", high = "firebrick") +
          labs(subtitle = "Average Percent of Latinx Jail Inmate to Latinx Population Per State",
          fill = "Mean % of Latinx in Jail Per County Latinx Population
          Ages 15 to 64",
               color = "Divisions",
          caption = "Figure 5. Map of the US with the percent of Latinx Jail Inmate relative to the Latinx population per state. 
          The percentages of every county in the state are averaged so each state has one average percent."
          ) +
      theme_bw() +
      theme(
      axis.line = element_blank(), # remove axis lines
      axis.text = element_blank(), # remove axis labels
      axis.ticks = element_blank(), # remove axis ticks
      axis.title = element_blank(), # remove axis titles
      plot.background = element_blank(), # remove gray background
      panel.grid.major = element_blank(), # remove major grid lines
      panel.grid.minor = element_blank(), # remove minor grid lines
      panel.border = element_blank() # remove border around plot
      )
  return(map_chart)
}
