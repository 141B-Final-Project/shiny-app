library(tidyverse)
library(plotly)
library(jsonlite)
library(httr)
library(choroplethr)
library(choroplethrMaps)
library(data.table)

############################## CODE FOR GETTING COUNTY DATA #################################################
# for loop to read data for all counties each year from 2009-2018 ###########################################
all_years_county <- NULL
years <- c("2009", "2010", "2011", "2012", "2013", "2014","2015", "2016", "2017", "2018")
for (i in seq_len(10)) {
    url <- paste0("https://api.census.gov/data/", years[i], "/acs/acs5/")
    all_years_county <- bind_rows(
        all_years_county,
        
        GET(url,
            query = list(
                get = "NAME,B01001_001E,B01001_002E,B01001_026E,B01001A_001E,B01001A_002E,B01001A_017E,B01001B_001E,B01001B_002E,B01001B_017E,B01001D_001E,B01001D_002E,B01001D_017E",
                `for` = "county:*"
            )) %>%
            content(as = "text") %>%
            fromJSON(simplifyMatrix = TRUE) %>%
            as.data.frame() %>%
            slice(-1)
        
    )
}

all_years_county <- all_years_county %>% 
    mutate(state_county_fips = paste0(V14, V15))
year <- c(rep(2009,3221),rep(2010,3221),rep(2011,3221),rep(2012,3221),rep(2013,3221),rep(2014,3220),
          rep(2015,3220),rep(2016,3220),rep(2017,3220),rep(2018,3220))
all_years_county <- all_years_county %>% mutate(year = year)

names(all_years_county) <- c("county_name", "total_population", "total_male", "total_female", 
                             "total_white", "total_white_male", "total_white_female", 
                             "total_black", "total_black_male", "total_black_female",
                             "total_asian", "total_asian_male", "total_asian_female", 
                             "state_number", "county_fips", "state_county_fips", "year"
)

all_years_county <- all_years_county %>%
    mutate(
        pct_total_white = as.numeric(all_years_county$total_white)/as.numeric(all_years_county$total_population),
        pct_total_black = as.numeric(all_years_county$total_black)/as.numeric(all_years_county$total_population),
        pct_total_asian = as.numeric(all_years_county$total_asian)/as.numeric(all_years_county$total_population)
    )

###################################### Prepare data for Sunburst Plot #######################################
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
names(total_pop_race)[3:5] <- c("White", "Black", "Asian")
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
total_pop_by_region <- inner_join(total_pop_race, regions_df, by="state")
total_pop_by_region$state <- as.factor(total_pop_by_region$state)

# create sunburst plot for years 2009 - 2018
names(total_pop_by_region)[2] <- "All"

# divide sunburst plot based on races
races <- c("All","White", "Black", "Asian")


#################################### R SHINY APPLICATION UI #################################################
# plots with nav bar
library(shiny)
ui <- navbarPage("Population Distribution",
           tabPanel("Race by Counties",
                    sidebarLayout(
                        sidebarPanel(
                            selectInput("val", "Select a variable", 
                                    choices = list("Total Population"="total_population","Percent White"="pct_total_white", "Percent Black"="pct_total_black", "Percent Asian"="pct_total_asian"), selected="Percent White"
                            ),
                            selectInput("state", "View a State", choices = list("None","Alabama","Alaska","Arizona","Arkansas","California","Colorado","Connecticut","Delaware","Florida","Georgia","Hawaii","Idaho","Illinois","Indiana","Iowa","Kansas","Kentucky","Louisiana","Maine","Maryland","Massachusetts","Michigan","Minnesota","Mississippi","Missouri","Montana","Nebraska","Nevada","New Hampshire","New Jersey","New Mexico","New York","North Carolina","North Dakota","Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island","South Carolina","South Dakota","Tennessee","Texas","Utah","Vermont","Virginia","Washington","West Virginia","Wisconsin","Wyoming"), selected="Alabama"),
                            selectInput('year','Select a Year', years, selected=years[1]),
                            downloadButton("downloadData", "Download")
                        ),
                        mainPanel(
                            plotOutput("county_map"),
                            tableOutput("table")
                        )
                    )
           ),
           tabPanel("Race by State and Region",
                    mainPanel(
                        selectInput('race','Select a Race', races, selected = races[1]),
                        plotlyOutput('plot')
                    )
           )

)

############################### R SHINY APPLICATION UI SERVER ##############################################
server <- function(input, output, session) {
    
    # Map
    output$county_map <- renderPlot({
        value <- as.numeric(all_years_county[, input$val])
        region <- as.numeric(all_years_county$state_county_fips)
        years <- all_years_county$year
        map_data <- as.data.frame(cbind(value,region,years))
        
        if(input$state !="None"){
            county_choropleth(subset(map_data, map_data$years == input$year), state_zoom=tolower(input$state))
        }
        
    })
    
    # Data table 
    output$table <- renderTable({
        value_dt <- as.numeric(subset(all_years_county, all_years_county$county_name %like% input$state & all_years_county$year == input$year)[,input$val])
        name_dt <- subset(all_years_county, all_years_county$county_name %like% input$state & all_years_county$year == input$year)$county_name
        
        if(input$state !="None"){
            dt <- as.data.frame(cbind(name_dt, value_dt))
            names(dt) <- c("County, State", input$val)
            dt
        }
        
    })
    
    # Download button
    output$downloadData <- downloadHandler(
        filename = function() {
            paste(input$state, ".csv", sep = "")
        },
        content = function(file) {
            value_dt <- as.numeric(subset(all_years_county, all_years_county$county_name %like% input$state & all_years_county$year == input$year)[,input$val])
            name_dt <- subset(all_years_county, all_years_county$county_name %like% input$state & all_years_county$year == input$year)$county_name
            if(input$state !="None"){
                dt <- as.data.frame(cbind(name_dt, value_dt))
                names(dt) <- c("County, State", input$val)
                dt
            }
            file_data <- dt
            
            write.csv(file_data, file, row.names = FALSE)
        }
    )
    
    # suburst plot data retreival
    sunburst_plot_data <- reactive({
        
        total_pop_by_region2 <- total_pop_by_region %>% 
            select("state",input$race,"year","region") 
        race_selected <- input$race
        
        total_pop_by_region2[,race_selected] <- as.numeric(as.character(total_pop_by_region2[,race_selected]))
        
        total_pop_by_region2 <- total_pop_by_region2 %>% mutate(pop = total_pop_by_region2[,race_selected])
        
        total_pop_by_region2 <-  total_pop_by_region2 %>% select("state","pop","year","region")
        
        bind_rows(
            total_pop_by_region2 %>%
                group_by(year) %>% 
                select(children = state, parents = region, pop = pop, yr=year),
            total_pop_by_region2 %>%
                group_by(region,year) %>%
                summarize(pop = sum(pop)) %>%
                transmute(children = region, parents = "US", pop = pop, yr=year),
            total_pop_by_region2 %>%
                group_by(year) %>% 
                summarize(pop = sum(pop)) %>%
                transmute(children = "US", parents = "", pop = pop, yr=year)
        )
    })
    
    # sunburst plot
    output$plot <- renderPlotly(
        plot1 <- plot_ly(sunburst_plot_data(),
                         ids = ~children, 
                         labels = ~children, 
                         parents = ~parents, 
                         values = ~pop, 
                         type = "sunburst", 
                         branchvalues = "total",
                         frame = ~yr)
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
