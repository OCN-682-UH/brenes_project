### date: 2024/12/03 ###
### creator: Brandon Brenes ###
### last edited: 2024/12/03 ###

### description: shiny app of an interactive analysis of C:N:P stoichiometry using
#data from the Hawaii Ocean Time-series website

### libraries ###

library(here)
library(shiny)
library(dplyr)
library(readr)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(rsconnect)


### load data ###
carbon_data <- read.csv(here("data", "carbon_data.csv"))
nitrogen_data <- read.csv(here("data", "nitrogen_data.csv"))
phosphorus_data <- read.csv(here("data", "phosphorus_data.csv"))

# display #

head(carbon_data)
head(nitrogen_data)
head(phosphorus_data)


### manipulate data ###

# join the carbon_data, nitrogen_data, phosphorus_data sets by matching them up
# by cruise number- use lubridate and join.
#get rid of unneeded duplicate columns 'julian' and 'date'

CNP_data <- carbon_data %>%
  inner_join(nitrogen_data %>% select(-date, -julian), by = "crn") %>%
  inner_join(phosphorus_data %>% select(-date, -julian), by = "crn") 


## data manipulation cont'd ##

# use date and mdy() to convert date to mdy, create a seasons column using lubridate and month()

CNP_data <- CNP_data %>%
  mutate(
    date = mdy(date),  # convert date to mdy type
    season = case_when(# create a new 'season' column based on the month
      month(date) %in% c(3, 4, 5) ~ "Spring",
      month(date) %in% c(6, 7, 8) ~ "Summer",
      month(date) %in% c(9, 10, 11) ~ "Fall",
      month(date) %in% c(12, 1, 2) ~ "Winter"
    ) # assign seasons to months
  )

# display #
head(CNP_data)

### output 1: HEATPLOT  ###

ggplot(CNP_data, aes(x = c_deep_mean, y = n_deep_mean)) +
  geom_tile(aes(fill = ..count..), stat = "bin2d", bins = 30) +  # create a binned heatmap
  scale_fill_gradient(low = "white", high = "red2") +  # color gradient for heat intensity
  labs(x = "C (Deep Mean)", y = "N (Deep Mean)", title = "C:N Heatmap in the Deep Ocean") +
  theme_minimal()




### manipulate data for box plot ###

# use a custom color scheme close to your preference but more polished
custom_colors <- c("Carbon" = "#F9C74F",   # Warm yellow
                   "Nitrogen" = "#F94144", # Vibrant red
                   "Phosphorus" = "#577590") # Muted blue


# prep deep data

deep_data <- CNP_data %>%
  select(c_deep_mean, n_deep_mean, p_deep_mean) %>% #select for each cnp value for deep mean
  pivot_longer(cols = everything(), # pivot these all longer to make element and value column
               names_to = "Element", 
               values_to = "Value") %>%
  mutate(Element = recode(Element, # elements assigned new labels
                          "c_deep_mean" = "Carbon",
                          "n_deep_mean" = "Nitrogen",
                          "p_deep_mean" = "Phosphorus"))


# prep shallow data

shallow_data <- CNP_data %>%
  select(c_shallow_mean, n_shallow_mean, p_shallow_mean) %>% # select for each cnp value for shallow mean
  pivot_longer(cols = everything(), # pivot these all longer to make element and value column
               names_to = "Element", 
               values_to = "Value") %>%
  mutate(Element = recode(Element, # elements assigned new labels
                          "c_shallow_mean" = "Carbon",
                          "n_shallow_mean" = "Nitrogen",
                          "p_shallow_mean" = "Phosphorus"))


### output 2: BOX PLOT ###

ggplot(deep_data, aes(x = Element, y = Value, fill = Element)) +
  geom_boxplot() +
  facet_wrap(~Element, scales = "free") + # facet by element, scale free of each other
  labs(
    title = "Box Plot of Carbon, Nitrogen, and Phosphorus (Deep Depth)",
    x = "Element",
    y = "Value (umol/kg)"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_blank(),        # remove facet titles
    axis.text.x = element_blank(),       # remove x-axis text
    axis.ticks.x = element_blank(),      # remove x-axis ticks 
    panel.border = element_rect(color = "black", fill = NA, size = 1), # black borders around facets
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5)  # center the title
  ) +
  scale_fill_manual(values = custom_colors)



########## Full Shiny app  #########

# load data->data manipulation described above

#### load data ####
carbon_data <- read.csv(here("data", "carbon_data.csv"))
nitrogen_data <- read.csv(here("data", "nitrogen_data.csv"))
phosphorus_data <- read.csv(here("data", "phosphorus_data.csv"))

##### data manipulation# ###
CNP_data <- carbon_data %>%
  inner_join(nitrogen_data %>% select(-date, -julian), by = "crn") %>%
  inner_join(phosphorus_data %>% select(-date, -julian), by = "crn")

CNP_data <- CNP_data %>%
  mutate(
    date = mdy(date),
    season = case_when(
      month(date) %in% c(3, 4, 5) ~ "Spring",
      month(date) %in% c(6, 7, 8) ~ "Summer",
      month(date) %in% c(9, 10, 11) ~ "Fall",
      month(date) %in% c(12, 1, 2) ~ "Winter"
    )
  )


###### user interface ######

ui <- fluidPage(
  # add custom css for spacing between sections
  tags$style(HTML("
    .layout-spacing {
      margin-bottom: 30px; /* space between sections */
    }
  ")),
  
  # app title #
  titlePanel("c:n:p of particles at station aloha"),
  
  ## section for heatmap select input ##
  
  div(
    class = "layout-spacing", # spacing between sections
    sidebarLayout(
      sidebarPanel(
        selectInput("element1", "select first element (x-axis)", 
                    choices = c("carbon" = "c", "nitrogen" = "n", "phosphorus" = "p")), # x-axis dropdown
        selectInput("element2", "select second element (y-axis)", 
                    choices = c("carbon" = "c", "nitrogen" = "n", "phosphorus" = "p")), # y-axis dropdown
        selectInput("depth_heatmap", "select depth", 
                    choices = c("shallow" = "shallow", "deep" = "deep")), # depth level dropdown
        selectInput("season_heatmap", "select season", 
                    choices = c("all", na.omit(unique(CNP_data$season)))) # season dropdown
      ),
      
      
 ## main panel to display the heatmap plot ##
 
      mainPanel(
        plotOutput("heatmap_plot") # display heatmap
      )
    )
  ),
  
 ## section for box plot input selection ##
 
  div(
    class = "layout-spacing", # spacing between sections
    sidebarLayout(
      sidebarPanel(
        selectInput("depth", "select depth", choices = c("shallow", "deep")), # depth dropdown
        selectInput("season", "select season", choices = c("all", na.omit(unique(CNP_data$season)))) # season dropdown
      ),
      
## main panel to display the box plot ##

      mainPanel(
        plotOutput("box_plot") # display box plot
      )
    )
  ),
  
## section for c:n:p averages input selection ##

  div(
    class = "layout-spacing", # spacing between sections
    sidebarLayout(
      sidebarPanel(
        selectInput("depth_avg", "select depth", choices = c("shallow", "deep")), # depth dropdown
        selectInput("season_avg", "select season", choices = c("all", na.omit(unique(CNP_data$season)))) # season dropdown
      ),
      
## main panel to display the computed averages ##

      mainPanel(
        uiOutput("avg_display") # display averages
      )
    )
  )
)



#### server ####

server <- function(input, output) {
  
  ## prepare data for box plot ##
  filtered_data_boxplot <- reactive({
    if (input$depth == "shallow") {
      data <- CNP_data %>%
        select(c_shallow_mean, n_shallow_mean, p_shallow_mean, season) %>% # select shallow data
        pivot_longer(
          cols = starts_with("c_") | starts_with("n_") | starts_with("p_"), 
          names_to = "element", values_to = "value" # reshape for plotting
        ) %>%
        mutate(element = recode(element,
                                "c_shallow_mean" = "carbon",
                                "n_shallow_mean" = "nitrogen",
                                "p_shallow_mean" = "phosphorus")) # rename for clarity
    } else {
      data <- CNP_data %>%
        select(c_deep_mean, n_deep_mean, p_deep_mean, season) %>% # select deep data
        pivot_longer(
          cols = starts_with("c_") | starts_with("n_") | starts_with("p_"), 
          names_to = "element", values_to = "value"
        ) %>%
        mutate(element = recode(element,
                                "c_deep_mean" = "carbon",
                                "n_deep_mean" = "nitrogen",
                                "p_deep_mean" = "phosphorus"))
    }
    
    if (input$season != "all") {
      data <- data %>% filter(season == input$season) # filter by selected season
    }
    
    return(data) 
    
### return prepared data ###
    
  })
  
  
#### OUTPUT 2: boxplot rendered #####
  
  output$box_plot <- renderPlot({
    data <- filtered_data_boxplot() # get reactive data
    
    ggplot(data, aes(x = element, y = value, fill = element)) +
      geom_boxplot() + # create box plot
      facet_wrap(~element, scales = "free", strip.position = "bottom") + # organize by element
      labs(
        title = paste("carbon, nitrogen, and phosphorus -", input$depth, "depth", input$season, "season"),
        x = "",
        y = "umol/kg"
      ) +
      theme_minimal() +
      scale_fill_manual(values = c("carbon" = "#f9c74f", "nitrogen" = "#f94144", "phosphorus" = "#577590")) +
      theme(
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        legend.position = "none", # remove legend
        axis.text.x = element_text(face = "bold", size = 12),
        strip.background = element_rect(color = "black", fill = "white", size = 1.5), # add facet border
        strip.text.x = element_blank(), # hide facet labels
        panel.border = element_rect(color = "black", fill = NA, size = 1) # add plot border
      )
  })

  # prepare data for heatmap
  filtered_data_heatmap <- reactive({
    element1_column <- paste0(tolower(input$element1), "_", tolower(input$depth_heatmap), "_mean") # get x-axis column
    element2_column <- paste0(tolower(input$element2), "_", tolower(input$depth_heatmap), "_mean") # get y-axis column
    
    CNP_data_filtered <- CNP_data
    if (input$season_heatmap != "all") {
      CNP_data_filtered <- CNP_data_filtered %>% filter(season == input$season_heatmap) # filter by season
    }
    
    CNP_data_filtered <- CNP_data_filtered %>%
      select(crn, season, 
             element1_value = !!sym(element1_column), 
             element2_value = !!sym(element2_column)) # select relevant columns
    
    return(CNP_data_filtered) # return prepared data
  })
  
  
#### OUTPUT 1: heatmap rendered #####
  
  output$heatmap_plot <- renderPlot({
    data <- filtered_data_heatmap()
    if (nrow(data) == 0) {
      return(NULL) # handle case with no data
    }
    
    x_limits <- range(CNP_data[[paste0(tolower(input$element1), "_deep_mean")]], 
                      CNP_data[[paste0(tolower(input$element1), "_shallow_mean")]], 
                      na.rm = TRUE)
    
    y_limits <- range(CNP_data[[paste0(tolower(input$element2), "_deep_mean")]], 
                      CNP_data[[paste0(tolower(input$element2), "_shallow_mean")]], 
                      na.rm = TRUE)
    
    ggplot(data, aes(x = element1_value, y = element2_value)) + 
      geom_tile(aes(fill = ..count..), stat = "bin2d", bins = 30) + 
      scale_fill_gradient(low = "white", high = "red2") + 
      labs(x = paste(input$element1, "(umol/kg)"), y = paste(input$element2, "(umol/kg)"), 
           title = paste(input$element1, "vs", input$element2, input$depth_heatmap, "depth", input$season_heatmap, "season")) +
      theme_minimal() +
      scale_x_continuous(limits = x_limits) + 
      scale_y_continuous(limits = y_limits) +
      theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))
  })
  
#### OUTPUT 3: calculate averages for c:n:p rendered #####

  output$avg_display <- renderUI({
    if (input$depth_avg == "shallow") {
      data_avg <- CNP_data %>% select(c_shallow_mean, n_shallow_mean, p_shallow_mean, season) # shallow data
    } else {
      data_avg <- CNP_data %>% select(c_deep_mean, n_deep_mean, p_deep_mean, season) # deep data
    }
    
    if (input$season_avg != "all") {
      data_avg <- data_avg %>% filter(season == input$season_avg) # filter by season
    }
    
    
### average CNP equation for given depth and seasons ###
    # normalize to p so divide all by p value
    # no NAs
    
    avg_carbon <- mean(data_avg[[paste0("c_", tolower(input$depth_avg), "_mean")]], na.rm = TRUE) / 
      mean(data_avg[[paste0("p_", tolower(input$depth_avg), "_mean")]], na.rm = TRUE) # calculate normalized carbon
    avg_nitrogen <- mean(data_avg[[paste0("n_", tolower(input$depth_avg), "_mean")]], na.rm = TRUE) / 
      mean(data_avg[[paste0("p_", tolower(input$depth_avg), "_mean")]], na.rm = TRUE) # calculate normalized nitrogen
    avg_phosphorus <- 1 # phosphorus normalized to itself is always 1
    
    avg_carbon <- round(avg_carbon, 0) # round to nearest 1
    avg_nitrogen <- round(avg_nitrogen, 0) 
    
    custom_colors <- c("carbon" = "#f9c74f", "nitrogen" = "#f94144", "phosphorus" = "#577590") # colors

    
    # text ouput for average CNP #
    #looks like a lot but it is just a fancy way to bold, size and assign colors to output
    
    avg_text <- paste(
      "<div style='text-align:center;'>",
      "<h3 style='font-size:24px; font-weight:bold;'>avg. carbon : nitrogen : phosphorus</h3>",
      "<span style='font-size:20px; font-weight:bold; color:", custom_colors["carbon"], ";'>", avg_carbon, "</span>",
      "<span style='font-size:20px; font-weight:bold; color:black;'> : </span>",
      "<span style='font-size:20px; font-weight:bold; color:", custom_colors["nitrogen"], ";'>", avg_nitrogen, "</span>",
      "<span style='font-size:20px; font-weight:bold; color:black;'> : </span>",
      "<span style='font-size:20px; font-weight:bold; color:", custom_colors["phosphorus"], ";'>1</span>",
      "</div>"
    )
    
    HTML(avg_text) # render html 
  })
}

#### run the app ####
shinyApp(ui, server)
