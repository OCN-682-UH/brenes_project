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
# by cruise number

## Join data sets to make combined CNP data set- Use lubridate and join.
#get rid of unneeded duplicate columns 'julian' and 'date'

CNP_data <- carbon_data %>%
  inner_join(nitrogen_data %>% select(-date, -julian), by = "crn") %>%
  inner_join(phosphorus_data %>% select(-date, -julian), by = "crn") 

# data manipulation cont'd #

# create a seasons column using lubridate

CNP_data <- CNP_data %>%
  mutate(
    date = mdy(date),  # convert date to mdy type
    season = case_when(
      month(date) %in% c(3, 4, 5) ~ "Spring",
      month(date) %in% c(6, 7, 8) ~ "Summer",
      month(date) %in% c(9, 10, 11) ~ "Fall",
      month(date) %in% c(12, 1, 2) ~ "Winter"
    )# assign seasons to months
  )

# display #
head(CNP_data)

## output 1: ##

ggplot(CNP_data, aes(x = c_deep_mean, y = n_deep_mean)) +
  geom_tile(aes(fill = ..count..), stat = "bin2d", bins = 30) +  # Create a binned heatmap
  scale_fill_gradient(low = "white", high = "red2") +  # Color gradient for heat intensity
  labs(x = "C (Deep Mean)", y = "N (Deecnpp Mean)", title = "C:N Heatmap in the Deep Ocean") +
  theme_minimal()


## output 2: ##
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Prepare data
deep_data <- CNP_data %>%
  select(c_shallow_mean, n_deep_mean, p_deep_mean) %>%
  pivot_longer(cols = everything(), 
               names_to = "Element", 
               values_to = "Value") %>%
  mutate(Element = recode(Element,
                          "c_shallow_mean" = "Carbon",
                          "n_deep_mean" = "Nitrogen",
                          "p_deep_mean" = "Phosphorus"))

# Use a custom color scheme close to your preference but more polished
custom_colors <- c("Carbon" = "#F9C74F",   # Warm yellow
                   "Nitrogen" = "#F94144", # Vibrant red
                   "Phosphorus" = "#577590") # Muted blue

# Prepare shallow data
shallow_data <- CNP_data %>%
  select(c_shallow_mean, n_shallow_mean, p_shallow_mean) %>%
  pivot_longer(cols = everything(), 
               names_to = "Element", 
               values_to = "Value") %>%
  mutate(Element = recode(Element,
                          "c_shallow_mean" = "Carbon",
                          "n_shallow_mean" = "Nitrogen",
                          "p_shallow_mean" = "Phosphorus"))


# Create the faceted box plot
ggplot(deep_data, aes(x = Element, y = Value, fill = Element)) +
  geom_boxplot() +
  facet_wrap(~Element, scales = "free") + # Facet by Element with independent scales
  labs(
    title = "Box Plot of Carbon, Nitrogen, and Phosphorus (Deep Depth)",
    x = "Element",
    y = "Value (umol/kg)"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_blank(),        # Remove facet titles
    axis.text.x = element_blank(),       # Remove x-axis text inside each facet
    axis.ticks.x = element_blank(),      # Remove x-axis ticks inside each facet
    panel.border = element_rect(color = "black", fill = NA, size = 1), # Add black borders around facets
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5)  # Center the title
  ) +
  scale_fill_manual(values = custom_colors)

### Full Shiny app combining C:N:P averages and other visualizations ###

# Load data
carbon_data <- read.csv(here("data", "carbon_data.csv"))
nitrogen_data <- read.csv(here("data", "nitrogen_data.csv"))
phosphorus_data <- read.csv(here("data", "phosphorus_data.csv"))

# Data manipulation
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


ui <- fluidPage(
  # Add custom CSS for spacing between sidebarLayout sections
  tags$style(HTML("
    .layout-spacing {
      margin-bottom: 30px; /* Adds space between sections */
    }
  ")),
  
  # Title Panel
  titlePanel("C:N:P of Particles at Station ALOHA"),
  
  # Sidebar Layout for Heatmap
  div(
    class = "layout-spacing",  # Add spacing between sections
    sidebarLayout(
      sidebarPanel(
        selectInput("element1", "Select First Element (x-axis)", choices = c("Carbon" = "C", "Nitrogen" = "N", "Phosphorus" = "P")),
        selectInput("element2", "Select Second Element (y-axis)", choices = c("Carbon" = "C", "Nitrogen" = "N", "Phosphorus" = "P")),
        selectInput("depth_heatmap", "Select Depth", choices = c("Shallow" = "Shallow", "Deep" = "Deep")),
        selectInput("season_heatmap", "Select Season", choices = c("All", na.omit(unique(CNP_data$season))))
      ),
      mainPanel(
        plotOutput("heatmap_plot")
      )
    )
  ),
  
  # Sidebar Layout for Box Plot
  div(
    class = "layout-spacing",
    sidebarLayout(
      sidebarPanel(
        selectInput("depth", "Select Depth", choices = c("Shallow", "Deep")),
        selectInput("season", "Select Season", choices = c("All", na.omit(unique(CNP_data$season))))
      ),
      mainPanel(
        plotOutput("box_plot")
      )
    )
  ),
  
  # Sidebar Layout for C:N:P Averages
  div(
    class = "layout-spacing",
    sidebarLayout(
      sidebarPanel(
        selectInput("depth_avg", "Select Depth", choices = c("Shallow", "Deep")),
        selectInput("season_avg", "Select Season", choices = c("All", na.omit(unique(CNP_data$season))))
      ),
      mainPanel(
        uiOutput("avg_display")  # Dynamic display for C:N:P averages
      )
    )
  )
)


# Server
server <- function(input, output) {
  
  # Reactive dataset for Box Plot
  filtered_data_boxplot <- reactive({
    if (input$depth == "Shallow") {
      data <- CNP_data %>%
        select(c_shallow_mean, n_shallow_mean, p_shallow_mean, season) %>%
        pivot_longer(cols = starts_with("c_") | starts_with("n_") | starts_with("p_"), 
                     names_to = "Element", values_to = "Value") %>%
        mutate(Element = recode(Element,
                                "c_shallow_mean" = "Carbon",
                                "n_shallow_mean" = "Nitrogen",
                                "p_shallow_mean" = "Phosphorus"))
    } else {
      data <- CNP_data %>%
        select(c_deep_mean, n_deep_mean, p_deep_mean, season) %>%
        pivot_longer(cols = starts_with("c_") | starts_with("n_") | starts_with("p_"), 
                     names_to = "Element", values_to = "Value") %>%
        mutate(Element = recode(Element,
                                "c_deep_mean" = "Carbon",
                                "n_deep_mean" = "Nitrogen",
                                "p_deep_mean" = "Phosphorus"))
    }
    
    if (input$season != "All") {
      data <- data %>% filter(season == input$season)
    }
    
    return(data)
  })
  
  output$box_plot <- renderPlot({
    data <- filtered_data_boxplot()
    
    ggplot(data, aes(x = Element, y = Value, fill = Element)) +
      geom_boxplot() +
      facet_wrap(~Element, scales = "free", strip.position = "bottom") +  # Move facet labels to the bottom
      labs(
        title = paste("Carbon, Nitrogen, and Phosphorus -", input$depth, "Depth", input$season, "Season"),
        x = "",
        y = "umol/kg"
      ) +
      theme_minimal() +
      scale_fill_manual(values = c("Carbon" = "#F9C74F", "Nitrogen" = "#F94144", "Phosphorus" = "#577590")) +
      theme(
        plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
        legend.position = "none",  # Remove legend
        axis.text.x = element_text(face = "bold", size = 12),
        strip.background = element_rect(color = "black", fill = "white", size = 1.5),  # Add box around facet labels
        strip.text.x = element_blank(),  # Hide facet labels
        panel.border = element_rect(color = "black", fill = NA, size = 1)  # Add border around each facet
      )
  })

  # Reactive dataset for Heatmap
  filtered_data_heatmap <- reactive({
    element1_column <- paste0(tolower(input$element1), "_", tolower(input$depth_heatmap), "_mean")
    element2_column <- paste0(tolower(input$element2), "_", tolower(input$depth_heatmap), "_mean")
    
    CNP_data_filtered <- CNP_data
    if (input$season_heatmap != "All") {
      CNP_data_filtered <- CNP_data_filtered %>% filter(season == input$season_heatmap)
    }
    
    CNP_data_filtered <- CNP_data_filtered %>%
      select(crn, season, 
             element1_value = !!sym(element1_column), 
             element2_value = !!sym(element2_column))
    
    return(CNP_data_filtered)
  })
  
  # Output: Heatmap for selected elements
  output$heatmap_plot <- renderPlot({
    data <- filtered_data_heatmap()
    
    if (nrow(data) == 0) {
      return(NULL)
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
           title = paste(input$element1, "vs", input$element2, input$depth_heatmap, "Depth", input$season_heatmap, "Season")) +
      theme_minimal() +
      scale_x_continuous(limits = x_limits) + 
      scale_y_continuous(limits = y_limits) +
      theme(plot.title = element_text(face = "bold", size = 14, hjust = 0.5))
  })
  
  output$avg_display <- renderUI({
    # Filter the data based on the selected depth and season
    if (input$depth_avg == "Shallow") {
      data_avg <- CNP_data %>%
        select(c_shallow_mean, n_shallow_mean, p_shallow_mean, season)
    } else {
      data_avg <- CNP_data %>%
        select(c_deep_mean, n_deep_mean, p_deep_mean, season)
    }
    
    # Filter by season if not "All"
    if (input$season_avg != "All") {
      data_avg <- data_avg %>% filter(season == input$season_avg)
    }
    
    # Calculate averages for C, N, and P normalized to Phosphorus
    avg_carbon <- mean(data_avg[[paste0("c_", tolower(input$depth_avg), "_mean")]], na.rm = TRUE) / 
      mean(data_avg[[paste0("p_", tolower(input$depth_avg), "_mean")]], na.rm = TRUE)
    
    avg_nitrogen <- mean(data_avg[[paste0("n_", tolower(input$depth_avg), "_mean")]], na.rm = TRUE) / 
      mean(data_avg[[paste0("p_", tolower(input$depth_avg), "_mean")]], na.rm = TRUE)
    
    avg_phosphorus <- mean(data_avg[[paste0("p_", tolower(input$depth_avg), "_mean")]], na.rm = TRUE) / 
      mean(data_avg[[paste0("p_", tolower(input$depth_avg), "_mean")]], na.rm = TRUE)  # P normalized to P is 1
    
    # Round the results to the nearest whole number
    avg_carbon <- round(avg_carbon, 0)
    avg_nitrogen <- round(avg_nitrogen, 0)
    avg_phosphorus <- round(avg_phosphorus, 0)
    
    # Define the custom colors
    custom_colors <- c("Carbon" = "#F9C74F",   # Warm yellow
                       "Nitrogen" = "#F94144", # Vibrant red
                       "Phosphorus" = "#577590") # Muted blue
    
    # Create styled HTML output for centered layout
    avg_text <- paste(
      "<div style='text-align:center;'>",
      "<h3 style='font-size:24px; font-weight:bold;'>Avg. Carbon : Nitrogen : Phosphorus</h3>",
      "<span style='font-size:20px; font-weight:bold; color:", custom_colors["Carbon"], ";'>", avg_carbon, "</span>",
      "<span style='font-size:20px; font-weight:bold; color:black;'> : </span>",
      "<span style='font-size:20px; font-weight:bold; color:", custom_colors["Nitrogen"], ";'>", avg_nitrogen, "</span>",
      "<span style='font-size:20px; font-weight:bold; color:black;'> : </span>",
      "<span style='font-size:20px; font-weight:bold; color:", custom_colors["Phosphorus"], ";'>", avg_phosphorus, "</span>",
      "</div>"
    )
    
    # Render the HTML-formatted text
    HTML(avg_text)
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)

