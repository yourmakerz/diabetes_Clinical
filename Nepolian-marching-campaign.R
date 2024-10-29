# Load required libraries
library(shiny)
library(leaflet)
library(dplyr)
library(readr)

# Load-datasets
cities <- read_csv("./data/minard-cities.csv") %>%
  filter(city != "" & !is.na(city))
troops <- read_csv("./data/minard-troops.csv")
weather <- read_csv("./data/minard-temp.csv") %>%
  filter(!is.na(temp))

# Prepare merged data
merged_data <- troops %>%
  left_join(weather, by = "long") %>%
  filter(!is.na(days))

#merg data for weather for iteration 4
weather_cities <- inner_join(cities, weather, by = "long")

# creating situations of temperature color for iteration 3
temperature_color <- function(temp) {
  case_when(
    temp >= 0 ~ "white",      #white color
    temp >= -10 ~ "#cce4ff",  #Light blue (70% white, 30% blue)
    temp >= -20 ~ "#99cfff",  #Pale blue (60% blue, 40% white)
    temp < -20 ~ "#0004ff"    #Blue (90% blue, 10% white)
  )
  
}

# campaign time line for iteration 4, reorder the period in sliding
#campaign_timeline <- c(
#  "June 1812" = 1,
#  "July 1812" = 2,
#  "August 1812" = 3,
#  "September 1812" = 4,
#  "October 1812" = 5,  # Reaches Moscow
#  "November 1812" = 6,
#  "December 1812" = 7   # Retreat back to Wilna
#)

# split the timeline
#advance_periods <- c(1, 2, 3, 4, 5)  # June to October (Advance)
#retreat_periods <- c(6, 7)           # November to December (Retreat)



# Define reactive value for temperature visibility
temp_visible <- reactiveVal(FALSE)

#iteration descriptions and back ground explanations
iteration_descriptions <- list(
  "iteration1" = HTML("<h4>Iteration 1: Background History</h4>
                       
<p>This is the Minard map which is developed by Joseph Minard, visualization of Napolean’s marching toward Moscow, Russia in 1812. 
Napolean’s wanted to force other European countries to stop trade with the UK so that he could invade them. But Czar Alexander from 
Russia saw that Napoleon was becoming powerful and he didn’t want to participate with Napoleon this is why he was furious and 
he gathered over 400,000 troops to attack Russia in June of year 1812.<p>
<p>During the six-month campaign, the Russian forces strategically retreated. 
They burning crops and towns to prevent the French from looting. Despite reaching Moscow in October, 
Napoleon found the city abandoned and without supplies and forcing him to order a retreat. 
As winter set in, extreme cold, hunger, and disease decimated his troops. From the original force, only 
a small fraction survived the journey back to France and this marking the campaign as a catastrophic failure.</p>
<h4>Marching map</h4>
<p>This is the overall marching of the troop from Kowno to Moscow then back to Kowno city. The red line represents the advance marhing and black line is for retreating. The weight of the line represents the troop size.
                      "),
  "iteration2" = HTML("<h4>Iteration 2: Cities & Survival counts</h4>
<p>Each city is marked with either red or black circles with denoting the number of survivors at that point. 
This visualization is the representing of how the army diminish gradually in size as it moved further into Russia and eventually retreated.<p>
<h4>'March Toward Moscow'</h4>
<p>Red Circles represent the cities reached during the advance march towards Moscow. 
For example, Kowno, Smorgoni, and Witebsk are labeled with significant numbers, indicating that large portions of the army were still intact at those points. 
This part of the journey represent Napoleon’s attempt to conquer Russia as his army moved deeper into enemy territory.<p>
<h4>'Retreat from Moscow'</h4>
<p>Black Circles represent cities encountered during the retreat. The retreat marks the catastrophic phase of the campaign. 
Napoleon’s forces were heavily depleted due to harsh weather, lack of food, and battles. Cities such as Smolensk, Minsk, 
and Mojaisk show significant diminishing survivor numbers as the French forces struggled to escape the brutal Russian winter and lack of supplies.</p>"),

"iteration3" = HTML("<h4>Iteration 3: Temperature Overlay Data Along the Route</h4>
<p>From the visualization the harsh weather conditions that Napoleon’s troops encountered on their march to Moscow and during their retreat. 
As shown on the map, temperature readings are represented by varying shades of blue to indicate different degrees of cold.</p>
<p>These extreme conditions played a vital role in the outcome of the campaign. 
As the soldiers advanced into Russia and retreated during winter, exposure to such low temperatures, 
combined with hunger and disease. All these factors decimated Napoleon’s forces. 
This iteration underlines the harsh Russian's winter representing that it's quite difficult to invade Russian.</p>
<p><b>White circles</b> represent areas with temperatures close to 0°C.<p>
<p><b>Light blue circles</b> (ranging from -5°C to -10°C) suggest moderate cold, which begins to strain the soldiers.<p>
<p><b>Darker blue shades</b> (for -11°C to -20°C) illustrate even more extreme conditions, leading to severe hardships.<p>
<p><b>Deep blue circles</b> (below -21°C) highlight the most brutal winter weather, where the troops faced life-threatening cold.<p> "),

"iteration4" = HTML("<h4>Iteration 4: Interactive Animation of the March</h4>
<p>his is the interactive animation of the march of Nepolian. 
The slider is the period of the journey from the beginning to the end. 
Nepolian took 6 months for this campaign and he has lost multiple men along the way. 
This iteration is rendering and providing the insights of how geography, weather, 
and time affected on Napoleon's campaign. 
Moreover, empahsizing the difficulities and devasting impact of the harsh Russian winter.</p>
<h4>Advance March (Red Line)</h4>
<p>The red line illustrates the advance of Napoleon's army starting from Kowno towards Moscow. 
Each blue marker hightlights the cities and the soldiers push eastward with markers representing 
key cities they went through.</p>

<h4>Survival list</h4>
<p><b>ThKowno: 340,000 troops</b>  started the campaign.</p>
<p><b>Witebsk:</b> By the time the army reached Witebsk, it had decreased to <b>175,000 survivors</b>.</p>
<p><b>Chjat:</b> t this point, only <b>127,100 troops</b> remained.</p>
<p><b>Moscow:</b> The army reached Moscow with <b> 100,000 soldiers</b>, end of the advance phase.</p>

<h4>Retreat (Black Line)</h4>
<p>After reaching Moscow and facing severe conditions, the black line marks the retreat route taken 
by the French troops. The retreat is marked with city points and follows a different path 
compared to the initial advance, reflecting the desperate circumstances. 
Since Moscow didnt provide any supplies and the winter had set in, Nepolian ordered a retreat.</p>

<h4>Survival list</h4>
<p><b>Mojaisk:</b> After leaving Moscow, the army had already dwindled to <b>20,000 soldiers</b>.</p>
<p><b>Smolensk:</b> <b>8,000 survivors</b> passed through Smolensk.</p>
<p><b>Wilna:</b> <b>6,000 survivors</b> remained and this is the end of this campaign.</p>


<p><b> Please press pause button to stop rendering</b> </p>
")
)

# To navigate each slides
ui <- navbarPage(
  "Napoleon's March - Visualization Clone",
  
  # Interactive Map Tab
  tabPanel(
    "4 Interative Iterations",
    sidebarLayout(
      sidebarPanel(
        selectInput("iteration", "Select Iteration", 
                    choices = list(
                      "Iteration 1: Basic March and Background" = "iteration1",
                      "Iteration 2: Cities & Labels" = "iteration2",
                      "Iteration 3: Temperature Overlay" = "iteration3",
                      "Iteration 4: Interactive Animation" = "iteration4"
                    )),
        
        #this is for rendering using animate library
        conditionalPanel(
          condition = "input.iteration == 'iteration4'",
          sliderInput("period", "Campaign Timeline", 
                      min = 1, max = nrow(cities), value = 1,
                      step = 1, animate = animationOptions(interval = 1000, loop = TRUE))
        )
      ),
      mainPanel(
        leafletOutput("march_map", height = 600),
        uiOutput("iteration_description")
      )
    )
  ),
  
  # References Tab
  tabPanel(
    "References",
    fluidPage(
      h4("References"),
      HTML("
        <h4>For map and visualization</h4>
        <ul>
          <li><a href='https://edspace.american.edu/visualwar/minard/' target='_blank'>Minard's Visualization on Visual War</a></li>
          <li><a href='https://chezvoila.com/blog/minard-map/' target='_blank'>Minard Map on Chez Voila</a></li>
        </ul>
        <h4>For shiny application tutorials</h4>
        <ul>
          <li><a href='https://www.youtube.com/watch?v=EC53ftrYf9A&list=PLtqF5YXg7GLkxx_GGXDI_EiAvkhY9olbe&index=6' target='_blank'>Shiny Application Tutorials</a></li>
        </ul>
        <h4>Napolian in Shiny reference</h4>
        <ul>
          <li><a href='hhttps://rdrr.io/cran/HistData/man/Minard.html' target='_blank'>Napolian in Shiny reference</a></li>
        </ul>
        <h4>Analyzing of the visualization</h4>
        <ul>
          <li><a href='https://thoughtbot.com/blog/analyzing-minards-visualization-of-napoleons-1812-march' target='_blank'>Analyzing of the visualization</a></li>
        </ul>
        
        
      ")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Render the leaflet map
  output$march_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 30, lat = 55, zoom = 5)
  })
  
  # Update the map dynamically based on the slider input
  observe({
    req(input$iteration)
      
    leafletProxy("march_map") %>%
      clearShapes() %>%
      clearMarkers()
    
    if (input$iteration == "iteration1") {
      # Plot the overall march using minard-cities data
      leafletProxy("march_map") %>%
        clearShapes() %>%
        addPolylines(
          data = cities %>% filter(direction == "Advance"),
          lng = ~long, lat = ~lat, group = ~city,
          color = "red", weight = 10,
          popup = ~paste("City:", city)
        ) %>%
        addPolylines(
          data = cities %>% filter(direction == "Retreat"),
          lng = ~long, lat = ~lat, group = ~city,
          color = "black", weight = 3,
          popup = ~paste("City:", city)
        )
      
      
    } else if (input$iteration == "iteration2") {
      leafletProxy("march_map") %>%
        clearShapes() %>%
        addCircleMarkers(
          data = cities %>% filter(direction == "Advance"),
          lng = ~long, lat = ~lat, radius = 8,
          color = "red", fillColor = "red", fillOpacity = 0.8,
          label = ~paste0("City: ", city, "Survivors: ", format(survivors, big.mark = ",", scientific = FALSE)),
          labelOptions = labelOptions(noHide = TRUE, direction = "auto")
        ) %>%
        addCircleMarkers(
          data = cities %>% filter(direction == "Retreat"),
          lng = ~long, lat = ~lat, radius = 8,
          color = "black", fillColor = "black", fillOpacity = 0.8,
          label = ~paste0("City: ", city, "Survivors: ", format(survivors, big.mark = ",", scientific = FALSE)),
          labelOptions = labelOptions(noHide = TRUE, direction = "auto")
        )
      
      
    } else if (input$iteration == "iteration3") {
      # the temperature column not empty
      temp_data <- merged_data %>% filter(!is.na(temp))
      
      # Temperature visualization using the custom palette
      leafletProxy("march_map") %>%
        clearShapes() %>%
        addCircleMarkers(
          data = temp_data, lng = ~long, lat = ~lat,
          radius = 30, color = ~temperature_color(temp), fillOpacity = 0.8,
          popup = ~paste0("Temperature: ", temp, "°C")
        ) %>%
        addLegend(
          "bottomright", 
          colors = c("white", "#cce4ff", "#99cfff", "#0004ff"), 
          labels = c("0°C", "-5 to -10°C", "-11 to -20°C", "-21 to -30°C"),
          title = "Temperature (°C)", opacity = 1
        )
      
    } else if (input$iteration == "iteration4") {
      current_period <- input$period
      
      # Render animated march with weather visibility toggle
      selected_data <- cities %>% 
        slice(1:input$period)
      
      #marching to moscow
      leafletProxy("march_map") %>%
        clearShapes() %>%
        clearMarkers() %>%
        
        #rendering red line for advance
        addPolylines(
          data = selected_data %>% filter(direction == "Advance"),
          lng = ~long, lat = ~lat, group = ~city,
          color = "red", weight = 14,
          popup = ~paste("City:", city)
        ) %>%
        
        #adding black for retreat
        addPolylines(
          data = selected_data %>% filter(direction == "Retreat"),
          lng = ~long, lat = ~lat, group = ~city,
          color = "black", weight = 5,
          popup = ~paste("City:", city)
        ) %>%
		# Add cities route
        addCircleMarkers(
          data = selected_data,
          lng = ~long, lat = ~lat, radius = 5,
          color = "blue", fillOpacity = 0.8,
          popup = ~paste("City:", city)
        )
      #weather only shows when same latitude value for a while
      if (temp_visible()){
        leafletProxy("march_map") %>%
          addCircleMarkers(
            data = weather_cities,
            lng = ~long, lat = ~lat, radius = 10,
            color = ~temperature_color(temp), fillOpacity = 0.8,
            popup = ~paste0("Temperature: ", temp, "°C")
          )
      }
    }
  })
  
  # Render the iteration description dynamically
  output$iteration_description <- renderUI({
    iteration_descriptions[[input$iteration]]
  })
}

# Run the Shiny app
shinyApp(ui, server)
