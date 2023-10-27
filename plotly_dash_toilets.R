

##############################
#Install packages (only once!)
##############################
#install.packages("plotly")
#install.packages("remotes")
#remotes::install_github("plotly/dashR", upgrade = "always")

##############
#Load packages
##############
library(tidyverse)
library(plotly)
library(dash)

#################
#Plotly-Dash code
###############@@

#0. Instantiate Dash App
app <- dash_app()

#1. Prepare data
toilets <- read.csv("berliner-toiletten-standorte.csv")
str(toilets) #tibble

toilets <- toilets |>
  mutate(Free = if_else(Price == 0, "free", "paid"))

toilets <- toilets |>
  mutate(District = case_when(
        (PostalCode >= 10115 & PostalCode <= 10179) | (PostalCode >= 13347 & PostalCode <= 13359) ~ "Mitte-Wedding",
        (PostalCode >= 10179 & PostalCode <= 10249) | (PostalCode >= 10961 & PostalCode <= 10999) ~ "Friedrichshain-Kreuzberg",
        (PostalCode >= 10367 & PostalCode <= 10407) | (PostalCode >= 10318 & PostalCode <= 10319) ~ "Lichtenberg",
        (PostalCode >= 10405 & PostalCode <= 10439) | (PostalCode >= 13086 & PostalCode <= 13129) ~ "Pankow",
        (PostalCode >= 10585 & PostalCode <= 10779) | (PostalCode >= 12159 & PostalCode <= 12163) | (PostalCode >= 13629 & PostalCode <= 14053) ~ "Charlottenburg-Wilmersdorf",
        (PostalCode >= 12099 & PostalCode <= 12109) | (PostalCode >= 10777 & PostalCode <= 10783) | (PostalCode >= 10823 & PostalCode <= 10829) | (PostalCode >= 12305 & PostalCode <= 12309) | (PostalCode >= 12277 & PostalCode <= 12279) ~ "Tempelhof-Schöneberg",
        (PostalCode >= 10999 & PostalCode <= 12059) | (PostalCode >= 12555 & PostalCode <= 12559) | (PostalCode >= 12487 & PostalCode <= 12489) ~ "Treptow-Köpenick",
        (PostalCode >= 12163 & PostalCode <= 12169) | (PostalCode >= 14163 & PostalCode <= 14169) | (PostalCode >= 12203 & PostalCode <= 12209) | (PostalCode >= 12247 & PostalCode <= 12249) | (PostalCode == 14109) ~ "Steglitz-Zehlendorf",
        (PostalCode >= 13407 & PostalCode <= 13439) | (PostalCode >= 13505 & PostalCode <= 13509) ~ "Reinickendorf",
        (PostalCode >= 12043 & PostalCode <= 12059) ~ "Neukölln",
        (PostalCode >= 12679 & PostalCode <= 12689) ~ "Marzahn-Hellersdorf",
        (PostalCode >= 13599 & PostalCode <= 13629 ~ "Spandau")
      ))

toilets |> filter(District != "NA") |>
  plot_ly(x = ~District, color = ~Free, type = "histogram") |>
  layout(
    title = "Distribution of Public Toilets in Berlin",
    xaxis = list(title = "Districts of Berlin"),
    yaxis = list(title = "No. of Public Toilets")
  )

toilets <- toilets |>
  mutate(Accessibility = if_else(isHandicappedAccessible == 0, "Handicapped Accessible", "Not Handicapped Accessible")) |>
  mutate(BabyFacilities = if_else(hasChangingTable == 0, "Changing table", "No changing table"))

#2 Back-end controls

#Input component(s)
color_options <- c(unique(toilets$District))
#district_dropdown <- dccDropdown(
#    id = 'district-dropdown',
#    options = district_options,
#    value = district_options[1] #default
#)

price_options <- c(unique(toilets$Free))
price_filter <- dccChecklist(
  id = 'price-filter',
  options = price_options,
  value = price_options #default, choose all
)

handicap_options <- c(unique(toilets$Accessibility))
handicap_filter <- dccChecklist(
  id = 'handicap-filter',
  options = handicap_options,
  value = handicap_options #default, choose all
)

baby_options <- c(unique(toilets$BabyFacilities))
baby_filter <- dccChecklist(
  id = 'baby-filter',
  options = baby_options,
  value = baby_options #default, choose all
)

#Output component(s)
toilets_map <- dccGraph(id = 'toilets-map')

#Callback(s)
app |> add_callback(
  output('toilets-map', 'figure'),
  list(
    #input('district-dropdown', 'value'),
    input('price-filter', 'value'),
    input('handicap-filter', 'value'),
    input('baby-filter', 'value')
  ),
  function(price_input, handicap_input, baby_input) { #district_input
    #function(color_input) {
    toilets <- toilets |>
      filter(Free %in% price_input) |>
      filter(Accessibility %in% handicap_input) |>
      filter(BabyFacilities %in% baby_input)
    fig = toilets |>
      filter(District != "NA") |>
      plot_ly(lat = ~Latitude, lon = ~Longitude, type = "scattermapbox",
              split = ~District, size = 5, #toilets[[color_input]]
              hoverinfo = "text+name", hovertext = ~Street) |>
      layout(
        title = "Berlin public toilets",
        mapbox = list(
          style = 'open-street-map',
          zoom = 10,
          center = list(lat = ~median(Latitude), lon = ~median(Longitude)))
      )
    return(fig) #not needed but just to be explicit
  }
)

#3 Front-end layout
layout_elements <- list(
  h1("IDS Workshop - Plotly-Dash test"),
  div("Using the Berlin Public Toilets dataset to test Scatter Map functionality..."),
  br(),
  div("Type:"),
  price_filter,
  br(),
  div("Accessibility:"),
  handicap_filter,
  br(),
  div("Baby facilities:"),
  baby_filter,
  br(),
  toilets_map
)

#4 Main app
app |> set_layout(layout_elements)
app |> run_app()
