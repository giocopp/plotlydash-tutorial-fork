##############################
#Install packages (only once!)
##############################
#install.packages("plotly")

##############
#Load packages
##############
library(tidyverse)
library(plotly)

############
#Plotly code
############

# Prepare data
beers <- read_csv('../data/beers.csv')
str(beers) #tibble

#Style groupings to reduce categories
beers <- beers |>
  mutate(style_group = case_when(
    (str_detect(style, "IPA") ~ "IPA"),
    (str_detect(style, "Ale") ~ "Ale"),
    (str_detect(style, "Lager") | style == "Altbier" ~ "Lager"),
    (str_detect(style, "Pilsner") | str_detect(style, "Pilsener") ~ "Pilsner"),
    (str_detect(style, "Stout") ~ "Stout"),
    (str_detect(style, "Barleywine") ~ "Barleywine"),
    (str_detect(style, "Porter") ~ "Porter"),
    (str_detect(style, "Radler") | str_detect(style, "Fruit") ~ "Radler/Fruit"),
    .default = "Other"
    )
  )
  
#1. Histograms
#Ref: https://plotly.com/r/histograms/

#1a. One-line "abv" histogram > interactive features
fig_1a <- beers |>
  plot_ly(x = ~abv, type = "histogram")
fig_1a

#1b. Add layout
fig_1b <- beers |>
  plot_ly(x = ~abv, type = "histogram", name = "ABV") |>
  layout(
    title = "ABV Histogram",
    xaxis = list(title = "Alcohol by Volume (ABV)"),
    yaxis = list(title = "Count of beers"),
    bargap=0.2
  )
fig_1b

#1c. One-line "ibu" histogram
fig_1c <- beers |>
  plot_ly(x = ~ibu, type = "histogram", name = "IBU")
fig_1c

#1d. Subplots
fig_1d <- subplot(fig1b, fig1c) |>
  layout(title = 'ABV and IBU histograms')
fig_1d

#2. Bar charts
#Ref: https://plotly.com/r/bar-charts/

#2a. "style" bar-chart
fig_2a <- beers |> count(style_group) |>
  plot_ly(x = ~style_group, y = ~n, type = "bar") |>
  layout(xaxis = list(categoryorder = "total descending"))
fig_2a

#3. Boxplots
#Ref: https://plotly.com/r/box-plots/

#3a. "abv" vs. "style" boxplot (x for horizontal, y for vertical)
fig_3a <- beers |>
  plot_ly(x = ~abv, color = ~style_group, type = "box", showlegend = FALSE)
fig_3a

fig_3b <- beers |>
  plot_ly(x = ~ibu, color = ~style_group, type = "box", showlegend = FALSE)
fig_3b

#4 Scatter plots
#Ref: https://plotly.com/r/line-and-scatter/

#4a. "abv" vs. "ibu" by "style"
fig_4a <- beers |>
  plot_ly(x = ~abv, y = ~ibu, color = ~style_group, type = "scatter", mode = "markers",
          text = ~paste('Beer: ', beer, '</br>Style: ', style, '</br>Style Group: ', style_group)) |>
  layout(
    title = "Beers scatter plot: ABV vs. IBU",
    xaxis = list(title = "Alcohol by Volume (ABV)"),
    yaxis = list(title = "International Bitterness Units (IBU)")
  )
fig_4a

#5 3d scatter plot
#Ref: https://plotly.com/r/3d-scatter-plots/

#5a. "abv" vs. "ibu" vs. "ounces" 3d scatter
fig_5a <- beers |>
  plot_ly(x = ~abv, y = ~ibu, z = ~ounces, color = ~style_group, type = "scatter3d", mode = "markers",
          text = ~paste('Beer: ', beer, '</br>Style: ', style, '</br>Style Group: ', style_group))
fig_5a
