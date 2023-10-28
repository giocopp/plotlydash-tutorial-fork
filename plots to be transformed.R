library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)
library(viridis)

View(beers)

# data manipulation
  beers_clean <- beers %>%
    group_by(state) %>% 
    summarize(
      total_beers = n(),
      ale_count = sum(str_detect(style, "Ale") | str_detect(style, "IPA")),
      ale_percentage = (ale_count / total_beers) * 100
    )
  
  top_beers_by_state <- beers %>%
    group_by(state) %>%
    top_n(3, abv) %>%
    summarize(tooltip = paste(beer, "(", style, ")", collapse = "\n"))
  
  beers_clean_join <- beers_clean %>%
    left_join(top_beers_by_state, by = "state")
  
  # first plot:
  ggplot1 <- beers_clean_join %>%
    arrange(desc(ale_percentage)) %>%
    ggplot(aes(x = reorder(state, -ale_percentage), y = ale_percentage, 
               text = tooltip)) +  # Adding the tooltip here
    geom_bar(stat = "identity", fill = "goldenrod") +
    coord_flip() +
    labs(title = "Percentage of ALE Beers by State",
         x = "State",
         y = "Percentage of ALE Beers")
  
  # first plot TRANSFORMED but to be adjusted in a meaningful way:
  plotly_obj_1 <- ggplotly(ggplot1, tooltip = "text")
  plotly_obj_1
  
  # second plot:
  plot_2 <-  beers_clean %>%
    ggplot(aes(x = reorder(state, -ale_count))) +
    geom_bar(aes(y = total_beers, fill = "Total Beers"), stat = "identity", position = "dodge") +
    geom_bar(aes(y = ale_count, fill = "ALE Beers"), stat = "identity", position = "dodge") +
    coord_flip() +
    labs(title = "Comparison of Total Beers and ALE Beers by State",
         x = "State",
         y = "Count") +
    scale_fill_manual(values = c("Total Beers" = "gold", "ALE Beers" = "goldenrod"))
  
  plotly_obj <- ggplotly(plot_2)
  plotly_obj_2
  
  # third plot:
 beer_percentages <- beers %>%
   group_by(state, brewery, style_group) %>%
   tally() %>%
   group_by(state, brewery) %>%
   mutate(percentage = round(n / sum(n) * 100, 2))
 
 p <- ggplot(beer_percentages, aes(x = state, y = percentage, fill = style_group, 
                                   text = paste("brewery:", brewery))) +
   geom_bar(stat = "identity") +
   scale_fill_viridis_d() +
   theme_minimal() +
   labs(title = "Percentage of Beer Types by Brewery and State", 
        x = "State", 
        y = "Percentage") +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))
 
 p_plotly <- ggplotly(p)
 p_plotly

  # fourth plot:
 plot_4 <- beers_clean %>%
    ggplot(aes(x = total_beers, y = ale_count, label = state)) +
    geom_point(aes(color = ale_percentage, size = ale_percentage), alpha = 0.6) +  # Set size and transparency
    geom_text(check_overlap = TRUE, vjust = 1.5, hjust = 1.5) +
    scale_color_viridis(option = "inferno") +
    scale_size(range = c(3, 20), name = "ALE %") +  # Adjust the range of bubble sizes
    labs(title = "ALE Beers vs Total Beers by State",
         x = "Total Beers",
         y = "ALE Beers",
         color = "ALE %") +
    theme_minimal()  
      
 plotly_obj_4 <- ggplotly(plot_4)
 plotly_obj_4
  