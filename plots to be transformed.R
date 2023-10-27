library(ggplot2)
library(tidyr)
library(dplyr)
library(stringr)
library(viridis)

# data manipulation
  beers_clean <- beers %>%
    group_by(state) %>% 
    summarize(
      total_beers = n(),
      ale_count = sum(str_detect(style, "Ale") | str_detect(style, "IPA")),
      ale_percentage = (ale_count / total_beers) * 100
    )
  
  # first plot:
  beers_clean %>%
    arrange(desc(ale_percentage)) %>%
    ggplot(aes(x = reorder(state, -ale_percentage), y = ale_percentage)) +
    geom_bar(stat = "identity", fill = "goldenrod") +
    coord_flip() +
    labs(title = "Percentage of ALE Beers by State",
         x = "State",
         y = "Percentage of ALE Beers")
  
  # first plot TRANSFORMED but to be adjusted in a meaningful way:
  plot_1 <- beers_clean %>%
    arrange(desc(ale_percentage)) %>%
    ggplot(aes(x = reorder(state, -ale_percentage), y = ale_percentage)) +
    geom_bar(stat = "identity", fill = "goldenrod") +
    coord_flip() +
    labs(title = "Percentage of ALE Beers by State",
         x = "State",
         y = "Percentage of ALE Beers")
  
  plotly_obj <- ggplotly(plot_1)
  plotly_obj
  
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
  plotly_obj
  
  # third plot:
 plot_3 <- beers_clean %>%
    ggplot(aes(x = total_beers, y = ale_count, label = state)) +
    geom_point(aes(color = ale_percentage), size = 3) +
    geom_text(check_overlap = TRUE, vjust = 1.5, hjust = 1.5) +
    scale_color_viridis(option = "inferno") +  
    labs(title = "ALE Beers vs Total Beers by State",
         x = "Total Beers",
         y = "ALE Beers",
         color = "ALE %") +
    theme_minimal()
 
 plotly_obj <- ggplotly(plot_3)
 plotly_obj 

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
      
 plotly_obj <- ggplotly(plot_4)
 plotly_obj  
  