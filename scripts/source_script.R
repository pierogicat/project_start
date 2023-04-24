
# data prep ---------------------------------------------------------------

# Join life history to data:

join_life_history <- 
  function(data) {
    left_join(
      data,
      dc_birds %>% 
        pluck("birds") %>% 
        
        # Convert life histories to title case:
        
        mutate(
          across(foraging:diet,
                 str_to_title)),
      by = join_by(spp == species))
  }

# plot theme --------------------------------------------------------------

my_theme <- 
  function() {
    theme(
      
      # Panel elements:
      
      panel.border = element_blank(),
      panel.grid = element_blank(),
      panel.background = element_blank(),
      
      # Axis elements:
      
      axis.line = 
        element_line(
          color = "black",
          lineend = "round"),
      axis.line.x = element_line(linewidth = 0.5),
      axis.line.y = element_line(linewidth = 0.3),
      
      # Text elements:
      
      axis.text = 
        element_text(
          size = 10,
          family = "serif"),
      axis.title = 
        element_text(
          size = 12,
          family = "serif"),
      axis.title.x = 
        element_text(
          margin = 
            margin(
              t = 10,
              r = 0,
              b = 0,
              l = 0)),
      axis.title.y = 
        element_text(
          margin = 
            margin(
              t = 0,
              r = 5,
              b = 0,
              l = 0)),
      plot.title = 
        element_text(
          size = 14,
          family = "serif",
          hjust = 0.5))
  }
