library(tidyverse)

source("functions.R")

create_art <- with_seed(function(n_ribbons = 5, n_points = 4, min_alpha = 0.5, max_alpha = 0.5) {
  df <- generate_ribbons(n_ribbons = n_ribbons, n_points = n_points, min_alpha = min_alpha, max_alpha = max_alpha)
  
  ggplot(df, aes(x, ymin = ymin, ymax = ymax, fill = group, alpha = I(alpha))) +
    geom_ribbon() +
    coord_cartesian(xlim = c(0, 1), ylim = c(-1, 1), expand = FALSE) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.background = element_rect(fill = "white")
    )
})

save_wallpaper(
  create_art(n_ribbons = 6, n_points = 10, min_alpha = 0.1, max_alpha = 1, seed = 6164),
  "loess_waves3.png"
)

save_wallpaper(
  create_art(n_ribbons = 6, n_points = 10, min_alpha = 0.1, max_alpha = 1, seed = 6164) + scale_fill_brewer(type = "div"),
  "loess_waves3_green.png"
)

save_wallpaper(
  create_art(n_ribbons = 6, n_points = 10, min_alpha = 0.1, max_alpha = 1, seed = 6164) + scale_fill_brewer(type = "div", palette = 6),
  "loess_waves3_red.png"
)

save_wallpaper(
  create_art(seed = 152),
  "loess_waves4.png"
)
