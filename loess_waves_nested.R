library(tidyverse)

source("functions.R")

generate_nested_ribbon_data <- function(n_points = 4, n_nested = 4, min_alpha = 0.1, max_alpha = 1) {
  alpha_decay <- runif(1, 0.1, 0.9)
  filter_fn <- sample(c(`<`, `>`), 1)[[1]]
  
  y <- runif(n_points, 0, 2) %>% smooth_points()
  center <- runif(n_points, -1, 1) %>% smooth_points(span = 1)
  alpha <- runif(1, min_alpha, max_alpha)
  
  
  result <- list()
  
  for (nest_id in seq_len(n_nested)) {
    result <- c(
      result,
      list(tibble(
        x = seq(from = 0, to = 1, length.out = length(y)),
        ymin = center - y,
        ymax = center + y,
        alpha = alpha,
        nest_id = nest_id
      ))
    )
    new_center <- runif(length(center), -1, 1)
    new_center <- new_center[filter_fn(new_center, center)]
    center <- c(center, new_center) %>% smooth_points(span = 1)
    alpha <- alpha * alpha_decay
  }
  
  bind_rows(result)
}

generate_nested_ribbons <- function(n_ribbons = 5, n_points = 4, min_alpha = 0.1, max_alpha = 1) {
  bind_rows(
    rerun(n_ribbons, generate_nested_ribbon_data(n_points, min_alpha = min_alpha, max_alpha = max_alpha)),
    .id = "group"
  ) %>%
    mutate(group = str_c(group, "_", nest_id))
}

create_art <- with_seed(function(n_ribbons = 5, n_points = 4, min_alpha = 0.1, max_alpha = 1) {
  df <- generate_nested_ribbons(n_ribbons = n_ribbons, n_points = n_points, min_alpha = min_alpha, max_alpha = max_alpha)
  
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
  create_art(max_alpha = 0.5, seed = 8601),
  "loess_waves_nested1.png"
)

save_wallpaper(
  create_art(min_alpha = 0, max_alpha = 0.5, seed = 5885),
  "loess_waves_nested2.png"
)
