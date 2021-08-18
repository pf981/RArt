library(tidyverse)

generate_loess_data <- function(n_points) {
  p <-
    tibble(
      x = seq(from = 0, to = 1, length.out = n_points),
      y = runif(n_points)
    ) %>%
    ggplot(aes(x, y)) +
      geom_smooth(se = FALSE, method = "loess", formula = y ~ x)
  
  center <- runif(n = 1, min = -1, max = 1)
  
  ggplot_build(p)$data[[1]] %>%
    transmute(
      x,
      ymin = -y + center,
      ymax = y + center
    )
    
}

generate_data <- function(n_ribbons = 5, n_points = 4, min_alpha = 0.5, max_alpha = 0.5) {
  bind_rows(
    rerun(n_ribbons, generate_loess_data(n_points) %>% mutate(alpha = runif(1, min_alpha, max_alpha))),
    .id = "group"
  )
}

plot_data <- function(df) {
  ggplot(df, aes(x, ymin = ymin, ymax = ymax, fill = group, alpha = I(alpha))) +
    geom_ribbon() +
    coord_cartesian(xlim = c(0, 1), ylim = c(-1, 1), expand = FALSE) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.background = element_rect(fill = "white")
    )
}

create_art <- function(n_ribbons = 5, n_points = 4, min_alpha = 0.5, max_alpha = 0.5, seed) {
  new_seed <- if (!missing(seed)) seed else sample(1:10000, 1)
  seed_backup <- get(".Random.seed", .GlobalEnv)
  message("n_ribbons: ", n_ribbons)
  message("n_points: ", n_ribbons)
  message("seed: ", new_seed)
  set.seed(new_seed)
  
  df <- generate_data(n_ribbons = n_ribbons, n_points = n_points, min_alpha = min_alpha, max_alpha = max_alpha)
  p <- plot_data(df)
  
  assign(".Randon.seed", seed_backup, .GlobalEnv)
  p
}

save_wallpaper <- function(p, name, folder = "output", width = 2560 * 3, height = 1440, dpi = 700, units = "px", ...) {
  ggsave(
    glue::glue("{folder}/{name}"),
    plot = p,
    width = width,
    height = height,
    dpi = dpi,
    units = units,
    ...
  )
}

save_wallpaper(
  create_art(n_ribbons = 6, n_points = 10, min_alpha = 0.1, max_alpha = 1, seed = 9579),
  "loess_waves.png"
)

save_wallpaper(
  create_art(n_ribbons = 6, n_points = 10, min_alpha = 0.1, max_alpha = 1, seed = 7601),
  "loess_waves2.png"
)



