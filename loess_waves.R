library(tidyverse)

smooth_points <- function(values, n_out = 800, span = 0.75, ...) {
  x <- seq(from = 0, to = 1, length.out = length(values))
  m <- loess(values ~ x, span = span, ...)
  predict(m, newdata = tibble(x = seq(from = 0, to = 1, length.out = n_out)))
}

generate_ribbon_data <- function(n_points) {
  tibble(
    y = runif(n_points, 0, 2) %>% smooth_points(),
    center = runif(n_points, -1, 1) %>% smooth_points(span = 1)
  ) %>%
    transmute(
      x = seq(from = 0, to = 1, length.out = length(y)),
      ymin = center - y,
      ymax = center + y
    )
}

generate_data <- function(n_ribbons = 5, n_points = 4, min_alpha = 0.5, max_alpha = 0.5) {
  bind_rows(
    rerun(n_ribbons, generate_ribbon_data(n_points) %>% mutate(alpha = runif(1, min_alpha, max_alpha))),
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

with_seed <- function(f) {
  function(..., seed) {
    new_seed <- if (!missing(seed)) seed else sample(1:10000, 1)
    seed_backup <- get(".Random.seed", .GlobalEnv)
    set.seed(new_seed)
    
    iwalk(list(...), ~message(.y, ": ", .x))
    message("seed: ", new_seed)
    
    result <- f(...)
    
    assign(".Randon.seed", seed_backup, .GlobalEnv)
    result
  }
}

create_art <- with_seed(function(n_ribbons = 5, n_points = 4, min_alpha = 0.5, max_alpha = 0.5) {
  df <- generate_data(n_ribbons = n_ribbons, n_points = n_points, min_alpha = min_alpha, max_alpha = max_alpha)
  plot_data(df)
})

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
