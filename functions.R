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

generate_ribbons <- function(n_ribbons = 5, n_points = 4, min_alpha = 0.5, max_alpha = 0.5) {
  bind_rows(
    rerun(n_ribbons, generate_ribbon_data(n_points) %>% mutate(alpha = runif(1, min_alpha, max_alpha))),
    .id = "group"
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
    
    assign(".Random.seed", seed_backup, .GlobalEnv)
    result
  }
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