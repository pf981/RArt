library(tidyverse)

source("functions.R")

resolve_range <- function(x) {
  if (length(x) <= 1) return(x)
  runif(1, x[1], x[2])
}

generate_nested_ribbon_data <- function(n_points = c(4, 8), n_nested = c(4, 8), alpha = c(0.1, 1), alpha_decay = c(0.1, 0.9), nest_direction = c("either", "up", "down", "neither")) {
  n_points <- resolve_range(n_points)
  n_nested <- resolve_range(n_nested)
  alpha <- resolve_range(alpha)
  alpha_decay <- resolve_range(alpha_decay)
  nest_direction <- rlang::arg_match(nest_direction)
  
  if (nest_direction == "either") {
    filter_fn <- sample(c(`<`, `>`), 1)[[1]]
  } else if (nest_direction == "neither") {
    filter_fn <- function(...) TRUE
  } else if (nest_direction == "up")  {
    filter_fn <- `>`
  } else if (nest_direction == "neither") {
    filter_fn <- `<`
  }
  
  y <- runif(n_points, 0, 2) %>% smooth_points()
  center <- runif(n_points, -1, 1) %>% smooth_points(span = 1)
  
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

generate_nested_ribbons <- function(n_ribbons = c(5, 10), n_points = c(4, 8), n_nested = c(4, 8), alpha = c(0.1, 1), alpha_decay = c(0.1, 0.9), nest_direction = c("either", "up", "down", "neither")) {
  n_ribbons <- resolve_range(n_ribbons)
  
  bind_rows(
    rerun(n_ribbons, generate_nested_ribbon_data(n_points = n_points, n_nested = n_nested, alpha = alpha, alpha_decay = alpha_decay, nest_direction = nest_direction)),
    .id = "group"
  ) %>%
    mutate(group = str_c(group, "_", nest_id))
}

create_art <- with_seed(function(n_ribbons = c(5, 10), n_points = c(4, 8), n_nested = c(4, 8), alpha = c(0.1, 1), alpha_decay = c(0.1, 0.9), nest_direction = c("either", "up", "down", "neither")) {
  df <- generate_nested_ribbons(n_ribbons = n_ribbons, n_points = n_points, n_nested = n_nested, alpha = alpha, alpha_decay = alpha_decay, nest_direction = nest_direction)
  
  ggplot(df, aes(x, ymin = ymin, ymax = ymax, fill = group, alpha = I(alpha))) +
    geom_ribbon() +
    coord_cartesian(xlim = c(0, 1), ylim = c(-1, 1), expand = FALSE) +
    theme_void() +
    theme(
      legend.position = "none",
      plot.background = element_rect(fill = "white")
    )
})


create_art(nest_direction = "neither")
create_art(nest_direction = "neither", seed = 5391) + scale_fill_random(seed = 6989) # THIS
create_art(nest_direction = "neither", seed = 5391) %>% random_color_grid()

create_art() + scale_fill_random()


create_art(seed = 7191) + scale_fill_random(seed = 8768)
create_art(seed = 7191, nest_direction = "neither", n_ribbons = 10) + scale_fill_random(seed = 8768)
create_art(seed = 7191, nest_direction = "neither", n_ribbons = 10) %>% random_color_grid()

create_art() + scale_fill_random()
p <- create_art(seed = 7191)
random_color_grid(p)



random_plot_grid(create_art)

save_wallpaper(
  create_art(max_alpha = 0.5, seed = 8601),
  "loess_waves_nested1.png"
)

save_wallpaper(
  create_art(min_alpha = 0, max_alpha = 0.5, seed = 5885),
  "loess_waves_nested2.png"
)

save_wallpaper(
  create_art(seed = 4501),
  "loess_waves_nested3.png"
)

# Use this to find candidate seeds
random_color_grid(create_art())

save_wallpaper(
  create_art(seed = 4786) + scale_fill_random(seed = 8447),
  "loess_waves_nested4.png"
)
