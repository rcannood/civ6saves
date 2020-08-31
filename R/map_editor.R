xy_ratio <- 0.5 / cos(30/180*pi)

#' @importFrom dplyr mutate %>%
add_coordinates <- function(tib) {
  r <- xy_ratio
  tib <- tib %>%
    mutate(
      x0 = ifelse(y %% 2 == 0, x, x + .5), y0 = y*r*1.5,
      x1 = x0,      y1 = y0 + r,
      x2 = x0 + .5, y2 = y0 + r * .5,
      x3 = x0 + .5, y3 = y0 - r * .5,
      x4 = x0,      y4 = y0 - r,
      x5 = x0 - .5, y5 = y0 - r * .5,
      x6 = x0 - .5, y6 = y0 + r * .5,
    )
}

#' @importFrom dplyr filter mutate bind_rows
get_river_coordinates <- function(tib) {
  bind_rows(
    tab %>% filter(river_e != -1) %>% mutate(xa = x2, xb = x3, ya = y2, yb = y3),
    tab %>% filter(river_se != -1) %>% mutate(xa = x3, xb = x4, ya = y3, yb = y4),
    tab %>% filter(river_sw != -1) %>% mutate(xa = x4, xb = x5, ya = y4, yb = y5)
  )
}

