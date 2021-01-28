xy_ratio <- 0.5 / cos(30/180*pi)

#' @importFrom dplyr mutate %>%
#' @export
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
#' @export
get_river_coordinates <- function(tib) {
  bind_rows(
    tib %>% filter(river_e != -1) %>% mutate(xa = x2, xb = x3, ya = y2, yb = y3),
    tib %>% filter(river_se != -1) %>% mutate(xa = x3, xb = x4, ya = y3, yb = y4),
    tib %>% filter(river_sw != -1) %>% mutate(xa = x4, xb = x5, ya = y4, yb = y5)
  )
}

#' @importFrom dplyr transmute
#' @export
get_border_coordinates <- function(tib) {
  bind_rows(
    left_join(
      tib %>% transmute(x0, y0, leader_name, x = ifelse(y %% 2 == 0, x, x + 1), y = y + 1, xa = x1, xb = x2, ya = y1, yb = y2, owner),
      tib %>% transmute(x, y, owner2 = owner),
      by = c("x", "y")
    ),
    left_join(
      tib %>% transmute(x0, y0, leader_name, x = x + 1, y, xa = x2, xb = x3, ya = y2, yb = y3, owner),
      tib %>% transmute(x, y, owner2 = owner),
      by = c("x", "y")
    ),
    left_join(
      tib %>% transmute(x0, y0, leader_name, x = ifelse(y %% 2 == 0, x, x + 1), y = y - 1, xa = x3, xb = x4, ya = y3, yb = y4, owner),
      tib %>% transmute(x, y, owner2 = owner),
      by = c("x", "y")
    ),
    left_join(
      tib %>% transmute(x0, y0, leader_name, x = ifelse(y %% 2 == 0, x - 1, x), y = y - 1, xa = x4, xb = x5, ya = y4, yb = y5, owner),
      tib %>% transmute(x, y, owner2 = owner),
      by = c("x", "y")
    ),
    left_join(
      tib %>% transmute(x0, y0, leader_name, x = x - 1, y, xa = x5, xb = x6, ya = y5, yb = y6, owner),
      tib %>% transmute(x, y, owner2 = owner),
      by = c("x", "y")
    ),
    left_join(
      tib %>% transmute(x0, y0, leader_name, x = ifelse(y %% 2 == 0, x - 1, x), y = y + 1, xa = x6, xb = x1, ya = y6, yb = y1, owner),
      tib %>% transmute(x, y, owner2 = owner),
      by = c("x", "y")
    )
  ) %>%
    filter(!is.na(owner), owner != owner2 | is.na(owner2)) %>%
    mutate(xa = xa * .9 + x0 * .1, xb = xb * .9 + x0 * .1, ya = ya * .9 + y0 * .1, yb = yb * .9 + y0 * .1)
}

#' @importFrom dplyr transmute
#' @export
get_road_coordinates <- function(tib) {
  data(roads, package = "civ6saves")

  tib <- tib %>% filter(road > 0)

  bind_rows(
    inner_join(
      tib %>% transmute(x = ifelse(y %% 2 == 0, x, x + 1), y = y + 1, xa = x0, ya = y0, road),
      tib %>% transmute(x, y, xb = x0, yb = y0),
      by = c("x", "y")
    ),
    inner_join(
      tib %>% transmute(x = x + 1, y, xa = x0, ya = y0, road),
      tib %>% transmute(x, y, xb = x0, yb = y0),
      by = c("x", "y")
    ),
    inner_join(
      tib %>% transmute(x = ifelse(y %% 2 == 0, x, x + 1), y = y - 1, xa = x0, ya = y0, road),
      tib %>% transmute(x, y, xb = x0, yb = y0),
      by = c("x", "y")
    ),
    inner_join(
      tib %>% transmute(x = ifelse(y %% 2 == 0, x - 1, x), y = y - 1, xa = x0, ya = y0, road),
      tib %>% transmute(x, y, xb = x0, yb = y0),
      by = c("x", "y")
    ),
    inner_join(
      tib %>% transmute(x = x - 1, y, xa = x0, ya = y0, road),
      tib %>% transmute(x, y, xb = x0, yb = y0),
      by = c("x", "y")
    ),
    inner_join(
      tib %>% transmute(x = ifelse(y %% 2 == 0, x - 1, x), y = y + 1, xa = x0, ya = y0, road),
      tib %>% transmute(x, y, xb = x0, yb = y0),
      by = c("x", "y")
    )
  ) %>%
    mutate(xb = (xa + xb) / 2, yb = (ya + yb) / 2) %>%
    left_join(roads, by = "road")
}
