#' @import ggplot2
#' @importFrom ggforce geom_regon
#' @importFrom RColorBrewer brewer.pal
#' @export
plot_empty_map <- function(tib) {
  ggplot(tab, aes(x = x0, y = y0, x0 = x0, y0 = y0, sides = 6, angle = 30 / 180 * pi, r = xy_ratio)) +
    theme_void() +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank()
    ) +
    coord_equal()
}

#' @importFrom rlang %|%
#' @export
plot_terrain_map <- function(tib, alpha = 1) {
  rivers <- get_river_coordinates(tib)

  plot_empty_map(tib) +
    ggforce::geom_regon(aes(fill = terrain_name), alpha = alpha) +
    ggforce::geom_regon(aes(fill = feature_name, r = xy_ratio * .9), data = tib %>% filter(feature_name == "Ice"), alpha = alpha) +
    geom_text(aes(label = "^"), tib %>% filter(terrain_form == "Hill"), alpha = alpha) +
    geom_text(aes(label = "^"), tib %>% filter(terrain_form == "Mountain"), fontface = "bold", alpha = alpha) +
    geom_segment(aes(x = xa, xend = xb, y = ya, yend = yb), colour = feature_palette[["River"]], rivers, size = 1, alpha = alpha) +
    scale_fill_manual(values = c(terrain_palette, feature_palette))
}

#' @export
plot_feature_map <- function(tib) {
  plot_terrain_map(tib, 0.3) +
    geom_point(aes(colour = feature_name), size = point_size, tib %>% filter(!is.na(feature_name)), shape = 20) +
    scale_colour_manual(values = feature_palette)
}

#' @export
pal_many_categories <- rep(c(
  RColorBrewer::brewer.pal(9, "Set1"),
  RColorBrewer::brewer.pal(8, "Set2"),
  RColorBrewer::brewer.pal(12, "Set3"),
  RColorBrewer::brewer.pal(8, "Pastel1"),
  RColorBrewer::brewer.pal(8, "Pastel2"),
  RColorBrewer::brewer.pal(8, "Dark2")
), 100)

#' @export
pal_some_categories <- c(
  RColorBrewer::brewer.pal(8, "Dark2"),
  RColorBrewer::brewer.pal(8, "Pastel2"),
  RColorBrewer::brewer.pal(8, "Set2")
)
