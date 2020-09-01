#!/usr/bin/env Rscript

library(tidyverse)
library(ggforce)
library(civ6saves)

input <- "/mnt/data/nextcloud/saves2"
input <- commandArgs(trailingOnly = TRUE)[[1]]

webm_file <- paste0(input, "/output.webm")

# convert to rds
walk(list.files(input, ".*\\.Civ6Save$", full.names = TRUE), function(file) {
  bin_file <- gsub("Civ6Save", "Civ6Save.bin", file)
  rds_file <- gsub("Civ6Save", "rds", file)

  if (!file.exists(bin_file)) {
    cat("Converting ", bin_file, "\n", sep = "")
    system(paste0("node node_modules/civ6-save-parser/index.js \"", file, "\" --outputCompressed"))
    file.copy(basename(bin_file), bin_file)
    file.remove(basename(bin_file))
  }

  if (!file.exists(rds_file)) {
    out <- read_save(bin_file)
    write_rds(out, rds_file, compress = "gz")
  }
})

rds_files <- list.files(input, ".*\\.rds$", full.names = TRUE)

tab <- map_df(rds_files, function(rds_file) {
  out <- read_rds(rds_file)

  out$map %>%
    add_coordinates() %>%
    mutate(file = rds_file, file_base = gsub("\\..*$", "", basename(file))) %>%
    left_join(terrains, by = "terrain") %>%
    left_join(features, by = "feature") %>%
    left_join(improvements, by = "improvement") %>%
    left_join(continents, by = "continent") %>%
    left_join(resources, by = "resource") %>%
    left_join(world_wonders, by = "world_wonder") %>%
    left_join(roads, by = "road")
}) %>%
  mutate(owner = factor(owner))

tab_static <- tab %>% filter(file == file[[1]]) %>% select(-file)
rivers <- get_river_coordinates(tab_static)

simple_borders <- bind_rows(
  left_join(tab %>% transmute(x0, y0, file, x = ifelse(y %% 2 == 0, x, x + 1), y = y + 1, xa = x1, xb = x2, ya = y1, yb = y2, owner), tab %>% transmute(file, x, y, owner2 = owner), by = c("x", "y", "file")) %>% filter(!is.na(owner), owner != owner2 | is.na(owner2)),
  left_join(tab %>% transmute(x0, y0, file, x = x + 1, y, xa = x2, xb = x3, ya = y2, yb = y3, owner), tab %>% transmute(file, x, y, owner2 = owner), by = c("x", "y", "file")) %>% filter(!is.na(owner), owner != owner2 | is.na(owner2)),
  left_join(tab %>% transmute(x0, y0, file, x = ifelse(y %% 2 == 0, x, x + 1), y = y - 1, xa = x3, xb = x4, ya = y3, yb = y4, owner), tab %>% transmute(file, x, y, owner2 = owner), by = c("x", "y", "file")) %>% filter(!is.na(owner), owner != owner2 | is.na(owner2)),
  left_join(tab %>% transmute(x0, y0, file, x = ifelse(y %% 2 == 0, x - 1, x), y = y - 1, xa = x4, xb = x5, ya = y4, yb = y5, owner), tab %>% transmute(file, x, y, owner2 = owner), by = c("x", "y", "file")) %>% filter(!is.na(owner), owner != owner2 | is.na(owner2)),
  left_join(tab %>% transmute(x0, y0, file, x = x - 1, y, xa = x5, xb = x6, ya = y5, yb = y6, owner), tab %>% transmute(file, x, y, owner2 = owner), by = c("x", "y", "file")) %>% filter(!is.na(owner), owner != owner2 | is.na(owner2)),
  left_join(tab %>% transmute(x0, y0, file, x = ifelse(y %% 2 == 0, x - 1, x), y = y + 1, xa = x6, xb = x1, ya = y6, yb = y1, owner), tab %>% transmute(file, x, y, owner2 = owner), by = c("x", "y", "file")) %>% filter(!is.na(owner), owner != owner2 | is.na(owner2))
) %>% mutate(xa = xa * .9 + x0 * .1, xb = xb * .9 + x0 * .1, ya = ya * .9 + y0 * .1, yb = yb * .9 + y0 * .1)

owner_palette <- setNames(
  c(pal_many_categories[seq_len(33)], "black", "black"),
  c(0:32, 62, 255)
)

alpha <- 0.01

g0 <-
  plot_empty_map(tab_static) +
  ggforce::geom_regon(aes(fill = terrain_name), alpha = alpha) +
  geom_text(aes(label = "^"), tab %>% filter(terrain_form == "Hill"), alpha = alpha) +
  geom_text(aes(label = "^"), tab %>% filter(terrain_form == "Mountain"), fontface = "bold", alpha = alpha) +
  geom_segment(aes(x = xa, xend = xb, y = ya, yend = yb), colour = feature_palette[["River"]], rivers, size = 1, alpha = alpha) +
  scale_fill_manual(values = terrain_palette) +
  theme(axis.line=element_blank(),
        axis.ticks=element_blank(),
        axis.title=element_blank(),
        axis.text.y=element_blank(),
        axis.text.x=element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = .5, size = 10),
        plot.margin = unit(c(-2, -2, -2, -2), "cm"),
        legend.position = "none")


walk(rds_files, function(rds_file) {
  pdf_file <- gsub("rds$", "pdf", rds_file)
  png_file <- gsub("rds$", "png", rds_file)

  tabf <- tab %>% filter(file == rds_file)
  simf <- simple_borders %>% filter(file == rds_file)
  tabfo <- tabf %>% filter(!is.na(owner))

  g <- g0 +
    ggnewscale::new_scale_fill() +
    ggforce::geom_regon(aes(r = civ6saves:::xy_ratio * .9), tabf %>% filter(feature_name == "Ice"), fill = feature_palette[["Ice"]], alpha = .4) +
    scale_fill_manual(values = owner_palette) +
    scale_colour_manual(values = owner_palette) +
    labs(title = gsub("\\.rds", "", basename(rds_file)))

  if (nrow(tabfo) > 0) {
    g <- g +
      ggforce::geom_regon(aes(fill = owner), tabf, alpha = .4) +
      geom_segment(aes(x = xa, xend = xb, y = ya, yend = yb, colour = owner, x0 = xa, y0 = ya), simf)
  }

  ggsave(pdf_file, g, width = 20, height = 12)
  pdftools::pdf_convert(pdf_file, format = "png", filenames = png_file)
  file.remove(pdf_file)
})

if (file.exists(webm_file)) file.remove(webm_file)
system(paste0("ffmpeg -framerate 1 -f image2 -i ", input, "/%*.png -c:v libvpx-vp9 -pix_fmt yuva420p ", webm_file))

