#!/usr/bin/env Rscript

library(tidyverse)
library(ggforce)
library(civ6saves)

input <- "/mnt/data/nextcloud/saves_france/"
input <- commandArgs(trailingOnly = TRUE)[[1]]

webm_file <- paste0(input, "/output.webm")

# convert to rds
walk(list.files(input, ".*\\.Civ6Save$", full.names = TRUE), function(file) {
  bin_file <- gsub("Civ6Save", "Civ6Save.bin", file) %>% basename()
  tsv_file <- gsub("Civ6Save", "tsv", file)
  rds_file <- gsub("Civ6Save", "rds", file)

  if (!file.exists(rds_file)) {
    cat("Generating ", rds_file, "\n", sep = "")

    # decompressing binary data, parsing uncompressed data
    yaml <- system(
      # paste0("node node_modules/civ6-save-parser/index.js \"", file, "\" --outputCompressed"),
      paste0("node node_modules/civ6-save-parser/index.js \"", file, "\""),
      intern = TRUE,
      ignore.stderr = TRUE
    )
    yaml_list <- yaml::yaml.load(yaml, handlers = list(int = identity))

    # parse compressed data
    system(paste0("node civ6save-editing/scripts/savetomaptsv.js \"", file, "\" \"", gsub("\\.tsv", "", tsv_file), "\""))
    out <- list(
      map = read_tsv(tsv_file, col_types = cols(.default = "c")) %>%
        mutate_at(vars(-starts_with("buffer")), function(x) {
          y <- bit64::as.integer64(x)
          if (max(y, na.rm = TRUE) <= .Machine$integer.max) {
            as.integer(y)
          } else {
            y
          }
        })
    )
    file.remove(tsv_file)
    # out <- read_save(bin_file)
    # file.remove(bin_file)

    out$actors <- yaml_list$ACTORS %>% map(function(li) map(li, function(li2) li2[["data"]])) %>% map_df(data.frame) %>% as_tibble()
    out$civs <- yaml_list$CIVS %>% map(function(li) map(li, function(li2) li2[["data"]])) %>% map_df(data.frame) %>% as_tibble()
    out$game_speed <- yaml_list$GAME_SPEED$data
    out$map_size <- yaml_list$MAP_SIZE$data
    out$mod_block_1 <- yaml_list$MOD_BLOCK_1$data %>% map(function(li) map(li, function(li2) li2[["data"]])) %>% map_df(data.frame) %>% as_tibble()
    out$mod_block_2 <- yaml_list$MOD_BLOCK_2$data %>% map(function(li) map(li, function(li2) li2[["data"]])) %>% map_df(data.frame) %>% as_tibble()
    out$mod_block_3 <- yaml_list$MOD_BLOCK_3$data %>% map(function(li) map(li, function(li2) li2[["data"]])) %>% map_df(data.frame) %>% as_tibble()
    out$game_turn <- yaml_list$GAME_TURN$data
    out$map_file <- yaml_list$MAP_FILE$data

    write_rds(out, rds_file, compress = "gz")
  }
})

rds_files <- list.files(input, ".*\\.rds$", full.names = TRUE)

# fetch static data
out_static <- read_rds(rds_files[[1]])
tab_static <- out_static$map %>%
  add_coordinates() %>%
  left_join(terrains, by = "terrain") %>%
  left_join(features, by = "feature")

rivers <- get_river_coordinates(tab_static)

# construct leader colours
owner_ids <- unlist(map(rds_files, function(rds_file) {
  read_rds(rds_file)$map$owner
})) %>% unique() %>% sort()

leader_colours <- bind_rows(
  out_static$civs %>%
    transmute(owner = row_number() - 1L, leader = LEADER_NAME) %>%
    left_join(leaders, by = "leader") %>%
    rename(leader_inner_colour = leader_outer_colour, leader_outer_colour = leader_inner_colour),
  tibble(
    owner = setdiff(owner_ids, c(seq_len(nrow(out_static$civs)) - 1, 62L, 255L)),
    leader = NA,
    leader_name = paste0("City State ", seq_along(owner)),
    leader_inner_colour = rainbow(length(owner)),
    leader_outer_colour = "#111111"
  ),
  tribble(
    ~owner, ~leader, ~leader_name, ~leader_inner_colour, ~leader_outer_colour,
    62L, "BARBARIAN", "Barbarian", "black", "black",
    255L, "LEADER_FREE_CITIES", "Free Cities", "red", "black"
  )
)
owner_outer_palette <- leader_colours %>% select(leader_name, leader_outer_colour) %>% deframe()
owner_inner_palette <- leader_colours %>% select(leader_name, leader_inner_colour) %>% deframe()

alpha <- 0.25

g0 <-
  plot_empty_map(tab_static) +
  ggforce::geom_regon(aes(fill = terrain_name), alpha = alpha) +
  geom_text(aes(label = "^"), tab_static %>% filter(terrain_form == "Hill"), alpha = alpha) +
  geom_text(aes(label = "^"), tab_static %>% filter(terrain_form == "Mountain"), fontface = "bold", alpha = alpha) +
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
        plot.title = element_text(hjust = .5, size = 16),
        plot.subtitle = element_text(hjust = .5, size = 12),
        # plot.margin = unit(c(-2, -2, -2, -2), "cm"),
        legend.position = "none")


walk(rds_files, function(rds_file) {
  pdf_file <- gsub("rds$", "pdf", rds_file)
  png_file <- gsub("rds$", "png", rds_file)

  if (!file.exists(png_file)) {
    out <- read_rds(rds_file)

    tab <- out$map %>%
      add_coordinates() %>%
      mutate(file = rds_file, file_base = gsub("\\..*$", "", basename(file))) %>%
      # left_join(resources, by = "resource") %>%
      # left_join(terrains, by = "terrain") %>%
      left_join(features, by = "feature") %>%
      left_join(improvements, by = "improvement") %>%
      # left_join(continents, by = "continent") %>%
      left_join(world_wonders, by = "world_wonder") %>%
      left_join(roads, by = "road") %>%
      left_join(leader_colours %>% select(owner, leader, leader_name), by = "owner")

    simple_borders <- bind_rows(
      left_join(tab %>% transmute(x0, y0, file, leader_name, x = ifelse(y %% 2 == 0, x, x + 1), y = y + 1, xa = x1, xb = x2, ya = y1, yb = y2, owner), tab %>% transmute(file, x, y, owner2 = owner), by = c("x", "y", "file")) %>% filter(!is.na(owner), owner != owner2 | is.na(owner2)),
      left_join(tab %>% transmute(x0, y0, file, leader_name, x = x + 1, y, xa = x2, xb = x3, ya = y2, yb = y3, owner), tab %>% transmute(file, x, y, owner2 = owner), by = c("x", "y", "file")) %>% filter(!is.na(owner), owner != owner2 | is.na(owner2)),
      left_join(tab %>% transmute(x0, y0, file, leader_name, x = ifelse(y %% 2 == 0, x, x + 1), y = y - 1, xa = x3, xb = x4, ya = y3, yb = y4, owner), tab %>% transmute(file, x, y, owner2 = owner), by = c("x", "y", "file")) %>% filter(!is.na(owner), owner != owner2 | is.na(owner2)),
      left_join(tab %>% transmute(x0, y0, file, leader_name, x = ifelse(y %% 2 == 0, x - 1, x), y = y - 1, xa = x4, xb = x5, ya = y4, yb = y5, owner), tab %>% transmute(file, x, y, owner2 = owner), by = c("x", "y", "file")) %>% filter(!is.na(owner), owner != owner2 | is.na(owner2)),
      left_join(tab %>% transmute(x0, y0, file, leader_name, x = x - 1, y, xa = x5, xb = x6, ya = y5, yb = y6, owner), tab %>% transmute(file, x, y, owner2 = owner), by = c("x", "y", "file")) %>% filter(!is.na(owner), owner != owner2 | is.na(owner2)),
      left_join(tab %>% transmute(x0, y0, file, leader_name, x = ifelse(y %% 2 == 0, x - 1, x), y = y + 1, xa = x6, xb = x1, ya = y6, yb = y1, owner), tab %>% transmute(file, x, y, owner2 = owner), by = c("x", "y", "file")) %>% filter(!is.na(owner), owner != owner2 | is.na(owner2))
    ) %>% mutate(xa = xa * .9 + x0 * .1, xb = xb * .9 + x0 * .1, ya = ya * .9 + y0 * .1, yb = yb * .9 + y0 * .1)

    cities <- tab %>% group_by(owner, city_1) %>% filter(district == min(district)) %>% ungroup()

    players <- out$actors %>% filter(ACTOR_TYPE == "CIVILIZATION_LEVEL_FULL_CIV") %>% select(leader = LEADER_NAME) %>% left_join(leaders, by = "leader")

    g <- g0 +
      ggnewscale::new_scale_fill() +
      ggforce::geom_regon(aes(r = civ6saves:::xy_ratio * .9), tab %>% filter(feature_name == "Ice"), fill = feature_palette[["Ice"]], alpha = .4) +
      labs(
        title = paste0("Turn: ", out$game_turn, ", Map: ", out$map_file),
        subtitle = paste0("Players: ", paste0(players$leader_name, collapse = ", "))
      )

    if (nrow(tab %>% filter(!is.na(owner))) > 0) {
      g <- g +
        ggforce::geom_regon(aes(fill = leader_name), tab, alpha = .6) +
        geom_segment(aes(x = xa, xend = xb, y = ya, yend = yb, colour = leader_name, x0 = xa, y0 = ya), simple_borders, size = 1) +
        scale_fill_manual(values = owner_outer_palette) +
        scale_colour_manual(values = owner_inner_palette) +
        geom_point(aes(x = x0, y = y0, colour = leader_name), cities, size = 3)
    }

    ggsave(pdf_file, g, width = 20, height = 11.3)
    pdftools::pdf_convert(pdf_file, format = "png", filenames = png_file, verbose = FALSE)
  }
})

if (file.exists(webm_file)) file.remove(webm_file)
system(paste0("ffmpeg -framerate 1 -f image2 -i ", input, "/%*.png -c:v libvpx-vp9 -pix_fmt yuva420p ", webm_file))
