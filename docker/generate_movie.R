#!/usr/bin/env Rscript

library(tidyverse)
library(ggforce)
library(civ6saves)

input <- "/mnt/data/nextcloud/civ6_saves/saves_france/"
#input <- "/mnt/data/nextcloud/saves_france/"
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
      paste0("node node_modules/civ6-save-parser/index.js \"", file, "\" --simple"),
      intern = TRUE,
      ignore.stderr = TRUE
    ) %>%
      gsub("`([^']*)'(.*)`", "`\\1\\2`", .) %>%
      gsub("`([^']*)'(.*)`", "`\\1\\2`", .) %>%
      gsub("`([^']*)'(.*)`", "`\\1\\2`", .) %>%
      gsub("`([^']*)'(.*)`", "`\\1\\2`", .) %>%
      gsub("`([^']*)'(.*)`", "`\\1\\2`", .) %>%
      gsub("`([^']*)'(.*)`", "`\\1\\2`", .) %>%
      gsub("`([^']*)'(.*)`", "`\\1\\2`", .) %>%
      gsub("`([^']*)'(.*)`", "`\\1\\2`", .) %>%
      gsub("`([^']*)`", "'\\1'", .)

    out <- yaml::yaml.load(yaml, handlers = list(int = identity))

    for (nam in c("ACTORS", "CIVS", "MOD_BLOCK_1", "MOD_BLOCK_2", "MOD_BLOCK_3")) {
      out[[nam]] <- out[[nam]] %>% map_df(data.frame) %>% as_tibble()
    }

    # parse compressed data
    system(paste0("node civ6save-editing/scripts/savetomaptsv.js \"", file, "\" \"", gsub("\\.tsv", "", tsv_file), "\""))
    out$MAP <- read_tsv(tsv_file, col_types = cols(.default = "c")) %>%
      mutate_at(vars(-starts_with("buffer")), function(x) {
        y <- bit64::as.integer64(x)
        if (max(y, na.rm = TRUE) <= .Machine$integer.max) {
          as.integer(y)
        } else {
          y
        }
      })

    file.remove(tsv_file)
    # out <- read_save(bin_file)
    # file.remove(bin_file)

    write_rds(out, rds_file, compress = "gz")
  }
})

rds_files <- list.files(input, ".*\\.rds$", full.names = TRUE)

# fetch static data
out_static <- read_rds(rds_files[[1]])
tab_static <- out_static$MAP %>%
  add_coordinates() %>%
  left_join(terrains, by = "terrain") %>%
  left_join(features, by = "feature")

rivers <- get_river_coordinates(tab_static)

# construct leader colours
owner_ids <- unlist(map(rds_files, function(rds_file) {
  read_rds(rds_file)$MAP$owner
})) %>% unique() %>% sort()

assertthat::assert_that(all(out_static$CIVS$LEADER_NAME %in% leaders$leader))

# assign colours to civs
leader_colours <- bind_rows(
  out_static$CIVS %>%
    transmute(owner = row_number() - 1L, leader = LEADER_NAME) %>%
    left_join(leaders, by = "leader") %>%
    rename(leader_inner_colour = leader_outer_colour, leader_outer_colour = leader_inner_colour),
  tibble(
    owner = setdiff(owner_ids, c(seq_len(nrow(out_static$CIVS)) - 1, 62L, 255L))
  ) %>% mutate(
    leader = NA_character_,
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

# check colours
# ggplot(leader_colours, aes(0, seq_along(owner))) +
#   geom_point(aes(colour = leader_inner_colour), size = 6) +
#   geom_point(aes(colour = leader_outer_colour), size = 4) +
#   geom_text(aes(x = 1, label = leader_name), hjust = 0) +
#   scale_colour_identity() +
#   expand_limits(x = 2) +
#   theme_minimal()

owner_outer_palette <- leader_colours %>% select(leader_name, leader_outer_colour) %>% deframe()
owner_inner_palette <- leader_colours %>% select(leader_name, leader_inner_colour) %>% deframe()

alpha <- 0.25

g0 <-
  plot_empty_map(tab_static) +
  ggforce::geom_regon(aes(fill = terrain_name), alpha = alpha) +
  geom_text(aes(label = "^"), tab_static %>% filter(terrain_form == "Hill"), alpha = alpha) +
  geom_text(aes(label = "^"), tab_static %>% filter(terrain_form == "Mountain"), fontface = "bold", alpha = alpha) +
  geom_segment(aes(x = xa, xend = xb, y = ya, yend = yb), colour = feature_palette[["River"]], rivers, size = 1, alpha = alpha) +
  scale_fill_manual(values = terrain_palette)


walk(rds_files, function(rds_file) {
  pdf_file <- gsub("rds$", "pdf", rds_file)
  png_file <- gsub("rds$", "png", rds_file)

  if (!file.exists(png_file)) {
    cat("Plotting ", rds_file, "\n", sep = "")
    out <- read_rds(rds_file)

    tab <- out$MAP %>%
      add_coordinates() %>%
      left_join(features, by = "feature") %>%
      left_join(improvements, by = "improvement") %>%
      left_join(world_wonders, by = "world_wonder") %>%
      left_join(roads, by = "road") %>%
      left_join(leader_colours %>% select(owner, leader, leader_name), by = "owner") %>%
      mutate(leader_name = factor(leader_name, levels = leader_colours$leader_name))

    civ_borders <- get_border_coordinates(tab)
    road_coords <- get_road_coordinates(tab)

    cities <- tab %>% group_by(owner, city_1) %>% filter(district == min(district)) %>% ungroup()

    players <- out$ACTORS %>% filter(ACTOR_TYPE == "CIVILIZATION_LEVEL_FULL_CIV") %>% select(leader = LEADER_NAME) %>% left_join(leaders, by = "leader")

    g <- g0 +
      ggforce::geom_regon(aes(r = civ6saves:::xy_ratio * .9), tab %>% filter(feature_name == "Ice"), fill = feature_palette[["Ice"]], alpha = .4) +
      labs(
        title = paste0("Turn ", out$GAME_TURN, " - ", tolower(gsub("MAPSIZE_", "", out$MAP_SIZE)), " - ", out$MAP_FILE),
        subtitle = paste0(players$leader_name, collapse = ", "),
        fill = "Terrain"
      )

    if (nrow(road_coords) > 0) {
      g <- g +
        geom_segment(aes(x = xa, xend = xb, y = ya, yend = yb, colour = road_name), road_coords %>% mutate(x0 = xa, y0 = ya), size = 1, alpha = .4) +
        scale_colour_manual(values = c("Ancient Road" = "#8e712b", "Railroad" = "darkgray", "Classical Road" = "#a2a2a2", "Industrial Road" = "#6e6e6e", "Modern Road" = "#424242")) +
        labs(colour = "Road")
    }

    if (nrow(tab %>% filter(!is.na(owner))) > 0) {
      g <- g +
        ggnewscale::new_scale_fill() +
        ggnewscale::new_scale_color() +
        ggforce::geom_regon(aes(fill = leader_name), tab %>% filter(!is.na(leader_name)), alpha = .6) +
        geom_segment(aes(x = xa, xend = xb, y = ya, yend = yb, colour = leader_name), civ_borders %>% filter(!is.na(leader_name)), size = 1) +
        geom_point(aes(x = x0, y = y0, colour = leader_name), cities %>% filter(!is.na(leader_name)), size = 3) +
        scale_fill_manual(values = owner_outer_palette) +
        scale_colour_manual(values = owner_inner_palette)
    }

    ggsave(pdf_file, g, width = 20, height = 13)
    pdftools::pdf_convert(pdf_file, format = "png", filenames = png_file, verbose = FALSE)
  }
})

cat("Combining pngs with ffmpeg\n")
if (file.exists(webm_file)) file.remove(webm_file)
system(paste0("ffmpeg -framerate 4 -f image2 -i ", input, "/%*.png -c:v libvpx-vp9 -pix_fmt yuva420p ", webm_file))
