library(tidyverse)
library(googlesheets4)

gs4_auth("brechtus@gmail.com")

map_dimensions <- read_sheet("1bOlgW25zpWOUTPcPcNDbfpXK5f90J2BBaBuzwypABQs", "map_dimensions") %>%
  mutate_at(vars(x, y, size), as.integer)
usethis::use_data(map_dimensions, overwrite = TRUE)

terrains <- read_sheet("1bOlgW25zpWOUTPcPcNDbfpXK5f90J2BBaBuzwypABQs", "terrains") %>%
  mutate(terrain = bit64::as.integer64(terrain))
usethis::use_data(terrains, overwrite = TRUE)

terrain_palette <- read_sheet("1bOlgW25zpWOUTPcPcNDbfpXK5f90J2BBaBuzwypABQs", "terrain_palette") %>%
  deframe()
usethis::use_data(terrain_palette, overwrite = TRUE)

feature_palette <- read_sheet("1bOlgW25zpWOUTPcPcNDbfpXK5f90J2BBaBuzwypABQs", "feature_palette") %>%
  deframe()
usethis::use_data(feature_palette, overwrite = TRUE)

features <- read_sheet("1bOlgW25zpWOUTPcPcNDbfpXK5f90J2BBaBuzwypABQs", "features") %>%
  mutate(feature = bit64::as.integer64(feature))
usethis::use_data(features, overwrite = TRUE)

resources <- read_sheet("1bOlgW25zpWOUTPcPcNDbfpXK5f90J2BBaBuzwypABQs", "resources") %>%
  mutate(resource = bit64::as.integer64(resource))
usethis::use_data(resources, overwrite = TRUE)

improvements <- read_sheet("1bOlgW25zpWOUTPcPcNDbfpXK5f90J2BBaBuzwypABQs", "improvements") %>%
  mutate(improvement = bit64::as.integer64(improvement))
usethis::use_data(improvements, overwrite = TRUE)

continents <- read_sheet("1bOlgW25zpWOUTPcPcNDbfpXK5f90J2BBaBuzwypABQs", "continents") %>%
  mutate(continent = bit64::as.integer64(continent))
usethis::use_data(continents, overwrite = TRUE)

roads <- read_sheet("1bOlgW25zpWOUTPcPcNDbfpXK5f90J2BBaBuzwypABQs", "roads")
usethis::use_data(roads, overwrite = TRUE)

world_wonders <- read_sheet("1bOlgW25zpWOUTPcPcNDbfpXK5f90J2BBaBuzwypABQs", "world_wonders") %>%
  mutate(world_wonder = bit64::as.integer64(world_wonder))
usethis::use_data(world_wonders, overwrite = TRUE)

leaders <- read_sheet("1bOlgW25zpWOUTPcPcNDbfpXK5f90J2BBaBuzwypABQs", "leaders")
usethis::use_data(leaders, overwrite = TRUE)
