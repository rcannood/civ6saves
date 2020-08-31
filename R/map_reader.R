#' @importFrom tibble tibble
#' @importFrom bit64 NA_integer64_
#' @importFrom dplyr %>% filter
#' @importFrom progress progress_bar
#' @export
read_save <- function(file) {
  # read binary file in memory
  bin <- readBin(file, "raw", n = 1024*1024*1024)

  # read map header
  mapheader_ix <- binSearch(bin, c(0x0E, 0x00, 0x00, 0x00, 0x0F, 0x00, 0x00, 0x00, 0x06, 0x00, 0x00, 0x00))[[1]]
  map_size <- readInt32LE(bin, mapheader_ix + 12)
  map_dim <- map_dimensions %>% filter(size == map_size)

  pb <- progress::progress_bar$new(total = map_size)

  # allocate tibble
  map <- tibble(
    x = rep(seq_len(map_dim$x) - 1L, map_dim$y),
    y = rep(seq_len(map_dim$y) - 1L, each = map_dim$x),
    offset = NA_integer_,
    travel_regions = bit64::NA_integer64_,
    connected_regions = bit64::NA_integer64_,
    landmass = bit64::NA_integer64_,
    terrain = bit64::NA_integer64_,
    feature = bit64::NA_integer64_,
    natural_wonder_order = NA_integer_,
    continent = bit64::NA_integer64_,
    number_of_units = NA_integer_,
    resource = bit64::NA_integer64_,
    resource_boolean = NA_integer_,
    improvement = bit64::NA_integer64_,
    improvement_owner = NA_integer_,
    road = NA_integer_,
    appeal = NA_integer_,
    river_e = NA_integer_,
    river_se = NA_integer_,
    river_sw = NA_integer_,
    river_count = NA_integer_,
    river_map = NA_integer_,
    cliff_map = NA_integer_,
    flags1 = NA_integer_,
    flags2 = NA_integer_,
    flags3 = NA_integer_,
    flags4 = NA_integer_,
    flags5 = NA_integer_,
    flags6 = NA_integer_,
    flags7 = NA_integer_,
    buffer1 = NA_character_,
    buffer1_flag = NA_integer_,
    buffer2 = NA_character_,

    # buffer4
    city_1 = bit64::NA_integer64_,
    city_2 = bit64::NA_integer64_,
    district = bit64::NA_integer64_,
    owner = NA_integer_,
    world_wonder = bit64::NA_integer64_,

    tile_length = NA_integer_
  )

  # read map
  map_ix <- mapheader_ix + 16

  for (ii in seq_len(map_size)) {
    start <- map_ix

    map$offset[[ii]] <- map_ix
    map$travel_regions[[ii]] <- readUInt32LE(bin, map_ix)
    map$connected_regions[[ii]] <- readUInt32LE(bin, map_ix + 4)
    map$landmass[[ii]] <- readUInt32LE(bin, map_ix + 8)
    map$terrain[[ii]] <- readUInt32LE(bin, map_ix + 12)
    map$feature[[ii]] <- readUInt32LE(bin, map_ix + 16)
    map$natural_wonder_order[[ii]] <- readUInt16LE(bin, map_ix + 20)
    map$continent[[ii]] <- readUInt32LE(bin, map_ix + 22)
    map$number_of_units[[ii]] <- readUInt8(bin, map_ix + 26)
    map$resource[[ii]] <- readUInt32LE(bin, map_ix + 27)
    map$resource_boolean[[ii]] <- readUInt16LE(bin, map_ix + 31)
    map$improvement[[ii]] <- readUInt32LE(bin, map_ix + 33)
    map$improvement_owner[[ii]] <- readInt8(bin, map_ix + 37)
    map$road[[ii]] <- readInt16LE(bin, map_ix + 38)
    map$appeal[[ii]] <- readInt16LE(bin, map_ix + 40)
    map$river_e[[ii]] <- readInt8(bin, map_ix + 42)
    map$river_se[[ii]] <- readInt8(bin, map_ix + 43)
    map$river_sw[[ii]] <- readInt8(bin, map_ix + 44)
    map$river_count[[ii]] <- readUInt8(bin, map_ix + 45)
    map$river_map[[ii]] <- readUInt8(bin, map_ix + 46)
    map$cliff_map[[ii]] <- readUInt8(bin, map_ix + 47)
    map$flags1[[ii]] <- readUInt8(bin, map_ix + 48)
    map$flags2[[ii]] <- readUInt8(bin, map_ix + 49)
    map$flags3[[ii]] <- readUInt8(bin, map_ix + 50)
    map$flags4[[ii]] <- readUInt8(bin, map_ix + 51)
    map$flags5[[ii]] <- readUInt8(bin, map_ix + 52)
    map$flags6[[ii]] <- readUInt8(bin, map_ix + 53)
    map$flags7[[ii]] <- readUInt8(bin, map_ix + 54)
    map_ix <- map_ix + 55

    if (bitwAnd(map$flags4[[ii]], 1)) {
      map$buffer1[[ii]] <- readHex(bin, map_ix, 24)
      map$buffer1_flag[[ii]] <- readUInt8(bin, map_ix + 20)
      map_ix <- map_ix + 24

      if (bitwAnd(map$buffer1_flag[[ii]], 1)) {
        map$buffer2[[ii]] <- readHex(bin, map_ix, 20)
        map_ix <- map_ix + 20
      }
    } else if (bitwAnd(map$flags4[[ii]], 2)) {
      map$buffer1[[ii]] <- readHex(bin, map_ix, 24)
      map$buffer1_flag[[ii]] <- readUInt8(bin, map_ix + 20)
      map$buffer2[[ii]] <- readHex(bin, map_ix + 24, 20)
      map_ix <- map_ix + 44
    }

    if (bitwAnd(map$flags2[[ii]], 64)) {
      map$city_1[[ii]] = readUInt32LE(bin, map_ix)
      map$city_2[[ii]] = readUInt32LE(bin, map_ix + 4)
      map$district[[ii]] = readUInt32LE(bin, map_ix + 8)
      map$owner[[ii]] = readUInt8(bin, map_ix + 12)
      map$world_wonder[[ii]] = readUInt32LE(bin, map_ix + 13)
      map_ix <- map_ix + 17
    }
    map$tile_length[[ii]] <- map_ix - start

    pb$tick()
  }


  lst(
    map = map
  )
}
