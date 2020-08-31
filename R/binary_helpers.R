#' @importFrom assertthat assert_that
binSearch <- function(x, pattern) {
  assertthat::assert_that(length(pattern) > 0)
  ix <- which(x == pattern[[1]])
  ix <- ix[ix <= length(x) - length(pattern) + 1]

  i <- 2
  while (i <= length(pattern)) {
    ix <- ix[x[ix+i-1] == pattern[[i]]]
    i <- i + 1
  }

  ix
}

readInt32LE <- function(x, index) {
  readBin(x[index + 0:3], what = "integer", endian = "little", signed = TRUE)
}

#' @importFrom bit64 as.integer64
readUInt32LE <- function(x, index) {
  sum(as.integer(x[index + 0:3]) * bit64::as.integer64(256)^(0L:3L))
}
readUInt8 <- function(x, index) {
  as.integer(x[[index]])
}
readInt8 <- function(x, index) {
  int <- as.integer(x[[index]])
  bitwAnd(int, 127) - bitwAnd(int, 128)
}
readInt16LE <- function(x, index) {
  int <- readUInt16LE(x, index)
  bitwAnd(int, 32767) - bitwAnd(int, 32768)
}
readUInt16LE <- function(x, index) {
  as.integer(x[index]) + as.integer(x[index+1]) * 256
}
readBit <- function(x, index, bit) {
  assertthat::assert_that(bit > 0, bit < 8)
  (bitwAnd(as.integer(x[index]), bitwShiftL(1, bit-1)) > 0) + 0
}
readRaw <- function(x, index, length) {
  x[index + seq_len(length) - 1]
}
readHex <- function(x, index, length) {
  paste0(as.character(readRaw(x, index, length)), collapse = "")
}
