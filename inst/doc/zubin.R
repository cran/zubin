## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>")

## ----setup--------------------------------------------------------------------
library(zubin)

## -----------------------------------------------------------------------------
wav <- bin_layout("<riff:s4 size:u32 wave:s4 fmt:s4 fmt_size:u32 format:u16 channels:u16
                   rate:u32 byte_rate:u32 block_align:u16 bits:u16 data:s4 data_size:u32")
wav
bin_size(wav)

## -----------------------------------------------------------------------------
samples <- bin_encode(round(32767 * sin(2 * pi * 440 * seq(0, 1, by = 1 / 8000))), "i16")
hdr <- bin_pack(wav, riff = "RIFF", size = 36 + length(samples), wave = "WAVE",
                fmt = "fmt ", fmt_size = 16, format = 1, channels = 1, rate = 8000,
                byte_rate = 16000, block_align = 2, bits = 16, data = "data",
                data_size = length(samples))
path <- tempfile(fileext = ".wav")
writeBin(c(hdr, samples), path)

## -----------------------------------------------------------------------------
x <- readBin(path, "raw", file.size(path))
h <- bin_unpack(x, wav, n = 1)
h[, c("channels", "rate", "bits", "data_size")]
pcm <- bin_decode(x, "i16", offset = bin_size(wav), n = h$data_size / 2)
head(pcm)
bin_hexdump(x, n = 48)

## -----------------------------------------------------------------------------
hdr <- bin_layout(">magic:u32 version:u16 count:u32")
rec <- bin_layout(">id:u32 ts:i64 price:f64 qty:i32 side:u8 x3")
trades <- data.frame(id = 1:4, ts = 1.7e12 + c(0, 15, 40, 41),
                     price = c(101.5, 101.25, 101.75, 101.5),
                     qty = c(100L, 50L, 75L, 10L), side = c(1L, 2L, 1L, 2L))

b <- bin_builder()
bin_put(b, bin_pack(hdr, magic = 0xCAFEBABE, version = 1, count = nrow(trades)))
bin_put(b, bin_pack(rec, trades))
b
path <- tempfile()
writeBin(bin_take(b), path)

## -----------------------------------------------------------------------------
x <- readBin(path, "raw", file.size(path))
h <- bin_unpack(x, hdr, n = 1)
h$magic == 0xCAFEBABE
bin_unpack(x, rec, offset = bin_size(hdr), n = h$count)

## -----------------------------------------------------------------------------
s <- bin_layout("=a:u8 b:u32 c:u16", align = TRUE)
s$fields
bin_size(s)

## -----------------------------------------------------------------------------
pts <- bin_layout("<id:u16 xyz:f32[3]")
x <- bin_pack(pts, id = 1:2, xyz = matrix(c(0, 1, 2, 3, 4, 5), 2))
bin_unpack(x, pts)
bin_unpack(x, pts, as = "list")$xyz

## -----------------------------------------------------------------------------
big <- bin_encode(2^53, "u64")
bin_decode(big, "u64")
try(bin_decode(bin_encode(2^53 + 2, "u64"), "u64"))

## -----------------------------------------------------------------------------
a <- bin_pack(rec, trades)
b <- a
b[c(9, 50)] <- as.raw(0)
bin_diff(a, b)

## -----------------------------------------------------------------------------
e <- tryCatch(bin_encode(c(1, 300), "u8"), zubin_range_error = function(e) e)
c(e$index, class(e)[1])

