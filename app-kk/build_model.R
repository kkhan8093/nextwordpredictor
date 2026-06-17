# Build a compact n-gram "stupid back-off" model from the SwiftKey corpora.
# Output: small .rds lookup tables that the Shiny app loads.
suppressPackageStartupMessages({
  library(data.table)
  library(stringr)
  library(dplyr)
  library(tidytext)
})
set.seed(2026)

data_dir <- "C:/Users/kkhan/nextwordpredictor/data/raw/final/en_US"
files <- file.path(data_dir, c("en_US.blogs.txt", "en_US.news.txt", "en_US.twitter.txt"))
out_dir <- "C:/Users/kkhan/nextwordpredictor/app-kk/model"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# --- read a random fraction of lines from a file ---
read_sample <- function(path, frac) {
  con <- file(path, open = "r", encoding = "UTF-8")
  txt <- readLines(con, skipNul = TRUE, warn = FALSE)
  close(con)
  txt <- iconv(txt, "UTF-8", "UTF-8", sub = "")
  txt[rbinom(length(txt), 1, frac) == 1]
}

cat("Reading 5% sample of each file...\n")
samp <- c(read_sample(files[1], 0.05),
          read_sample(files[2], 0.05),
          read_sample(files[3], 0.05))
cat("Sampled lines:", length(samp), "\n")

# --- clean: lowercase, drop urls, keep letters + apostrophes ---
cat("Cleaning text...\n")
clean <- samp %>%
  str_to_lower() %>%
  str_replace_all("https?://\\S+|www\\.\\S+", " ") %>%
  str_replace_all("[^a-z' ]", " ") %>%
  str_replace_all("\\s+", " ") %>%
  str_trim()
clean <- clean[clean != ""]
rm(samp); invisible(gc())

dt <- data.table(text = clean)

# --- unigrams (fallback) ---
cat("Building unigrams...\n")
uni <- dt %>% unnest_tokens(w, text) %>% count(w, sort = TRUE)
uni_top <- head(uni$w, 10)                       # global most-common words

# --- bigrams: top-3 next word per first word ---
cat("Building bigrams...\n")
bi <- as.data.table(dt %>% unnest_tokens(bg, text, token = "ngrams", n = 2) %>%
                      filter(!is.na(bg)) %>% count(bg, sort = TRUE))
bi <- bi[n >= 2]
bi[, c("w1", "w2") := tstrsplit(bg, " ", fixed = TRUE)]
bi <- bi[!is.na(w2)]
setorder(bi, w1, -n)
bi_top <- bi[, head(.SD, 3), by = w1, .SDcols = c("w2", "n")]
setkey(bi_top, w1)

# --- trigrams: top-3 next word per two-word prefix ---
cat("Building trigrams...\n")
tri <- as.data.table(dt %>% unnest_tokens(tg, text, token = "ngrams", n = 3) %>%
                       filter(!is.na(tg)) %>% count(tg, sort = TRUE))
tri <- tri[n >= 2]
tri[, c("w1", "w2", "w3") := tstrsplit(tg, " ", fixed = TRUE)]
tri <- tri[!is.na(w3)]
tri[, prefix := paste(w1, w2)]
setorder(tri, prefix, -n)
tri_top <- tri[, head(.SD, 3), by = prefix, .SDcols = c("w3", "n")]
setkey(tri_top, prefix)

# --- save compact model ---
saveRDS(tri_top, file.path(out_dir, "tri_top.rds"), compress = "xz")
saveRDS(bi_top,  file.path(out_dir, "bi_top.rds"),  compress = "xz")
saveRDS(uni_top, file.path(out_dir, "uni_top.rds"), compress = "xz")

cat("\n==== MODEL BUILT ====\n")
cat("trigram prefixes:", uniqueN(tri_top$prefix), " rows:", nrow(tri_top), "\n")
cat("bigram firsts   :", uniqueN(bi_top$w1),     " rows:", nrow(bi_top), "\n")
sz <- function(f) round(file.info(file.path(out_dir, f))$size / 2^20, 2)
cat("sizes (MB): tri", sz("tri_top.rds"), "| bi", sz("bi_top.rds"), "| uni", sz("uni_top.rds"), "\n")
