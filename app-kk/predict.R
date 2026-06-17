# Prediction logic for the Next-Word Predictor (sourced by app.R and tests).
library(data.table)

tri_top <- readRDS("model/tri_top.rds")
bi_top  <- readRDS("model/bi_top.rds")
uni_top <- readRDS("model/uni_top.rds")
setkey(tri_top, prefix)
setkey(bi_top, w1)

clean_input <- function(x) {
  x <- tolower(x)
  x <- gsub("https?://\\S+|www\\.\\S+", " ", x)
  x <- gsub("[^a-z' ]", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

# Return up to k predicted next words using stupid back-off.
predict_next <- function(phrase, k = 3) {
  toks <- strsplit(clean_input(phrase), " ")[[1]]
  toks <- toks[toks != ""]
  n <- length(toks)
  res <- character(0)

  if (n >= 2) {                                   # trigram: last two words
    pre <- paste(toks[n - 1], toks[n])
    hit <- tri_top[.(pre), nomatch = 0L]
    if (nrow(hit)) res <- hit$w3
  }
  if (length(res) < k && n >= 1) {                # back off to bigram: last word
    hit <- bi_top[.(toks[n]), nomatch = 0L]
    if (nrow(hit)) res <- unique(c(res, hit$w2))
  }
  if (length(res) < k) {                          # back off to unigram
    res <- unique(c(res, uni_top))
  }
  head(res, k)
}
