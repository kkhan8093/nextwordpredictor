setwd("C:/Users/kkhan/nextwordpredictor/app-kk")
source("predict.R")

phrases <- c(
  "the president of the united",
  "happy mothers",
  "i can't wait to",
  "thanks for the",
  "looking forward to the"
)

cat("==== PREDICTION TEST (top 3 each) ====\n")
ok <- TRUE
for (p in phrases) {
  pred <- predict_next(p, 3)
  if (length(pred) == 0) ok <- FALSE
  cat(sprintf("%-32s -> %s\n", p, paste(pred, collapse = ", ")))
}
cat("\nEvery phrase returned a prediction:", ok, "\n")
