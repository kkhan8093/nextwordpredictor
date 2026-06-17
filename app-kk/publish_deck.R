Sys.setenv(RSTUDIO_PANDOC = "C:/Users/kkhan/pandoc/pandoc-3.10")

# Re-knit the deck with the live app URL
rmarkdown::render("C:/Users/kkhan/nextwordpredictor/app-kk/pitch-deck.Rmd", quiet = TRUE)

# Upload to RPubs (returns a claim URL to finish publishing in the browser)
library(rsconnect)
res <- rpubsUpload(
  title       = "Next-Word Predictor - Capstone Pitch",
  contentFile = "C:/Users/kkhan/nextwordpredictor/app-kk/pitch-deck.html",
  originalDoc = "C:/Users/kkhan/nextwordpredictor/app-kk/pitch-deck.Rmd"
)

cat("\n==== DECK RPUBS UPLOAD ====\n")
cat("continueUrl :", if (is.null(res$continueUrl)) "(none)" else res$continueUrl, "\n")
if (!is.null(res$error)) cat("error       :", res$error, "\n")
saveRDS(res, "C:/Users/kkhan/nextwordpredictor/app-kk/rpubs_deck_result.rds")
