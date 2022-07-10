pkgs <- c(
  "devtools",
  "dplyr",
  "lubridate",
  "golem",
  "pins",
  "shiny",
  "reactable",
  "impactR",
  "visualizeR",
  "ggblanket"
)

for (i in pkgs) {
  library(i, character.only = TRUE)
}
