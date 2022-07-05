pkgs <- c(
  "devtools",
  "dplyr",
  "lubridate",
  "golem",
  "pins",
  "shiny",
  "reactable",
  "impactR"
)

for (i in pkgs) {
  library(i, character.only = TRUE)
}
