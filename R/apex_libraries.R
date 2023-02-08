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
  "shinyscreenshot",
  "ggplot2",
  "forcats"
)

for (i in pkgs) {
  library(i, character.only = TRUE)
}
