
#
# Uwaga: 
# Funkcja percent_map jest zaprojektowana do działania ze zbiorem danych stany.rds
# Może nie działać poprawnie z innymi zbiorami danych jeżeli kolejność ich wierszy
# nie będzie dokładnie odpowiadać kolejności, w jakiej pakiet "maps" generuje wykresy danych.

percent_map <- function(var, color, legend.title, min = 0, max = 100) {

  # wygeneruj wektor kolorów wypełniających dla mapy
  shades <- colorRampPalette(c("white", color))(100)
  
  # ogranicz gradient do procentów, które występują między min a max
  var <- pmax(var, min)
  var <- pmin(var, max)
  percents <- as.integer(cut(var, 100, 
    include.lowest = TRUE, ordered = TRUE))
  fills <- shades[percents]

  # wygeneruj kartogram (mapę typu choropleth)
  
  # wypełnij hrabstwa kolorem
  map("county", fill = TRUE, col = fills, 
    resolution = 0, lty = 0, projection = "polyconic", 
    myborder = 0, mar = c(0,0,0,0))
  
  # nałóż granice stanów
  map("state", col = "white", fill = FALSE, add = TRUE,
    lty = 1, lwd = 1, projection = "polyconic", 
    myborder = 0, mar = c(0,0,0,0))
  
  # dodaj legendę
  inc <- (max - min) / 4
  legend.text <- c(paste0(min, " % lub mniej"),
    paste0(min + inc, " %"),
    paste0(min + 2 * inc, " %"),
    paste0(min + 3 * inc, " %"),
    paste0(max, " % lub więcej"))
  
  legend("bottomleft",
    legend = legend.text, 
    fill = shades[c(1, 25, 50, 75, 100)], 
    title = legend.title)
}

