#' @title My DDT
#'
#' @param df data frame
#' @param species species of fish
#'
#' @return a scatterplot showing length vs weight for a specific species
#' @export
#'
#' @examples \dontrun{myddt(df = ddt, species = "CCATFISH")}

library(dplyr)
library(ggplot2)
library(Intro2R)
myddt <- function(df, SPECIES){

  print(df)
  table <- table(df$RIVER) /length(df$RIVER)
  print(table)
  newdf <- df %>% filter(SPECIES == {{SPECIES}})
  print(newdf)
  if (SPECIES == "CCATFISH"){
    write.csv(x = newdf, "LvsWforCCATFISH.csv", row.names = FALSE)
  }
  if (SPECIES == "SMBUFFALO"){
    write.csv(x = newdf, "LvsWforSMBUFFALO.csv", row.names = FALSE)
  }
  g <- ggplot(newdf, aes_string(x="WEIGHT",y="LENGTH")) +
    geom_point(aes_string(col = "RIVER")) +
    geom_smooth(formula = y~x +I(x^2), method = "lm") +
    ggtitle("Emily Hunt")
  print(g)
}

myddt(df = ddt, SPECIES = "CCATFISH")
myddt(df = ddt, SPECIES = "SMBUFFALO")
