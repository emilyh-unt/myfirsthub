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
myddt <- function(df, species){

  print(df)
  table <- table(df$RIVER) /length(df$RIVER)
  print(table)

  df1 <- filter(df, species == SPECIES)
  print(df1)
  if (species == "CCATFISH"){
    write.csv(x = df1, "LvsWforCCATFISH.csv", row.names = FALSE)
  }
  if (species == "SMBUFFALO"){
    write.csv(x = df1, "LvsWforSMBUFFALO.csv", row.names = FALSE)
  }
  g <- ggplot(df1, aes_string(x="WEIGHT",y="LENGTH")) +
    geom_point(aes_string(col = "RIVER")) +
    geom_smooth(formula = y~x +I(x^2), method = "lm") +
    ggtitle("Emily Hunt")
  print(g)
}

myddt(df = ddt, species = "CCATFISH")
myddt(df = ddt, species = "SMBUFFALO")
