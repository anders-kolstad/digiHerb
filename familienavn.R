# Finner familienavn til slekt, fra GBIF
# Get family name
library(rgbif)
library(dplyr)
setwd("/home/anders/Pictures/Anders digitale herbarium/Karplanter")

# Finn slektsnavn

# To do. Søk blant unique(slekt)

paths <-list.files(recursive = T, full.names = F)
lib <- as.data.frame(paths)
lib$temp <-  substr(paths, 5, nchar(paths))

lib <- tidyr::separate(data=lib,
                       col = temp,
                       into = c("Vitenskapelig navn"),
                       extra = "drop",
                       sep = " - ")
lib <- tidyr::separate(data=lib,
                       col = "Vitenskapelig navn",
                       into = c("Slekt"),
                       extra = "drop",
                       sep = " ")
slekt <- select(lib, Slekt)
rm(lib)
slekt2 <- as.data.frame(unique(slekt$Slekt))
colnames(slekt2) <- "slekt"

getFam <- function (x) {
  return(tryCatch(unlist(name_backbone(name=x[1], 
                                rank='genus', kingdom='plants')['family']), 
                  error=function(e) NULL))
}

slekt2$familie <- apply(slekt2, MARGIN = 1, 
                     FUN = getFam)

slekt2$familie <- as.character(slekt2$familie)

saveRDS(slekt2, 'slektOGfam.RData')
#getwd()
