# Finner familienavn til slekt, fra GBIF
# Get family name
library(rgbif)
library(dplyr)


# Finn slektsnavn
pathToImg <- "docs/img"

# list images with paths
dat <- list.files(path = pathToImg,
                     recursive = T, full.names = F) |>
  as_tibble() |>
  tidyr::separate(col = value,
                       into = c("Vitenskapelig navn"),
                       extra = "drop",
                       sep = " - ") |>
  tidyr::separate(
                       col = "Vitenskapelig navn",
                       into = c("Slekt"),
                       extra = "drop",
                       sep = " ") |>
  distinct(Slekt)

dat2 <- dat
dat <- dat[1:5,]
dat <- dat2
dat2 <- dat[1:5,]
# Function to get family name from gbif backbone
getFam <- function(x) {
  tryCatch({
    out <- rgbif::name_backbone(
      name = x[1],
      rank = "genus",
      kingdom = "plants",
      verbose = TRUE
    ) |>
      filter(confidence > 90, matchType != "HIGHERRANK") |>
      slice_head(n = 1) |>
      pull(family)
  if (length(out) == 0) NA_character_ else out[[1]]    
  }, error = function(e) NA_character_)
}

# A test. Acer sometimes get a famility, somethimes not. 
# I think it's a timeout issue. 
# Maybe I can run the function a few times, and overlap the results to fill some gaps?
#apply(dat2, MARGIN = 1, 
#                     FUN = getFam)

dat <- dat |>
  mutate(familie = map_chr(Slekt, getFam))


# Then I might do some hard coding

#dat <- dat |>
#  mutate(familie = case_when(
#    Slekt == "Bartsia" ~ "Orobanchacea",
#    Slekt == 
#    .default = familie
#  ))

saveRDS(slekt2, 'slektOGfam.RData', ascii = T)
#getwd()
