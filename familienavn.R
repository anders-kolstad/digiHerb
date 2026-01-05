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


# Function to get family name from gbif backbone
getFam <- function(x) {
  out <- tryCatch(
      name_backbone(
        name = x[1],
        rank = 'genus',
        kingdom = 'plants',
        verbose = TRUE
      ),
      error = function(e) NULL
    )

  out <- tryCatch(
      out |>
        filter(confidence > 90, matchType != "HIGHERRANK") |>
        slice_head(n = 1) |>
        select(family),
      error = function(e) NULL
    )

  return(out$family)
}

unlist(apply(dat[1:5,], MARGIN = 1, 
                     FUN = getFam))

dat$familie <- apply(dat, MARGIN = 1, 
                     FUN = getFam)

dat$familie[[2]]
unlist(dat$familie)


dat <- dat |>
  group_by(Slekt) |>
  rowwise() |>
  mutate(familie = getFam(dat))


dat$familie <- as.character(dat$familie)


dat$Slekt[dat$familie=="NULL"]

test <- name_backbone(name = "Brachypodium", rank = 'genus', kingdom ='plants')
test['family']
# Hard-coding

dat <- dat |>
  mutate(familie = case_when(
    Slekt == "Bartsia" ~ "Orobanchacea",
    Slekt == 
    .default = familie
  ))

saveRDS(slekt2, 'slektOGfam.RData', ascii = T)
#getwd()
