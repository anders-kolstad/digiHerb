#

library(rdrop2)

# token <- drop_auth()
# saveRDS(token, file = "token.rds")

#library(dplyr)
drop_acc() 
drop_dir(path = "/fotoherbarium")

# list file names
(entries <- drop_dir(path = "/fotoherbarium")[,'name'])

# maybe files should be grouped in folders by family...
drop_dir() %>% 
  filter(.tag == "folder") %>%
  select(name)

# test download of an image
testImg <- drop_download(paste0("/fotoherbarium/", entries[1,1]))
#2-5 sec

library(imager)
imager::plot(testImg)
parrots <- load.example("parrots")
plot(parrots)
