library(magick)
library(purrr)
library(tidyverse)

list.files(path = "~/Desktop/Export_MS/Submission2/make_gif/", pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps = 0.5) %>% # animates, can opt for number of loops
  image_write("~/Desktop/Export_MS/Submission2/make_gif/f3.gif") 


