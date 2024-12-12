library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("janitor")

Char_Week <- 8

#Fantasy Pros Data
FP_QB <- read_csv("~/R Stuff/FantasyPros/Data/FantasyPros_2023_Ros_QB_Rankings.csv") %>% 
  select(1:7)
FP_RB <- read_csv("~/R Stuff/FantasyPros/Data/FantasyPros_2023_Ros_RB_Rankings.csv") %>% 
  select(1:7)
FP_WR <- read_csv("~/R Stuff/FantasyPros/Data/FantasyPros_2023_Ros_WR_Rankings.csv") %>% 
  select(1:7)
FP_TE <- read_csv("~/R Stuff/FantasyPros/Data/FantasyPros_2023_Ros_TE_Rankings.csv") %>% 
  select(1:7)
FP_DST <- read_csv("~/R Stuff/FantasyPros/Data/FantasyPros_2023_Ros_DST_Rankings.csv") %>% 
  select(1:7)
FP_K <- read_csv("~/R Stuff/FantasyPros/Data/FantasyPros_2023_Ros_K_Rankings.csv") %>% 
  select(1:7)

#rbind
combined <- rbind(FP_QB, FP_RB, FP_WR, FP_TE, FP_DST, FP_K) %>% 
  select(2:7)


write_csv(combined, (eval(paste("~/R Stuff/FantasyPros/Tidy/Ros_", Char_Week, ".csv", sep = ""))))
