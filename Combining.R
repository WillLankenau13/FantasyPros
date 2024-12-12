library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("janitor")

Char_Week <- 14
Year <- 2024

#Fantasy Pros Data
FP_QB <- read_csv((eval(paste("~/R Stuff/FantasyPros/FantasyProsDownloads/", Year, "/FantasyPros_", Year, "_Week_", Char_Week, "_QB_Rankings.csv", sep = "")))) %>% 
  select(1:7)
FP_RB <- read_csv((eval(paste("~/R Stuff/FantasyPros/FantasyProsDownloads/", Year, "/FantasyPros_", Year, "_Week_", Char_Week, "_RB_Rankings.csv", sep = "")))) %>% 
  select(1:7)
FP_WR <- read_csv((eval(paste("~/R Stuff/FantasyPros/FantasyProsDownloads/", Year, "/FantasyPros_", Year, "_Week_", Char_Week, "_WR_Rankings.csv", sep = "")))) %>% 
  select(1:7)
FP_TE <- read_csv((eval(paste("~/R Stuff/FantasyPros/FantasyProsDownloads/", Year, "/FantasyPros_", Year, "_Week_", Char_Week, "_TE_Rankings.csv", sep = "")))) %>% 
  select(1:7)
FP_DST <- read_csv((eval(paste("~/R Stuff/FantasyPros/FantasyProsDownloads/", Year, "/FantasyPros_", Year, "_Week_", Char_Week, "_DST_Rankings.csv", sep = "")))) %>% 
  select(1:7)

#Yahoo
Yahoo <- read_csv(eval(paste("~/R Stuff/FantasyPros/Yahoo/", Year, "/Yahoo_Week_", Char_Week, ".csv", sep = ""))) %>% 
  select(ID:Starting) %>% 
  mutate(full_name = paste(`First Name`, `Last Name`))

Yahoo <- Yahoo[complete.cases(Yahoo[,c("Last Name")]),]

Yahoo$full_name <- str_replace_all(Yahoo$full_name, "[^[:alnum:]]", " ")
Yahoo$full_name <- str_replace_all(Yahoo$full_name, "\\s+", " ")
Yahoo$full_name <- trimws(Yahoo$full_name)

#Mahomes
FP_QB <- FP_QB %>% 
  mutate(`PLAYER NAME` = ifelse(`PLAYER NAME` == "Patrick Mahomes II", "Patrick Mahomes", `PLAYER NAME`))

#Rbind
FP <- rbind(FP_QB, FP_RB, FP_WR, FP_TE, FP_DST)

#
FP <- FP %>% 
  rename("player_name" = "PLAYER NAME")

FP$player_name <- str_replace_all(FP$player_name, "[^[:alnum:]]", " ")
FP$player_name <- str_replace_all(FP$player_name, "\\s+", " ")
FP$player_name <- trimws(FP$player_name)

#Join
combined <- full_join(Yahoo, FP, by = c("full_name" = "player_name"))



#Tidy
combined <- combined %>% 
  select(full_name, Position, Team, Opponent, Salary, `PROJ. FPTS`, MATCHUP, `START/SIT`) %>% 
  mutate(ppd = `PROJ. FPTS`/Salary) %>% 
  select(full_name, Position, Team, Opponent, Salary, `PROJ. FPTS`, ppd, MATCHUP, `START/SIT`) %>% 
  filter(!is.na(ppd))

combined$ppd <- round(combined$ppd, 4)

colnames(combined) <- c("Player", "Pos", "Team", "Opp", "Salary", "FPTS", "ppd", "Matchup", "StartSit")


#Points Above Replacement
PAR <- function(m_sal, position){
  
  if(position == "Flex"){
    df <- combined %>% 
      filter(Pos == "RB" | Pos == "WR")
  } else {
    df <- combined %>% 
      filter(Pos == position)
  }

  rep_player <- df %>% 
    filter(Salary == m_sal)
  
  rep_pts <- max(rep_player$FPTS)
  
  df <- df %>% 
    mutate(min_sal = m_sal,
           rep_points = rep_pts)
  
  return(df)
}

n_combined <- rbind(PAR(20, "QB"),
                  PAR(10, "Flex"),
                  PAR(10, "TE"),
                  PAR(10, "DEF"))

#Points above replacement per dollar
n_combined <- n_combined %>% 
  mutate(PAR_PD = ifelse(Salary > min_sal, (FPTS - rep_points)/(Salary - min_sal), 0))


#Write
write_csv(n_combined, (eval(paste("~/R Stuff/FantasyPros/Tidy/", Year, "/Week_", Char_Week, ".csv", sep = ""))))



