library("tidyverse")


#Year and Week
Char_Week <- 15
Year <- 2024

#Read Fantasy Pros Data
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

#Read Yahoo Data
Yahoo <- read_csv(eval(paste("~/R Stuff/FantasyPros/Yahoo/", Year, "/Yahoo_Week_", Char_Week, ".csv", sep = ""))) %>% 
  select(ID:Starting) %>% 
  mutate(player = paste(`First Name`, `Last Name`))

###Player Names Function
player_names_func <- function(df){
  
  if("player" %in% colnames(df)){
    df$player <- str_replace_all(df$player, "[^[:alnum:]]", " ")
    df$player <- str_replace_all(df$player, "\\s+", " ")
    df$player <- str_replace_all(df$player, " III", " ")
    df$player <- str_replace_all(df$player, " II", " ")
    df$player <- str_replace_all(df$player, " Jr", " ")
    df$player <- trimws(df$player)
    
  }
  
  if("pos" %in% colnames(df)){
    df <- df %>% 
      mutate(pos = ifelse(player == "Taysom Hill", "TE", pos),
             pos = ifelse(pos == "FB", "RB", pos))
  }
  if("position" %in% colnames(df)){
    df <- df %>% 
      mutate(position = ifelse(player == "Taysom Hill", "TE", position),
             position = ifelse(position == "FB", "RB", position))
  }
  if("Pos" %in% colnames(df)){
    df <- df %>% 
      mutate(Pos = ifelse(player == "Taysom Hill", "TE", Pos),
             Pos = ifelse(Pos == "FB", "RB", Pos))
  }
  if("Position" %in% colnames(df)){
    df <- df %>% 
      mutate(Position = ifelse(player == "Taysom Hill", "TE", Position),
             Position = ifelse(Position == "FB", "RB", Position))
  }
  
  if("team" %in% colnames(df)){
    df$team[df$team == "GNB"] <- "GB"
    df$team[df$team == "JAX"] <- "JAC"
    df$team[df$team == "KAN"] <- "KC"
    df$team[df$team == "LVR"] <- "LV"
    df$team[df$team == "NWE"] <- "NE"
    df$team[df$team == "NOR"] <- "NO"
    df$team[df$team == "SFO"] <- "SF"
    df$team[df$team == "TAM"] <- "TB"
  }
  
  if("opp" %in% colnames(df)){
    df$opp[df$opp == "GNB"] <- "GB"
    df$opp[df$opp == "JAX"] <- "JAC"
    df$opp[df$opp == "KAN"] <- "KC"
    df$opp[df$opp == "LVR"] <- "LV"
    df$opp[df$opp == "NWE"] <- "NE"
    df$opp[df$opp == "NOR"] <- "NO"
    df$opp[df$opp == "SFO"] <- "SF"
    df$opp[df$opp == "TAM"] <- "TB"
  }
  
  df[df == "DJ Moore"] <- "D J Moore"
  df[df == "DJ Chark"] <- "D J Chark"
  df[df == "DK Metcalf"] <- "D K Metcalf"
  df[df == "PJ Walker"] <- "P J Walker"
  df[df == "Eli Mitchell"] <- "Elijah Mitchell"
  df[df == "Gabe Davis"] <- "Gabriel Davis"
  df[df == "Mitch Trubisky"] <- "Mitchell Trubisky"
  df[df == "Josh Palmer"] <- "Joshua Palmer"
  df[df == "Robbie Chosen"] <- "Robbie Anderson"
  df[df == "Michael Pittman Jr"] <- "Michael Pittman"
  df[df == "Ken Walker"] <- "Kenneth Walker"
  df[df == "Chigoziem Okonkwo"] <- "Chig Okonkwo"
  df[df == "Washington Football Team"] <- "Washington Commanders"
  
  return(df)
}


#Tidy the Yahoo Data
Yahoo <- Yahoo[complete.cases(Yahoo[,c("Last Name")]),]

Yahoo <- player_names_func(Yahoo) #player names func


#Tidy Fantasy Pros data
FP <- rbind(FP_QB, FP_RB, FP_WR, FP_TE, FP_DST)

FP <- FP %>% 
  rename("player" = "PLAYER NAME") #rename column

FP <- player_names_func(FP) #player names func


#Join yahoo and fantasy pros data
combined <- full_join(Yahoo, FP, by = c("player"))


#Tidy the combined data
 combined <- combined %>% 
  select(player, Position, Team, Opponent, Salary, `PROJ. FPTS`, MATCHUP, `START/SIT`) %>% 
  mutate(ppd = `PROJ. FPTS`/Salary) %>% #Points per dollar
  select(player, Position, Team, Opponent, Salary, `PROJ. FPTS`, ppd, MATCHUP, `START/SIT`) %>% 
  filter(!is.na(ppd))

#round points per dollar to 4 decimals
combined$ppd <- round(combined$ppd, 4)

#rename col names
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

#get points above replacement
n_combined <- rbind(PAR(20, "QB"),
                  PAR(10, "Flex"),
                  PAR(10, "TE"),
                  PAR(10, "DEF"))

#Points above replacement per dollar
n_combined <- n_combined %>% 
  mutate(PAR_PD = ifelse(Salary > min_sal, (FPTS - rep_points)/(Salary - min_sal), 0))


#Write
write_csv(n_combined, (eval(paste("~/R Stuff/FantasyPros/Tidy/", Year, "/Week_", Char_Week, ".csv", sep = ""))))



