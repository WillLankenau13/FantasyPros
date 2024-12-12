library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("janitor")
library("lpSolve")


Char_Week <- 14
Year <- 2024

#changes bam bam

optim <- read_csv(eval(paste("~/R Stuff/FantasyPros/Tidy/", Year, "/Week_", Char_Week, ".csv", sep = "")))
optim_wo <- read_csv(eval(paste("~/R Stuff/FantasyPros/Tidy/", Year, "/Week_", Char_Week, "_With_Betting_Odds.csv", sep = "")))

optim_wo <- optim_wo %>% 
  mutate(FPTS...6 = Avg) %>% 
  select(Player:PAR_PD)

colnames(optim_wo) <- colnames(optim)

optim <- optim %>%
  filter(!(Player %in% optim_wo$Player))

optim <- rbind(optim, optim_wo)

#manual scores
optim <- optim %>% 
  mutate()

#filter out 
monday_night <- c("GB", "DET", "KC", "LAC", "CIN", "DAL")

optim <- optim %>% 
  filter(!(Team %in% monday_night))

#not picking
not_picking <- c("Deebo Samuel Sr")

optim <- optim %>% 
  filter(!(Player %in% not_picking))

#picking
players_picking <- c()

#filter out Fs and Ds
optim <- optim %>% 
  arrange(Salary, desc(FPTS)) %>% 
  mutate(ones = 1,
         zeroes = 0,
         picking = ifelse(Player %in% players_picking, 1, 0))

#filter out cheap
# optim <- optim %>% 
#   filter(Salary >= 11)



Pos_Optim <- function(p, n){
  
  df <- optim %>% 
    filter(Pos == p)
  #loop
  i <- n + 1
  if(n == 1){
    l <- list(df$FPTS[1])
  } else if (n == 2){
    l <- list(df$FPTS[1], df$FPTS[2])
  } else if (n == 3){
    l <- list(df$FPTS[1], df$FPTS[2], df$FPTS[3])
  } else if (n == 4){
    l <- list(df$FPTS[1], df$FPTS[2], df$FPTS[3], df$FPTS[4])
  }
  while(i <= nrow(df)){
    pts <- df$FPTS[i]
    if(pts <= l[n]){
      df <- df[-i,]
      i <- i - 1
    } else if(n == 1){
      l[1] <- pts
    } else if(pts <= l[n-1]){
      l[n] <- pts
    } else if(n == 2){
      l[2] <- l[1]
      l[1] <- pts
    } else if(pts <= l[n-2]){
      l[n] <- l[n-1]
      l[n-1] <- pts
    } else if(n == 3){
      l[3] <- l[2]
      l[2] <- l[1]
      l[1] <- pts
    } else if(pts <= l[n-3]){
      l[n] <- l[n-1]
      l[n-1] <- l[n-2]
      l[n-2] <- pts
    }
    i <- i + 1
  }
  return(df)
}


QB_o <- Pos_Optim("QB", 1)
RB_o <- Pos_Optim("RB", 3)
WR_o <- Pos_Optim("WR", 4)
TE_o <- Pos_Optim("TE", 2)
DST_o <- Pos_Optim("DEF", 1)

#All possible flex
Flex_o <- rbind(RB_o, WR_o, TE_o)

#All possible non-flex
# RB_o <- Pos_Optim("RB", 2)
# WR_o <- Pos_Optim("WR", 3)
# TE_o <- Pos_Optim("TE", 1)

Players_o <- rbind(QB_o, RB_o, WR_o, TE_o, DST_o)


#Optimization

#Decision Matrix
Objective.in <- c(Players_o$FPTS)

#Constraint Matrix
Const.mat <- matrix(c(Players_o$Salary,
                      Players_o$ones,
                      Players_o$picking,
                      QB_o$ones, RB_o$zeroes, WR_o$zeroes, TE_o$zeroes, DST_o$zeroes,
                      QB_o$zeroes, RB_o$ones, WR_o$zeroes, TE_o$zeroes, DST_o$zeroes,
                      QB_o$zeroes, RB_o$zeroes, WR_o$ones, TE_o$zeroes, DST_o$zeroes,
                      QB_o$zeroes, RB_o$zeroes, WR_o$zeroes, TE_o$ones, DST_o$zeroes,
                      QB_o$zeroes, RB_o$zeroes, WR_o$zeroes, TE_o$zeroes, DST_o$ones,
                      QB_o$zeroes, RB_o$ones, WR_o$zeroes, TE_o$zeroes, DST_o$zeroes,
                      QB_o$zeroes, RB_o$zeroes, WR_o$ones, TE_o$zeroes, DST_o$zeroes,
                      QB_o$zeroes, RB_o$zeroes, WR_o$zeroes, TE_o$ones, DST_o$zeroes
                      ), nrow = 11, byrow = TRUE)

#Define Constraints
Salary_con <- 200
Player_con <- 9
Picking_con <- length(players_picking)
QB_con <- 1
RB_con <- 3
WR_con <- 4
TE_con <- 2
DST_con <- 1
min_RB_con <- 2
min_WR_con <- 3
min_TE_con <- 1

#Constraint Rhs
Const.rhs <- c(Salary_con, Player_con, Picking_con, QB_con, RB_con, WR_con, TE_con, DST_con, min_RB_con, min_WR_con, min_TE_con)

#Constraint Directions
Const.dir<-c("<=", "=", "=", "=", "<=", "<=", "<=", "=", ">=", ">=", ">=")

#Optimize
Optimum<-lp(direction = "max", Objective.in, Const.mat, Const.dir, Const.rhs, all.bin = TRUE)

#matrices
fpts_matrix <- matrix(c(Players_o$FPTS))
salary_matrix <- matrix(c(Players_o$Salary))

solution_matrix <- Optimum[["solution"]]

#Print Expected Points
sum(fpts_matrix*solution_matrix)

#Print Salary
sum(salary_matrix*solution_matrix)

#Print Team
Players_o["Selection"] <- solution_matrix

team <- Players_o %>% 
  filter(Selection == 1) %>% 
  select(Player, Pos, Team, Opp, Salary, FPTS, ppd, Matchup, StartSit, PAR_PD)



#Info
team
sum(fpts_matrix*solution_matrix)














