library("tidyverse")
library("lubridate")
library("incidence")
library("stringr")
library("janitor")


Char_Week <- 4
total_salary <- 200

optim <- read_csv(eval(paste("~/R Stuff/FantasyPros/Tidy/Week_", Char_Week, ".csv", sep = "")))

#filter out 
monday_night <- c("KC","NYJ", "JAC", "ATL", "SEA", "NYG")

optim <- optim %>% 
  filter(!(Team %in% monday_night))

#not picking
not_picking <- c("Denver Broncos")

optim <- optim %>% 
  filter(!(Player %in% not_picking))

#filter out Fs and Ds
optim <- optim %>% 
  arrange(Salary, desc(FPTS))

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
Flex_o <- rbind(RB_o, WR_o, TE_o) %>% 
  arrange(Salary)

#All possible non-flex
RB_o <- Pos_Optim("RB", 2)
WR_o <- Pos_Optim("WR", 3)
TE_o <- Pos_Optim("TE", 1)

#Base Team
b_team <- rbind(QB_o[1,], RB_o[1:2,], WR_o[1:3,], TE_o[1,], DST_o[1,])

#Players
QB <- QB_o[1,]
RB1 <- RB_o[1,]
RB2 <- RB_o[2,]
WR1 <- WR_o[1,]
WR2 <- WR_o[2,]
WR3 <- WR_o[3,]
TE <- TE_o[1,]
DST <- DST_o[1,]

#Flex
Flex_o <- Flex_o %>% 
  filter(!(Player %in% b_team$Player))

Flex <- Flex_o[1,]

#Remove Used Players
QB_o <- QB_o[-1,]
RB_o <- RB_o[-(1:2),]
WR_o <- WR_o[-(1:3),]
TE_o <- TE_o[-1,]
DST_o <- DST_o[-1,]
Flex_o <- Flex_o[-1,]



#Find the best upgrade
max_upgrade <- 0
replaced <- ""
replacer <- ""

#### QB ####
QB_sal <- QB$Salary[1]
QB_fpts <- QB$FPTS[1]

c <- 1
while(c <= nrow(QB_o)){
  t_QB_sal <- QB_o$Salary[c]
  t_QB_fpts <- QB_o$FPTS[c]
  
  if(QB_sal == t_QB_sal){
    if(t_QB_fpts > QB_fpts){
      upgrade_ppd <- 100000
    } else {
      upgrade_ppd <- 0
    }
  } else {
    upgrade_ppd <- (t_QB_fpts - QB_fpts)/(t_QB_sal - QB_sal)
  }
  
  if(upgrade_ppd > max_upgrade){
    max_upgrade <- upgrade_ppd
    replaced <- QB$Player[1]
    replacer <- QB_o$Player[c]
  }
  c <- c + 1
}

#### TE ####
TE_sal <- TE$Salary[1]
TE_fpts <- TE$FPTS[1]

c <- 1
while(c <= nrow(TE_o)){
  t_TE_sal <- TE_o$Salary[c]
  t_TE_fpts <- TE_o$FPTS[c]
  
  if(TE_sal == t_TE_sal){
    if(t_TE_fpts > TE_fpts){
      upgrade_ppd <- 100000
    } else {
      upgrade_ppd <- 0
    }
  } else {
    upgrade_ppd <- (t_TE_fpts - TE_fpts)/(t_TE_sal - TE_sal)
  }
  
  if(upgrade_ppd > max_upgrade){
    max_upgrade <- upgrade_ppd
    replaced <- TE$Player[1]
    replacer <- TE_o$Player[c]
  }
  c <- c + 1
}

##### DST ####
DST_sal <- DST$Salary[1]
DST_fpts <- DST$FPTS[1]

c <- 1
while(c <= nrow(DST_o)){
  t_DST_sal <- DST_o$Salary[c]
  t_DST_fpts <- DST_o$FPTS[c]
  
  if(DST_sal == t_DST_sal){
    if(t_DST_fpts > DST_fpts){
      upgrade_ppd <- 100000
    } else {
      upgrade_ppd <- 0
    }
  } else {
    upgrade_ppd <- (t_DST_fpts - DST_fpts)/(t_DST_sal - DST_sal)
  }
  
  if(upgrade_ppd > max_upgrade){
    max_upgrade <- upgrade_ppd
    replaced <- DST$Player[1]
    replacer <- DST_o$Player[c]
  }
  c <- c + 1
}


#### Flex ####

#####






