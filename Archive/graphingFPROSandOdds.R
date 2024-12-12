optim_wo <- read_csv(eval(paste("~/R Stuff/FantasyPros/Tidy/", Year, "/Week_", Char_Week, "_With_Betting_Odds.csv", sep = "")))

data <- optim_wo %>% 
  select(Player, Pos, Salary...5, FPTS...6, FPTS...21)

colnames(data) <- c("Player", "Pos", "Salary", "FPROS", "Odds")

data <- data %>% 
  filter(!is.na(Odds))


ggplot(data, aes(FPROS, Odds)) +
  geom_point() +
  geom_abline(slope = 1)



