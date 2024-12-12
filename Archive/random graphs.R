

graph_stuff <- n_combined %>% 
  filter(PAR_PD > -1)

wr <- graph_stuff %>% 
  filter(Pos == "WR")

ggplot(wr, aes(Salary, PAR_PD)) +
  geom_jitter()


