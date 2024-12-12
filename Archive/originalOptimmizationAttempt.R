#####

# while(qb < nrow(QB_o)){
#   while(rb1 < nrow(RB_o)-1){
#     while(rb2 < nrow(RB_o)){
#       while(wr1 < nrow(WR_o)-2){
#         t_fpts <- QB_o$FPTS[qb] + RB_o$FPTS[rb1] + RB_o$FPTS[rb2] + WR_o$FPTS[wr1]
#         t_salary <- QB_o$Salary[qb] + RB_o$Salary[rb1] + RB_o$Salary[rb2] + WR_o$Salary[wr1]
#         if((t_fpts < min_points - m_dst - m_flex - m_te - m_wr - m_wr) | t_salary > 150){
#           dst <- 1
#           flex <- 1
#           te <- te + 1
#         } else {
#         while(wr2 < nrow(WR_o)-1){
#           t_fpts <- QB_o$FPTS[qb] + RB_o$FPTS[rb1] + RB_o$FPTS[rb2] + WR_o$FPTS[wr1] + WR_o$FPTS[wr2]
#           t_salary <- QB_o$Salary[qb] + RB_o$Salary[rb1] + RB_o$Salary[rb2] + WR_o$Salary[wr1] + WR_o$Salary[wr2]
#           if((t_fpts < min_points - m_dst - m_flex - m_te - m_wr) | t_salary > 160){
#             dst <- 1
#             flex <- 1
#             te <- te + 1
#           } else {
#           while(wr3 < nrow(WR_o)){
#             t_fpts <- QB_o$FPTS[qb] + RB_o$FPTS[rb1] + RB_o$FPTS[rb2] + WR_o$FPTS[wr1] + WR_o$FPTS[wr2] + WR_o$FPTS[wr3]
#             t_salary <- QB_o$Salary[qb] + RB_o$Salary[rb1] + RB_o$Salary[rb2] + WR_o$Salary[wr1] + WR_o$Salary[wr2] + WR_o$Salary[wr3]
#             if((t_fpts < min_points - m_dst - m_flex - m_te) | t_salary > 170){
#               dst <- 1
#               flex <- 1
#               te <- te + 1
#             } else {
#             while(te < nrow(TE_o)){
#               t_fpts <- QB_o$FPTS[qb] + RB_o$FPTS[rb1] + RB_o$FPTS[rb2] + WR_o$FPTS[wr1] + WR_o$FPTS[wr2] + WR_o$FPTS[wr3] + TE_o$FPTS[te]
#               t_salary <- QB_o$Salary[qb] + RB_o$Salary[rb1] + RB_o$Salary[rb2] + WR_o$Salary[wr1] + WR_o$Salary[wr2] + WR_o$Salary[wr3] + TE_o$Salary[te]
#               if((t_fpts < min_points - m_dst - m_flex) | t_salary > 180){
#                 dst <- 1
#                 flex <- 1
#                 te <- te + 1
#               } else {
#               while(flex < nrow(Flex_o)){
#                 t_fpts <- QB_o$FPTS[qb] + RB_o$FPTS[rb1] + RB_o$FPTS[rb2] + WR_o$FPTS[wr1] + WR_o$FPTS[wr2] + WR_o$FPTS[wr3] + TE_o$FPTS[te] + Flex_o$FPTS[flex]
#                 t_salary <- QB_o$Salary[qb] + RB_o$Salary[rb1] + RB_o$Salary[rb2] + WR_o$Salary[wr1] + WR_o$Salary[wr2] + WR_o$Salary[wr3] + TE_o$Salary[te] + Flex_o$Salary[flex]
#                 if((t_fpts < min_points - m_dst) | t_salary > 190){
#                   dst <- 1
#                   flex <- flex + 1
#                 } else {
#                   while(dst < nrow(DST_o)){
#                     team <- rbind(QB_o[qb, ], RB_o[rb1, ], RB_o[rb2, ], WR_o[wr1, ], WR_o[wr2, ], WR_o[wr3, ], TE_o[te, ], Flex_o[flex, ], DST_o[dst, ])
#                     t_salary <- sum(team$Salary)
#                     t_fpts <- sum(team$FPTS)
#                     if((t_salary <= 200) & (t_fpts > min_points)){
#                       temp <- data.frame(FPTS = t_fpts,
#                                          Salary = t_salary,
#                                          QB = QB_o$Player[qb],
#                                          RB1 = RB_o$Player[rb1],
#                                          RB2 = RB_o$Player[rb2],
#                                          WR1 = WR_o$Player[wr1],
#                                          WR2 = WR_o$Player[wr2],
#                                          WR3 = WR_o$Player[wr3],
#                                          TE = TE_o$Player[te],
#                                          Flex = Flex_o$Player[flex],
#                                          DST = DST_o$Player[dst])
#                       teams <- rbind(teams, temp)
#                       
#                     }
#                     dst <- dst + 1
#                   }
#                 }
#                 dst <- 1
#                 flex <- flex + 1
#                 }
#               }
#               dst <- 1
#               flex <- 1
#               te <- te + 1
#               }
#             }
#             dst <- 1
#             flex <- 1
#             te <- 1
#             wr3 <- wr3 + 1
#            }
#           }
#           dst <- 1
#           flex <- 1
#           te <- 1
#           wr2 <- wr2 + 1
#           wr3 <- wr2 + 1
#           }
#         }
#         dst <- 1
#         flex <- 1
#         te <- 1
#         wr1 <- wr1 + 1
#         wr2 <- wr1 + 1
#         wr3 <- wr2 + 1
#       }
#       dst <- 1
#       flex <- 1
#       te <- 1
#       wr1 <- 1
#       wr2 <- 2
#       wr3 <- 3
#       rb2 <- rb2 + 1
#     }
#     dst <- 1
#     flex <- 1
#     te <- 1
#     wr1 <- 1
#     wr2 <- 2
#     wr3 <- 3
#     rb1 <- rb1 + 1
#     rb2 <- rb1 + 1
#     print(rb)
#   }
#   dst <- 1
#   flex <- 1
#   te <- 1
#   wr1 <- 1
#   wr2 <- 2
#   wr3 <- 3
#   rb1 <- 1
#   rb2 <- 2
#   qb <- qb + 1
#   print(qb)
# }


