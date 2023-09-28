distance <- c(4.3, 1.4, 1.4, 0.6, 1.4, 6.2, 6.6, 4.8, 5.3)
minutes <- c(19, 5 ,5 , 4, 5, 15, 20, 15, 12)
 cor(x=distance, y=minutes)
 
r_vals <- data.frame(X=c(1:100)) %>% as_tibble()

r_vals <- r_vals %>% rowwise() %>%
  mutate(A_PerfectPos = X,
         B_StrongPos = X + sample(-25:25, 1, replace = TRUE),
         C_ModeratePos = X + sample(-50:50, 1, replace = TRUE),
         D_WeakPos = X + sample(-125:125, 1, replace = TRUE),
         E_Negligble = sample(-50:50, 1, replace = TRUE),
         F_WeakNeg = -X + sample(-125:125, 1, replace =TRUE),
         G_ModerateNeg = -X + sample(-50:50, 1, replace = TRUE),
         H_StrongNeg = -X + sample(-25:25, 1, replace = TRUE),
         I_PerfectNeg = -X)

