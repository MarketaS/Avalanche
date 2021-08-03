library(data.table)
library(ggplot2)

all_aic <- readRDS(all_aic, "./data/all_aic.rds")

sm <- all_aic$sm
sm2 <- all_aic$sm2
sm3 <- all_aic$sm3
sm4 <- all_aic$sm4
sm5 <- all_aic$sm5
sm6 <- all_aic$sm6

sm$name <- "SM1"
sm2$name <- "SM2"
sm3$name <- "SM3"
sm4$name <- "SM4"
sm5$name <- "SM5"
sm6$name <- "SM6"

sm_all <- rbind(sm, sm2, sm3, sm4, sm5, sm6)

sm_all[, c("var", "time") := tstrsplit(new_candi, '_', keep = c(1,2))]

ggplot(sm_all)+
  geom_point(aes(x = V3, y = aic, color = var))+
  facet_wrap(~name, scales = "free_y")
