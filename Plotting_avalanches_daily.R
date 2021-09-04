source('Wet_avalanche_regime.R')

###################
## Decadal scale ##
###################

######## Wet avalanches according to C
######
W.Aval_Count_C <- ggplot(data = Aval_meteo[C == 2], aes(x = Decade)) +
  #geom_bar(position="dodge") +
  geom_bar(stat="count",  position = 'stack', fill = "coral") +
  ggtitle(paste0("Wet Avalanche distribution over the decades \n (after deQuervain) - ", Nr_wet_aval, 
                 " Wet avalanches C = 2"))  +
  labs(fill = "Avalanche wetness")

ggsave(paste0(result.dir,"/Wet_avalanches/Wet_Avalanches_C_Count.png"), dpi = 600, width = 9.2, height = 8, units = "in")

W.Aval_CumRain_C <- ggplot(Aval_meteo[C == 2], aes(x = Decade, y = CumRain5_Ta)) +
  geom_boxplot(position="dodge2", color="red", fill="orange", alpha=0.2) + 
  labs(x = '', y = '5day Cummulative Rain (Ta) [mm]')  +
  theme_classic() +
  ggtitle('Rain before avalanche (Ta) - Wet avalanche (C = 2)')

ggsave(paste0(result.dir,"/Wet_avalanches/Wet_Avalanches_C_Cum5_Ta.png"), dpi = 600, width = 9.2, height = 8, units = "in")

W.Aval_CumRainTw_C <- ggplot(Aval_meteo[C == 2], aes(x = Decade, y = CumRain5_Tw)) +
  geom_boxplot(position="dodge2", color="red", fill="orange", alpha=0.2) + 
  labs(x = '', y = '5day Cummulative Rain (Tw) [mm]')  +
  theme_classic() +
  ggtitle('Rain before avalanche (Tw) - Wet avalanche (C = 2)')

# Comparing cumRain based on different temp definition
grid.arrange(W.Aval_CumRain_C, W.Aval_CumRainTw_C, nrow = 2)

W.Aval_CumTemp_C <- ggplot(Aval_meteo[C == 2], aes(x = Decade, y = CumTemp5)) +
  geom_boxplot(position="dodge2", color="red", fill="orange", alpha=0.2) + 
  labs(x = '', y = '5day Cummulative temperature [°C]')  +
  theme_classic() +
  ggtitle('Temperature before avalanche - Wet avalanche (C = 2)')

ggsave(paste0(result.dir,"/Wet_avalanches/Wet_Avalanches_C_CumTemp5.png"), dpi = 600, width = 9.2, height = 8, units = "in")

W.Aval_Snow_C <- ggplot(Aval_meteo[C == 2], aes(x = Decade, y = SCE)) +
  geom_boxplot(position="dodge2", color="red", fill="orange", alpha=0.2) + 
  labs(x = '', y = 'Snow depth [cm]')  +
  theme_classic() +
  ggtitle('Snow depth - Wet avalanche (C = 2)')

grid.arrange(W.Aval_Count_C, W.Aval_CumRain_C, W.Aval_CumTemp_C,W.Aval_Snow_C, nrow = 4)


# Wet avalanches according to C - size
W.Aval_Count_C_size <- ggplot(data = Aval_meteo[C == 2], aes(x = Decade, fill = Size_categ_1)) +
  #geom_bar(position="dodge") +
  geom_bar(position = 'dodge2') + #, fill = "coral"
  ggtitle(paste0("Wet Avalanche size distribution over the decades \n (after deQuervain) - ", Nr_wet_aval, 
                 " Wet avalanches C = 2"))  +
  labs(fill = "Size category")

W.Aval_CumRain_C_size <- ggplot(Aval_meteo[C == 2], aes(x = Decade, y = CumRain5_Ta, fill = Size_categ_1)) +
  geom_boxplot(position="dodge2")+
  theme_classic() + labs(x = '', y = '5day Cummulative Rain (Ta) [mm]', fill = 'Avalanche size') +
  ggtitle('Rain (Ta = 0.46°C) before avalanche - Wet avalanche (C = 2)')

ggsave(paste0(result.dir,"/Wet_avalanches/Wet_Avalanches_C_Cum5Rain_Ta_Size.png"), dpi = 600, width = 9.2, height = 8, units = "in")

W.Aval_CumTemp_C_size <- ggplot(Aval_meteo[C == 2], aes(x = Decade, y = CumTemp5, fill = Size_categ_1)) +
  geom_boxplot(position="dodge2")+
  theme_classic() + labs(x = '', y = '5day Cummulative temperature [°C]', fill = 'Avalanche size') +
  ggtitle('Temperature before avalanche - Wet avalanche (C = 2)')

ggsave(paste0(result.dir,"/Wet_avalanches/Wet_Avalanches_C_CumTemp5_Size.png"), dpi = 600, width = 9.2, height = 8, units = "in")

grid.arrange(W.Aval_Count_C_size, W.Aval_CumRain_C_size, W.Aval_CumTemp_C_size, nrow = 3)
ggsave(paste0(result.dir,"/Wet_avalanches/Wet_Avalanches_C_size.png"), dpi = 600, width = 9.2, height = 10, units = "in")


# WetAll

W.Aval_Count_WetAll <- ggplot(data = Aval_meteo[Wet_aval_all == 'WetAll'], aes(x = Decade, fill = Size_categ_2)) +
  #geom_bar(position="dodge") +
  geom_bar(stat="count", position = 'stack', fill = "aquamarine4") +
  
  ggtitle(paste0("Wet Avalanche distribution over the decades \n (Our definition) - ", Nr_wet_aval2, 
                 " Wet avalanches \n 5day rain >= ", RainLimit, 'mm or 5day sum Temperature >=', TempLimit, '°C'))  +
  labs(fill = "Avalanche wetness")

ggsave(paste0(result.dir,"/Wet_avalanches/Wet_Avalanches_OurDefAll_Count.png"), dpi = 600, width = 9.2, height = 8, units = "in")

W.Aval_CumRain_WetAll <- ggplot(Aval_meteo[Wet_aval_all == 'WetAll'], aes(x = Decade, y = CumRain5_Ta)) +
  geom_boxplot(position="dodge2", color="red", fill="aquamarine4", alpha=0.2) + 
  labs(x = '', y = '5day Cummulative Rain (Ta) [mm]')  +
  theme_classic() +
  ggtitle('Rain before avalanche - Wet avalanche (WetAll)')

ggsave(paste0(result.dir,"/Wet_avalanches/Wet_Avalanches_WetAll_Cum5Rain_Ta.png"), dpi = 600, width = 9.2, height = 8, units = "in")

W.Aval_CumTemp_WetAll <- ggplot(Aval_meteo[Wet_aval_all == 'WetAll'], aes(x = Decade, y = CumTemp5)) +
  geom_boxplot(position="dodge2", color="red", fill="aquamarine4", alpha=0.2) + 
  labs(x = '', y = '5day Cummulative temperature [°C]')  +
  theme_classic() +
  ggtitle('Temperature before avalanche - Wet avalanche (Our Definition - WetAll)')

ggsave(paste0(result.dir,"/Wet_avalanches/Wet_Avalanches_WetAll_Cum5Temp.png"), dpi = 600, width = 9.2, height = 8, units = "in")


grid.arrange(W.Aval_Count_WetAll, W.Aval_CumRain_WetAll, W.Aval_CumTemp_WetAll, nrow = 3)

######### WetAll size
W.Aval_Count_WetAll_size <- ggplot(data = Aval_meteo[Wet_aval_all == 'WetAll'], aes(x = Decade, fill = Size_categ_2)) +
  #geom_bar(position="dodge") +
  geom_bar(stat="count", position = 'dodge2') +
  ggtitle(paste0("Wet Avalanche distribution over the decades \n (Our definition) - ", Nr_wet_aval2, 
                 " Wet avalanches \n 5day rain >= ", RainLimit, 'mm or 5day sum Temperature >=', TempLimit, '°C'))  +
  labs(fill = "Size category")

W.Aval_CumRain_WetAll_size <- ggplot(Aval_meteo[Wet_aval_all == 'WetAll'], aes(x = Decade, y = CumRain5_Ta, fill = Size_categ_1)) +
  geom_boxplot(position="dodge2")+
  theme_classic() + labs(x = '', y = '5day Cummulative Rain (Ta) [mm]', fill = 'Size category') +
  ggtitle('Rain before avalanche - Wet avalanche (WetAll)')

ggsave(paste0(result.dir,"/Wet_avalanches/Wet_Avalanches_WetAll_Cum5Rain_Ta_Size.png"), dpi = 600, width = 9.2, height = 8, units = "in")


W.Aval_CumTemp_WetAll_size <- ggplot(Aval_meteo[Wet_aval_all == 'WetAll'], aes(x = Decade, y = CumTemp5, fill = Size_categ_1)) +
  geom_boxplot(position="dodge2") + 
  labs(x = '', y = '5day Cummulative temperature [°C]')  +
  theme_classic() + labs(fill = 'Size category') +
  ggtitle('Temperature before avalanche - Wet avalanche (Our Definition - WetAll)')

grid.arrange(W.Aval_Count_WetAll_size, W.Aval_CumRain_WetAll_size, W.Aval_CumTemp_WetAll_size, nrow = 3)

###################
##   30y scale   ##
###################
# Wet avalanches according to C
W30.Aval_Count_C <- ggplot(data = Aval_meteo[C == 2], aes(x = Thirty)) +
  #geom_bar(position="dodge") +
  geom_bar(stat="count",  position = 'stack', fill = "coral") +
  ggtitle(paste0("Wet Avalanche distribution over the 30y (after deQuervain) - ", Nr_wet_aval, 
                 " Wet avalanches C = 2"))  +
  labs(fill = "Avalanche wetness")

W30.Aval_CumRain_C <- ggplot(Aval_meteo[C == 2], aes(x = Thirty, y = CumRain5_Ta)) +
  geom_boxplot(position="dodge2", color="red", fill="orange", alpha=0.2) + 
  labs(x = '', y = '5day Cummulative Rain (Ta) [mm]')  +
  theme_classic() +
  ggtitle('Rain before avalanche - Wet avalanche (C = 2)')

ggsave(paste0(result.dir,"/Wet_avalanches/Wet_Avalanches_C_Cum5_Ta.png"), dpi = 600, width = 9.2, height = 8, units = "in")

W30.Aval_CumTemp_C <- ggplot(Aval_meteo[C == 2], aes(x = Thirty, y = CumTemp5)) +
  geom_boxplot(position="dodge2", color="red", fill="orange", alpha=0.2) + 
  labs(x = '', y = '5day Cummulative temperature [°C]')  +
  theme_classic() +
  ggtitle('Temperature before avalanche - Wet avalanche (C = 2)')


W30.Aval_Snow_C <- ggplot(Aval_meteo[C == 2], aes(x = Thirty, y = SCE)) +
  geom_boxplot(position="dodge2", color="red", fill="orange", alpha=0.2) + 
  labs(x = '', y = 'Snow depth [cm]')  +
  theme_classic() +
  ggtitle('Snow depth - Wet avalanche (C = 2)')

grid.arrange(W30.Aval_Count_C, W30.Aval_CumRain_C, W30.Aval_CumTemp_C, W30.Aval_Snow_C, nrow = 4)

###################
## Decadal scale ##
###################

##########################
# Slab avalanches #

Slab.Aval_Count_A <- ggplot(data = Aval_meteo[A == 3 | A == 4], aes(x = Decade, fill = Size_categ_1)) +
  #geom_bar(position="dodge") +
  geom_bar(stat="count", position = 'stack', fill = "cornflowerblue") +
  ggtitle(paste0("Slab Avalanche distribution over the decades \n (after deQuervain) - ", Nr_slab_aval, 
                 " Slab avalanches A = 3 or 4"))  


slab.Aval_CumRain_A <- ggplot(Aval_meteo[A == 3 | A == 4], aes(x = Decade, y = CumRain5_Ta)) +
  geom_boxplot(position="dodge2", color="red", fill="cornflowerblue", alpha=0.2) + 
  labs(x = '', y = '5day Cummulative Rain (Ta) [mm]')  +
  theme_classic() +
  ggtitle('Rain before avalanche - Slab avalanche (A = 3 or 4)')


slab.Aval_CumTemp_A <- ggplot(Aval_meteo[A == 3 | A == 4], aes(x = Decade, y = CumTemp5)) +
  geom_boxplot(position="dodge2", color="red", fill="cornflowerblue", alpha=0.2) + 
  labs(x = '', y = '5day Cummulative temperature [°C]')  +
  theme_classic() +
  ggtitle('Temperature before avalanche - Slab avalanche (A = 3 or 4)')

grid.arrange(Slab.Aval_Count_A, slab.Aval_CumRain_A,slab.Aval_CumTemp_A, nrow = 3)

##########################
# Slab avalanches size #
Slab.Aval_Count_A_size <- ggplot(data = Aval_meteo[A == 3 | A == 4], aes(x = Decade, fill = Size_categ_1)) +
  #geom_bar(position="dodge") +
  geom_bar(stat="count", position = 'dodge2') +
  ggtitle(paste0("Slab Avalanche size distribution over the decades \n (after deQuervain) - ", Nr_slab_aval, 
                 " Slab avalanches A = 3 or 4"))  

slab.Aval_CumRain_A_size <- ggplot(Aval_meteo[A == 3 | A == 4], aes(x = Decade, y = CumRain5_Ta, fill = Size_categ_1)) +
  geom_boxplot(position="dodge2") + 
  labs(x = '', y = '5day Cummulative Rain (Ta) [mm]')  +
  theme_classic() +
  ggtitle('Rain before avalanche - Slab avalanche (A = 3 or 4)')


slab.Aval_CumTemp_A_size <- ggplot(Aval_meteo[A == 3 | A == 4], aes(x = Decade, y = CumTemp5, fill = Size_categ_1)) +
  geom_boxplot(position="dodge2") + 
  labs(x = '', y = '5day Cummulative temperature [°C]')  +
  theme_classic() +
  ggtitle('Temperature before avalanche - Slab avalanche (A = 3 or 4)')

grid.arrange(Slab.Aval_Count_A_size, slab.Aval_CumRain_A_size, slab.Aval_CumTemp_A_size, nrow = 3)

###################
##   30y scale   ##
###################
Slab.Aval30_Count_A <- ggplot(data = Aval_meteo[A == 3 | A == 4], aes(x = Thirty, fill = Size_categ_1)) +
  #geom_bar(position="dodge") +
  geom_bar(stat="count", position = 'stack', fill = "cornflowerblue") +
  ggtitle(paste0("Slab Avalanche distribution over the 30y (after deQuervain) - ", Nr_slab_aval, 
                 " Slab avalanches A = 3 or 4"))  

slab.Aval30_CumRain_A <- ggplot(Aval_meteo[A == 3 | A == 4], aes(x = Thirty, y = CumRain5_Ta)) +
  geom_boxplot(position="dodge2", color="red", fill="cornflowerblue", alpha=0.2) + 
  labs(x = '', y = '5day Cummulative Rain (Ta) [mm]')  +
  theme_classic() +
  ggtitle('Rain before avalanche - Slab avalanche (A = 3 or 4)')

slab.Aval30_CumTemp_A <- ggplot(Aval_meteo[A == 3 | A == 4], aes(x = Thirty, y = CumTemp5)) +
  geom_boxplot(position="dodge2", color="red", fill="cornflowerblue", alpha=0.2) + 
  labs(x = '', y = '5day Cummulative temperature [°C]')  +
  theme_classic() +
  ggtitle('Temperature before avalanche - Slab avalanche (A = 3 or 4)')

slab.Aval30_Snow_A <- ggplot(Aval_meteo[A == 3 | A == 4], aes(x = Thirty, y = SCE)) +
  geom_boxplot(position="dodge2", color="red", fill="cornflowerblue", alpha=0.2) + 
  labs(x = '', y = 'Snow depth [cm]')  +
  theme_classic() +
  ggtitle('Snow depth before avalanche - Slab avalanche (A = 3 or 4)')

grid.arrange(Slab.Aval30_Count_A, slab.Aval30_CumRain_A,slab.Aval30_CumTemp_A, slab.Aval30_Snow_A, nrow = 4)

###############

#################
# Decadal Scale #
#################

# Cornice avalanches #
cornice.Aval_Count_A <- ggplot(data = Aval_meteo[A == 5], aes(x = Decade, fill = Size_categ_1)) +
  #geom_bar(position="dodge") +
  geom_bar(stat="count", position = 'stack', fill = "deeppink") +
  ggtitle(paste0("cornice Avalanche distribution over the decades \n (after deQuervain) - ", Nr_cornice_aval, 
                 " cornice avalanches A = 5"))  


cornice.Aval_CumRain_A <- ggplot(Aval_meteo[A == 5], aes(x = Decade, y = CumRain5_Ta)) +
  geom_boxplot(position="dodge2", color="red", fill="deeppink", alpha=0.2) + 
  labs(x = '', y = '5day Cummulative Rain (Ta) [mm]')  +
  theme_classic() +
  ggtitle('Rain before avalanche - cornice avalanche (A = 5)')


cornice.Aval_CumTemp_A <- ggplot(Aval_meteo[A == 5], aes(x = Decade, y = CumTemp5)) +
  geom_boxplot(position="dodge2", color="red", fill="deeppink", alpha=0.2) + 
  labs(x = '', y = '5day Cummulative temperature [°C]')  +
  theme_classic() +
  ggtitle('Temperature before avalanche - cornice avalanche (A = 5)')

grid.arrange(cornice.Aval_Count_A, cornice.Aval_CumRain_A,cornice.Aval_CumTemp_A, nrow = 3)

# Cornice avalanches size #
cornice.Aval_Count_A_size <- ggplot(data = Aval_meteo[A == 5], aes(x = Decade, fill = Size_categ_1)) +
  #geom_bar(position="dodge") +
  geom_bar(stat="count", position = 'dodge2') +
  ggtitle(paste0("cornice Avalanche size distribution over the decades \n (after deQuervain) - ", Nr_cornice_aval, 
                 " cornice avalanches A == 5"))  

cornice.Aval_CumRain_A_size <- ggplot(Aval_meteo[A == 5], aes(x = Decade, y = CumRain5_Ta, fill = Size_categ_1)) +
  geom_boxplot(position="dodge2") + 
  labs(x = '', y = '5day Cummulative Rain (Ta) [mm]')  +
  theme_classic() +
  ggtitle('Rain before avalanche - cornice avalanche (A == 5)')


cornice.Aval_CumTemp_A_size <- ggplot(Aval_meteo[A == 5], aes(x = Decade, y = CumTemp5, fill = Size_categ_1)) +
  geom_boxplot(position="dodge2") + 
  labs(x = '', y = '5day Cummulative temperature [°C]')  +
  theme_classic() +
  ggtitle('Temperature before avalanche - cornice avalanche (A == 5)')

grid.arrange(cornice.Aval_Count_A_size, cornice.Aval_CumRain_A_size, cornice.Aval_CumTemp_A_size, nrow = 3)

#################
#   30y Scale   #
#################
# Cornice avalanches #
cornice.Aval30_Count_A <- ggplot(data = Aval_meteo[A == 5], aes(x = Thirty, fill = Size_categ_1)) +
  #geom_bar(position="dodge") +
  geom_bar(stat="count", position = 'stack', fill = "deeppink") +
  ggtitle(paste0("cornice Avalanche distribution over the 30y (after deQuervain) - \n",  Nr_cornice_aval, 
                 " cornice avalanches A = 5"))  


cornice.Aval30_CumRain_A <- ggplot(Aval_meteo[A == 5], aes(x = Thirty, y = CumRain5_Ta)) +
  geom_boxplot(position="dodge2", color="red", fill="deeppink", alpha=0.2) + 
  labs(x = '', y = '5day Cummulative Rain (Ta) [mm]')  +
  theme_classic() +
  ggtitle('Rain before avalanche - cornice avalanche (A = 5)')


cornice.Aval30_CumTemp_A <- ggplot(Aval_meteo[A == 5], aes(x = Thirty, y = CumTemp5)) +
  geom_boxplot(position="dodge2", color="red", fill="deeppink", alpha=0.2) + 
  labs(x = '', y = '5day Cummulative temperature [°C]')  +
  theme_classic() +
  ggtitle('Temperature before avalanche - cornice avalanche (A = 5)')

cornice.Aval30_Snow_A <- ggplot(Aval_meteo[A == 5], aes(x = Thirty, y = SCE)) +
  geom_boxplot(position="dodge2", color="red", fill="deeppink", alpha=0.2) + 
  labs(x = '', y = 'Snow depth [cm]')  +
  theme_classic() +
  ggtitle('SD before avalanche - cornice avalanche (A = 5)')

grid.arrange(cornice.Aval30_Count_A, cornice.Aval30_CumRain_A,cornice.Aval30_CumTemp_A, cornice.Aval30_Snow_A, nrow = 4)
