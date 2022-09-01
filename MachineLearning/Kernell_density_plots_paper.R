# Kernell density plots
# Created by @Roman.Juras
# Data source @Valerian.Spusta, @Jan.Blahut
# For avalanche paper: Souckova et al. 2022

# read the dataset of wet and slab avalanche; define your result directory first
Aval_WetSlab_filter <- read.table(paste0(result.dir, "Aval_WetSlab_filter.csv"), sep = ';', header = T)


# Paper plot #
##############
lab_size = 18
lab_size_y = 18
lab_size_x = 18
Title_size = 14
Tick_size = 25

Plot_wet_RAMMS_vals <- ggplot(Aval_WetSlab[WetAval == 1], aes(x = RAMMS_size, group = Thirty, fill = Thirty, colour = Thirty)) + 
  
  geom_density(alpha = 0.3) + 
  theme(legend.position = c(0.8,0.8),panel.background = element_rect(fill = NA), 
        panel.grid.major = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size=Tick_size, angle = 30, vjust = 0.5),
        axis.text.y = element_text(size=Tick_size),
        axis.title.y = element_text(size = lab_size+3),
        axis.title.x = element_text(size = lab_size+3),
        legend.text=element_text(size=Title_size+10),
        legend.title=element_text(size=Title_size+10, colour = 'white'),
        plot.title = element_text(size = Title_size+4)) + labs(x = '', y = 'Wet avalanche density') +
  scale_x_continuous(breaks = c(0,0.25, 0.5, 0.75, 1)) +
  annotate("text", x = 0.1, y = 1.8, label = "(a)", size = Title_size) +
  geom_vline(xintercept = 0.25, linetype="dotted", color = "gray50", size=1) +
  geom_vline(xintercept = 0.5, linetype="dotted", color = "gray50", size=1) +
  geom_vline(xintercept = 0.75, linetype="dotted", color = "gray50", size=1) +
  geom_vline(xintercept = 1, linetype="dotted", color = "gray50", size=1) +
  annotate("text", x = 0.1, y = 1, label = "very \n small", size = Title_size - 7, color = "gray50") +
  annotate("text", x = 0.37, y = 1, label = "small", size = Title_size - 7, color = "gray50") +
  annotate("text", x = 0.63, y = 1, label = "medium", size = Title_size - 7, color = "gray50") +
  annotate("text", x = 0.89, y = 1, label = "large", size = Title_size - 7, color = "gray50") +
  annotate("text", x = 1.25, y = 1, label = "very large", size = Title_size - 7, color = "gray50") 

Plot_slab_RAMMS_vals <- ggplot(Aval_size[!is.na(Thirty)], aes(x = RAMMS_size, fill = Thirty, colour = Thirty)) + 
  geom_density(position = 'identity', alpha = 0.3) + #ggtitle('Slab Avalanches') +
  theme(legend.position = 'none', panel.background = element_rect(fill = NA), 
        panel.grid.major = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size=Tick_size, angle = 30, vjust = 0.5),
        axis.text.y = element_text(size=Tick_size),
        axis.title.y = element_text(size = lab_size+3),
        axis.title.x = element_text(size = lab_size+3),
        legend.text=element_text(size=Title_size+10),
        legend.title=element_text(size=Title_size+10),
        plot.title = element_text(size = Title_size+4)) + labs(x = 'Size', y = 'Slab avalanche density', fill = '') +
  scale_x_continuous(breaks = c(0,0.25, 0.5, 0.75, 1)) +
  annotate("text", x = 0.1, y = 1.8, label = "(c)", size = Title_size) +
  geom_vline(xintercept = 0.25, linetype="dotted", color = "gray50", size=1) +
  geom_vline(xintercept = 0.5, linetype="dotted", color = "gray50", size=1) +
  geom_vline(xintercept = 0.75, linetype="dotted", color = "gray50", size=1) +
  geom_vline(xintercept = 1, linetype="dotted", color = "gray50", size=1) +
  annotate("text", x = 0.1, y = 1, label = "very \n small", size = Title_size - 7, color = "gray50") +
  annotate("text", x = 0.37, y = 1, label = "small", size = Title_size - 7, color = "gray50") +
  annotate("text", x = 0.63, y = 1, label = "medium", size = Title_size - 7, color = "gray50") +
  annotate("text", x = 0.89, y = 1, label = "large", size = Title_size - 7, color = "gray50") +
  annotate("text", x = 1.25, y = 1, label = "very large", size = Title_size - 7, color = "gray50") 
  
#
Plot_slab_season_day <- Aval_WetSlab %>%
  filter(SlabAval > 0) %>%
  ggplot(aes(x = Season_day, group = Thirty, fill = Thirty, colour = Thirty)) + 
  geom_density(alpha = 0.3) +   
  labs(x = 'Date - season', y = '') +
  theme(legend.position = 'none', panel.background = element_rect(fill = NA), 
        panel.grid.major = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size=Tick_size, angle = 30, vjust = 0.5),
        axis.text.y = element_text(size=Tick_size),
        axis.title.y = element_text(size = lab_size+3),
        axis.title.x = element_text(size = lab_size+3),
        legend.text=element_text(size=Title_size+10),
        legend.title=element_text(size=Title_size+10),
        plot.title = element_text(size = Title_size+4)) +
  scale_x_continuous(breaks = c(1,32, 62, 93, 124, 152, 152+31, 152+31+30),
                     labels = c('01-10', '01-11', '01-12', '01-01', '01-02', '01-03', '01-04', '01-05')) +
  annotate("text", x = 19, y = 0.016, label = "(d)", size = Title_size) +
  geom_vline(xintercept = 32, linetype="dotted", color = "gray50", size=1) +
  geom_vline(xintercept = 62, linetype="dotted", color = "gray50", size=1) +
  geom_vline(xintercept = 93, linetype="dotted", color = "gray50", size=1) +
  geom_vline(xintercept = 124, linetype="dotted", color = "gray50", size=1) +
  geom_vline(xintercept = 152, linetype="dotted", color = "gray50", size=1) +
  geom_vline(xintercept = 152+31, linetype="dotted", color = "gray50", size=1) +
  geom_vline(xintercept = 152+31+30, linetype="dotted", color = "gray50", size=1) 


Plot_wet_season_day <- 
  Aval_WetSlab %>%
  filter(WetAval > 0) %>%
  ggplot(aes(x = Season_day, group = Thirty, fill = Thirty, colour = Thirty)) + 
  geom_density(alpha = 0.3) + 
  labs(x = '', y = '') +
  theme(legend.position = 'none', panel.background = element_rect(fill = NA), 
        panel.grid.major = element_blank(), axis.line = element_line(colour = "black"),
        axis.text.x = element_text(size=Tick_size, angle = 30, vjust = 0.5),
        axis.text.y = element_text(size=Tick_size),
        axis.title.y = element_text(size = lab_size+3),
        axis.title.x = element_text(size = lab_size+3),
        legend.text=element_text(size=Title_size+10),
        legend.title=element_text(size=Title_size+10),
        plot.title = element_text(size = Title_size+4)) + 
  scale_x_continuous(breaks = c(1,32, 62, 93, 124, 152, 152+31, 152+31+30),
                     labels = c('01-10', '01-11', '01-12', '01-01', '01-02', '01-03', '01-04', '01-05')) +
  annotate("text", x = 27, y = 0.016, label = "(b)", size = Title_size) +
  geom_vline(xintercept = 32, linetype="dotted", color = "gray50", size=1) +
  geom_vline(xintercept = 62, linetype="dotted", color = "gray50", size=1) +
  geom_vline(xintercept = 93, linetype="dotted", color = "gray50", size=1) +
  geom_vline(xintercept = 124, linetype="dotted", color = "gray50", size=1) +
  geom_vline(xintercept = 152, linetype="dotted", color = "gray50", size=1) +
  geom_vline(xintercept = 152+31, linetype="dotted", color = "gray50", size=1) +
  geom_vline(xintercept = 152+31+30, linetype="dotted", color = "gray50", size=1) 

Fig3 <- plot_grid(Plot_wet_RAMMS_vals, Plot_wet_season_day,  Plot_slab_RAMMS_vals, Plot_slab_season_day,
                  ncol = 2, nrow = 2, align = 'hv')

ggsave("Fig_3.pdf", height = 10.7, width = 16.68, units = "in", dpi = 300)
ggsave("Fig_3.png", height = 10.7, width = 16.86, units = "in", dpi = 300)
