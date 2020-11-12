## ---------------------------
library(ggplot2)
library(gridExtra)

## ---------------------------
# Hydrologic flow regime figure:

# Use DF: dmax from NRES710_Dat.r
summary(dmax)
# Value used to transform the data
coeff <- 0.35

# A few constants
temperatureColor <- "#224b66"
#rgb(0.2, 0.6, 0.9, 1)
priceColor <- "#68acd9"

fig1 <- ggplot(dmax, aes(x=wtr_yr)) +
  geom_line( aes(y=discharge.max.yr ), size=0.5, color=temperatureColor) +
  geom_point(aes(y=discharge.max.yr), shape =19, color=temperatureColor) +
  geom_line( aes(y=(SWE.max.yr)/coeff), size=0.5, color="#68acd9") +
  geom_point( aes(y=(SWE.max.yr)/coeff), shape =19, color="#68acd9") +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = 'Stream discharge ('~ft^3~s^-1*')',
    
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*coeff, name="SWE (in)")
  ) + 
  scale_x_continuous(limits = c(2000, 2020), breaks=seq(2000,2020,4)) +
  theme_bw() +
  
  theme(
    axis.title.y = element_text(color = temperatureColor, size=13),
    axis.title.y.right = element_text(color = priceColor, size=13)
  )  + facet_wrap(~GaugeSite)

ggsave(paste0(outputDir,"/MSM_HydroregimeNRES710.pdf"), fig1, scale = 1.5, width = 12, height = 6, units = c("cm"), dpi = 500)

range(fig5.df$BM.HSHO)

## ---------------------------
# Figures for max Discharge SWE and ROS
# use df: dmax
# fig1 <- ggplot(dmax, aes(x=wtr_yr, y=SWE.max.yr, GaugeSite)) +
#   geom_line() +
#   geom_point() +
#   scale_y_continuous(
#     # Features of the first axis
#     # name = 'Stream discharge ('~ft^3~s^-1*')'
#     

Fig_SWEFlow <- ggplot(dmax, aes(x=SWE.max.yr, y=discharge.max.yr, color=GaugeSite)) +
  geom_point(aes(x=SWE.max.yr, y=discharge.max.yr, color=GaugeSite),shape =1, size =2) +
  stat_smooth(method="lm", se=F, colour="gray") +
  scale_x_continuous(limits = c(0, 70), breaks=seq(0,70,20)) +
  scale_y_continuous(limits = c(0, 420), breaks=seq(0, 400, 100)) +
  theme_classic() + 
  scale_color_manual(values=c("#3B6064", "#C9E4CA", "#87BBA2", "#55828B")) +
  ylab('Stream discharge ('~ft^3~s^-1*')') + xlab("SWE (in)") + 
  guides(fill = guide_legend(override.aes = list(color = NA)), 
         color = FALSE,
         shape = FALSE) +
  facet_wrap(~GaugeSite)


Fig_ROSFlow <- ggplot(dmax_winter, aes(x=ROS_max, y=discharge.max.yr, color=GaugeSite)) +
  geom_point(aes(x=ROS_max, y=(discharge.max.yr), color=GaugeSite),shape =1, size=2) +
  stat_smooth(method="lm", se=F, colour="gray")  +  
  #stat_smooth(method="lm", se=F, formula=y ~ poly(x, 3, raw=TRUE), colour="#999999") +
  #scale_x_continuous(limits = c(0, 65), breaks=seq(0,65,20)) +
  scale_y_continuous(limits = c(0, 350), breaks=seq(0, 350, 100)) +
  theme_classic() + ylab('Stream discharge ('~ft^3~s^-1*')') + xlab("Annual ROS events") +
  scale_color_manual(values=c("#3B6064", "#C9E4CA", "#87BBA2", "#55828B")) +
  guides(fill = guide_legend(override.aes = list(color = NA)), 
         color = FALSE,
         shape = FALSE) +
  facet_wrap(~GaugeSite)


Fig_SOSFlow <- ggplot(dmax_winter, aes(x=Snow_max, y=discharge.max.yr, color=GaugeSite)) +
  geom_point(aes(x=ROS_max, y=(discharge.max.yr), color=GaugeSite),shape =1, size=2) +
  stat_smooth(method="lm", se=F, colour="gray")  +  
  #stat_smooth(method="lm", se=F, formula=y ~ poly(x, 3, raw=TRUE), colour="#999999") +
  #scale_x_continuous(limits = c(0, 65), breaks=seq(0,65,20)) +
  scale_y_continuous(limits = c(0, 350), breaks=seq(0, 350, 100)) +
  theme_classic() + ylab('Stream discharge ('~ft^3~s^-1*')') + xlab("Annual ROS events") +
  scale_color_manual(values=c("#3B6064", "#C9E4CA", "#87BBA2", "#55828B")) +
  guides(fill = guide_legend(override.aes = list(color = NA)), 
         color = FALSE,
         shape = FALSE) +
  facet_wrap(~GaugeSite)
  


# Fig_SWEFlow.2 <- ggplot(dmax, aes(y=SWE.max.yr, x=(discharge.max.yr))) +
#   geom_point(aes(y=SWE.max.yr, x=(discharge.max.yr)),shape =1, colour ="#224b66", alpha = 0.7) +
#   stat_smooth(method="lm", se=F, colour="gray")  +  
#   #stat_smooth(method="lm", se=F, formula=y ~ poly(x, 3, raw=TRUE), colour="#999999") +
#   #scale_x_continuous(limits = c(0, 65), breaks=seq(0,65,20)) +
#   #scale_y_continuous(limits = c(0, 0.65), breaks=seq(0,0.65, 0.15),
#   #                   labels = scales::number_format(accuracy = 0.01)) +
#   theme_classic() + xlab('Stream discharge ('~ft^3~s^-1*')') + ylab("SWE (in)") 
# 
# 
# Fig_SWEFlow.1 <- ggplot(dmax_winter, aes(y=ROS_max, x=(discharge.max.yr))) +
#   geom_point(aes(y=ROS_max, x=(discharge.max.yr)),shape =1, colour ="#224b66", alpha = 0.7) +
#   stat_smooth(method="lm", se=F, colour="gray")  +  
#   #stat_smooth(method="lm", se=F, formula=y ~ poly(x, 3, raw=TRUE), colour="#999999") +
#   #scale_x_continuous(limits = c(0, 65), breaks=seq(0,65,20)) +
#   #scale_y_continuous(limits = c(0, 0.65), breaks=seq(0,0.65, 0.15),
#   #                   labels = scales::number_format(accuracy = 0.01)) +
#   theme_classic() + xlab('Stream discharge ('~ft^3~s^-1*')') + ylab("Weekly ROS events") 


pannel_b <- grid.arrange(Fig_SWEFlow, Fig_ROSFlow,
                         nrow = 1)
ggsave(paste0(outputDir,"/MSM_FlowdriversNRES710.pdf"), pannel_b, scale = 1.3, width = 18, height = 8, units = c("cm"), dpi = 500)



p <- ggplot(dmax, aes(y=SWE.max.yr, x=discharge.max.yr, colour =as.factor(GaugeSite), shape =as.factor(GaugeSite))) +
  geom_point() + + geom_smooth() +
  theme_classic() + facet_wrap(~GaugeSite)

p <- ggplot(dmax_winter, aes(y=ROS_max, x=discharge.max.yr, colour =as.factor(GaugeSite), shape =as.factor(GaugeSite))) +
  geom_point() + geom_smooth() +
  theme_classic() + facet_wrap(~GaugeSite)

#ggsave(paste0(outputDir,"/MSM_HydroregimeNRES710.pdf"), fig1, scale = 1.5, width = 12, height = 6, units = c("cm"), dpi = 500)
