# Aggregation for all model outputs

BW_mmOutput$site <- "Black Wood"
Gen_mmOutput$site <- "General"
Ward_mmOutput$site <- "Ward"

allGPP <- rbind(Ward_mmOutput, Gen_mmOutput, BW_mmOutput)

plot(allGPP$date, allGPP$GPP.daily)

AllGPP_plot <- ggplot(allGPP, aes(x=date, y=GPP.daily, color=site)) +
  geom_line() + theme_classic()

AllER_plot <- ggplot(allGPP, aes(x=date, y=ER.daily, color=site)) +
  geom_line() + theme_classic()

AllKd_plot <- ggplot(allGPP, aes(x=date, y=K600.daily, color=site)) +
  geom_line() + theme_classic()


  # stat_smooth(method="lm", se=T, colour="#636363", level = 0.95) +
  # scale_x_continuous(limits = c(0, 70), breaks=seq(0,70,20)) +
  # scale_y_continuous(limits = c(-25, 355), breaks=seq(0, 350, 70)) +
  # theme_classic() + 
  # scale_color_manual(values=c("#3B6064", "#C9E4CA", "#87BBA2", "#55828B")) +
  # ylab('Stream discharge ('~ft^3~s^-1*')') + xlab("SWE (in)") + 
  # guides(fill = guide_legend(override.aes = list(color = NA)), 
  #        color = FALSE,
  #        shape = FALSE) +
  # facet_wrap(~GaugeSite)
  