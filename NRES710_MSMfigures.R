# Use DF: dmax from NRES710_Dat.r
summary(dmax)
# Value used to transform the data
coeff <- 0.35

# A few constants
temperatureColor <- "#224b66"
#rgb(0.2, 0.6, 0.9, 1)
priceColor <- "#68acd9"

fig1 <- ggplot(dmax, aes(x=year)) +
  geom_line( aes(y=discharge.max.wyr), size=0.5, color=temperatureColor) +
  geom_point(aes(y=discharge.max.wyr), shape =19, color=temperatureColor) +
  geom_line( aes(y=(SWE.max.wyr)/coeff), size=0.5, color="#68acd9") +
  geom_point( aes(y=(SWE.max.wyr)/coeff), shape =19, color="#68acd9") +
  
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

ggsave(paste0(outputDir,"/MSM_NRES710.pdf"), fig1, scale = 1.5, width = 12, height = 6, units = c("cm"), dpi = 500)

range(fig5.df$BM.HSHO)