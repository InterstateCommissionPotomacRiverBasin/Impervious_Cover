training <- base.cur %>% 
  filter(TYPE == "train")

validation <- base.cur %>% 
  filter(TYPE == "test")
#==============================================================================
rpart.cur <- plot_rpart(base.cur[base.cur$TYPE %in% "train", ], "cur", alt.cols, "All")
splits.df <- as.data.frame(rpart.cur$ALTERATION_FLASHINESS$splits)
splits.df$characteristic <- row.names(splits.df)
splits.df <- splits.df %>% 
  select(characteristic, count, index, improve) %>% 
  filter(count > 0) %>% 
  group_by(count) %>% 
  filter(improve == max(improve))

if (nrow(splits.df) == 3) {
  test.df <- training %>% 
    select(TYPE, WATERSHED, AREA, KARST, PRECIP, FCODE, SLOPE, SOIL) %>% 
    tidyr::gather(CHARACTERISITIC, VALUE, AREA:SOIL)
  test.df2 <- test.df %>% 
    group_by(WATERSHED) %>% 
    mutate(CLASS = ifelse())

  
  class.1 <- CHARACTERISITIC %in% splits.df$characteristic[1] &
    VALUE >= splits.df$index[1]
  test$FLASH_CLASS <- ifelse(test$SLOPE >= 7.35, 1,
                             ifelse(test$SLOPE < 7.35 & test$SLOPE >= 4.16, 2,
                                    ifelse(test$SLOPE < 4.16 & test$AREA >= 7.8, 3,
                                           ifelse(test$SLOPE < 4.16 & test$AREA < 7.8, 4, 10))))
}
#==============================================================================
plot_whiskers <- function() {
  
  
  ggplot(test, aes(FLASH_CLASS, ALTERATION_FLASHINESS, color = FLASH_CLASS)) + 
    geom_boxplot(outlier.alpha = 0, color = "black") + 
    geom_jitter(alpha = 0.30) + 
    scale_colour_manual(values = c("green", "yellow", "orange", "red")) +
    theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "black"),
          axis.title.x = element_text(size = 8),
          axis.title.y = element_text(size = 8),
          legend.title = element_blank(),
          plot.margin = unit(c(0.25, 0, 0.25, 0), "cm"),
          legend.key.height = unit(0.25, units = "cm")) 
}