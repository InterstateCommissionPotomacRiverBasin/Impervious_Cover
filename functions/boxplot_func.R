training <- base.cur %>% 
  filter(TYPE == "train")

validation <- base.cur %>% 
  filter(TYPE == "test")
#==============================================================================
rpart.cur <- plot_rpart(base.cur, "cur", alt.cols, "All")
splits.df <- as.data.frame(rpart.cur$ALTERATION_FLASHINESS$splits)
splits.df <- as.data.frame(rpart.cur$ALTERATION_DH17$splits)
splits.df$characteristic <- row.names(splits.df)
splits.df <- splits.df %>% 
  select(characteristic, count, index, improve) %>% 
  filter(count > 0) %>% 
  group_by(count) %>% 
  filter(improve == max(improve))
library(rpart.utils)
test <- rpart.lists(rpart.cur$ALTERATION_FLASHINESS)
test2 <- data.frame(THRESH = unlist(test$L))
test2$CHARACTERISTIC <- names(test$L)

if (nrow(splits.df) == 3) {
  test <- base.cur %>% 
    select(TYPE, WATERSHED, AREA, KARST, PRECIP, FCODE, SLOPE, SOIL) %>% 
    tidyr::gather(CHARACTERISITIC, VALUE, AREA:SOIL)

  rattle::fancyRpartPlot(rpart.cur$ALTERATION_FLASHINESS)
  my.formula <- paste("ALTERATION_FLASHINESS", "~ AREA + KARST + PRECIP + FCODE + SLOPE + SOIL")
  library(rpart)
  test <- rpart(my.formula, data = base.cur, method = "anova",
        control = list(maxdepth = 2, minsplit = 20, maxcompete = 3, xval = 10000))
  library(partykit)
  leaf.labs <- test$frame %>% 
    dplyr::filter(var == "<leaf>") %>% 
    dplyr::pull(n) %>% 
    paste("n =", .)
  mlab <- function(id, nobs, labs) paste("n =", labs[id])
  plot(as.party(test)) #, tp_args = list(mainlab = test$frame))
  plot(as.party(test))
  
  node1.var <- splits.df$CHARACTERISTIC[1]
  node1.num <- splits.df$THRESH[1]
  node2.var <- splits.df$CHARACTERISTIC[2]
  node2.num <- splits.df$THRESH[2]
  node3.var <- splits.df$CHARACTERISTIC[3]
  node3.num <- splits.df$THRESH[3]


  test$FLASH_CLASS <- ifelse(test[, node1.var] < node1.num & test[, node1.var] < node2.num, 1,
                             ifelse(test[, node1.var] < node1.num & test[, node1.var] < node2.num, 1,
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
