summary(all$YearBuilt)


plot(all$YearBuilt)


plot(all$YrSold)

all %>%
  select(YrSold, OverallQual, SalePrice) %>%
  filter(!is.na(SalePrice)) %>%
  group_by(YrSold, OverallQual) %>%
  summarise(median_price = median(SalePrice), mean_price = mean(SalePrice)) -> year_by_quality_data

ggplot(data = year_by_quality_data, aes(x = YrSold, y = median_price))+
  geom_line(aes(color = "Median"))+
  facet_wrap(~ OverallQual)+
  geom_line(data = year_by_quality_data, aes(x = YrSold, y = mean_price, color = "Mean"), )+
  scale_y_continuous(breaks = seq(0, 750000, by= 100000), 
                     labels = scales::comma)+
  geom_text(data = anno_data, aes(label = n_qual,
                                  group = NULL))+
  geom_vline(xintercept = 2007.8, linetype = "dotted") +
  theme_light()+
  yikai_labels+
  yikai_themes+
  scale_x_continuous(breaks = c(2007, 2008, 2009), labels = c("07", "08", "09"))+
  scale_y_continuous(breaks = c(100000, 200000, 400000, 600000), labels = c("100k", "200k", "400k", "600k"))+
  theme(panel.grid.major.x = element_blank())+
  theme(legend.position = c(0.8,0.1), legend.direction = "horizontal")+
  theme(legend.background = element_rect(color = "grey" ))+
  theme(panel.spacing.y =
          unit(1, "lines"))+
  labs(y = 'Mean and Median of SalePrice')+
  labs(x = 'Year')+
  labs(title = 'Change of price by Year by OverallQual')
  



n_qual = paste("N = ", table(all$OverallQual))
x_location = rep(2009, 10)
y_location = c(rep(500000, 9), 200000)
anno_data = data.frame(n_qual = n_qual,
                       YrSold = x_location,
                       median_price = y_location,
                       OverallQual = c(1:10))
  

anno_data
table(all$OverallQual)


tmp = c(1,1,1,2,2,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6,8,8,9,9,9)

barplot(tmp, col= yikai_palette[1:8])


orig = all[!is.na(all$SalePrice), "SalePrice"]

log_SalePrice <- log(orig)
log_SalePrice = as.data.frame(log_SalePrice)
stacked = c(orig, log_SalePrice)
group = c(rep(0, length(orig)), rep(1, length(log_SalePrice)))


dat = data.frame(stacked  = stacked, group = group )
dat$group = as.factor(dat$group)

ggplot(data = log_SalePrice)+
  geom_density(aes(x = log_SalePrice ))+

library()

quick_RF <- randomForest(x=all[1:1460,-79], y=all$SalePrice[1:1460], ntree=100,importance=TRUE)
imp_RF <- importance(quick_RF)
saveRDS(imp_DF, "RF_importance.RDS")

imp_DF <- data.frame(Variables = row.names(imp_RF), MSE = imp_RF[,1])
imp_DF <- imp_DF[order(imp_DF$MSE, decreasing = TRUE),]

ggplot(imp_DF[1:20,], aes(x=reorder(Variables, MSE), y=MSE, fill=MSE)) + geom_bar(stat = 'identity') + 
  labs(x = 'Variables',y= '% increase MSE if variable is randomly permuted') + 
  coord_flip() + theme(legend.position="none")+
  theme_ipsum()


p <- ggplot(imp_DF[1:10,], aes(x=reorder(Variables, MSE), y= MSE, label = round(MSE, 1))) +
  geom_segment( aes(x=reorder(Variables, MSE), xend=reorder(Variables, MSE), y=0, yend= MSE ), color=ifelse(imp_DF[1:10,]$Variables %in% c("Neighborhood", "MSSubClass"), "orange", "grey"), size=ifelse(imp_DF[1:10,]$Variables  %in% c("Neighborhood", "MSSubClass"), 1.5, 0.7) ) +
  geom_point( color=ifelse(imp_DF[1:10,]$Variables %in% c("Neighborhood", "MSSubClass"), "orange", "grey"), size=ifelse(imp_DF[1:10,]$Variables  %in% c("Neighborhood", "MSSubClass"), 9, 5) ) +
  #theme_ipsum() +
  coord_flip() +
  theme(
    legend.position="none"
  ) +
  geom_text(color = "white", size = ifelse(imp_DF[1:10,]$Variables %in% c("Neighborhood", "MSSubClass"), 3, 2))+
  xlab("") +
  ylab("Value of Y") +
  ggtitle("Feature importance via Random Forrest")+
  labs(x = 'Variables',y= '% increase MSE if variable is randomly permuted')+
  theme_bw()+
  yikai_labels +
  yikai_themes+
  ylim(0, 22)+
  theme(plot.title.position = "plot")+
  annotate("text", x=9, y=imp_DF[1:10,]$MSE[which(imp_DF[1:10,]$Variables=="Neighborhood")]*1.1, 
             label= paste('Important categorical variable'),
             color="orange", size=3, angle=0, fontface="bold", hjust=0)+
  annotate("text", x=6, y=imp_DF[1:10,]$MSE[which(imp_DF[1:10,]$Variables=="MSSubClass")]*1.2, 
           label= paste('Another categorical variable to explore'),
           color="orange", size=3, angle=0, fontface="bold", hjust=0)+
  theme(axis.title.x = element_text(hjust=-0.06))+
  theme(panel.grid.major.y = element_blank())
p

imp_DF = readRDS("~/Data_product_pipeline/kunoichi/data/RF_importance.RDS")

DFnumeric 

saveRDS(DFnorm, "normed_numerical_vars.rds")

pcaOut      = prcomp(DFnorm,scale=F,center=F)
data.frame(pcaOut$x) %>%
ggplot() +
  geom_point(aes(x = PC1, y = PC2))



p = ggplot(data=all[!is.na(all$SalePrice),], aes(x=GrLivArea, y=SalePrice))+
  geom_point(color = ifelse(all$GrLivArea[!is.na(all$SalePrice)]>4500, "red", "grey50")) + 
  geom_smooth(method = "lm", se=FALSE, color="black", lty = "dotted") +
  scale_y_continuous(breaks= seq(0, 800000, by=100000), labels = scales::comma) +
  geom_text_repel(aes(label = ifelse(all$GrLivArea[!is.na(all$SalePrice)]>4500, rownames(all), '')))+
  theme_bw()+
  theme(plot.subtitle = element_text(color = 'grey70'))+
  theme(panel.border = element_blank())+
  theme(plot.title = element_text(lineheight = .8, face = "bold", size = 16))+
  theme(panel.grid.minor = element_blank())+
  theme(plot.subtitle = element_text(color = 'darkgray'))+
  theme(axis.line.x = element_line(color = 'darkgrey', size = 0.5))+
  labs(title = "Relationship between SalesPrice and GrLivArea")+
  labs(subtitle = "Data Source: House Prices Competition in Kaggle")+
  labs(caption = "Vis: @zhao_yikai with ggplot")+
  xlim(0, 6000)
p

library(ggplot2)

ggplot(all[!is.na(all$SalePrice),], aes(x= Neighborhood, y=SalePrice)) +
  #geom_bar(stat='summary', fun.y = "median") +
  stat_summary(fun.y = "median",  geom = "bar")+
  scale_y_continuous(breaks= seq(0, 800000, by=50000), labels = scales::comma) +
  geom_label(stat = "count", aes(label = ..count.., y = ..count..), size=3) +
  geom_hline(yintercept=163000, linetype="dashed", color = "red")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  yikai_themes+
  yikai_labels+
  theme(panel.grid.major.x  = element_blank())+
  labs(title = "Median SalePrice by Neighborhood")

data_circular = all[!is.na(all$SalePrice),] %>%
  group_by(Neighborhood) %>%
  summarise("median" = median(SalePrice))%>%
  arrange(median)

data_label_circular = data_circular
number_of_bar <- nrow(data_label_circular)
angle <-  90 - 360 * (as.numeric(data_label_circular$Neighborhood, 
                                         data_label_circular$median )-0.5) /number_of_bar
data_label_circular$hjust<-ifelse( angle < -90, 1.2, -0.2)

# flip angle BY to make them readable
data_label_circular$angle<-ifelse(angle < -90, angle+180, angle)
# Make the circular plot


data_circular$NeighRich[data_circular$Neighborhood %in% c('StoneBr', 'NridgHt', 'NoRidge')] <- 2
data_circular$NeighRich[!data_circular$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale', 'StoneBr', 'NridgHt', 'NoRidge')] <- 1
data_circular$NeighRich[data_circular$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale')] <- 0

saveRDS(data_circular, "~/Data_product_pipeline/kunoichi/data/data_circular.rds")

ggplot(data_circular, aes(x= Neighborhood, y=median, fill = as.factor(NeighRich))) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  # This add the bars with a blue color
  geom_bar( stat= 'identity') +
  
  # Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
  ylim(-400000, 320000) +
  
  # Custom the theme: no axis title and no cartesian grid
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank() ) +
  # This makes the coordinate polar instead of cartesian.
  coord_polar(start = 0)+
  geom_text(data=data_label_circular, aes(x=Neighborhood, y=median, label=Neighborhood, hjust=hjust), 
            color="black", fontface="bold",alpha=0.6, 
            size=2.5, angle= data_label_circular$angle, 
            inherit.aes = FALSE ) +
  yikai_themes+
  yikai_labels+
  labs(title = "Median house price by neighborhood")+
  scale_fill_manual(values = c("#E7B800", "#00AFBB", "#FC4E07"))+
  theme(legend.position = "none")




