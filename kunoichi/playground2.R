# try spider plot

#install.packages('fmsb')
library(fmsb)

all %>% 
  select("BsmtQual", "FireplaceQu", "GarageQual", "KitchenQual",
         "ExterQual", "Neighborhood") -> qual_data

colnames(qual_data) = c("BsmtQual", "FireplaceQual", "GarageQual", "KitchenQual",
                        "ExterQual", "Neighborhood")

qual_data$NeighRich[qual_data$Neighborhood %in% c('StoneBr', 'NridgHt', 'NoRidge')] <- 2
qual_data$NeighRich[!qual_data$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale', 'StoneBr', 'NridgHt', 'NoRidge')] <- 1
qual_data$NeighRich[qual_data$Neighborhood %in% c('MeadowV', 'IDOTRR', 'BrDale')] <- 0

median_qual_data = qual_data %>%
  group_by(NeighRich) %>%
  summarise_at(vars(ends_with("Qual")), median)
row(median_qual_data) = c("economical", "middle", "wealthy")
median_qual_data<- rbind(rep(5,5) , rep(0,5) , median_qual_data)



colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )

radarchart(median_qual_data[1:5], axistype=1 , 
           #custom polygon
           pcol=colors_border , pfcol=colors_in, plwd=4 , plty=1,
           #custom the grid
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,5,1), cglwd=0.8,
           #custom labels
           vlcex=1, 
           cex.lab=1.5)
legend(x=0.7, y=1.3, legend = c("economical", "middle", "wealthy"), bty = "n", 
       pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)





DFnorm = readRDS("~/Data_product_pipeline/kunoichi/data/normed_numerical_vars.rds")
pcaOut      = prcomp(DFnorm,scale=F,center=F)

data.frame(pcaOut$x) %>%
  ggplot(aes(x = PC1, y = PC2)) +
  geom_point(color = ifelse( pcaOut$x[, 1]> 8.5 | pcaOut$x[, 1 ] < -8 | pcaOut$x[,2 ] < -8  , 
                            "#E91E63", "grey"))+
  geom_text_repel(aes(label = ifelse(pcaOut$x[, 1]> 8.5 | pcaOut$x[, 1 ] < -8 | pcaOut$x[,2 ] < -8,
                                     rownames(all), '')))+
  yikai_labels+
  theme_bw()+
  yikai_themes+
  annotate(
    geom = "curve", x = -3, y = -5.5, xend = 9.6, yend = 0.1, 
    curvature = .3, arrow = arrow(length = unit(2, "mm"))) +
  annotate(
    geom = "curve", x = -6, y = -6, xend = 3, yend = -8, 
    curvature = .1, arrow = arrow(length = unit(2, "mm")), alpha = 0.8)+
  annotate(geom = "curve",x = - 7, y = -4.5, xend = -9.2, yend = 1, curvature = -0.1,
           arrow = arrow(length = unit(2, "mm"))  )+
  annotate(geom = "text", x = -9.5, y = -5.3, label = "Potential outliers", 
           hjust = "left", color = "#E91E63", 
           size = 6, fontface = "bold")+
  labs(title = "Extreme observations from PCA")


train1 = readRDS("data/training_x.rds")
#all <- all[-c(524, 1299),]

saveRDS(train1,"data/training_x.rds" )
saveRDS(all$SalePrice[!is.na(all$SalePrice)], "lasso_y.rds")
my_control <-trainControl(method="cv", number=5)
lassoGrid <- expand.grid(alpha = 1, lambda = seq(0.001,0.1,by = 0.0005))
lasso_mod <- train(x=train1, y=all$SalePrice[!is.na(all$SalePrice)], method='glmnet', trControl= my_control, tuneGrid=lassoGrid) 
lasso_mod$bestTune
plot(lasso_mod)

min(lasso_mod$results$RMSE)



dim(train1)
length(all$SalePrice[!is.na(all$SalePrice)])

dtrain_no_y <- xgb.DMatrix(data = as.matrix(train1))
XGBpred <- predict(xgb_mod, dtrain_no_y)

length(XGBpred)
length(label_train)

plot(exp(label_train) - exp(XGBpred) )

lassoImportance

train1[1,] %*% lassoImportance

class(lassoImportance)


a = as.matrix(train1[1,], drop = T)
a = as.vector(a)

train <- read.csv("~/Data_product_pipeline/kunoichi/data/train_house.csv", stringsAsFactors = F)
test <- read.csv("~/Data_product_pipeline/kunoichi/data/test_house.csv", stringsAsFactors = F)
test_labels <- test$Id
test$Id <- NULL
train$Id <- NULL
test$SalePrice <- NA
all <- rbind(train, test)




all <- all[-c(524, 1299),]
train1 = readRDS("data/training_x.rds")

label_train = log(all$SalePrice[!is.na(all$SalePrice)])
lassoOut = cv.glmnet(x=as.matrix(train1), y=label_train, alpha=1)
plot(lassoOut)
lassoOut$lambda.1se

betaHatTemp = coef(lassoOut,s='lambda.1se')[-1]
Srefitted       = which(abs(betaHatTemp) > 1e-16)


Xdf             = as.data.frame(train1[,Srefitted])
refittedOut     = lm(label_train ~ ., data = Xdf)
betaHatRefitted = coef(refittedOut)[-1]
final_beta = round(betaHatRefitted, 2)
names(final_beta)
top_10_coeff = final_beta[order(abs(final_beta), decreasing = T)[1:10]]
top_10_coeff



dt = data.frame(varname = names(top_10_coeff),
                coeffs = top_10_coeff )

ggplot(dt) +
  # remove axes and superfluous grids
  theme_classic() +
  theme(axis.title = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line = element_blank()) +
  
  
  # add a dummy point for scaling purposes
  #geom_point(aes(x = 12, y = discipline), 
           #  size = 0, col = "white") + 
  
  # add the horizontal discipline lines
  geom_hline(yintercept = 1:10, col = "grey80") +
  
  # add a point for each male success rate
  geom_point(aes(x = coeffs, y = as.numeric(varname)),
             size = 11, col = "#9DBEBB") +
  yikai_labels+
  scale_y_discrete(breaks = dt$varname)



logo <- image_read("../images/TAMU_logo.png")


p = ggplot(dt, aes(x=reorder(varname, coeffs), y= coeffs, label = round(coeffs, 2))) +
  #geom_segment( aes(x=reorder(varname, coeffs), xend=reorder(varname, coeffs), y=0, yend= coeffs ) ) +
  geom_point( size = 12, 
              color = ifelse(dt$color, "#9DBEBB", "#468189")) +
  coord_flip() +
  theme(
    legend.position="none"
  ) +
  geom_text(color = "white", size =  3.5)+
  xlab("") +
  ylab("Value of Y") +
  ggtitle("Feature importance via Refitted LASSO")+
  labs(x = 'Variables',y= 'Regression Coefficients')+
  theme_bw()+
  yikai_labels +
  yikai_themes+
  theme(plot.title.position = "plot")+
  theme(axis.title.x = element_text(hjust=-0.06))+
  theme(axis.title.y = element_text(hjust=-0.02))+
  theme(panel.grid.major.x = element_blank())+
  ylim(c(-0.1, 0.15))
p
grid::grid.raster(logo, x = 0.07, y = 0.03, just = c('left', 'bottom'), width = unit(1, 'inches'))



#################################



# Libraries
library(GGally)
library(dplyr)

train1$
  

# Data set is provided by R natively
data <- iris

# Plot
data %>%
  arrange(desc(Species)) %>%
  ggparcoord(
    columns = 1:4, groupColumn = 5, order = "anyClass",
    showPoints = TRUE, 
    title = "Original",
    alphaLines = 1
  ) + 
  scale_color_manual(values=c( "#69b3a2", "#E8E8E8", "#E8E8E8") ) +
  theme_ipsum()+
  theme(
    legend.position="Default",
    plot.title = element_text(size=10)
  ) +
  xlab("")



## parallel plot

all[!is.na(all$SalePrice), ] %>%
  group_by(Neighborhood, YrSold) %>%
  summarise(median_price = median(SalePrice)) -> parallel_plot_data

parallel_plot_data %>%
  spread(YrSold, value = median_price) -> parallel_plot_data_wide


apply(parallel_plot_data_wide, 2, anyNA)






