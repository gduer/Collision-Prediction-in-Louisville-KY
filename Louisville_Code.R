library(caret)
library(glmnet)
library(Metrics)
library(ggplot2)
library(sf)
library(dplyr)
library(tidyverse)
library(ggmap)

options(scipen=999)

# 1 Introduction

Crashes <- st_read("Crashes.shp")
LouisvilleBoundary <- st_read("LouisvilleBoundary.shp")

#Setup map theme
mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 14,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=1)
  )
}

baseMap1 <- get_googlemap("Poplar Hills, KY", 
                          source = "stamen", 
                          zoom = 10, 
                          maptype= 'roadmap',
                          color = 'bw',
                          size = c(600, 400))

library(maptools)
LouisvilleBoundary <- readShapeSpatial("LouisvilleBoundary.shp")
LouisvilleBoundary <- fortify(LouisvilleBoundary)

ggmap(baseMap1) + geom_polygon(data=LouisvilleBoundary, aes(x = long, y = lat, group = group), fill='white', colour='black', size=0.2, alpha = 0.5) + geom_point(data = Crashes, aes(x=GPS_LONGIT, y=GPS_LATITU, fill = 'Crash \nLocations'), color= 'red', size = 0.1) + theme(legend.title=element_blank(), legend.position = c(0.89, 0.95), legend.background = element_rect(color = "black", fill = "grey90", size = 0.2, linetype = "solid")) + labs(title="2017 Crash Locations", subtitle = "Louisville, KY") + mapTheme()


## 1.1 Abstract

library(ggmap)

baseMap2 <- get_map(location = "Bardstown Rd and Grinstead Drive, Louisville, KY", 
                    source = "google", 
                    zoom = 17, 
                    maptype= 'hybrid',
                    color = 'bw')

baseMap3 <- get_map(location = "Downtown Louisville, KY", 
                    source = "google", 
                    zoom = 17, 
                    maptype= 'hybrid',
                    color = 'bw')

ggmap(baseMap2) + geom_point(data = Crashes, aes(x=GPS_LONGIT, y=GPS_LATITU), color= 'red', size = 1) + mapTheme()
ggmap(baseMap3) + geom_point(data = Crashes, aes(x=GPS_LONGIT, y=GPS_LATITU, fill = "Crash \nLocations"), color= 'red', size = 1) + mapTheme() +
  theme(legend.title=element_blank(), legend.position = c(0.89, 0.95), legend.background = element_rect(color = "black", fill = "grey90", size = 0.1, linetype = "solid"))



## 1.4 Modeling strategy 

segments <- st_read("segments_3_21.shp")
Predictors <- segments %>% as.data.frame %>%dplyr::select(-FID_1, -geometry)

colnames(Predictors)


### 2.1 Encoding Network Structure
  
NetworkTbl_int <- read_csv('NetworkData_int.csv')
NetworkTbl_rd <- read_csv('NetworkData_roads.csv')

NetworkTbl <- rbind(NetworkTbl_int, NetworkTbl_rd)

NT <- dplyr::select(NetworkTbl, FID_1, FID_12_13)
colnames(NT) <- c('FID_1', 'Neighb_ID')
NT <- NT[order(NT$FID_1),] 

NT_Table <- head(NT, n = 10)
colnames(NT_Table) <- c("Segment ID", "Neighbor ID")

kable(NT_Table, "html") %>%
  kable_styling("hover") %>%
  kable_styling(bootstrap_options = "striped", font_size = 12)


#merge speed limit to the network table
NT <- merge(x = NT, y = segments[,c('FID_1', 'SPEED')], by.x='Neighb_ID', by.y='FID_1', All = TRUE)

#aggregate speeds by variance 
Speed_Vars <- aggregate(NT$SPEED, by=list(Category=NT$FID_1), FUN=var)

#clean up NAs
Speed_Vars[is.na(Speed_Vars)] <- 0

#merge back into dataset
segments <- merge(segments, Speed_Vars[,c('Category','x')], by.x='FID_1', by.y='Category', all.x = TRUE)
segments$Var_SPEED <- segments$x

segments$x <- NULL

Speed_Vars <- segments %>% as.data.frame %>% dplyr::select(FID_1, Var_SPEED)
Var_Table <- head(Speed_Vars[order(Speed_Vars$FID_1),], n=10)

colnames(Var_Table) <- c("Segment ID", "Speed Variance Experienced")

kable(Var_Table, "html") %>%
kable_styling("hover") %>%
kable_styling(bootstrap_options = "striped", font_size = 12)

# 3 Exploratory Analysis 

## 3.1 The Distribution of Collisions

library(scales)
library(gridExtra)

hist <- ggplot(segments, aes(x=Count_2017)) + geom_histogram(bins = 80) +xlim(-1,75) + ylab("Number of Segments") + ggtitle("Collision Count Distribution") + xlab("Collision Count (2017)") 

hist_log <- ggplot(segments, aes(x=Count_2017)) + geom_histogram(bins = 50) +xlim(-1,75) + ggtitle("Collision Count Distribution (Log-Scale)") + ylab("Number of Segments") + xlab("Collision Count (2017)") +
  scale_y_continuous(trans = log_trans(), breaks = c(0, 10, 100, 1000, 10000))

grid.arrange(hist,hist_log, heights=unit(0.75, "npc"), ncol=2) 

mapTheme <- function(base_size = 12) {
theme(
text = element_text( color = "black"),
plot.title = element_text(size = 14,colour = "black"),
plot.subtitle=element_text(face="italic"),
plot.caption=element_text(hjust=0),
axis.ticks = element_blank(),
panel.background = element_rect(fill = "black"),
axis.title = element_blank(),
axis.text = element_blank(),
axis.title.x = element_blank(),
axis.title.y = element_blank(),
panel.grid.minor = element_blank(),
panel.grid.major = element_blank(),
panel.border = element_rect(colour = "black", fill=NA, size=2),
legend.position = c(0, 1),
legend.justification = c(0, 1),
legend.background = element_rect(colour = NA, fill = "black"),
legend.title = element_text(colour = "white"),
legend.text = element_text(colour = "white")
)
}

library(viridis)

ggplot() +
geom_sf(data = segments, aes(fill=cut(Count_2017, c(-Inf, 0, 1, 5, 10, Inf)))) +
scale_fill_viridis("Collision Count in 2017", discrete = TRUE, direction = 1, option="viridis", labels   = c("0","1","2-5","6-10","10 or more")) + ggtitle("Spatial Distribution of Collisions") + 
scale_color_viridis("", discrete = TRUE, direction = 1, option="viridis", labels = c("0","1","2-5","6-10","11 or more")) +
geom_sf(data = segments, aes(colour=cut(Count_2017, c(-Inf, 0, 1, 5, 10, Inf))), size = 0.1, show_guide=FALSE) +
mapTheme()

tracts <- st_read("tracts.shp")

ggplot() +
geom_sf(data = tracts, aes(fill=Avg_abs_re)) +
scale_fill_viridis("Average Collision/Mile in 2017", direction = 1, option="viridis") + ggtitle("Spatial Distribution of Collision Density by Tract") + mapTheme()


## 3.2 Examining our Predictors

library(dplyr)
type_means_df <- segments %>% group_by(TYPE) %>% summarise(me = mean(COLS_MILE))

ggplot(type_means_df, aes(x=reorder(TYPE, -me), y=me)) + 
  geom_bar(stat="identity", fill = "#4682b4") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title="Average Collisions/Mile by Segment Type",y = 'Average Collisions/Mile', x = 'Segment Type') 

Intersections <- segments[segments$Intersect == 1,]
Intersections$Control <- ifelse( Intersections$TrafficSig == 1, "Signal", ifelse( Intersections$Stop_Sign == 1, "Stop_Sign",  ifelse( Intersections$Yield_Sign == 1, "Yield_Sign", "None" ) ))
control_means_df <- Intersections %>% group_by(Control) %>% summarise(me = mean(COLS_MILE))

ggplot(control_means_df, aes(x=reorder(Control,-me), y=me)) + 
geom_bar(stat="identity", fill = "#4682b4") + 
theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
labs(title="Intersection Average Collisions/Mile by Control Type",y = 'Average Collisions/Mile', x = 'Intersection Control') 

Edges <- segments[segments$Intersect == 0,]
speed_means_df <- Edges %>% group_by(SPEED) %>% summarise(me = mean(COLS_MILE))

ggplot(speed_means_df, aes(x=SPEED, y=me)) + 
  geom_bar(stat="identity", fill = "#4682b4") + 
  theme(axis.text.x = element_text()) +
  labs(title="Average Collisions/Mile by Speed Limit",y = 'Average Collisions/Mile', x = 'Speed Limit (MPH)') 

ggplot(segments[segments$Intersect == 1,], aes(y=COLS_MILE, x=Var_SPEED)) + geom_point() + geom_smooth(method="lm")  + 
  theme(axis.text.x = element_text()) +
  labs(title="Average Collisions/Mile vs Speed Variance",y = 'Collisions/Mile', x = 'Speed Variance')


# 4 Feature Selection
  
## 4.1 Feature Selection with Boruta
  
# run Boruta analysis
boruta.train <- Boruta(Count_2017 ~ ., data = training %>% as.data.frame %>% dplyr::select(built_env, usage, location, network, misc), doTrace = 0)

# save selected features
borutaVars <- getSelectedAttributes(boruta.train)
  
built_env <- c("TrafficSig", "ONEWAY", "BIKEWAY", "rank", "avg_width", "No_Left", "Stop_Sign", "Slow_Sign", "SpdLim_Sig", "Yield_Sign", "Discnt_Lne", "Frght_R", "potholes", "missign")
  
usage <- c("SPEED", "ParkingCit", "Cite_Dens", "volume", "hevtraff", "medtraff", "stndtraff", "lev1j", "lev2j", "lev3j", "lev4j", "hazobj",  "rdkill")
  
location <- c("Dist_Alcoh", "ZONING_COD", "DISTRICT", "POP_DEN", "BSNS_DENS", "Emply_D", "Shop_Dn", "Downt_Dist", "NearArtery", "GEOID")
  
network <- c("Intersect", "TYPE", "N_NEIGHBS", "Var_SPEED", "Length")
  
dont_include <- c("COLS_MILE", "Count_2016", "geometry", "FID_1", "Count_2017")
  
## 4.2 Multi-colinearity

library(dplyr)
  
segments <- st_read("segments_3_21.shp")
  
segments <- segments %>% dplyr::select(built_env, usage, location, network, Count_2017)
  
## identify Numeric Feature Names
num_features <- names(which(sapply(segments, is.numeric)))
segments_num <- segments %>% as.data.frame %>% dplyr::select(num_features)
  
#Correlation Matrix
CorMatrix <- cor(segments_num)
  
library(corrplot)

corrplot(CorMatrix, method = "color", order = "AOE", addCoef.col="grey", addCoefasPercent = FALSE, number.cex = .7)
  
library(FactoMineR)
library(factoextra)
    
waze_vars <- subset(as.data.frame(segments), select = c('lev3j', 'lev1j', 'lev2j', 'lev4j', 'missign', 'rdkill', 'potholes', 'hevtraff', 'medtraff', 'stndtraff', 'hazobj', 'volume'))
    
waze_usage.pca <- PCA(waze_vars, scale.unit = TRUE, ncp = 2)
    
eig.val <- get_eigenvalue(waze_usage.pca)
    
segments <- cbind.data.frame(segments,waze_usage.pca$ind$coord)
segments <- segments %>% dplyr::select(-lev3j, -lev1j, -lev2j, -lev4j, -missign, -rdkill, -potholes, -hevtraff, -medtraff, -stndtraff, -hazobj, -volume)
    
names(segments)[names(segments) == 'Dim.1'] <- 'waze_use_1'
names(segments)[names(segments) == 'Dim.2'] <- 'waze_use_2'
segments$Row.names <- NULL

library(kableExtra)
    
built_env <- c("TrafficSig", "ONEWAY", "BIKEWAY", "rank", "avg_width", "No_Left", "Stop_Sign", "Slow_Sign", "SpdLim_Sig", "Yield_Sign", "Discnt_Lne", "Frght_R")
    
usage <- c("SPEED", "ParkingCit", "Cite_Dens", "waze_use_1", "waze_use_2")
    
location <- c("Dist_Alcoh", "ZONING_COD", "DISTRICT", "POP_DEN", "BSNS_DENS", "Emply_D", "Shop_Dn", "Downt_Dist", "NearArtery", "GEOID")
    
network <- c("Intersect", "TYPE", "N_NEIGHBS", "Var_SPEED", "Length")

    
# 5 Model Selection
    
## 5.1 Set-up
    
set.seed(888)
segments <- segments %>% as.data.frame %>% dplyr::select(built_env, usage, location, network, Count_2017)
    
partition <- createDataPartition(y=segments$GEOID ,p=.80,list=F)
training <- segments[partition,]
testing <- segments[-partition,]
    
## 5.2 Prediction Baseline
    
Type_Averages <- aggregate(Count_2017~TYPE+GEOID, segments, mean)
colnames(Type_Averages) <- c("TYPE", "GEOID", "Type_Est")

segment_baseline <- merge(x = segments, y = Type_Averages, by = c("GEOID","TYPE"))
    
baseline <- mae(segment_baseline$Type_Est, segment_baseline$Count_2017)
    
    
## 5.3 Model Validation
    
fit_ols <- lm(Count_2017 ~ ., data = training)
    
y_hat_ols <- predict(fit_ols,testing)
ols_mae <- mae(testing$Count_2017, y_hat_ols)

#Poisson
fit_poisson <- glm(Count_2017 ~ ., data = training, family="poisson")
    
y_hat_poisson <- predict(fit_poisson,testing, type = "response")
    
pois_mae <- mae(testing$Count_2017, y_hat_poisson)


## Try Ridge
fit.ridge.cv <- cv.glmnet(data.matrix(training %>% as.data.frame %>% dplyr::select(-Count_2017)), training$Count_2017, alpha=0,type.measure="mae")
    
y_hat_l2 <- predict.cv.glmnet(fit.ridge.cv,data.matrix(testing %>% as.data.frame %>% dplyr::select(-Count_2017)))
    
ridge_mae <- mae(testing$Count_2017, y_hat_l2)

## Try Lasso

fit.lasso.cv <- cv.glmnet(data.matrix(training %>% as.data.frame %>% dplyr::select(-Count_2017)), training$Count_2017, alpha=1,type.measure="mae")

y_hat_l1 <- predict.cv.glmnet(fit.lasso.cv,data.matrix(testing %>% as.data.frame %>% dplyr::select(-Count_2017)))
    
las_mae <- mae(testing$Count_2017, y_hat_l1)

## Try Elastic Net

fit.net.cv <- cv.glmnet(data.matrix(training %>% as.data.frame %>% dplyr::select(-Count_2017)), training$Count_2017, alpha=0.5,type.measure="mae")

y_hat_l1l2 <- predict.cv.glmnet(fit.net.cv,data.matrix(testing %>% as.data.frame %>% dplyr::select(-Count_2017)))
    
enet_mae <- mae(testing$Count_2017, y_hat_l1l2)

## Try CART
library(rpart)
    
fit_tree <- rpart(Count_2017 ~ .
    , minsplit = 100
    , maxdepth = 15
    , data=training)
    
y_hat_tree <- predict(fit_tree,testing)
    
tree_mae <- mae(testing$Count_2017, y_hat_tree)

require(randomForest)
    
fit_rf30 <- randomForest(Count_2017 ~ .
    , method="poisson"
    , ntree = 30
    , na.action = na.exclude
    , data=training %>% 
    as.data.frame %>% dplyr::select(-NearArtery, -GEOID))
    
    
y_hat_rf30 <- predict(fit_rf30,testing)
    
rf_mae <- mae(testing$Count_2017, y_hat_rf30)
    
library(xgboost)
set.seed(777)
    
XGB_data <- data.matrix(training %>% as.data.frame %>% dplyr::select(-Count_2017))
    
fit_xgb <- xgboost(XGB_data, training$Count_2017
    , max_depth = 10
    , eta = 0.05
    , nthread = 2
    , nrounds = 1000
    , subsample = .7
    , colsample_bytree = 0.9
    , booster = "gbtree"
    , eval_metric = "mae"
    , objective="count:poisson"
    , base_score = 0.56
    , silent = 1)
    
y_hat_xgb <- predict(fit_xgb, data.matrix(testing %>% as.data.frame))
    
xgb_mae <- mae(testing$Count_2017, y_hat_xgb)
    
library(scales)

#storing the results
results <- data.frame()
    
base <- c("baseline", round(baseline,2), "N/A") 
    
ols <- c("OLS", round(ols_mae,2), percent((baseline-ols_mae)/baseline))
pois <- c("Poisson", round(pois_mae,2), percent((baseline-pois_mae)/baseline))
ridge <- c("Ridge", round(ridge_mae,2), percent((baseline-ridge_mae)/baseline))
lass <- c("Lasso", round(las_mae,2), percent((baseline-las_mae)/baseline))
elnet <- c("Elastic Net", round(enet_mae,2), percent((baseline-enet_mae)/baseline))
tree <- c("CART", round(tree_mae,2), percent((baseline-tree_mae)/baseline))
rf <- c("Random Forest", round(rf_mae,2), percent((baseline-rf_mae)/baseline))
xgb <- c("XGBoost", round(xgb_mae,2), percent((baseline-xgb_mae)/baseline))
    
results <- rbind(base, ols, pois, ridge, lass, elnet, tree, rf, xgb)
rownames(results) <- c()
colnames(results) <- c("Model", "Mean Absolute Error (MAE)", "Baseline Improvement")
    
library(dplyr)
library(kableExtra)
    
results %>%
    kable("html", caption = "Table 5.3.1: Model Results") %>%
    kable_styling("hover") %>%
    group_rows("Regression Models", 2, 3, label_row_css = "background-color: #666; color: #fff;") %>%
    group_rows("Penalized Regression Models", 4, 6, label_row_css = "background-color: #666; color: #fff;") %>%
    group_rows("Tree-Based Models", 7, 9, label_row_css = "background-color: #666; color: #fff;") %>%
    row_spec(0, bold = T, color = "white", background = "#000000") %>%
    row_spec(9, background = "#88C7C1")
    
# Cross-Validation
fitControl <- trainControl(method = "cv", number = 10)
    
set.seed(555) #set seed for random number generator
    
#Poisson 10-FOLD CV
glmFit <- train(Count_2017 ~ ., 
                    data = segments %>% as.data.frame %>% dplyr::select(-NearArtery, -GEOID), 
                    method = "glm",
                    family = "poisson",
                    trControl = fitControl,
                    na.action=na.exclude)
    
glmFit_Tbl <- as.data.frame(glmFit$resample)
    
    
#Random Forest 10-FOLD CV
rfFit <- train(Count_2017 ~ ., 
                   data = segments %>% as.data.frame %>% dplyr::select(-NearArtery, -GEOID) ,
                   method = "rf",
                   ntree = 15,
                   trControl = fitControl,
                   na.action=na.exclude,
                   metric = "MAE", 
                   maximize = F)
    
rfFit_Tbl <- as.data.frame(rfFit$resample)
    
    
#XGB 10-FOLD CV
fitControl <- trainControl(method = "cv", number = 10)
    
XGB_data <- segments %>% as.data.frame
    
tuneGrid <- data.frame(nrounds = 800, max_depth = 10, eta = 0.02, gamma = 0, colsample_bytree = .7, 
                           min_child_weight = 1, subsample = .7)
    
xgbFit <- train(Count_2017 ~ ., 
                    data = XGB_data,
                    method = "xgbTree",
                    trControl = fitControl,
                    na.action=na.exclude,
                    tuneGrid = tuneGrid,
                    metric = "MAE", 
                    maximize = F)
    
xgbFit_Tbl <- as.data.frame(xgbFit$resample)
    
#merge all results into one table
CV_table <- list(glmFit_Tbl[,c("MAE","Resample")],rfFit_Tbl[,c("MAE","Resample")],xgbFit_Tbl[,c("MAE","Resample")]) %>%
      Reduce(function(df1,df2) inner_join(df1,df2,by="Resample"), .)
    
colnames(CV_table) <- c("Poisson", "Fold", "Random Forest", "XGBoost")

#put table into long format for plotting
library(tidyverse)
CV_table_long <- gather(CV_table, Method, value, -Fold)
    
#plot results
library(yarrr)
    
colors <- as.vector(piratepal(palette = "info"))[2:4]
    
ggplot(CV_table_long, aes(Fold, value)) +   
      geom_bar(aes(fill = Method), color = "black", stat="identity", width=0.6, position = position_dodge(width=0.6)) + ylab("MAE") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      scale_fill_manual("Method", values = c(colors)) +ggtitle("10-Fold Cross-Validation Results")


## Spatial Cross-Validation

spatialCV <- function(dataFrame, uniqueID, dependentVariable) {
  
  #initialize variables  
  training <- 0
  testing <- 0
  tuner <- 0
  currentUniqueID <- 0
  uniqueID_List <- 0
  y <- 0
  endList <- list()
  
  #create a list that is all the spatial group unqiue ids in the data frame (ie all the neighborhoods)    
  uniqueID_List <- unique(dataFrame[[uniqueID]])  
  x <- 1
  y <- length(uniqueID_List)
  
  #create a counter and while it is less than the number of counties...  
  while(x <= y) 
  {
    
    #call a current county    
    currentUniqueID <- uniqueID_List[x]
    
    #create a training set comprised of units not in that county and a test set of units
    #that are that county
    training <- dataFrame[ which(dataFrame[[uniqueID]] != uniqueID_List[x]),]
    testing <- dataFrame[ which(dataFrame[[uniqueID]] == uniqueID_List[x]),]
    
    trainingX <- training[ , -which(names(training) %in% c(dependentVariable))]
    testingX <- testing[ , -which(names(testing) %in% c(dependentVariable))]
    
    trainY <- training[[dependentVariable]]
    testY <- testing[[dependentVariable]]
    
    #tune a model - note I have hardwired the dependent variable and the trainControl    
    
    XGB_data <- data.matrix(trainingX %>% as.data.frame %>% dplyr::select(-GEOID))
    
    library(xgboost)
    
    tuner <- xgboost(XGB_data, training$Count_2017
                     , max_depth = 10
                     , eta = 0.05
                     , nthread = 2
                     , nrounds = 800
                     , subsample = .7
                     , colsample_bytree = 0.9
                     , booster = "gbtree"
                     , eval_metric = "mae"
                     , objective="count:poisson"
                     , base_score = 0.57
                     , silent = 1)
    
    #come up with predictions on the test county
    thisPrediction <- predict(tuner, data.matrix(testingX))
    #calculate mape and mae and count of records for a given training set (to get a sense of sample size).
    thisMAPE <- mean(abs(testY - thisPrediction) / testY ) 
    thisMAE <- mean(abs(testY - thisPrediction))  
    countTestObs <- nrow(testingX)
    
    #add to the final list the current county and the MAPE    
    thisList <- cbind(as.character(currentUniqueID), thisMAPE, thisMAE,countTestObs)
    #create a final list to output
    endList <- rbind(endList, thisList)
    #iterate counter    
    x <- x + 1 
    
  } 
  #return the final list of counties and associated MAPEs  
  return (as.data.frame(endList))
}

segments_no_0 <- segments[segments$Count_2017 != 0,]

SCV <- spatialCV(segments_no_0, 'GEOID', 'Count_2017')
    
# 7 Model Results
    
## 7.1 Feature Importance
    
colors <- as.vector(piratepal(palette = "info"))[1:4]
    
## Plot the feature importance
importance_matrix <- xgb.importance(colnames(XGB_data), model = fit_xgb)
importance <- as.data.frame(importance_matrix)
    
type <- c("Either", "NA", "Demand", "Demand","Demand","Demand","Demand","Demand","Demand","Demand","Demand","Demand","Demand", "Demand", "Supply", "Demand","Demand","Demand", "Supply", "Supply", "Demand", "Supply", "Demand", "Supply", "Supply", "Supply", "Demand","Demand", "Supply", "Supply", "Supply", "Supply")
    
bucket <- c("Road Usage/Control", "Network/Segment Characteristics", "Location", "Network/Segment Characteristics","Location","Built Environment","Road Usage/Control","Built Environment","Road Usage/Control","Built Environment","Location","Road Usage/Control","Location", "Location", "Network/Segment Characteristics", "Location","Location","Location", "Network/Segment Characteristics", "Built Environment", "Road Usage/Control","Built Environment", "Built Environment", "Built Environment", "Network/Segment Characteristics", "Built Environment", "Built Environment","Built Environment", "Built Environment", "Built Environment", "Built Environment", "Built Environment")
    
importance <- cbind(importance, type, bucket)
    
ggplot(data=importance, aes(x=reorder(Feature, Gain), y=Gain, fill=bucket, width = 0.8)) +
    geom_bar(stat="identity", color = "black") + coord_flip() + xlab("Feature") + ylab("Contribution") + ggtitle("Feature Contribution to Model") +
    theme(legend.title=element_blank()) + scale_fill_manual("Method", values = c(colors))

ggplot(data=importance, aes(x=reorder(Feature, Gain), y=Gain, fill=type, width = 0.8)) +
    geom_bar(stat="identity", color = "black") + coord_flip() + xlab("Feature") + ylab("Contribution") + ggtitle("Feature Contribution to Model") +
    theme(legend.title=element_blank()) + scale_fill_manual("Method", values = c(colors))
    
## 7.2 Plotting Results
    
y_hat_xgb <- predict(fit_xgb, data.matrix(segments %>% as.data.frame %>% dplyr::select(colnames(XGB_data))))
    
segments_geo <- st_read("segments_4_26_geo.shp")
    
#create a new dataset which will include our results
    
segments_w_pred <- segments_geo
segments_w_pred$COLS_MILE <- segments_w_pred$Count_2017/segments_w_pred$Length
    
#save the results
segments_w_pred$y_hat_xgb <- as.integer(y_hat_xgb)
    
#calculate residuals (errors)
segments_w_pred$Resids <- segments_w_pred$Count_2017 - segments_w_pred$y_hat_xgb
    
#calculate absolute errors
segments_w_pred$abs_res <- abs(segments_w_pred$Resids)
    
#normalize the predictions by dividing the predicted count by segment length
segments_w_pred$Pred_COLSM <- segments_w_pred$y_hat_xgb/segments_w_pred$Length
    
y_hat_xgb <- predict(fit_xgb, data.matrix(testing %>% as.data.frame %>% dplyr::select(colnames(XGB_data))))
    
xgb_results <- as.data.frame(cbind(y_hat_xgb, testing$Count_2017))
colnames(xgb_results) <- c("Predicted", "Observed")
    
ggplot(xgb_results, aes(xgb_results$Observed-xgb_results$Predicted)) + geom_histogram(bins = 200) +
    labs(x="Residuals",y="Count") + xlim(-5,5) + ggtitle("Residual Distibution")

#Observed & Predicted
ggplot(data = xgb_results, aes(x = xgb_results$Predicted , y = xgb_results$Observed)) +
      geom_point(size = 1) + xlab("Predicted") + ylab("Observed") + ggtitle("Observed Values vs. Predicted Values") +  
      theme(plot.title = element_text(hjust = 0.5)) + geom_smooth(method="lm")  
    

#Observed & Residuals
library(gridExtra)
    
ErrorAct <- ggplot(data = xgb_results, aes(x = Observed , y = Observed-Predicted)) +
    geom_point(size = 1) + xlab("Observed") + ylab("Error (Actual)") + ggtitle("") +  
    theme(plot.title = element_text(hjust = 0.5)) + geom_smooth(method="lm") + geom_hline(yintercept = 0) 
    
xgb_results_not0 <- xgb_results[xgb_results$Observed != 0,]
    ErrorPct <- ggplot(data = xgb_results_not0, aes(x = Observed , y = (Observed-Predicted)/Observed)) +
    geom_point(size = 1) + xlab("Observed") + ylab("Error (Percentage)") + ggtitle("") +  
    theme(plot.title = element_text(hjust = 0.5)) + geom_smooth(method="lm") + geom_hline(yintercept = 0) 
    
grid.arrange(ErrorAct,ErrorPct, heights=unit(0.75, "npc"), ncol=2,  top = "Error as a function of Observed")

    
## 7.3 Mapping The Results

library(viridis)
library(BAMMtools)
    
pred.jenks <- append(-Inf, getJenksBreaks(segments_w_pred$Pred_COLSM, 5)) %>% append(Inf)
    
ggplot() +
      geom_sf(data = segments_w_pred, aes(fill=cut(Pred_COLSM, pred.jenks))) +
      scale_fill_viridis("Predicted Risk\n(Natural Breaks)", discrete = TRUE, direction = 1, option="viridis", labels   = c("Low", "","","", "High")) + ggtitle("Model Predictions") + 
      scale_color_viridis("", discrete = TRUE, direction = 1, option="viridis", labels = c("Low", "","","", "High")) +
      geom_sf(data = segments_w_pred, aes(colour=cut(Pred_COLSM, pred.jenks)), size = 0.1, show_guide=FALSE) +
      mapTheme()
    
error.jenks <- append(-Inf, getJenksBreaks(segments_w_pred$abs_res, 5)) %>% append(Inf)
error.jenks[3] <- 1
    
ggplot() +
    geom_sf(data = segments_w_pred, aes(fill=cut(abs_res, error.jenks))) +
    scale_fill_viridis("Absolute Errors\n(Natural Breaks)", discrete = TRUE, direction = 1, option="viridis", labels   = c("Low", "","","", "High")) + ggtitle("Predictions Errors") + 
    scale_color_viridis("Absolute Residuals\n(Quintiles)", discrete = TRUE, direction = 1, option="viridis", labels = c("Low", "","","", "High")) +
    geom_sf(data = segments_w_pred, aes(colour=cut(abs_res, error.jenks)), size = 0.1, show_guide=FALSE) +
    mapTheme()

    
#### Taking a closer look
    
library(raster)
library(rgeos)
library(viridis)
library(gridExtra)
    
# create a function that allows us to clip our segments
    crop_custom <- function(poly.sf, coords) {
      poly.sf.p <- st_transform(poly.sf, 4326)
      poly.sp <- as(poly.sf.p, "Spatial")
      poly.sp.crop <- crop(poly.sp, extent(c(coords)))
      cropped <- st_as_sf(poly.sp.crop)
      cropped
    }
    

#downtown
coords <- c(-85.774107,-85.737028,38.23594, 38.262835)
downtown <- crop_custom(segments_w_pred, coords)
    
dt.pred <- ggplot() + geom_sf(data = downtown, aes(fill=cut(Pred_COLSM, pred.jenks)), size = 0.5) + geom_sf(data = downtown, aes(color=cut(Pred_COLSM, pred.jenks)), show_guide=FALSE) +
      scale_color_viridis("", discrete = TRUE, direction = 1, option="viridis") +
      scale_fill_viridis("", discrete = TRUE, direction = 1, option="viridis", labels = c("Low", "","","", "High")) + ggtitle("Collision Risk: Prediction") + mapTheme() + theme(legend.background = element_blank())
    
dt.obs <- ggplot() + geom_sf(data = downtown, aes(colour=cut(COLS_MILE, pred.jenks)), size = 0.5, show_guide=FALSE) + 
      scale_color_viridis("", discrete = TRUE, direction = 1, option="viridis") +
      theme(legend.position="none") + ggtitle("Collision Density: Actual") + mapTheme()
    
grid.arrange(dt.pred,dt.obs,heights=unit(0.99, "npc"), ncol=2)
    
#Bardstown
coords <- c(-85.731792,-85.694713,38.218848, 38.245749)
Bardstown <- crop_custom(segments_w_pred, coords)
    
Bardstown.pred <- ggplot() + geom_sf(data = Bardstown, aes(fill=cut(Pred_COLSM, pred.jenks)), size = 0.5) + 
      geom_sf(data = Bardstown, aes(color=cut(Pred_COLSM, pred.jenks)), show_guide=FALSE) +
      scale_color_viridis("", discrete = TRUE, direction = 1, option="viridis") +
      scale_fill_viridis("", discrete = TRUE, direction = 1, option="viridis", labels = c("Low", "","","", "High")) +
      ggtitle("Collision Density: Prediction") + mapTheme() + theme(legend.background = element_blank())
    
Bardstown.obs <- ggplot() + geom_sf(data = Bardstown, aes(colour=cut(COLS_MILE, pred.jenks)), size = 0.5, show_guide=FALSE) + scale_color_viridis("", discrete = TRUE, direction = 1, option="viridis") +
      theme(legend.position="none") + ggtitle("Collision Density: Actual") + mapTheme()
    
grid.arrange(Bardstown.pred,Bardstown.obs, heights=unit(0.99, "npc"), ncol=2)

#Johnsontown
coords <- c(-85.890083,-85.810635,38.082091,38.145399)
Johnsontown <- crop_custom(segments_w_pred, coords)
    

Johnsontown.pred <- ggplot() + geom_sf(data = Johnsontown, aes(fill=cut(Pred_COLSM, pred.jenks)), size = 0.5) + 
      geom_sf(data = Johnsontown, aes(color=cut(Pred_COLSM, pred.jenks)), show_guide=FALSE) +
      scale_color_viridis("", discrete = TRUE, direction = 1, option="viridis") +
      scale_fill_viridis("", discrete = TRUE, direction = 1, option="viridis", labels = c("Low", "","","", "High")) +
      ggtitle("Collision Density: Prediction") + mapTheme() + theme(legend.background = element_blank())
    
Johnsontown.obs <- ggplot() + geom_sf(data = Johnsontown, aes(colour=cut(COLS_MILE, pred.jenks)), size = 0.5, show_guide=FALSE) + scale_color_viridis("", discrete = TRUE, direction = 1, option="viridis") +
      theme(legend.position="none") + ggtitle("Collision Density: Actual") + mapTheme()
    
grid.arrange(Johnsontown.pred,Johnsontown.obs, heights=unit(1.2, "npc"), ncol=2)
  
## 7.4 Moran's I
    
segments_w_pred_no_0 <- segments_w_pred[segments_w_pred$Count_2017 != 0, ]
segments_w_pred_no_0$mape <- abs(segments_w_pred_no_0$Count_2017 - segments_w_pred_no_0$y_hat_xgb)/segments_w_pred_no_0$Count_2017
    
centroids <- st_transform(st_centroid(segments_w_pred_no_0),4326 )
centroids <- centroids %>% dplyr::select(mape, geometry)
centroids$lat <- unlist(centroids$geometry)[ c(TRUE,FALSE) ]
centroids$lon <- unlist(centroids$geometry)[ c(FALSE,TRUE) ]
    
library(spdep)
coords <- cbind(centroids$lat, centroids$lon)
spatialWeights <- knn2nb(knearneigh(coords, 4))
moranTest <- moran.mc(segments_w_pred_no_0$mape, nb2listw(spatialWeights, style="W"), nsim=999)
    
ggplot(as.data.frame(moranTest$res), aes(moranTest$res)) + 
      geom_histogram(binwidth = 0.002) +
      geom_vline(aes(xintercept = moranTest$statistic), colour = "red",size=1) +
      scale_x_continuous(limits = c(-0.2, 0.2)) + xlab("Moran's I Statistic") + ylab("Count") +
      labs(title="Observed and permuted Moran's I")
  