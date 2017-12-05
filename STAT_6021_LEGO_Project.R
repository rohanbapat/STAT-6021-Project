### STAT 6021 Final Project
### Predicting Lego Prices and  Themes
#Rohan Bapat,  rb2te
#Jennifer Cruser,   jc4pg
#Abhijith Mandya,  am6ku
#Yi Hao,  yh8a


library(RCurl)
library(readr)
library(dplyr)

# Read input dataframes from Github
colors_df <- read.csv(text =getURL('https://raw.githubusercontent.com/rohanbapat/STAT-6021-Project/master/colors.csv'))
inventories_df <- read.csv(text =getURL('https://raw.githubusercontent.com/rohanbapat/STAT-6021-Project/master/inventories.csv'))
inventoryparts_df <- read.csv(text =getURL('https://raw.githubusercontent.com/rohanbapat/STAT-6021-Project/master/inventory_parts.csv'))
inventorysets_df <- read.csv(text =getURL('https://raw.githubusercontent.com/rohanbapat/STAT-6021-Project/master/inventory_sets.csv'))
partcategories_df <- read.csv(text =getURL('https://raw.githubusercontent.com/rohanbapat/STAT-6021-Project/master/part_categories.csv'))
parts_df <- read.csv(text =getURL('https://raw.githubusercontent.com/rohanbapat/STAT-6021-Project/master/parts.csv'))
sets_df <- read.csv(text =getURL('https://raw.githubusercontent.com/rohanbapat/STAT-6021-Project/master/sets.csv'))
themes_df <- read.csv(text =getURL('https://raw.githubusercontent.com/rohanbapat/STAT-6021-Project/master/themes.csv'))
prices_df <- read.csv(text =getURL('https://raw.githubusercontent.com/rohanbapat/STAT-6021-Project/master/AllLegoPrices.csv'))


###### 1. Data Merging #############

# Rename columns with same name but different definition
colnames(partcategories_df)[colnames(partcategories_df) == "name"] <- "partcat_name"
colnames(parts_df)[colnames(parts_df) == "name"] <- "part_name"
colnames(colors_df)[colnames(colors_df) == "name"] <- "color_name"
colnames(themes_df)[colnames(themes_df) == "name"] <- "sub_theme"
colnames(themes_df)[colnames(themes_df) == "id"] <- "sub_theme_id"
colnames(themes_df)[colnames(themes_df) == "parent_id"] <- "theme_id"
colnames(sets_df)[colnames(sets_df) == "name"] <- "set_name"
colnames(inventoryparts_df)[colnames(inventoryparts_df) == "quantity"] <- "inventorypart_quantity"
colnames(inventorysets_df)[colnames(inventorysets_df) == "quantity"] <- "inventorysets_quantity"
colnames(prices_df)[colnames(prices_df) == "Theme"] <- "theme_name"
colnames(prices_df)[colnames(prices_df) == "Subtheme"] <- "sub_theme"
colnames(prices_df)[colnames(prices_df) == "Pieces"] <- "num_parts"
colnames(prices_df)[colnames(prices_df) == "Name"] <- "set_name"
colnames(prices_df)[colnames(prices_df) == "Year"] <- "year"
colnames(sets_df)[colnames(sets_df) == "theme_id"] <- "sub_theme_id"

## dropping columns from prices that are unnecessary
prices_df <- subset(prices_df, select=-c(UKPrice,CAPrice, EUPrice, ImageURL, OwnedBy, WantedBy, Variant, SetID, Minifigs, Number))
sum(is.na(prices_df$num_parts))
#[1] 3070

# determine if we have enough data to drop any sets without prices
nrow(prices_df) - sum(is.na(prices_df$USPrice))
#[1] 7989
# drop price NAs
price_df <- prices_df[!(is.na(prices_df$USPrice)), ]
# Could drop piece NA  - takes down to 6000 obs - not sure if aligns with set data 
#price_df <- price_df[!(is.na(price_df$Pieces)), ]

# impute the theme ids for those theme_id = NA (will equal the subtheme)
themes_df$theme_id[is.na(themes_df$theme_id)] <- themes_df$sub_theme_id[is.na(themes_df$theme_id)]


# Merge all dataframes to master dataframe

# 1. Consider inventoryparts as the base dataset (Since it is the largest in size)
# Merge inventoryparts with colors on color_id <> id
master_df <- merge(x = inventoryparts_df, y = colors_df,by.x = "color_id", by.y = "id")

# 2. Merge parts and partcategories on part_cat_id <> id
parts_partcat_merge <- merge(x = parts_df, y = partcategories_df, by.x = "part_cat_id", by.y = "id")

# 3. Merge parts_partcat_merge with master on partnum
master_df <- merge(master_df, parts_partcat_merge, by = "part_num", all.x = TRUE)

# 4. Merge sets and themes on theme_id <> id
sets_themes_merge <- merge(x = sets_df, y = themes_df, by.x = "sub_theme_id", by.y = "sub_theme_id")

# 5. Merge set_themes_merge with the prices dta on set_name, year

set_themes_merge2 <- merge(sets_themes_merge, price_df, by=c('set_name', 'year'))
#part numbers were close to not identical, dropping the second column that was created, ditto with the subtheme
set_themes_merge2 <- subset(set_themes_merge2, select=-c(num_parts.y, sub_theme.y))
# rename the columns that got changed
colnames(set_themes_merge2 )[colnames(set_themes_merge2 ) == "num_parts.x"] <- "num_parts"
colnames(set_themes_merge2 )[colnames(set_themes_merge2 ) == "sub_theme.y"] <- "sub_theme"

# 6. Merge inventories with inventorysets on (id, set_num)<>(inventory_id, set_num)
inventories_invset_merge <- merge(x = inventories_df, y = inventorysets_df, by.x = c("id","set_num"), by.y = c("inventory_id","set_num"), all = TRUE)

# 7. Merge inventory sets with inventories_st_merge on set_num
inventorysets_settheme_merge <- merge(inventories_invset_merge, set_themes_merge2, by = "set_num")

# 8. Merge master with inventories_st_merge on inventory_id <> id
master_df2 <- merge(x = master_df, y = inventorysets_settheme_merge, by.x = "inventory_id", by.y = "id", all = T)

# 9. Master with no set_num NAs
master_df_final <- master_df2[!(is.na(master_df2$set_num)), ]

dim(master_df_final)
str(master_df_final)

#Convert respective columns to factor/numeric
colnames(master_df_final)
num <- c("num_parts","inventorypart_quantity","inventorysets_quantity","part_num","year","USPrice")

master_df_final[num] <- lapply(master_df_final[num], as.numeric)

cat <- c("is_spare", "color_name","is_trans","part_name","partcat_name",
         "set_num","version","set_name", "sub_theme.x",              
         "theme_name")

master_df_final[cat] <- lapply(master_df_final[cat], as.factor)

###### 2. Data Partition and Data Cleaning  #############
#Dropping unwanted columns
master_wanted <- subset(master_df_final, select = -c(color_id,rgb,sub_theme_id,inventory_id, part_cat_id, theme_id,inventorysets_quantity))

#Check for NA
na_count <-data.frame(sapply(master_wanted, function(y) sum(length(which(is.na(y))))))

#Remove 0 Price values
master_wanted <- master_wanted[!master_wanted$USPrice==0,]
#Remove all NAs
master_wanted <- master_wanted[complete.cases(master_wanted), ]

set.seed(1)
sam <- sample(357281, 357281/2, replace =F)
train <- master_wanted[sam, ]
test <- master_wanted[-sam, ]

##Remove new levels in theme_name between test and train
te1 <-unique(test$theme_name)
tr1<-unique(train$theme_name)

te1[!(te1 %in% tr1)]
tr1[!(te1 %in% tr1)]

dim(test)
test <- test[!test$theme_name=="Power Functions",]
test <- test[!test$theme_name=="Dinasours",]
train <- train[!train$theme_name==c("Power Functions","Dinosaurs"),]

#Remove new levels in sub_theme.x between test and train
se1 <-unique(test$sub_theme.x)
sr1<-unique(train$sub_theme.x)

se1[!(se1 %in% sr1)]
sr1[!(se1 %in% sr1)]

train <- train[!train$sub_theme.x==c("Master Building Academy","Fusion"),]

test <- test[!test$sub_theme.x=="Master Building Academy",]
test <- test[!test$sub_theme.x=="Fusion",]
test <- test[!test$sub_theme.x == "The Lord of the Rings",]
test <- test[!test$sub_theme.x == "Marvel Super Heroes" ,]

dim(test)
dim(train)

#Remove new levels in partcat_name between test and train
pe1 <-unique(test$partcat_name)
pr1<-unique(train$partcat_name)

pe1[!(pe1 %in% pr1)]
pr1[!(pe1 %in% pr1)]

dim(test)
dim(train)

#Remove new levels in year between test and train
ye1 <-unique(test$year)
yr1<-unique(train$year)

ye1[!(ye1 %in% yr1)]
yr1[!(ye1 %in% yr1)]

test <- test[!test$year==c("1964","1966"),]
train <- train[!train$year==c("1964","1966"),]
dim(test)
dim(train)

###### 3. Data Exploration ######

#--------------  More expensive sets

#look at overall summary of the Prices
summary(set_themes_merge2$USPrice)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.00    7.00   16.50   29.42   38.00  789.99 

set_themes_merge2[order(set_themes_merge2$USPrice), ]
#Identity and Landscape Kit 2013 432 2000430-1 2844 Master Building Academy 432 Serious Play        789.99
# Connections Kit           2013 432 2000431-1 2448 Master Building Academy 432 Serious Play        754.99
# Death Star                2016 158 75159-1   4023 Star Wars               158 Star Wars           499.99
# Window Exploration Bag    2010 507 2000409-1 5200 Educational and Dacta   507 Serious Play        484.99
# Death Star                2008 174 10188-1   3807 Star Wars Episode 4/5/6 171 Star Wars           399.99
#Mindstorms EV3             2013 262 31313-1   600  EV3                     258 Mindstorms          349.99
# The SHIELD Helicarrier    2015 487 76042-1   2995 Avengers                482 Marvel Super Heroes 349.99
# Sydney Opera House        2013 276 10234-1   2988 Sculptures              276 Advanced Models     319.99


#-------------  Average Price per Theme and Subtheme

#too many themes to be overly helpful
ggplot(set_themes_merge2, aes(x=factor(theme_name), y=USPrice)) + stat_summary(fun.y="mean", geom="bar")

# aggregate in new table and plot
theme_price <- aggregate( USPrice ~ theme_name, set_themes_merge2, mean )
top_theme <- subset(theme_price, USPrice >= 75, select = c(theme_name, USPrice))
ggplot(top_theme, aes(x=factor(theme_name), y=USPrice)) + stat_summary( geom="bar")

# aggregate subthemes in new table and plots
subtheme_price <- aggregate( USPrice ~ sub_theme, set_themes_merge2, mean )
top_subtheme <- subset(subtheme_price, USPrice >= 150, select = c(sub_theme, USPrice))
ggplot(top_subtheme, aes(x=factor(sub_theme), y=USPrice)) + stat_summary( geom="bar")

#----------- Average Number of Parts per Theme 

summary(set_themes_merge2$num_parts)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.0    39.0   106.0   245.5   302.5  5922.0 

#theme parts aggregated
theme_parts <- aggregate( num_parts ~ theme_name, set_themes_merge2, mean )
top_theme_parts <- subset(theme_parts, num_parts >= 700, select = c(theme_name, num_parts))
ggplot(top_theme_parts, aes(x=factor(theme_name), y=num_parts)) + stat_summary( geom="bar")

#----------- Price vs Num parts

###### 4. Variable Selection #############

library(leaps)

#Data Exploration and Variable Selection

plot(USPrice~year+is_trans+version+theme_name+sub_theme.x+partcat_name+is_spare+color_name, data = train)

#Year seems to have a high related positive relationship
#version - related 1, or 2, positive

###### 5. Fit Linear Regression Models #############

#1. Build linear model using year variable

# s.null <- lm(USPrice~1, data=train)
# s.full <- lm(USPrice~ is_trans + partcat_name + theme_name + sub_theme.x + version + year, 
#              data = train)
# 
# Stepwise selection
# step(s.null, scope=list(lower=s.null, upper=s.full), direction="both")

## Variable Selection:

## part_num, part_name, set_num, set_num, and num_part have thousands levels, not good for modeling
## color_name, rgb, is_spares are not related much with USPrice.

## Therefore,
## 6 Variables: year, version, in_trans, theme_name, sub_theme.x, partcat_nam, 
## which have high association with USPrice, are selected as predictors for modeling.

library(MASS)
set.seed(123)

#2. Built linear regression model using the following 6 variables based on step wise selection

#Check the full model
price.lm <- lm(USPrice~ is_trans + partcat_name + theme_name + sub_theme.x + version + year, data = train,x = TRUE, y = TRUE)
summary(price.lm)

###### 6. Check Model Accuracy  #############

# Normal Probability Plot

qqnorm(rstudent(price.lm))
qqline(rstudent(price.lm))
## The plot shows a linear pattern in general, although some fat tail at right-upper end.


#Residual plot vs. fitted values
ti <- rstudent(price.lm)
yhat <- fitted(price.lm)
plot(yhat,ti)
abline(h=0)

## Plot shows balanced residual distribution.


###### 7. Transformation  ############# 

out <- boxcox(price.lm)
range(out$x[out$y > max(out$y)-qchisq(0.95,1)/2])
## alpha = 0.14
## Try log transfomation of y response

#Log transformation of response variable

summary(train$USPrice) 
summary(log10(train$USPrice))

train$USPrice_trans <- log(train$USPrice)


#Run linear regression again
price_trans.lm <- lm(USPrice_trans~ is_trans + partcat_name + theme_name + sub_theme.x + version + year, data = train, x =T, y =T)

# Normal Probability Plot

qqnorm(rstudent(price_trans.lm))
qqline(rstudent(price_trans.lm))

## The plot shows a better linear pattern in general.


#Residual plot vs. fitted values
ti <- rstudent(price_trans.lm)
yhat <- fitted(price_trans.lm)
plot(yhat,ti)
abline(h=0)

## The plot shows the residuals have balanced distribution

###### 8. Cross Validation: K=5 #############
library(lmvar)
cv.error = cv.lm(price.lm, K=5, max_cores = 4)
cv.error
# Mean absolute error              :  28.77336 
# Sample standard deviation        :  0.2601679 
# 
# Mean squared error               :  1877.226 
# Sample standard deviation        :  58.14435 
# 
# Square root of mean squared error:  43.32227 
# Sample standard deviation        :  0.6726511 

cv.trans.error = cv.lm(price_trans.lm, K=5, max_cores = 4)
cv.trans.error

# Mean absolute error              :  0.5816995 
# Sample standard deviation        :  0.00293064 
# 
# Mean squared error               :  0.5656086 
# Sample standard deviation        :  0.005487647 
# 
# Square root of mean squared error:  0.7520616 
# Sample standard deviation        :  0.003647294 

###### 9. Prediction of Test Data Set  ############# 

#Prediction of USPrice using all 6 variables
trans_pred <- predict(price_trans.lm, newdata = test)
length(pred)

trans_mse <- mean(log(test$USPrice) - trans_pred)^2

#[1] 0.0001064955

pred <- predict(price.lm, newdata = test)
length(pred)

mse <- mean(test$USPrice - pred)^2
mse

#[1] 0.003365837

#prediction using log-tranformed model
###### 10. Logistic Regression ########

library(pROC)

#look at the possible themes to look at 
unique(set_themes_merge2$theme_name)
# see the count in each theme
table(set_themes_merge2$theme_name)
# Some themes that are sufficiently large to explore
#Bionicle - 220
# City -287
# Duplo - 174
#Castle - 139
# ninjago - 123
# star wars- 313
# town - 235

# decided to use just the set_themes_merge2 data instead of the master to reduce complexity  - also,looking to identify the set theme (not individual pieces)
#create new dataframe with column y/n for star wars as theme
sw_master <- set_themes_merge2
for (i in 1:nrow(sw_master)){
  if (sw_master$theme_name[i] == 'Star Wars'){
    sw_master$sw[i] <- 1
  }
  else {
    sw_master$sw[i] <- 0
  }
}

#Update types
sapply(sw_master, typeof)
sw_master$theme_name <- as.factor(sw_master$theme_name)
sw_master$sub_theme_id <- as.factor(sw_master$sub_theme_id)
sw_master$set_name <- as.factor(sw_master$set_name)
sw_master$sub_theme_id <- as.factor(sw_master$sub_theme_id)
sw_master$set_num <- as.factor(sw_master$set_num)
sw_master$theme_id <- as.factor(sw_master$theme_id)
sw_master$sw <- as.factor(sw_master$sw)

colnames(sw_master)[colnames(sw_master) == "sub_theme.x"] <- "sub_theme"
sw_master$sub_theme <- as.factor(sw_master$sub_theme)

# drop the sub theme id, theme and theme id for model building
sw_master2 <- subset(sw_master, select=-c(sub_theme, theme_id, theme_name))

## randomly split the data to create training and testing sets
set.seed(19)
sw_train = sample(1:nrow(sw_master2), nrow(sw_master2) * .75)
sw.train <- sw_master2[sw_train, ]
sw.test <- sw_master2[-sw_train, ]


#basic model
sw.glm <- glm(sw~ year + sub_theme_id + num_parts + USPrice, data=sw.train, family=binomial)
summary(sw.glm)
# subtheme id definitely not sinificant used this way, try model without it
sw.glm <- glm(sw~ year + num_parts + USPrice, data=sw.train, family=binomial)
summary(sw.glm)
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.2542  -0.4173  -0.3598  -0.2733   2.6084  

#Coefficients:
#  Estimate   Std. Error z value     Pr(>|z|)    
#(Intercept) -124.2536099   21.8165813  -5.695 0.0000000123 ***
#  year           0.0604580    0.0108537   5.570 0.0000000254 ***
#  num_parts      0.0004578    0.0002139   2.140       0.0323 *  
#  USPrice        0.0010714    0.0020575   0.521       0.6025    

#Null deviance: 1672.9  on 3364  degrees of freedom
#Residual deviance: 1609.1  on 3361  degrees of freedom
#AIC: 1617.1
# year and the number of parts are definitely the most significant 

## assess the model based on deviance
1-pchisq(1672.9, df=3361)
#[1] 1
# probably not the most predictive model

## Predict the probability of success
options(scipen=999)
sw.pred <- predict(sw.glm, type="response")

## Assess deviance residuals and influential point measures
sw.resid <- residuals(sw.glm, type="deviance")
plot(sw.pred,sw.resid)
# plot shows two distinct groups. There are so outlying points

summary(influence.measures(sw.glm))
# there are a significant number of points showing statistically influential points via DFFITS

# check the ROC curve - it does not look encouraging 
sw.train$prob <- sw.pred
sw.roc <- roc(sw ~ prob, data = sw.train)
plot(sw.roc) 

## try again the test set
sw.predvect=predict(sw.glm, newdata = sw.test, type="response" ) 
sw.predvect<- ifelse(sw.predvect> 0.7,1,0)
misClasificError <- mean(sw.predvect != sw.test$sw)
misClasificError
#[1] 0.07486631

## prediction far better than expected. 


### Logistic Regression to find any theme related to movies
## the idea here is that the movie themes have different characteristics than the classic lego sets
## create dummy var for the movie themed legos
unique(set_themes_merge2$theme_name)
lego_movie <- set_themes_merge2
for (i in 1:nrow(lego_movie)){
  if (lego_movie$theme_name[i] == 'Star Wars'| lego_movie$theme_name[i] == 'SpongeBob SquarePants'| lego_movie$theme_name[i] == 'Avatar The Last Airbender'|lego_movie$theme_name[i] == 'Toy Story'
      |lego_movie$theme_name[i] == 'The Hobbit' | lego_movie$theme_name[i] == 'Disney'|lego_movie$theme_name[i] == 'Marvel Super Heroes'|lego_movie$theme_name[i] == 'Harry Potter'
      |lego_movie$theme_name[i] == 'DC Comics Super Heroes'|lego_movie$theme_name[i] == 'Batman'|lego_movie$theme_name[i] == 'The LEGO Batman Movie'|lego_movie$theme_name[i] == 'The LEGO Movie'
      |lego_movie$theme_name[i] == 'DC Super Hero Girls'|lego_movie$theme_name[i] == 'The Lord of the Rings'|lego_movie$theme_name[i] == 'Prince of Persia'|lego_movie$theme_name[i] == 'Teenage Mutant Ninja Turtles'
      |lego_movie$theme_name[i] == 'The Angry Birds Movie'|lego_movie$theme_name[i] == 'The Lone Ranger'|lego_movie$theme_name[i] == 'Indiana Jones'
      |lego_movie$theme_name[i] == 'The LEGO Ninjago Movie'|lego_movie$theme_name[i] == 'Jurassic World'|lego_movie$theme_name[i] == 'Spider-Man'|lego_movie$theme_name[i] == 'Pirates of the Caribbean'
      |lego_movie$theme_name[i] == 'Scooby-Doo'|lego_movie$theme_name[i] == 'Mickey Mouse'|lego_movie$theme_name[i] == 'The Simpsons'){
    lego_movie$movie[i] <- 1
  }
  else {
    lego_movie$movie[i] <- 0
  }
}

#Update data types
sapply(lego_movie, typeof)
lego_movie$theme_name <- as.factor(lego_movie$theme_name)
lego_movie$sub_theme_id <- as.factor(lego_movier$sub_theme_id)
lego_movie$set_name <- as.factor(lego_movie$set_name)
lego_movie$sub_theme_id <- as.factor(lego_movie$sub_theme_id)
lego_movie$set_num <- as.factor(lego_movie$set_num)
lego_movie$theme_id <- as.factor(lego_movie$theme_id)
lego_movie$movie <- as.factor(lego_movie$movie)
lego_movie$sub_theme <- as.factor(lego_movie$sub_theme)

# drop the sub theme id, theme and theme id for model building
lego_movie2 <- subset(lego_movie, select=-c(sub_theme, theme_id, theme_name))

## randomly split the data to create training and testing sets
set.seed(17)
movie_train = sample(1:nrow(lego_movie2), nrow(lego_movie2) * .75)
movie.train <- lego_movie2[movie_train, ]
movie.test <- lego_movie2[-movie_train, ]
movie.train <- subset(movie.train, select=-c(set_name))

movie.glm <- glm(movie~., data=movie.train, family=binomial)
summary(movie.glm)
#including subtheme_id was a bad choice. It created unnecessary complexity and did not improve the model at all. 
#Null deviance: 2706.823902697652  on 3364  degrees of freedom
#Residual deviance:    0.000000022146  on   59  degrees of freedom
#AIC: 6612

## Assess goodness-of-fit using deviance 
1-pchisq(0.000000017802 , df=58)
#1

## Test variables using the likelihood ratio test
movie.glm.red <- glm(movie~.-sub_theme_id, data=movie.train, family=binomial)
lrtest(movie.glm, movie.glm.red)
#second model looks better but not by much

#not trying the iterative approach as there are few explanatory variables and the two previous models have indicated focusing on 
#year, num_parts and price

movie.glm2 <- glm(movie~ num_parts+ year + USPrice, data=movie.train, family=binomial)
summary(movie.glm2)
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.6936  -0.6225  -0.4852  -0.3126   2.4127  

#Coefficients:
#  Estimate   Std. Error z value             Pr(>|z|)    
#(Intercept) -174.9453952   17.4839565 -10.006 < 0.0000000000000002 ***
#  num_parts      0.0005622    0.0001950   2.883              0.00394 ** 
#  year           0.0860753    0.0086955   9.899 < 0.0000000000000002 ***
#  USPrice       -0.0002033    0.0019099  -0.106              0.91523    
#Null deviance: 2706.8  on 3364  degrees of freedom
#Residual deviance: 2535.0  on 3361  degrees of freedom
#AIC: 2543

### Again, number of parts and year are the most significant

## assess goodness of fit using deviance
1-pchisq(2550.4 , df=3266)
#1

## compare with a model that excludes price - there appears to be some colinearity between price and number of parts, which makes sense
#however, the deviance wasn't vastly improved by the removal of price so we will continue to use the model that includes it. 
movie.glm3 <- glm(movie~ num_parts+ year , data=movie.train, family=binomial)
summary(movie.glm3)
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.6727  -0.6227  -0.4848  -0.3127   2.4131  

#Coefficients:
#  Estimate    Std. Error z value             Pr(>|z|)    
#(Intercept) -174.90818232   17.47966730 -10.006 < 0.0000000000000002 ***
#  num_parts      0.00054431    0.00009947   5.472         0.0000000445 ***
#  year           0.08605593    0.00869324   9.899 < 0.0000000000000002 ***
#Null deviance: 2706.8  on 3364  degrees of freedom
#Residual deviance: 2535.0  on 3362  degrees of freedom
#AIC: 2541

## Predict the probability of success
options(scipen=999)
movie.pred <- predict(movie.glm2, type="response")

## Assess deviance residuals and influential point measures
movie.resid <- residuals(movie.glm2, type="deviance")
plot(movie.pred,movie.resid)
# plot shows two distinct groups. Overall looks good but not a perfect predicitor

summary(influence.measures(movie.glm2))
# there are a few  points showing statistically influential points via DFFITS. There are also some significance in the hat matrix, which
#aligns with the suspected colinearity. 

# check the ROC curve -  No terrible. Is an improvement over random guess. 
movie.train$prob <- movie.pred
movie.roc <- roc(movie ~ prob, data = movie.train)
plot(movie.roc) 

## try against the test set
movie.predvect=predict(movie.glm2, newdata = movie.test, type="response" ) 
movie.predvect<- ifelse(movie.predvect> 0.5,1,0)
misClasificError_movie <- mean(movie.predvect != movie.test$movie)
misClasificError_movie
#0.1461676

