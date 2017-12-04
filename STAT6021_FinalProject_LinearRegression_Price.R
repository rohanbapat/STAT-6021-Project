
install.packages("RCurl")

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

#Convert respective columsn to factor/numeric
colnames(master_df_final)
num <- c("num_parts","inventorypart_quantity","inventorysets_quantity","part_num","year","USPrice")

master_df_final[num] <- lapply(master_df_final[num], as.numeric)

cat <- c("inventory_id","color_id", "is_spare", "color_name" ,"rgb","is_trans","part_name","partcat_name",
         "set_num","version","set_name","sub_theme_id", "sub_theme.x","theme_id",              
         "theme_name")

master_df_final[cat] <- lapply(master_df_final[cat], as.factor)

###### 2. Data Partition and Data Cleaning  #############
#Dropping unwanted columns
master_wanted <- subset(master_df_final, select = -c(color_id,rgb,sub_theme_id,inventory_id, part_cat_id, theme_id,inventorysets_quantity))

#Check for NA
na_count <-data.frame(sapply(master_wanted, function(y) sum(length(which(is.na(y))))))

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
test <- test[!test$theme_name=="Dinosaurs",]
train <- train[!train$theme_name=="Power Functions",]
train <- train[!train$theme_name=="Dinosaurs",]
dim(test)
dim(train)

#Remove new levels in sub_theme.x between test and train
se1 <-unique(test$sub_theme.x)
sr1<-unique(train$sub_theme.x)

se1[!(se1 %in% sr1)]
sr1[!(se1 %in% sr1)]

test <- test[!test$sub_theme.x=="Master Building Academy",]
test <- test[!test$sub_theme.x=="Fusion",]
train <- train[!train$sub_theme.x=="Master Building Academy",]
train <- train[!train$sub_theme.x=="Fusion",]
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

###### 3. Variable Selection #############

library(leaps)

# s.null <- lm(USPrice~1, data=train)
# s.full <- lm(USPrice~ is_trans + partcat_name + theme_name + sub_theme.x + version + year, 
#              data = train)
# 
# Stepwise selection
# step(s.null, scope=list(lower=s.null, upper=s.full), direction="both")

     #I tried to do stepwise variable selection. But because there are too many factor variables 
     #and too many levels in each factor variable, it is hard to run.
     #So I decided to select variable in the following ways.


#Data Exploration and Variable Selection
train <- subset(train, train$USPrice < 350)

plot(USPrice~year+is_trans+version+theme_name+sub_theme.x+partcat_name+is_spare+rgb+color_name, data = train)

#Year seems to have a high related positive relationship
#version - related 1, or 2, positive

    ## Variable Selection:

    ## inventary_id, theme_id, part_cat_id, sub_theme_id, color_id are not good variables for modeling.
    ## part_num, part_name, set_num, set_num, and num_part have thousands levels, not good for modeling
    ## color_name, rgb, is_spares are not related much with USPrice.

    ## Therefore,
    ## 6 Variables: year, version, in_trans, theme_name, sub_theme.x, partcat_nam, 
    ## which have high association with USPrice, are selected as predictors for modeling.

###### 4. Fit Linear Regression Models #############

#1. Build linear model using year variable


pricey.lm <-lm(USPrice ~ year, data = train)
summary(pricey.lm) 
      # By year is significant

null_model <- lm(USPrice~1, data = train)

full_model <- lm(USPrice~., data = train)

## Stepwise selection
step(null_model,scope = list(lower=null_model, upper=full_model), direction="forward")


#2. Built linear regression model using the following 6 variables based on step wise selection
price.lm <- lm(USPrice~ is_trans + partcat_name + theme_name + sub_theme.x + version + year, data = train)
summary(price.lm)

     # All 6 variables year, version, is_trans, partcat_name, theme_name, sub_theme.x are significant.


###### 4. Cross Validation: K=5 #############

library(boot)
set.seed(123)

#Check the full model
price.lm <- glm(USPrice~ is_trans + partcat_name + theme_name + sub_theme.x + version + year, data = train)
cv.error = cv.glm(train, price.lm, K=5)
cv.error$delata[1]
   ## problem with CV, maybe because too many levels in each factor variables
   ## error message: prediction from a rank-deficient fit may be misleading

#Check the USPrice~year Model
cv.error1 <- cv.glm(train, pricey.lm, K=5)
cv.error1$delta
   ## error message: prediction from a rank-deficient fit may be misleading

#Calculate mean squared errro of the model
mse <- mean(train$USPrice - yhat)^2
mse  
    ##[1] 6.495178e-22
  
  
###### 5. Check Model Accuracy  #############

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


###### 6. Transformation  ############# 

library(MASS)
train0 <- train[!train$USPrice==0,]
price0.lm <- glm(USPrice~ is_trans + partcat_name + theme_name + sub_theme.x + version + year, data = train0)

out <- boxcox(price0.lm)
range(out$x[out$y > max(out$y)-qchisq(0.95,1)/2])
     ## alpha = 0.18
     ## Try log transfomation of y response

#Log transformation of response variable

summary(train0$USPrice) 
summary(log10(train0$USPrice))

train0$USPrice_trans <- log(train0$USPrice)


#Run linear regression again
price0_trans.lm <- glm(USPrice_trans~ is_trans + partcat_name + theme_name + sub_theme.x + version + year, data = train0)

# Normal Probability Plot

qqnorm(rstudent(price0_trans.lm))
qqline(rstudent(price0_trans.lm))

      ## The plot shows a better linear pattern in general.


#Residual plot vs. fitted values
ti <- rstudent(price0_trans.lm)
yhat <- fitted(price0_trans.lm)
plot(yhat,ti)
abline(h=0)

       ## The plot shows the residuals have balanced distributed.


###### 7. Muticollinearity  ##############

#multicollinearity check need to use numeric avariable, since our variables are factors, this doesn't apply here.
 


###### 8. Prediction of Test Data Set  ############# 

#Predict using year predictor variable
pred1 <- predict(pricey.lm, newdata = test)
length(pred1)

mse <- mean(test$USPrice - pred1)^2
mse
    #[1] 2.189539

#Prediction of USPrice using all 6 variables
pred <- predict(price0.lm, newdata = test)
length(pred)

mse <- mean(test$USPrice - pred)^2
mse
    #[1] 1.815016

#prediction using log-tranformed model

#remove the new level in test
test <- test[!test$sub_theme.x == "The Lord of the Rings",]

pred0 <- predict(price0_trans.lm, newdata = test)
    ## error message: prediction from a rank-deficient fit may be misleading


###### 9. Summary of the Final Linear Regression Mode  ############# 

# Predictor variables: year, theme_name, sub_theme.x, version, partcat_name, is_trans
# Response variables: USPrice

# Final linear Regression Model:
price.lm <- glm(USPrice~ is_trans + partcat_name + theme_name + sub_theme.x + version + year, data = train)

#Prediction of USPrice using all 6 variables:
pred <- predict(price0.lm, newdata = test)
mse <- mean(test$USPrice - pred)^2
mse

#Test MSE is 1.815016
 
  

###### 10. ggplot to show the variable relationships with USPrice  ############# 

# 1. Plot to show that Lego Price ("USPrice") increases with Years ("year")


# 2. The top 10 Theme Names ("theme_name") with highest price ("USPrice")


# 3. The top 10 SUb_Theme names ("sub_theme.x") with highest price ("USPrice")


# 4. The top 10 Part Name ("partcat_name") with highest price ("USPrice")





