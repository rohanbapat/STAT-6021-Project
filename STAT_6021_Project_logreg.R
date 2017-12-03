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


###### Data Cleaning #############

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
colnames(set_themes_merge2 )[colnames(set_themes_merge2 ) == "sub_theme.x"] <- "sub_theme"

# 6. Merge inventories with inventorysets on (id, set_num)<>(inventory_id, set_num)
inventories_invset_merge <- merge(x = inventories_df, y = inventorysets_df, by.x = c("id","set_num"), by.y = c("inventory_id","set_num"), all = TRUE)

# 7. Merge inventory sets with inventories_st_merge on set_num
inventorysets_settheme_merge <- merge(inventories_invset_merge, set_themes_merge2, by = "set_num")

# 8. Merge master with inventories_st_merge on inventory_id <> id
master_df2 <- merge(x = master_df, y = inventorysets_settheme_merge, by.x = "inventory_id", by.y = "id", all = T)

# 9. Master with no set_num NAs
master_df_final <- master_df2[!(is.na(master_df2$set_num)), ]


####### Couple of Log Reg Approaches to predicting certain themes #########
library(lmtest)
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
sw_train = sample(1:nrow(sw_master2), nrow(sw_master2) * .75)
sw.train <- sw_master2[sw_train, ]
sw.test <- sw_master2[-sw_train, ]


#basic model
sw.glm <- glm(sw~ year + sub_theme_id + num_parts + USPrice, data=sw.train, family=binomial)
summary(sw.glm)
# subtheme id definitely not sinificant used this way, try model without it
sw.glm <- glm(sw~ year + num_parts + USPrice, data=sw.train, family=binomial)
summary(sw.glm)
#Coefficients:
 # Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -1.165e+02  2.256e+01  -5.163 2.44e-07 ***
#  year         5.657e-02  1.122e-02   5.042 4.62e-07 ***
#  num_parts    6.094e-04  2.261e-04   2.696  0.00703 ** 
#  USPrice      7.735e-04  2.062e-03   0.375  0.70758    

#Null deviance: 1690.2  on 3269  degrees of freedom
#Residual deviance: 1624.6  on 3266  degrees of freedom
#AIC: 1632.6
# year and the number of parts are definitely the most significant 

## assess the model based on deviance
1-pchisq(1624.6, df=3266)
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
#[1] 0.07155963

## prediction far better than expected. 


### Logistic Regression to find any theme related to movies
## create dummy var for the movie themed legos
unique(set_themes_merge2$theme_name)
sw_movie <- set_themes_merge2
for (i in 1:nrow(sw_movie)){
  if (sw_movie$theme_name[i] == 'Star Wars'| sw_movie$theme_name[i] == 'SpongeBob SquarePants'| sw_movie$theme_name[i] == 'Avatar The Last Airbender'|sw_movie$theme_name[i] == 'Toy Story'
      |sw_movie$theme_name[i] == 'The Hobbit' | sw_movie$theme_name[i] == 'Disney'|sw_movie$theme_name[i] == 'Marvel Super Heroes'|sw_movie$theme_name[i] == 'Harry Potter'
      |sw_movie$theme_name[i] == 'DC Comics Super Heroes'|sw_movie$theme_name[i] == 'Batman'|sw_movie$theme_name[i] == 'The LEGO Batman Movie'|sw_movie$theme_name[i] == 'The LEGO Movie'
      |sw_movie$theme_name[i] == 'DC Super Hero Girls'|sw_movie$theme_name[i] == 'The Lord of the Rings'|sw_movie$theme_name[i] == 'Prince of Persia'|sw_movie$theme_name[i] == 'Teenage Mutant Ninja Turtles'
      |sw_movie$theme_name[i] == 'The Angry Birds Movie'|sw_movie$theme_name[i] == 'The Lone Ranger'|sw_movie$theme_name[i] == 'Indiana Jones'
      |sw_movie$theme_name[i] == 'The LEGO Ninjago Movie'|sw_movie$theme_name[i] == 'Jurassic World'|sw_movie$theme_name[i] == 'Spider-Man'|sw_movie$theme_name[i] == 'Pirates of the Caribbean'
      |sw_movie$theme_name[i] == 'Scooby-Doo'|sw_movie$theme_name[i] == 'Mickey Mouse'|sw_movie$theme_name[i] == 'The Simpsons'){
    sw_movie$movie[i] <- 1
  }
  else {
    sw_movie$movie[i] <- 0
  }
}


