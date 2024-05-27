rm(list = ls())

setwd("~/Documents/NUS/Y1S1/DSE1101/Midterm Project")

install.packages("e1071")

library(readxl)
library(boot)         # for cross validations
library(kknn)
library(e1071)
library(sjPlot)
library(sjmisc)
library(sjlabelled)
library(tree)
library(rpart)


hdb_data <- read_excel("HDB_data_2021_sample.xlsx")
hdb_data$resale_price = (hdb_data$resale_price)/1000  #Normalise to return smaller values
summary(hdb_data$resale_price)

#Used for Part 2: Explaining how data is split
set.seed(0773795)
ntrain=5000 #set size of the training sample

tr = sample(1:nrow(hdb_data),ntrain)  # draw ntrain observations from original data
train = hdb_data[tr,]   # Training sample
test = hdb_data[-tr,]   # Testing sample
summary(hdb_data$resale_price)

#Barplot to show resale price
barplot(table(cut(hdb_data$resale_price, breaks = seq(200, 1300, by = 100),
        labels =paste(seq(200, 1200, by =100), seq(300, 1300, by = 100), sep="-"))))

sd(hdb_data$resale_price)

#Principal Component Analysis for the training data
prall_hdb = prcomp(train, center = TRUE)
plot(prall_hdb, type = "l")
prall_hdb_s = summary(prall_hdb)
prall_hdb$rotation
cor(prall_hdb)[1]scree = prall_hdb_s$importance[2,] #save the proportion of variance explained

#Scree plot:
plot(scree, main = "Scree Plot", xlab = "Variables affecting HDB Resale Price", 
     ylab = "Proportion of Variance Explained", ylim = c(0,1), type = 'l', cex = .8)

pcr.fit=pcr(resale_price~ . ,data=hdb_data, scale=TRUE, validation="CV")

#Making a linear model with all the variables less the postal code as it is a duplicate of the town
lm_all = lm(resale_price ~ town_ANG.MO.KIO + town_BEDOK + town_BISHAN + 
                    town_BUKIT.BATOK + town_BUKIT.MERAH + town_BUKIT.PANJANG + 
                    town_BUKIT.TIMAH + town_CENTRAL.AREA + town_CHOA.CHU.KANG + 
                    town_CLEMENTI + town_GEYLANG + town_HOUGANG + town_JURONG.EAST + 
                    town_JURONG.WEST + town_KALLANG.WHAMPOA + town_MARINE.PARADE + 
                    town_PASIR.RIS + town_PUNGGOL + town_QUEENSTOWN + town_SEMBAWANG + 
                    town_SENGKANG + town_SERANGOON + town_TAMPINES + town_TOA.PAYOH + 
                    town_WOODLANDS + town_YISHUN + Remaining_lease + floor_area_sqm + 
                    max_floor_lvl + storey_range_01.TO.03 +
                    storey_range_04.TO.06 + storey_range_07.TO.09 + 
                    storey_range_10.TO.12 + storey_range_13.TO.15 + 
                    storey_range_16.TO.18 + storey_range_19.TO.21 + 
                    storey_range_21.TO.25 + storey_range_25.TO.27 + 
                    storey_range_26.TO.30 + storey_range_31.TO.33 + 
                    storey_range_31.TO.35 + storey_range_36.TO.40 + 
                    storey_range_37.TO.39 + storey_range_43.TO.45 + 
                    storey_range_46.TO.48 + total_dwelling_units + 
                    exec_sold + multigen_sold + studio_apartment_sold + X1room_rental + 
                    X2room_rental + X3room_rental + other_room_rental + mature + 
                    flat_type_1.ROOM + flat_type_2.ROOM + flat_type_3.ROOM + 
                    flat_type_4.ROOM + flat_type_5.ROOM + flat_type_EXECUTIVE + 
                    flat_type_MULTI.GENERATION + flat_model_2.room + flat_model_adjoined.flat + 
                    flat_model_apartment + flat_model_dbss + flat_model_improved + 
                    flat_model_improved.maisonette + flat_model_maisonette + 
                    flat_model_model.a + flat_model_model.a.maisonette + flat_model_model.a2 + 
                    flat_model_multi.generation + flat_model_new.generation + 
                    flat_model_premium.apartment + flat_model_premium.apartment.loft + 
                    flat_model_premium.maisonette + flat_model_simplified + flat_model_standard + 
                    flat_model_terrace + flat_model_type.s1 + flat_model_type.s2 + 
                    Dist_nearest_GHawker + nearest_ghawker_no_of_cooked_food_stalls + 
                    nearest_ghawker_no_of_mkt_produce_stalls + nearest_ghawker_no_of_stalls + 
                    Dist_nearest_mall + no_malls_0.5km + no_malls_1km + no_malls_2km + 
                    Dist_nearest_beach + beach_within_2km + Dist_nearest_waterbody + 
                    waterbody_within_2km + Dist_nearest_CC + Dist_nearest_station + 
                    NSL + EWL + NEL + CCL + DTL + TEL + LRT + unique_no_mrt_0.5km + 
                    unique_no_mrt_1km + Dist_nearest_primary_school + Nearest_primary_school_gender_BOYS..SCHOOL + 
                    Nearest_primary_school_gender_CO.ED.SCHOOL + Nearest_primary_school_gender_GIRLS..SCHOOL + 
                    no_primary_schools_1km + no_primary_schools_2km + Dist_nearest_GAI_primary_school + 
                    Nearest_GAI_primary_school_gender_BOYS..SCHOOL + Nearest_GAI_primary_school_gender_CO.ED.SCHOOL + 
                    Nearest_GAI_primary_school_gender_GIRLS..SCHOOL + no_GAI_primary_schools_1km + 
                    no_GAI_primary_schools_2km + Dist_nearest_G_primary_school + 
                    Nearest_G_primary_school_gender_CO.ED.SCHOOL + Nearest_G_primary_school_gender_GIRLS..SCHOOL + 
                    no_G_primary_schools_1km + no_G_primary_schools_2km + Dist_nearest_secondary_school + 
                    Nearest_secondary_school_gender_BOYS..SCHOOL + Nearest_secondary_school_gender_CO.ED.SCHOOL + 
                    Nearest_secondary_school_gender_GIRLS..SCHOOL + Dist_nearest_GAI_secondary_school + 
                    Nearest_GAI_secondary_school_gender_BOYS..SCHOOL + Nearest_GAI_secondary_school_gender_CO.ED.SCHOOL + 
                    Nearest_GAI_secondary_school_gender_GIRLS..SCHOOL + Dist_nearest_G_secondary_school + 
                    Nearest_G_secondary_school_gender_BOYS..SCHOOL + Nearest_G_secondary_school_gender_CO.ED.SCHOOL + 
                    Nearest_G_secondary_school_gender_GIRLS..SCHOOL + Dist_nearest_jc + 
                    Dist_nearest_GAI_jc + Dist_nearest_G_jc + Dist_nearest_polytechnic + 
                    Dist_nearest_university + Dist_nearest_hospital + Nearest_hospital_ownership_Non.profit + 
                    Nearest_hospital_ownership_Private + Nearest_hospital_ownership_Public + 
                    Dist_nearest_A_hospital + Nearest_A_hospital_ownership_Non.profit + 
                    Nearest_A_hospital_ownership_Private + Nearest_A_hospital_ownership_Public + 
                    Dist_CBD + Dist_nearest_ADF + ADF_within_0.5km + ADF_within_1km + 
                    ADF_within_2km, data = train) # Using a Multiple Linear Regression, the Adjusted R Squared Value is 0.9275, which is relatively high
sum_all = summary(lm_all)
sum_all

# LM for Remaining Lease
lm_Remaining_lease = lm(resale_price ~ Remaining_lease,
                  train)
summary(lm_Remaining_lease)
plot(train$resale_price ~ train$Remaining_lease,
     xlab = "Remaining Lease",
     ylab = "HDB Resale Price",
     main = "Remaining Lease vs Resale Price",
     pch = 19, col = "gray50")

abline(lm_Remaining_lease,
       col = "Red")
draw(y = x)

#LM for floor_area_sqm
lm_floor_area = lm(resale_price ~ floor_area_sqm,
                   train)

plot(train$resale_price ~ train$floor_area_sqm,
     xlab = "Floor Area (sqm)",
     ylab = "Resale Price",
     main = "Floor Area (sqm) vs Resale Price",
     pch = 19, col = "gray50")

abline(lm_floor_area,
       col = "Red")

#LM for amenities
lm_amenities = lm(resale_price ~ Dist_nearest_GHawker + Dist_nearest_mall + Dist_nearest_station + Dist_nearest_CC + Dist_nearest_beach
                  + Dist_nearest_primary_school + Dist_nearest_secondary_school + Dist_nearest_jc + Dist_nearest_polytechnic + Dist_nearest_hospital,
                  data = train)
summary(lm_amenities)
tab_model(lm_amenities)

#LM for MRT
lm_mrt = lm(resale_price ~ Dist_nearest_station + NSL + EWL + NEL + CCL + DTL + TEL + LRT,
            data = train)
data_mrt = data.frame(NSL = 1,
                      EWL = 0,
                      NEL = 0,
                      CCL = 0,
                      DTL = 0,
                      TEL = 0,
                      LRT = 0,
                      Dist_nearest_station = 0.3)
mrt = (train$NSL + hdb_data$EWL + hdb_data$NEL + train$CCL + train$DTL + train$TEL + train$LRT)
predict(lm_mrt, data_mrt, type = "response")
summary(lm_mrt)$coef
tab_model(lm_mrt)

cor(train$CCL, train$Dist_CBD)

#Big Tree (Figure 2)
temp = tree(resale_price~.,data=train,mindev=0.0001)
length(unique(temp$where))
hdb.tree=prune.tree(temp,best=7)
cv.amen = cv.tree(temp, , prune.tree) #10-fold cross-validation 
# need to put the empty space if not argument will return an error
bestcp = cv.amen$size[max(which(cv.amen$dev == min(cv.amen$dev)))]
plot(hdb.tree,type="uniform")
text(hdb.tree,col="blue",label=c("yval"),cex=.8)

big.tree = rpart(resale_price~.,method= "anova" ,data=train, minsplit=5,cp=.0005)

#Count the leaves
length(unique(big.tree$where))
best.tree = prune(big.tree,cp=bestcp) #get tree for best cp on CV
plot(best.tree)
text(best.tree,digits=4,use.n=TRUE,fancy=FALSE,bg='lightblue') #note fancy=TRUE option would give a "nicer" look (see end of file for even nicer looking plot using rpart.plot)

treefit=predict(best.tree,newdata=test,type="vector") #prediction on test data
mean((test$medv-treefit)^2)

#Linear model for max floor
lm_max_flr = lm(train$resale_price ~ train$max_floor_lvl)
summary(lm_max_flr)
plot(train$max_floor_lvl, train$resale_price,
     xlab = "Maximum Floor Level",
     ylab = "Resale Price",
     )
abline(lm_max_flr, col = "red")

cor(train$max_floor_lvl,  train$Remaining_lease)

#Linear model for distance to CBD
lm_cbd = lm(resale_price ~ Dist_CBD,
               data = train)
summary(lm_cbd)
plot(train$Dist_CBD, train$resale_price,
     xlab = "Distance to CBD",
     ylab =" Resale Price")
abline(lm_cbd,col = "red")

#New Model
lm_new = lm(resale_price ~ Dist_CBD + max_floor_lvl + flat_model_dbss + floor_area_sqm + Remaining_lease + mature + Dist_nearest_station + NSL + EWL + NEL + CCL + DTL,
            data = train)
summary(lm_new)

#Predicting on 154 Toa Payoh
values = data.frame(Dist_CBD = 5.3,
                    max_floor_lvl = 30,
                    floor_area_sqm = 121,
                    flat_model_dbss = 0,
                    Remaining_lease = 79,
                    mature = 1,
                    Dist_nearest_station = 0.1,
                    NSL = 1,
                    EWL = 0,
                    NEL = 0,
                    CCL = 0,
                    DTL = 0)

predict(lm_new,newdata = values, type = "response")

#Correlation between resale price and mature estate
cor(train$resale_price, train$mature)
