
install.packages("MASS")
install.packages("car")

#load the excel data
cars_assignment <- read.csv("CarPrice_Assignment.csv")

#summaryof the data
summary(cars_assignment)
str(cars_assignment)

library(tidyr)
#data preparation
#separate car brand and name
cars_assignment <- separate(cars_assignment, CarName, into=c("Brand", "Model"), sep = " ")

#drop car model and car id to avoid any confusion in building model
cars_assignment <- cars_assignment[,-4]
cars_assignment <- cars_assignment[,-1]

#correct the names of the brand of car
summary(factor(cars_assignment$Brand))

cars_assignment$Brand[which(cars_assignment$Brand == "maxda")] <- "mazda"
cars_assignment$Brand[which(cars_assignment$Brand == "nissan")] <- "Nissan"
cars_assignment$Brand[which(cars_assignment$Brand == "porcshce")] <- "porsche"
cars_assignment$Brand[which(cars_assignment$Brand == "toyouta")]<- "toyota"
cars_assignment$Brand[which(cars_assignment$Brand == "vokswagen")] <- "volkswagen"
cars_assignment$Brand[which(cars_assignment$Brand == "vw")] <- "volkswagen"

#no NA's values as of now too be treated
#DUMMY variable creation as there are lot of categorical value in dataset
#fuel type , aspiration , door number , engine location have 2 levels so converting those levels to 0,1 respectively
levels(cars_assignment$fueltype) <- c(0,1)
levels(cars_assignment$aspiration) <- c(0,1)
levels(cars_assignment$doornumber) <- c(0,1)
levels(cars_assignment$enginelocation) <- c(0,1)

cars_assignment$fueltype <- as.numeric(levels(cars_assignment$fueltype))[cars_assignment$fueltype]
cars_assignment$aspiration <- as.numeric(levels(cars_assignment$aspiration))[cars_assignment$aspiration]
cars_assignment$doornumber <- as.numeric(levels(cars_assignment$doornumber))[cars_assignment$doornumber]
cars_assignment$enginelocation <- as.numeric(levels(cars_assignment$enginelocation))[cars_assignment$enginelocation]

summary(cars_assignment)
str(cars_assignment)

#carbody, drivewheel , fuelsystem,engine type , cylinder number , brand
dummy_carbody <- data.frame(model.matrix(~carbody,data = cars_assignment))
dummy_carbody <- dummy_carbody[,-1]
cars_assignment <- cbind(cars_assignment[,-6],dummy_carbody)

dummy_drivewheel <- data.frame(model.matrix(~drivewheel,data = cars_assignment))
dummy_drivewheel <- dummy_drivewheel[,-1]
cars_assignment <- cbind(cars_assignment[,-6],dummy_drivewheel)

dummy_brand <- data.frame(model.matrix(~Brand,data = cars_assignment))
dummy_brand <- dummy_brand[,-1]
cars_assignment <- cbind(cars_assignment[,-2],dummy_brand)

dummy_enginetype <- data.frame(model.matrix(~enginetype,data = cars_assignment))
dummy_enginetype <- dummy_enginetype[,-1]
cars_assignment <- cbind(cars_assignment[,-11],dummy_enginetype)

dummy_cylindernumber <- data.frame(model.matrix(~cylindernumber,data = cars_assignment))
dummy_cylindernumber <- dummy_cylindernumber[,-1]
cars_assignment <- cbind(cars_assignment[,-11],dummy_cylindernumber)

dummy_fuelsystem <- data.frame(model.matrix(~fuelsystem,data = cars_assignment))
dummy_fuelsystem <- dummy_fuelsystem[,-1]
cars_assignment <- cbind(cars_assignment[,-12],dummy_fuelsystem)

# separate training and testing data
set.seed(100)
trainindices= sample(1:nrow(cars_assignment), 0.7*nrow(cars_assignment))
train = cars_assignment[trainindices,]
test = cars_assignment[-trainindices,]

# Build model 1 containing all variables
model_1 <-lm(price~.,data=train)
summary(model_1)

library("MASS")
library("car")
# In stepAIC function, we pass our first model i.e model_1 and 
# direction is ser as both, because in stepwise,  both the forward selection 
# of variables and backward elimination of variables happen simultaneously 
step <- stepAIC(model_1, direction="both")

step

#storing the last equation of step model into model_2
model_2 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + 
                enginesize + boreratio + stroke + peakrpm + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                Brandbmw + Brandbuick + Branddodge + Brandhonda + Brandjaguar + 
                Brandmazda + Brandmitsubishi + BrandNissan + Brandpeugeot + 
                Brandplymouth + Brandporsche + Brandrenault + Brandsaab + 
                Brandsubaru + Brandtoyota + Brandvolkswagen + enginetyperotor + 
                cylindernumberfive + fuelsystem2bbl + fuelsystemmpfi + highwaympg + 
                Brandmercury , data= cars_assignment)
summary(model_2)

#check for mullticolinearity
vif(model_2)

#remove drivewheelrwd because of high p value and VIF value
model_3 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + 
                enginesize + boreratio + stroke + peakrpm + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + 
                Brandbmw + Brandbuick + Branddodge + Brandhonda + Brandjaguar + 
                Brandmazda + Brandmitsubishi + BrandNissan + Brandpeugeot + 
                Brandplymouth + Brandporsche + Brandrenault + Brandsaab + 
                Brandsubaru + Brandtoyota + Brandvolkswagen + enginetyperotor + 
                cylindernumberfive + fuelsystem2bbl + fuelsystemmpfi + highwaympg + 
                Brandmercury , data= cars_assignment)
summary(model_3)

#check for mullticolinearity
vif(model_3)

#remove stroke because of high p value and VIF value
model_4 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + 
                enginesize + boreratio + peakrpm + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + 
                Brandbmw + Brandbuick + Branddodge + Brandhonda + Brandjaguar + 
                Brandmazda + Brandmitsubishi + BrandNissan + Brandpeugeot + 
                Brandplymouth + Brandporsche + Brandrenault + Brandsaab + 
                Brandsubaru + Brandtoyota + Brandvolkswagen + enginetyperotor + 
                cylindernumberfive + fuelsystem2bbl + fuelsystemmpfi + highwaympg + 
                Brandmercury , data= cars_assignment)
summary(model_4)

#check for mullticolinearity
vif(model_4)

#remove brandsubaru because of high p value and VIF value
model_5 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + 
                enginesize + boreratio + peakrpm + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + 
                Brandbmw + Brandbuick + Branddodge + Brandhonda + Brandjaguar + 
                Brandmazda + Brandmitsubishi + BrandNissan + Brandpeugeot + 
                Brandplymouth + Brandporsche + Brandrenault + Brandsaab + 
                Brandtoyota + Brandvolkswagen + enginetyperotor + 
                cylindernumberfive + fuelsystem2bbl + fuelsystemmpfi + highwaympg + 
                Brandmercury , data= cars_assignment)
summary(model_5)

#check for mullticolinearity
vif(model_5)

#remove brandmercury because it has high p value 
model_6 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + 
                enginesize + boreratio + peakrpm + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + 
                Brandbmw + Brandbuick + Branddodge + Brandhonda + Brandjaguar + 
                Brandmazda + Brandmitsubishi + BrandNissan + Brandpeugeot + 
                Brandplymouth + Brandporsche + Brandrenault + Brandsaab + 
                Brandtoyota + Brandvolkswagen + enginetyperotor + 
                cylindernumberfive + fuelsystem2bbl + fuelsystemmpfi + highwaympg  , data= cars_assignment)
summary(model_6)

#check for mullticolinearity
vif(model_6)

#remove brandsaab because it has high p value 
model_7 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + 
                enginesize + boreratio + peakrpm + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + 
                Brandbmw + Brandbuick + Branddodge + Brandhonda + Brandjaguar + 
                Brandmazda + Brandmitsubishi + BrandNissan + Brandpeugeot + 
                Brandplymouth + Brandporsche + Brandrenault + 
                Brandtoyota + Brandvolkswagen + enginetyperotor + 
                cylindernumberfive + fuelsystem2bbl + fuelsystemmpfi + highwaympg  , data= cars_assignment)
summary(model_7)

#check for mullticolinearity
vif(model_7)

#remove brandjaguar because it has high p value 
model_8 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + 
                enginesize + boreratio + peakrpm + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + 
                Brandbmw + Brandbuick + Branddodge + Brandhonda + 
                Brandmazda + Brandmitsubishi + BrandNissan + Brandpeugeot + 
                Brandplymouth + Brandporsche + Brandrenault + 
                Brandtoyota + Brandvolkswagen + enginetyperotor + 
                cylindernumberfive + fuelsystem2bbl + fuelsystemmpfi + highwaympg  , data= cars_assignment)
summary(model_8)

#check for mullticolinearity
vif(model_8)

#remove brandmazada because it has high p value 
model_9 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + 
                enginesize + boreratio + peakrpm + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + 
                Brandbmw + Brandbuick + Branddodge + Brandhonda + 
                Brandmitsubishi + BrandNissan + Brandpeugeot + 
                Brandplymouth + Brandporsche + Brandrenault + 
                Brandtoyota + Brandvolkswagen + enginetyperotor + 
                cylindernumberfive + fuelsystem2bbl + fuelsystemmpfi + highwaympg  , data= cars_assignment)
summary(model_9)

#check for mullticolinearity
vif(model_9)

#remove cylinderfive because it has high p value 
model_10 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + 
                enginesize + boreratio + peakrpm + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + 
                Brandbmw + Brandbuick + Branddodge + Brandhonda + 
                Brandmitsubishi + BrandNissan + Brandpeugeot + 
                Brandplymouth + Brandporsche + Brandrenault + 
                Brandtoyota + Brandvolkswagen + enginetyperotor + 
                 fuelsystem2bbl + fuelsystemmpfi + highwaympg  , data= cars_assignment)
summary(model_10)

#check for mullticolinearity
vif(model_10)

#remove brandhonda because it has high p value and VIF
model_11 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + 
                 enginesize + boreratio + peakrpm + carbodyhardtop + 
                 carbodyhatchback + carbodysedan + carbodywagon + 
                 Brandbmw + Brandbuick + Branddodge + 
                 Brandmitsubishi + BrandNissan + Brandpeugeot + 
                 Brandplymouth + Brandporsche + Brandrenault + 
                 Brandtoyota + Brandvolkswagen + enginetyperotor + 
                 fuelsystem2bbl + fuelsystemmpfi + highwaympg  , data= cars_assignment)
summary(model_11)

#check for mullticolinearity
vif(model_11)

#remove brandpuegouet because it has high p value and VIF
model_12 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + 
                 enginesize + boreratio + peakrpm + carbodyhardtop + 
                 carbodyhatchback + carbodysedan + carbodywagon + 
                 Brandbmw + Brandbuick + Branddodge + 
                 Brandmitsubishi + BrandNissan + 
                 Brandplymouth + Brandporsche + Brandrenault + 
                 Brandtoyota + Brandvolkswagen + enginetyperotor + 
                 fuelsystem2bbl + fuelsystemmpfi + highwaympg  , data= cars_assignment)
summary(model_12)

#check for mullticolinearity
vif(model_12)

#remove brandvolkswagen because it has comparitevly high p value and VIF
model_13 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + 
                 enginesize + boreratio + peakrpm + carbodyhardtop + 
                 carbodyhatchback + carbodysedan + carbodywagon + 
                 Brandbmw + Brandbuick + Branddodge + 
                 Brandmitsubishi + BrandNissan + 
                 Brandplymouth + Brandporsche + Brandrenault + 
                 Brandtoyota +  enginetyperotor + 
                 fuelsystem2bbl + fuelsystemmpfi + highwaympg  , data= cars_assignment)
summary(model_13)

#check for mullticolinearity
vif(model_13)

#remove brandrenault because it has comparitevly high p value and VIF
model_14 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + 
                 enginesize + boreratio + peakrpm + carbodyhardtop + 
                 carbodyhatchback + carbodysedan + carbodywagon + 
                 Brandbmw + Brandbuick + Branddodge + 
                 Brandmitsubishi + BrandNissan + 
                 Brandplymouth + Brandporsche + 
                 Brandtoyota +  enginetyperotor + 
                 fuelsystem2bbl + fuelsystemmpfi + highwaympg  , data= cars_assignment)
summary(model_14)

#check for mullticolinearity
vif(model_14)

#remove highwaympg because it has comparitevly high p value and VIF
model_15 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + 
                 enginesize + boreratio + peakrpm + carbodyhardtop + 
                 carbodyhatchback + carbodysedan + carbodywagon + 
                 Brandbmw + Brandbuick + Branddodge + 
                 Brandmitsubishi + BrandNissan + 
                 Brandplymouth + Brandporsche + 
                 Brandtoyota +  enginetyperotor + 
                 fuelsystem2bbl + fuelsystemmpfi   , data= cars_assignment)
summary(model_15)

#check for mullticolinearity
vif(model_15)

#remove highwaympg because it has comparitevly high p value and VIF
model_16 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + 
                 enginesize + boreratio + peakrpm + carbodyhardtop + 
                 carbodyhatchback + carbodysedan + carbodywagon + 
                 Brandbmw + Brandbuick + Branddodge + 
                 Brandmitsubishi + BrandNissan + 
                 Brandplymouth + Brandporsche + 
                 Brandtoyota +  enginetyperotor + 
                 fuelsystem2bbl  , data= cars_assignment)
summary(model_16)

#check for mullticolinearity
vif(model_16)

#remove brandtoyota because it has comparitevly high p value and VIF
model_17 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + 
                 enginesize + boreratio + peakrpm + carbodyhardtop + 
                 carbodyhatchback + carbodysedan + carbodywagon + 
                 Brandbmw + Brandbuick + Branddodge + 
                 Brandmitsubishi + BrandNissan + 
                 Brandplymouth + Brandporsche + 
                 enginetyperotor + 
                 fuelsystem2bbl  , data= cars_assignment)
summary(model_17)

#check for mullticolinearity
vif(model_17)

#remove branddoge because it has comparitevly high p value 
model_18 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + 
                 enginesize + boreratio + peakrpm + carbodyhardtop + 
                 carbodyhatchback + carbodysedan + carbodywagon + 
                 Brandbmw + Brandbuick +  
                 Brandmitsubishi + BrandNissan + 
                 Brandplymouth + Brandporsche + 
                 enginetyperotor + 
                 fuelsystem2bbl  , data= cars_assignment)
summary(model_18)

#check for mullticolinearity
vif(model_18)

#remove brandplymouth because it has comparitevly high p value 
model_19 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + 
                 enginesize + boreratio + peakrpm + carbodyhardtop + 
                 carbodyhatchback + carbodysedan + carbodywagon + 
                 Brandbmw + Brandbuick +  
                 Brandmitsubishi + BrandNissan + 
                  Brandporsche + 
                 enginetyperotor + 
                 fuelsystem2bbl  , data= cars_assignment)
summary(model_19)

#check for mullticolinearity
vif(model_19)

#remove fuelsystem2bbl because it has comparitevly high p and VIF value
model_20 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + 
                 enginesize + boreratio + peakrpm + carbodyhardtop + 
                 carbodyhatchback + carbodysedan + carbodywagon + 
                 Brandbmw + Brandbuick +  
                 Brandmitsubishi + BrandNissan + 
                 Brandporsche + 
                 enginetyperotor , data= cars_assignment)
summary(model_20)

#check for mullticolinearity
vif(model_20)

#remove brandnissan because it has comparitevly high p value
model_21 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + 
                 enginesize + boreratio + peakrpm + carbodyhardtop + 
                 carbodyhatchback + carbodysedan + carbodywagon + 
                 Brandbmw + Brandbuick +  
                 Brandmitsubishi + 
                 Brandporsche + 
                 enginetyperotor , data= cars_assignment)
summary(model_21)

#check for mullticolinearity
vif(model_21)

#remove boreratio because it has comparitevly high p and VIF value
model_22 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + 
                 enginesize +  peakrpm + carbodyhardtop + 
                 carbodyhatchback + carbodysedan + carbodywagon + 
                 Brandbmw + Brandbuick +  
                 Brandmitsubishi + 
                 Brandporsche + 
                 enginetyperotor , data= cars_assignment)
summary(model_22)

#check for mullticolinearity
vif(model_22)

#remove carbodysedan because it has comparitevly high p and VIF value
model_23 <- lm(price ~ aspiration + enginelocation + carwidth + curbweight + 
                 enginesize +  peakrpm + carbodyhardtop + 
                 carbodyhatchback +  carbodywagon + 
                 Brandbmw + Brandbuick +  
                 Brandmitsubishi + 
                 Brandporsche + 
                 enginetyperotor , data= cars_assignment)
summary(model_23)

#check for mullticolinearity
vif(model_23)

#remove curbweight because it has comparitevly high  VIF value
model_24 <- lm(price ~ aspiration + enginelocation + carwidth + 
                 enginesize +  peakrpm + carbodyhardtop + 
                 carbodyhatchback +  carbodywagon + 
                 Brandbmw + Brandbuick +  
                 Brandmitsubishi + 
                 Brandporsche + 
                 enginetyperotor , data= cars_assignment)
summary(model_24)

#check for mullticolinearity
vif(model_24)

#remove carbodywagon because it has comparitevly high p value
model_25 <- lm(price ~ aspiration + enginelocation + carwidth + 
                 enginesize +  peakrpm + carbodyhardtop + 
                 carbodyhatchback +  
                 Brandbmw + Brandbuick +  
                 Brandmitsubishi + 
                 Brandporsche + 
                 enginetyperotor , data= cars_assignment)
summary(model_25)

#check for mullticolinearity
vif(model_25)

#remove carbodyhardtop because it has comparitevly high p value
model_26 <- lm(price ~ aspiration + enginelocation + carwidth + 
                 enginesize +  peakrpm + 
                 carbodyhatchback +  
                 Brandbmw + Brandbuick +  
                 Brandmitsubishi + 
                 Brandporsche + 
                 enginetyperotor , data= cars_assignment)
summary(model_26)

#check for mullticolinearity
vif(model_26)

#remove engine because it has comparitevly high VIF value
model_27 <- lm(price ~ aspiration +  carwidth + 
                 enginesize +  peakrpm + 
                 carbodyhatchback +  
                 Brandbmw + Brandbuick +  
                 Brandmitsubishi + 
                 Brandporsche + 
                 enginetyperotor , data= cars_assignment)
summary(model_27)

#check for mullticolinearity
vif(model_27)

#remove carbodyhatchback because it has comparitevly high p value
model_28 <- lm(price ~ aspiration +  carwidth + 
                 enginesize +  peakrpm + 
                 Brandbmw + Brandbuick +  
                 Brandmitsubishi + 
                 Brandporsche + 
                 enginetyperotor , data= cars_assignment)
summary(model_28)

#check for mullticolinearity
vif(model_28)

#remove Brandmitsubishi because it has comparitevly high p value
model_29 <- lm(price ~ aspiration +  carwidth + 
                 enginesize +  peakrpm + 
                 Brandbmw + Brandbuick +  
                 Brandporsche + 
                 enginetyperotor , data= cars_assignment)
summary(model_29)

#check for mullticolinearity
vif(model_29)

#remove carwidth because it has comparitevly high p value
model_30 <- lm(price ~ aspiration +   
                 enginesize +  peakrpm + 
                 Brandbmw + Brandbuick +  
                 Brandporsche + 
                 enginetyperotor , data= cars_assignment)
summary(model_30)

#check for mullticolinearity
vif(model_30)

# predicting the results in test dataset
Predict_1 <- predict(model_30,test)
test$test_price <- Predict_1

# Now, we need to test the r square between actual and predicted price. 
r <- cor(test$price,test$test_price)
rsquared <- cor(test$price,test$test_price)^2

#as the rsquared between the actual and predict price is nearly same as the r square of the model hence we can say that model is stable
#so the significant attribute which actually determine the price of the car in USA are aspiration , engine size , peak rpm , brand value of the car, if engine type is rotor or not
#hence the company which is trying to enter the US market should do pricing according to these significant attributes