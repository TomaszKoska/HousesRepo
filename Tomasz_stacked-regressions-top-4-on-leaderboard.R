library("devtools")
install_github("TomaszKoska/RUtils")
library(RUtils)


library(e1071)




impCustomPreparation <- function(fo){
  
  
  
  # fo <- buildForecastingObject("C:\\Users\\Tomek\\Desktop\\kaggle\\houses\\raw\\train.csv",
  #                              "C:\\Users\\Tomek\\Desktop\\kaggle\\houses\\raw\\test.csv","SalePrice","")
  
  #Deleting outliers
  
  fo$train$SalePrice <- log(fo$train$SalePrice)
  fo$trainFull$SalePrice <- log(fo$trainFull$SalePrice)
  fo$forecast$SalePrice <- log(fo$forecast$SalePrice)
  
  
  df <- rbind(fo$trainFull,fo$forecast)
  inTrain <- rep(FALSE,nrow(df))
  inTrain[1:1460] <- T
  
  
  df$MSSubClass <- as.factor(df$MSSubClass)
  levels(df$MSSubClass) <- paste(rep("class",nrow(df)),levels(df$MSSubClass),sep="")
  # df$OverallQual <- as.factor(df$OverallQual)
  df$OverallCond <- as.factor(df$OverallCond)
  # df$BsmtFullBath <- as.factor(df$BsmtFullBath)
  # df$BsmtHalfBath <- as.factor(df$BsmtHalfBath)
  # df$FullBath <- as.factor(df$FullBath)
  # df$HalfBath <- as.factor(df$HalfBath)
  # df$BedroomAbvGr <- as.factor(df$BedroomAbvGr)
  # df$KitchenAbvGr <- as.factor(df$KitchenAbvGr)
  # df$TotRmsAbvGrd <- as.factor(df$TotRmsAbvGrd)
  # df$Fireplaces <- as.factor(df$Fireplaces)
  # df$GarageCars <- as.factor(df$GarageCars)
  df$MoSold <-as.factor(df$MoSold)
  df$MoSold <-as.factor(df$YrSold)
  levels(df$MoSold) <- paste(rep("month",nrow(df)),levels(df$MoSold),sep="")
  levels(df$MoSold) <- paste(rep("year",nrow(df)),levels(df$YrSold),sep="")
  
  
  varName= "PoolQC"
  df[,varName] <- as.character(df[,varName])
  df[is.na(df[,varName]),varName] <- "NoPool"
  df[,varName] <- as.factor(df[,varName])
  
  varName= "MiscFeature"
  #df[is.na(df$MiscFeature) & df$MiscVal !=0,]
  df[2550,varName] <- "Gar2"
  
  df[,varName] <- as.character(df[,varName])
  df[is.na(df[,varName]),varName] <- "Missing"
  df[,varName] <- as.factor(df[,varName])
  addSome <- toDummies(df,"MiscFeature")
  addSome$MiscFeature_Missing <- NULL
  df$MiscFeature <- NULL
  df <- cbind(df,addSome)
  
  
  df$Alley <- as.character(df$Alley)
  df$Alley[is.na(df$Alley)] <- "NoAlley"
  df$Alley <- as.factor(df$Alley)
  
  
  varName= "Fence"
  df[,varName] <- as.character(df[,varName])
  df[is.na(df[,varName]),varName] <- "NoFence"
  df[,varName] <- as.factor(df[,varName])
  
  
  
  varName <- "FireplaceQu"
  
  df$FireplaceQu <- as.character(df$FireplaceQu)
  df$FireplaceQu[is.na(df$FireplaceQu)] <- "NoFireplace"
  df$FireplaceQu <- as.factor(df$FireplaceQu)
  
  
  #Lootfrontage by area
  # all_data["LotFrontage"] = all_data.groupby("Neighborhood")["LotFrontage"].transform(lambda x: x.fillna(x.median()))
  
  b<-by(df$LotFrontage,df$Neighborhood,FUN = median, na.rm = T)
  v<-data.frame(Neighborhood = names(b), LotFrontage = as.vector(b))
  
  r = merge(df, v, by="Neighborhood", suffixes=c(".real", ".fake"))
  na.idx = which(is.na(df$LotFrontage))
  df[na.idx,"LotFrontage"] = r[na.idx,"LotFrontage.fake"]
  
  
  
  
  df$GarageType <- as.character(df$GarageType)
  df$GarageType[is.na(df$GarageType)] <- "NoGarage"
  df$GarageType <- as.factor(df$GarageType)
  
  df$GarageCond <- as.character(df$GarageCond)
  df$GarageCond[is.na(df$GarageCond)] <- "NoGarage"
  df$GarageCond <- as.factor(df$GarageCond)
  
  
  varName <- "GarageFinish"
  df[,varName] <- as.character(df[,varName])
  df[df$GarageType == "NoGarage",varName] <- "NoGarage"
  df[,varName] <- as.factor(df[,varName])
  df$GarageFinish[df$Id == 2127] <- "Unf"
  df$GarageFinish[df$Id == 2577] <- "Unf"
  
  
  
  df$GarageCond[df$Id == 2127] <- "TA"
  df$GarageCond[df$Id == 2577] <- "TA"
  
  
  varName= "GarageCars"
  df[df$GarageType == "NoGarage", varName ] <- 0
  df[df$Id == 2577,varName] <- 1
  df[df[,varName]=="5",varName] <- "4"
  # df[,varName] <- droplevels(df[,varName])
  table(df[inTrain,varName])
  
  varName= "GarageQual"
  df[,varName] <- as.character(df[,varName])
  df[df$GarageType == "NoGarage",varName] <- "NoGarage"
  df[,varName] <- as.factor(df[,varName])
  df$GarageQual[df$Id == 2127] <- "TA"
  df$GarageQual[df$Id == 2577] <- "TA"
  
  varName= "GarageArea"
  df[df$GarageType == "NoGarage", varName ] <- 0
  df[df$Id == 2577,varName] <- 372
  
  varName <- "GarageYrBlt"
  df[is.na(df[,varName]), varName] <- 0
  
  
  varName <- "BsmtFinSF1"
  df[is.na(df[,varName]), varName] <- 0
  
  varName <- "BsmtFinSF2"
  df[is.na(df[,varName]), varName] <- 0
  
  varName <- "BsmtUnfSF"
  df[is.na(df[,varName]), varName] <- 0
  
  
  varName <- "TotalBsmtSF"
  df[is.na(df[,varName]), varName] <- 0
  
  
  varName <- "BsmtFullBath"
  df[is.na(df[,varName]), varName] <- 0
  
  varName <- "BsmtHalfBath"
  df[is.na(df[,varName]), varName] <- 0
  
  
  df$BsmtQual <- as.character(df$BsmtQual)
  df$BsmtQual[is.na(df$BsmtQual)] <- "NoBasement"
  df$BsmtQual <- as.factor(df$BsmtQual)
  
  df$BsmtCond <- as.character(df$BsmtCond)
  df$BsmtCond[is.na(df$BsmtCond)] <- "NoBasement"
  df$BsmtCond <- as.factor(df$BsmtCond)
  
  df$BsmtExposure <- as.character(df$BsmtExposure)
  df$BsmtExposure[is.na(df$BsmtExposure)] <- "NoBasement"
  df$BsmtExposure <- as.factor(df$BsmtExposure)
  
  df$BsmtFinType1 <- as.character(df$BsmtFinType1)
  df$BsmtFinType1[is.na(df$BsmtFinType1)] <- "NoBasement"
  df$BsmtFinType1 <- as.factor(df$BsmtFinType1)
  
  
  df$BsmtFinType2 <- as.character(df$BsmtFinType2)
  df$BsmtFinType2[is.na(df$BsmtFinType2)] <- "NoBasement"
  df$BsmtFinType2 <- as.factor(df$BsmtFinType2)
  
  
  
  
  varName <- "MasVnrType"
  
  df[,varName] <- as.character(df[,varName])
  df[is.na(df[,varName]),varName] <- "None"
  df[,varName] <- as.factor(df[,varName])
  
  varName <- "MasVnrArea"
  df[is.na(df[,varName]),varName] <- 0
  
  
  
  
  varName <- "MSZoning" # wrucam na sztywno z lasu losowego
  df[is.na(df[,varName]), ]
  df[1916,varName] <- "RL"
  df[2217,varName] <- "RL"
  df[2251,varName] <- "RL"
  df[2905,varName] <- "RL"
  
  df$Utilities <- NULL
  
  
  
  varName <- "KitchenQual"
  df[is.na(df[,varName]), varName] <- "TA"
  
  varName <- "Functional"
  df[is.na(df[,varName]), varName] <- "Typ"
  
  
  varName <- "Electrical"
  df[is.na(df[,varName]), varName] <- "SBrkr"
  
  
  varName <- "KitchenQual"
  df[is.na(df[,varName]), varName] <- "TA"
  
  
  
  varName= "SaleType"
  df$SaleType[df$Id == 2490] <- "WD"
  
  
  
  varName <- "MSSubClass"
  df$MSSubClass[df$MSSubClass=="class150"] <- "class120"
  df$MSSubClass <- droplevels(df$MSSubClass)
  
  
  varName <- "Exterior1st"
  df[is.na(df[,varName]),varName] <- "VinylSd"
  varName <- "Exterior2nd"
  df[is.na(df[,varName]),varName] <- "VinylSd"
  
  
  df$TotalSF <- df$TotalBsmtSF + df$X1stFlrSF + df$X2ndFlrSF
  
  

  fo$train <- df[inTrain,]
  fo$trainFull <- df[inTrain,]
  fo$forecast <- df[!inTrain,]
  print(dim(df[!inTrain,]))
  fo$description <- append(fo$description,"And then we made custom changes!")
  
  
  fo$train <- fo$trainFull[fo$trainFull$GrLivArea<4000 & fo$trainFull$SalePrice<300000,]
  
  
  fo
  
}


fo <- buildForecastingObject("C:\\Users\\Tomek\\Desktop\\kaggle\\houses\\raw\\train.csv",
                             "C:\\Users\\Tomek\\Desktop\\kaggle\\houses\\raw\\test.csv","SalePrice","")



diagnose(fo,minObs =5)


fo <- impCustomPreparation(fo)
diagnose(fo,minObs =5)


toBoxCox<-as.character(whichAreSkewed(fo$train)$name)


fo <-traBoxCox(fo,lambda = 0.15, choosenVariables = toBoxCox)

diagnose(fo,minObs =5)



# fo<-redRemoveSelected(fo,namesToRemove = c("PoolQC","Fence","MiscFeature"))
# fo <- impCaretDefault(fo=fo,forbiddenVariables = c("Id"),trainControl = trainControl(method="repeatedcv", number=2, repeats=1,verboseIter = T))

 # fo <- outNumericStdDev(fo,howManyDevsAway = 3,ignoredVariables = c("Id"),maxRemovedPercentage = 0.05)
 # diagnose(fo)

# fo <- traZbijacz(fo,minCount = 100)
# diagnose(fo)



# fo2<-traNumericAutoTransformer(fo,forbiddenVariables = c("Id"),functionsToTest = c("log","square"))


fo <- traNormalizeNumerics(fo,c("Id"))
diagnose(fo)

# 
# fo<- redRemoveSelected(fo,c("LotFrontage"))
# diagnose(fo)

fo <- redRemoveZeroVar(fo)
diagnose(fo)




library(caret)
#ridge
modelName <- "ridge"
info <- getModelInfo(modelName)

train_control <- trainControl(method="repeatedcv", number=10, repeats=1,verboseIter = TRUE)
grid <- expand.grid(lambda=seq(0.2,0.5,0.1))
model <-  train(SalePrice ~ ., data = fo$train, method = modelName,trControl=train_control,tuneGrid=grid)
# model.merged <-  train(SalePrice ~ ., data = df.merged,method = modelName,trControl=train_control)


model$bestTune
result <- predict(model, newdata = fo$forecast)
fullPredict(fo,model)



