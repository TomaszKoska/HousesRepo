library("devtools")
install_github("TomaszKoska/RUtils")
library(RUtils)


impCustomPreparation <- function(fo){
  
  
  df <- rbind(fo$trainFull,fo$forecast)
  inTrain <- rep(FALSE,nrow(df))
  inTrain[1:1460] <- T
  print(table(inTrain))
  
  df$MSSubClass <- as.factor(df$MSSubClass)
  levels(df$MSSubClass) <- paste(rep("class",nrow(df)),levels(df$MSSubClass),sep="")
  df$OverallQual <- as.factor(df$OverallQual)
  df$OverallCond <- as.factor(df$OverallCond)
  df$BsmtFullBath <- as.factor(df$BsmtFullBath)
  df$BsmtHalfBath <- as.factor(df$BsmtHalfBath)
  df$FullBath <- as.factor(df$FullBath)
  df$HalfBath <- as.factor(df$HalfBath)
  df$BedroomAbvGr <- as.factor(df$BedroomAbvGr)
  df$KitchenAbvGr <- as.factor(df$KitchenAbvGr)
  df$TotRmsAbvGrd <- as.factor(df$TotRmsAbvGrd)
  df$Fireplaces <- as.factor(df$Fireplaces)
  df$GarageCars <- as.factor(df$GarageCars)
  df$MoSold <-as.factor(df$MoSold)
  levels(df$MoSold) <- paste(rep("month",nrow(df)),levels(df$MoSold),sep="")
  ######################################sensowna impuacja danych
  varName <- "MSSubClass"
  df$MSSubClass[df$MSSubClass=="class150"] <- "class120"
  df$MSSubClass <- droplevels(df$MSSubClass)
  table(df$MSSubClass)
  
  varName <- "TotRmsAbvGrd"
  df[df[,varName]=="13",varName] <- "14"
  df[df[,varName]=="15",varName] <- "14"
  df[,varName] <- droplevels(df[,varName])
  table(df[inTrain,varName])
  
  varName <- "Fireplaces"
  df[df[,varName]=="4",varName] <- "3"
  df[,varName] <- droplevels(df[,varName])
  table(df[inTrain,varName])
  
  
  
  
  varName <- "MSZoning" # wrucam na sztywno z lasu losowego
  df[is.na(df[,varName]), ]
  df[1916,varName] <- "RL"
  df[2217,varName] <- "RL"
  df[2251,varName] <- "RM"
  df[2905,varName] <- "RL"
  
  #LotFrontage - zostawiłem dla lasu
  forRf <- c("LotFrontage")
  
  df$Alley <- as.character(df$Alley)
  df$Alley[is.na(df$Alley)] <- "NoAlley"
  df$Alley <- as.factor(df$Alley)
  
  
  varName <- "Utilities" #ta zmienna jest totalnie do wywalenia
  df[,varName] <- NULL
  
  
  df <- cbind(df, toDummiesForTwo(df, "Exterior2nd", "Exterior1st"))
  df$Exterior1st <- NULL
  df$Exterior2nd <- NULL
  
  df <- cbind(df, toDummiesForTwo(df, "Condition1", "Condition2"))
  df$Condition1 <- NULL
  df$Condition2 <- NULL
  
  varName <- "MasVnrType"
  
  df[,varName] <- as.character(df[,varName])
  df[is.na(df[,varName]),varName] <- "None"
  df[,varName] <- as.factor(df[,varName])
  
  varName <- "MasVnrArea"
  df[is.na(df[,varName]),varName] <- 0
  
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
  
  
  varName <- "BsmtFinSF1"
  df[is.na(df[,varName]), varName] <- 0
  
  varName <- "BsmtFinSF2"
  df[is.na(df[,varName]), varName] <- 0
  
  varName <- "BsmtUnfSF"
  df[is.na(df[,varName]), varName] <- 0
  
  varName <- "Electrical"
  df[is.na(df[,varName]), varName] <- "SBrkr"
  
  varName <- "TotalBsmtSF"
  df[is.na(df[,varName]), varName] <- 0
  
  
  varName <- "BsmtFullBath"
  df[is.na(df[,varName]), varName] <- 0
  
  varName <- "BsmtHalfBath"
  df[is.na(df[,varName]), varName] <- 0
  
  varName <- "KitchenQual"
  df[is.na(df[,varName]), varName] <- "TA"
  
  varName <- "Functional"
  df[is.na(df[,varName]), varName] <- "Typ"
  
  
  varName <- "FireplaceQu"
  
  df$FireplaceQu <- as.character(df$FireplaceQu)
  df$FireplaceQu[is.na(df$FireplaceQu)] <- "NoFireplace"
  df$FireplaceQu <- as.factor(df$FireplaceQu)
  #forRf <- append(forRf,varName)
  
  df$FullBath <- as.character(df$FullBath)
  df$FullBath[df$FullBath==4] <- 3
  df$FullBath <- as.factor(df$FullBath)
  
  
  
  df$GarageType <- as.character(df$GarageType)
  df$GarageType[is.na(df$GarageType)] <- "NoGarage"
  df$GarageType <- as.factor(df$GarageType)
  
  df$GarageCond <- as.character(df$GarageCond)
  df$GarageCond[is.na(df$GarageCond)] <- "NoGarage"
  df$GarageCond <- as.factor(df$GarageCond)
  
  
  varName <- "garageAge"
  df$GarageYrBlt[df$GarageYrBlt == 2207] <- 2007
  df$garageAge <- df$YrSold - df$GarageYrBlt
  df$garageAge[df$garageAge<0] <- 0
  
  
  a<- cut_width(df$garageAge,10)
  levels(a) <- c("5","15","25","35","45","55","65","75","85","95","105","115")
  df[,varName] <- as.character(a)
  df[a=="105" | a=="115" | is.na(a) ,varName] <- "other"
  df[,varName] <- as.factor(df[,varName])
  
  
  varName <- "GarageYrBlt"
  finVar <- as.numeric(as.character(df[,varName]))
  finVar[(df[,varName])<=1958]<- 1958
  finVar[(df[,varName])>1958 & (df[,varName])<=1964]<- 1964
  finVar[(df[,varName])>1964 & (df[,varName])<=1987]<- 1987
  finVar[(df[,varName])>1987 & (df[,varName])<=1995]<- 1995
  finVar[(df[,varName])>1995 & (df[,varName])<=2000]<- 2000
  finVar[(df[,varName])>2000 & (df[,varName])<=2003]<- 2003
  finVar[(df[,varName])>2003 & (df[,varName])<=2005]<- 2005
  finVar[(df[,varName])>2005 & (df[,varName])<=2006]<- 2006
  finVar[(df[,varName])>2006 & (df[,varName])<=2007]<- 2007
  finVar[(df[,varName])>2007 & (df[,varName])<=2008]<- 2008
  finVar[(df[,varName])>2008 & (df[,varName])<=2010]<- 2010
  finVar[is.na(df[,varName])]<- "unknown"
  finVar <- as.factor(finVar)
  df$GarageYrBlt <- finVar
  
  
  
  
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
  df[,varName] <- droplevels(df[,varName])
  table(df[inTrain,varName])
  
  
  
  varName= "GarageArea"
  df[df$GarageType == "NoGarage", varName ] <- 0
  df[df$Id == 2577,varName] <- 372
  
  varName= "GarageQual"
  df[,varName] <- as.character(df[,varName])
  df[df$GarageType == "NoGarage",varName] <- "NoGarage"
  df[,varName] <- as.factor(df[,varName])
  df$GarageQual[df$Id == 2127] <- "TA"
  df$GarageQual[df$Id == 2577] <- "TA"
  
  
  varName= "PoolQC"
  df[,varName] <- as.character(df[,varName])
  df[is.na(df[,varName]),varName] <- "NoPool"
  df[,varName] <- as.factor(df[,varName])
  
  varName= "Fence"
  df[,varName] <- as.character(df[,varName])
  df[is.na(df[,varName]),varName] <- "NoFence"
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
  
  
  varName= "SaleType"
  df$SaleType[df$Id == 2490] <- "WD"
  
  varName= "age"
  df$age <- df$YrSold - df$YearBuilt
  df$age[df$age<0] <- 0
  
  varName= "sinceLastRemod"
  df$sinceLastRemod <- df$YrSold - df$YearRemodAdd
  df$sinceLastRemod[df$sinceLastRemod<0] <- 0
  
  
  
  fo$train <- df[inTrain,]
  fo$trainFull <- df[inTrain,]
  fo$forecast <- df[!inTrain,]
  print(dim(df[!inTrain,]))
  fo$description <- append(fo$description,"And then we made custom changes!")
  
  fo
  
}


fo <- buildForecastingObject("C:\\Users\\Tomek\\Desktop\\kaggle\\houses\\raw\\train.csv",
                             "C:\\Users\\Tomek\\Desktop\\kaggle\\houses\\raw\\test.csv","SalePrice","")

head(fo$train)

diagnose(fo)


fo <- impCustomPreparation(fo)
diagnose(fo)


# fo<-redRemoveSelected(fo,namesToRemove = c("PoolQC","Fence","MiscFeature"))
# fo <- impCaretDefault(fo=fo,forbiddenVariables = c("Id"),trainControl = trainControl(method="repeatedcv", number=2, repeats=1,verboseIter = T))

# fo <- outNumericStdDev(fo,2,ignoredVariables = c("Id"),maxRemovedPercentage = 0.05)
# diagnose(fo)

fo <- traZbijacz(fo,50)
diagnose(fo)


fo <- traNormalizeNumerics(fo,c("Id"))
diagnose(fo)


fo<- redRemoveSelected(fo,c("LotFrontage"))
diagnose(fo)

fo2<-traNumericAutoTransformer(fo,forbiddenVariables = c("Id"),functionsToTest = c("log","square"))
