if (Sys.info()["nodename"] == "ALPHA-GJNZ322") path = "C:/Users/Yuqin/Google Drive/Sharing/house_price"
setwd(path)
library(dplyr)

train <- read.csv("train.csv")
test <- read.csv("test.csv")

train$flag <- "train"
test$SalePrice <- NA
test$flag <- "test"
all_data <- rbind(train, test)

#### investigation on each covariate ###
all_data$MSSubClass <- as.factor(all_data$MSSubClass)
all_data %>% group_by(MSSubClass) %>% summarize(n = n(), price = median(SalePrice, na.rm = T))
all_data$MSSubClass[all_data$MSSubClass == "150"] <- NA # to impute this value

all_data %>% group_by(MSZoning) %>% summarize(n = n(), price = median(SalePrice, na.rm = T))

summary(all_data$LotFrontage)
cor(all_data$LotFrontage, all_data$SalePrice, use = "complete.obs", method = "spearman")

summary(all_data$LotArea)
cor(all_data$LotArea, all_data$SalePrice, use = "complete.obs", method = "spearman")

table(all_data$Street)
table(all_data$Alley)
all_data$Alley <- as.character(all_data$Alley)
all_data$Alley[is.na(all_data$Alley)] <- "Not Avaliable"
all_data$Alley <- as.factor(all_data$Alley)

summary(all_data$LotShape)
summary(all_data$LandContour)
summary(all_data$Utilities) # not use

summary(all_data$LotConfig)
summary(all_data$LandSlope)

summary(all_data$Neighborhood)

summary(all_data$Condition1)
summary(all_data$Condition2)
# merge to condition indicators
# not to use norm
for (cond in levels(all_data$Condition1)) {
  all_data[[cond]] <- as.factor(all_data$Condition1 == cond | all_data$Condition2 == cond)
}

summary(all_data$BldgType)
summary(all_data$HouseStyle)
summary(all_data$OverallQual)
summary(all_data$OverallCond)
summary(all_data$YearBuilt)
summary(all_data$YearRemodAdd)
summary(all_data$RoofStyle)
summary(all_data$RoofMatl)

summary(all_data$Exterior1st)
summary(all_data$Exterior2nd)
# merge to condition indicators
# not to use norm
for (exter in levels(all_data$Exterior2nd)) {
  all_data[[exter]] <- as.factor(all_data$Exterior1st == exter | all_data$Exterior2nd == exter)
  all_data[[exter]][is.na(all_data$Exterior2nd)] <- NA
}

summary(all_data$MasVnrType)
all_data %>% group_by(MasVnrType) %>% summarize(n = n(), price = mean(SalePrice, na.rm = T))
summary(all_data$MasVnrArea)
all_data %>% group_by(MasVnrType) %>% summarize(n = n(), area = mean(MasVnrArea, na.rm = T))
all_data$MasVnrArea[is.na(all_data[["MasVnrType"]])]
# impute NA's

summary(all_data$ExterQual)
summary(all_data$ExterCond)
summary(all_data$Foundation)

summary(all_data$BsmtQual) #use
summary(all_data$BsmtCond) #use
summary(all_data$BsmtExposure) #use
summary(all_data$BsmtFinType1)
summary(all_data$BsmtFinSF1)
summary(all_data$BsmtFinType2)
summary(all_data$BsmtFinSF2)
summary(all_data$BsmtUnfSF)
summary(all_data$TotalBsmtSF) #use

all_data$BsmtQual <- as.character(all_data$BsmtQual)
all_data$BsmtQual[is.na(all_data$BsmtQual)] <- "No Base"
all_data$BsmtQual <- as.factor(all_data$BsmtQual)

all_data$BsmtExposure <- as.character(all_data$BsmtExposure)
all_data$BsmtExposure[is.na(all_data$BsmtExposure)] <- "No Base"
all_data$BsmtExposure <- as.factor(all_data$BsmtExposure)

all_data$BsmtCond <- as.character(all_data$BsmtCond)
all_data$BsmtCond[is.na(all_data$BsmtCond)] <- "No Base"
all_data$BsmtCond <- as.factor(all_data$BsmtCond)

summary(all_data$TotalBsmtSF - all_data$BsmtUnfSF - all_data$BsmtFinSF1 - all_data$BsmtFinSF2)
table(all_data$BsmtFinType1, all_data$BsmtFinType2)

# add sqft for each type of base
for (base in levels(all_data$BsmtFinType1)) {
  all_data[[paste0("base", base)]] <- (all_data$BsmtFinType1 == base) * all_data$BsmtFinSF1 +
    (all_data$BsmtFinType2 == base) * all_data$BsmtFinSF2
  all_data[[paste0("base", base)]][is.na(all_data$TotalBsmtSF - all_data$BsmtUnfSF - all_data$BsmtFinSF1 - all_data$BsmtFinSF2)] <- NA
}

summary(all_data$Heating)
summary(all_data$HeatingQC)
summary(all_data$CentralAir)
summary(all_data$Electrical)

summary(all_data[["X1stFlrSF"]])
summary(all_data[["X2ndFlrSF"]])
all_data$HighQualFinSF <- all_data[["X1stFlrSF"]] + all_data[["X2ndFlrSF"]] # use

summary(all_data$LowQualFinSF) # use
summary(all_data$GrLivArea - all_data$LowQualFinSF - all_data[["X2ndFlrSF"]] - all_data[["X1stFlrSF"]]) # not use

summary(all_data$BsmtFullBath)
summary(all_data$BsmtHalfBath)
summary(all_data$FullBath)
summary(all_data$HalfBath) # combine?
all_data$BsmtFullBath <- as.factor(all_data$BsmtFullBath)
all_data$BsmtHalfBath <- as.factor(all_data$BsmtHalfBath)
all_data$FullBath <- as.factor(all_data$FullBath)
all_data$HalfBath <- as.factor(all_data$HalfBath)

all_data$BedroomAbvGr <- as.factor(all_data$BedroomAbvGr)
all_data$KitchenAbvGr <- as.factor(all_data$KitchenAbvGr)


summary(all_data$KitchenAbvGr)
summary(all_data$KitchenQual) #impute

summary(all_data$TotRmsAbvGrd)
all_data$TotRmsAbvGrd <- as.factor(all_data$TotRmsAbvGrd)
summary(all_data$Functional)

summary(all_data$Fireplaces)
all_data$Fireplaces <- as.factor(all_data$Fireplaces)
summary(all_data$FireplaceQu)

all_data$FireplaceQu <- as.character(all_data$FireplaceQu)
all_data$FireplaceQu[is.na(all_data$FireplaceQu)] <- "No Fire"
all_data$FireplaceQu <- as.factor(all_data$FireplaceQu)

summary(all_data$GarageType)
all_data$GarageType <- as.character(all_data$GarageType)
all_data$GarageType[is.na(all_data$GarageType)] <- "No Gar"
all_data$GarageType <- as.factor(all_data$GarageType)
summary(all_data$GarageFinish)
all_data$GarageFinish <- as.character(all_data$GarageFinish)
all_data$GarageFinish[is.na(all_data$GarageFinish)] <- "No Gar"
all_data$GarageFinish <- as.factor(all_data$GarageFinish)

summary(all_data$GarageYrBlt) # not use?
plot(all_data$GarageYrBlt, all_data$YearBuilt)
all_data$GarageYrBlt[is.na(all_data$GarageYrBlt)] <- all_data$YearBuilt[is.na(all_data$GarageYrBlt)]

summary(all_data$GarageCars)
all_data$GarageCars <- as.factor(all_data$GarageCars)
summary(all_data$GarageArea)
summary(all_data$GarageQual)
all_data$GarageQual <- as.character(all_data$GarageQual)
all_data$GarageQual[is.na(all_data$GarageQual)] <- "No Gar"
all_data$GarageQual <- as.factor(all_data$GarageQual)

summary(all_data$GarageCond)
all_data$GarageCond <- as.character(all_data$GarageCond)
all_data$GarageCond[is.na(all_data$GarageCond)] <- "No Gar"
all_data$GarageCond <- as.factor(all_data$GarageCond)

summary(all_data$PavedDrive)

summary(all_data$WoodDeckSF)
summary(all_data$OpenPorchSF)
summary(all_data$EnclosedPorch)
summary(all_data$X3SsnPorch)
summary(all_data$ScreenPorch)
#use this var
all_data$TotalPorch <- all_data$WoodDeckSF + all_data$OpenPorchSF + all_data$EnclosedPorch + all_data$X3SsnPorch + all_data$ScreenPorch

summary(all_data$PoolArea)
summary(all_data$PoolQC)
all_data$PoolQC <- as.character(all_data$PoolQC)
all_data$PoolQC[is.na(all_data$PoolQC)] <- "No Pool"
all_data$PoolQC <- as.factor(all_data$PoolQC)

summary(all_data$Fence)
all_data$Fence <- as.character(all_data$Fence)
all_data$Fence[is.na(all_data$Fence)] <- "No Fence"
all_data$Fence <- as.factor(all_data$Fence)

summary(all_data$MiscFeature) # not use but add back the price
all_data$MiscFeature <- as.character(all_data$MiscFeature)
all_data$MiscFeature[is.na(all_data$MiscFeature)] <- "No Misc"
all_data$MiscFeature <- as.factor(all_data$MiscFeature)

summary(all_data$MiscVal) # not use but add back the price

summary(all_data$MoSold)
all_data$MoSold <- as.factor(all_data$MoSold)
summary(all_data$YrSold)
all_data$YrSold <- as.factor(all_data$YrSold)
summary(all_data$SaleType)
summary(all_data$SaleCondition)

predictors <- c(
  "MSSubClass", "MSZoning", 
  "LotFrontage", "LotArea", 
  "Street", "Alley", 
  "LotShape", "LandContour", "LotConfig", "LandSlope",
  "Neighborhood", 
  setdiff(levels(all_data$Condition1), "Norm"),
  "BldgType", "HouseStyle", "OverallQual", "OverallCond", "YearBuilt", "YearRemodAdd", 
  "RoofStyle", "RoofMatl", 
  levels(all_data$Exterior2nd), 
  "MasVnrType", "MasVnrArea",
  "ExterQual", "ExterCond", "Foundation",
  "BsmtQual", "BsmtCond", "BsmtExposure", "TotalBsmtSF",
  paste0("base", levels(all_data$BsmtFinType1)),
  "Heating", "HeatingQC", "CentralAir", "Electrical", 
  "HighQualFinSF", "LowQualFinSF", "GrLivArea",
  "BsmtFullBath", "BsmtHalfBath", "FullBath", "HalfBath", 
  "BedroomAbvGr", "KitchenAbvGr", "KitchenQual", "TotRmsAbvGrd", 
  "Functional",
  "Fireplaces", "FireplaceQu",
  "GarageType", "GarageFinish", "GarageYrBlt", "GarageQual", "GarageCond", "GarageCars", "GarageArea", "PavedDrive", 
  "TotalPorch",
  "PoolArea", "PoolQC",
  "Fence",
  "MoSold", "YrSold", "SaleType", "SaleCondition")

# impute data
all_cov <- rfunsuper(all_data[, predictors], iter = 5, ntree = 100)
summary(all_data[, predictors])
summary(all_cov)

for(var in names(all_cov)) {
  if (is.factor(all_cov[[var]])) print(table(all_cov[, var], all_data[, var], useNA = "ifany"))
}

all_data$FinalPrice <- all_data$SalePrice - all_data$MiscVal

all_cov$flag <- all_data$flag
all_cov$Id <- all_data$Id
all_cov$FinalPrice <- all_data$FinalPrice

my_train <- all_cov %>% filter(flag == "train") %>% sample_frac(0.8)
my_valid <- all_cov %>% filter(flag == "train", !(Id %in% my_train$Id))

### start to try many different models and evaluate error
rf <- 
  randomForest(
    x=my_train[, predictors], 
    y=my_train$FinalPrice, 
    xtest = my_valid[, predictors],
    ytest = my_valid$FinalPrice,
    ntree=500,
    importance = TRUE,
    keep.forest = TRUE)
error <- plot(rf)
plot(100:500, error[100:500], type = "l")
importance(rf)
varImpPlot(rf)

### now fit residual again
res <- my_train$FinalPrice - predict(rf)
res.test <- my_valid$FinalPrice - predict(rf, newdata = my_valid)
rf2 <- 
  randomForest(
    x=my_train[, predictors], 
    y=res,
    xtest=my_valid[, predictors],
    ytest=res.test,
    ntree=500,
    importance=TRUE,
    keep.forest = TRUE)
error2 <- plot(rf2)
plot(100:500, error2[100:500], type = "l")
importance(rf2)
varImpPlot(rf2)

pred <- 
  predict(rf, all_cov[all_cov$flag == "test", predictors]) + 
  predict(rf2, all_cov[all_cov$flag == "test", predictors]) + 
  all_data[all_data$flag == "test", "MiscVal"]
write.csv(pred, "second_prediction.csv")

library("party")
x <- ctree(FinalPrice ~ ., data=my_train[, c(predictors, "FinalPrice")])
plot(x, type="simple")

rfunsuper <- function (x, iter=5, ntree=100){
  # three different functions 
  #   Return k-neighbor weighted mean for numeric variables or
  #   most weighted frequent factor element for factor variables.
  KWmean <- function (value, weight, k=10){
    if (missing(weight)){
      w <- rep.int(1, length(value))
    }else if (length(weight) != length(value)){
      stop("'value' and 'weight' must have the same length")
    }
    k <- min(k, length(value))
    if (is.numeric(value)){
      order.weight <- order(weight, decreasing = T)
      ww <- weight[order.weight]
      vv <- value[order.weight]
      ret <- sum(ww[1:k] * vv[1:k]) / sum(ww[1:k])
      
    }else if(is.factor(value)){ 
      wgt.sum <- tapply(weight, value, sum)
      # most weighted frequent factor element
      ret <- names(subset (wgt.sum, wgt.sum == max(wgt.sum, na.rm=T)))
    }else{
      stop("'value' is neither numeric nor factor")
    }
    return(ret)
  }
  #   Return relative distance between `x.impute' to `x.org' 
  relatImpOrg <- function (x.impute, x.org){    
    #   x.impute: imputed data
    #   x.org: original data
    ncol.x <- length(x.org)
    x.abs.org <-  matrix(abs(as.numeric(unlist(x.org))), ncol=ncol.x)
    max.x <- apply(x.abs.org, 2, max) # for normalization of features size
    # `x.impute' and `x.org' may include factor elements
    if (FALSE){ # available for only numeric
      diff.x <- (x.impute - x.org) / max.x # normalize
      diff.rel <- sum(diff.x^2) / sum((x.org / max.x)^2) 
    }else{
      mat.x.impute <- matrix(as.numeric(unlist(x.impute)), ncol=ncol.x)
      mat.x.org <- matrix(as.numeric(unlist(x.org)), ncol=ncol.x)
      max.numx <- as.numeric(unlist(max.x))
      diff.x <- sweep((mat.x.impute - mat.x.org), 2, max.numx, FUN="/") 
      size.org <- sweep(mat.x.org, 2, max.numx, FUN="/")
      diff.rel <- sum(diff.x^2) / sum(size.org^2)
    }
    cat ("diff.rel =", sum(diff.x^2), "/", sum(size.org^2), "=", diff.rel, "\n")
    return(diff.rel)
  }
  #   Impute or revise NA elements using the data proximity.
  prox.nafix <- function (na.values, rough.values, x.prox){
    #   na.values: data vector that includes NA; unchanged.
    #   rough.values: rough data vector to be replaced; NAs cannot include.
    #   x.prox: data proximity matrix; each element is positive and <= 1.
    if (length(na.values) != length(rough.values)){
      stop("'na.values' and 'rough.values' must have the same length");
    }else if (length(rough.values) != ncol(x.prox)){
      stop("'rough.values' and 'x.prox' size incorrect");
    }
    # NA imputation ONLY for NA data
    na.list <- which(is.na(na.values))
    if (length(na.list) == 0){
      # no NAs
      return(rough.values)
    }
    replaced.vales <- rough.values
    for (i in 1:length(na.list)){
      j <- na.list[i]
      x.prox[j,j] <- 0 # ignore the weight of the data to be imputed.
      replaced.vales[j] <- KWmean (rough.values, x.prox[,j])
    }
    return(replaced.vales)
  }
  require(randomForest)
  x.roughfixed <- na.roughfix(x)
  #For numeric variables, NAs are replaced with column medians. 
  #For factor variables, NAs are replaced with the most frequent levels (breaking ties at random).
  rf.impute <- x
  while (iter){
    x.rf <- randomForest(x.roughfixed, ntree=ntree)
    #randomForest implements Breiman's random forest algorithm
    x.prox <- x.rf$proximity
    #a matrix of proximity measures among the input 
    #(based on the frequency that pairs of data points are in the same terminal nodes).
    for (i in 1:ncol(x)){
      rf.impute[,i] <- prox.nafix(x[,i], x.roughfixed[,i], x.prox)
    }
    diff.rel <- relatImpOrg(rf.impute, x.roughfixed)
    if (diff.rel < 1e-5){
      break
    }else{
      x.roughfixed <- rf.impute
      iter <- iter -1
    }
  }
  print(x.rf)
  return(rf.impute)
}