# Load Required Libraries

library(mlr)
library(caret)
library(dplyr)
library(Metrics)
library(openxlsx)
library(gtools)
library(knitr)
library(car)
library(magrittr)
library(scales)
library(randomForest)
library(stringi)
library(stringr)
library(gdata)
library(e1071)
library(rpivotTable)



## Asssigning Base File, Input File and Output File Location

baseFileLocation = "D:\\USPA - Customer Centric Assortment\\Model Training\\2. RoS Prediction Model\\"
regionFileLocation = paste0(baseFileLocation,"2. Overall Nation Model\\")
inputLocation = paste0(regionFileLocation, "Input\\")
outputLocation = paste0(regionFileLocation, "Output\\")
predictionLocation = paste0(outputLocation, "Predictions\\")
modelRDSLocation = paste0(outputLocation, "RDS Files\\")



# Importing Input Data (Raw Data)

USPARawData <- read.csv(paste0(baseFileLocation,"0. Model Data\\","Model Training Data.csv"))
USPARawData <- USPARawData %>% mutate_if(is.factor, as.character)



## Keep Style_Code for "US" and "UD" sub_brand & "JN", "SH","TR","TS" material Type

USPARawData<- USPARawData %>% filter(Sub_Brand %in% c("UD", "US") & Material_Type %in% c("JN", "SH","TR","TS"))


## Keep Style_Code with Trading_Days > 0

USPARawData <- USPARawData %>% filter(Trading_Days > 0)



## Brand Presence Calculation


## "Brand_option" and "Total_option" coumn  Creation

Brand_option <- USPARawData %>% group_by(Sales_Season,Sub_Brand,Material_Type) %>% summarise(Brandoption=n_distinct(Style_Code))
Total_option <- USPARawData %>% group_by(Sales_Season) %>% summarise(Total_option=n_distinct(Style_Code))
Brand_option <- as.data.frame(Brand_option)
Total_option <- as.data.frame(Total_option)
USPARawData <- USPARawData %>% left_join(Brand_option, by = c("Sales_Season","Sub_Brand","Material_Type"))
USPARawData <- USPARawData %>% left_join(Total_option, by = c("Sales_Season"))
USPARawData <- USPARawData %>% mutate(Brand_presence = (Brandoption)/Total_option)



## Missing Color Style Code Mapping

Missing_color_mapping<- read.xlsx(paste0(inputLocation,'Missing Style Code Color_v1.xlsx'))
Missing_color_mapping <- Missing_color_mapping %>% mutate_if(is.factor, as.character)
USPARawData1 <- USPARawData %>% left_join(Missing_color_mapping, by = c("Style_Code"))
USPARawData2 <- USPARawData1 %>% mutate(Color=if_else(is.na(Color.y),Color.x,Color.y))
USPARawData2 <- USPARawData2[,!names(USPARawData2) %in% c('Color.x', 'Color.y')]


## Function to remove Leading Whitespace

trim.leading <- function (x)  sub("^\\s+", "", x)

## Function to remove Trailing Whitespace

trim.trailing <- function (x) sub("\\s+$", "", x)

## Function to remove Leading or Trailing Whitespace

trim1 <- function (x) gsub("^\\s+|\\s+$", "", x)

## Function to remove 'extra' spaces in between string

trim_between <- function (x) gsub("\\s+"," ", x)



## Applying function to remove leading and trailing space from entire column in data

removeSpaces <- names(USPARawData2)
removeSpaces <- removeSpaces[!removeSpaces %in% c("Trading_Days","Sum_Quantity", "MRP", "Days_Difference", 
                                                  "Brandoption", "Total_option", "Brand_presence")]
for(h in removeSpaces)
{
  USPARawData2[, h]<- trim.leading(USPARawData2[,h])
  USPARawData2[, h]<- trim.trailing(USPARawData2[,h])
  USPARawData2[, h]<- trim1(USPARawData2[,h])
  USPARawData2[, h]<- trim_between(USPARawData2[,h])
}


## Taking store which is open for more than 1 year and removing data points from SS19 and AW19 season

USPARawData2 <- USPARawData2 %>% filter(!(Sales_Season %in% c("SS19","AW19")))
USPARawData2 <- USPARawData2 %>% filter(LFL.TAG == "LFL YES")



## Imputing Null Data (Blank Value) in a row with "UNKNOWN" value

nullData <- sapply(USPARawData2, function(x)any(trimws(x) == ""))
nullCols <- names(nullData[which(array(nullData)==TRUE)])

for(cols in nullCols){
  USPARawData2[which(USPARawData2[,cols]==""),cols] <- "UNKNOWN"
}




## Fabric Mapping

Fabric_Mapping <- read.xlsx(paste0(inputLocation,'Fabric_Mapping.xlsx'))
USPARawData3 <- USPARawData2 %>% left_join(Fabric_Mapping, by = "Fabric")
USPARawData3 <- USPARawData3[,!names(USPARawData3) %in% c('Fabric')]




## Feature Engineering


## New Column Creation


## 1. "Lumionosity" Column Creation


RGB_combo <- read.csv(paste0(inputLocation,'RGB_Mapping_V5.csv'))
RGB_combo_df <- RGB_combo %>% dplyr::select(Color,R,G,B,Melange_Flag,ColorGroup)
RGB_combo_df <- RGB_combo_df %>% mutate(Lumionosity = 0.21*R + 0.71*G + 0.07*B) %>% dplyr::select(-R,-G,-B)
RGB_combo_df <- RGB_combo_df %>% mutate_if(is.factor, as.character)

ModelData1 <- USPARawData3 %>% left_join(RGB_combo_df, by = "Color")



# Calculation of HIT and ROS at Style Level


## 2. "DD_Median" Column Creation

ModelData1$Days_Difference <- as.numeric(ModelData1$Days_Difference)
DD <- ModelData1 %>% group_by(Style_Code) %>% summarise(DD_Median = median(Days_Difference))
DD <- as.data.frame(DD)
ModelData1 <-  left_join(ModelData1, DD, by = c("Style_Code"))



## 3. "sumQty" and "TradDays" coumn  Creation (By Taking total of "sum_quantity" and "Trading_days" by "style_code")

sumQtyData <- ModelData1 %>% group_by(Style_Code) %>% summarise(sumQty = sum(Sum_Quantity))
TradingData <- ModelData1 %>% group_by(Style_Code) %>% summarise(TradDays = sum(Trading_Days))
sumQtyData <- as.data.frame(sumQtyData)
TradingData <- as.data.frame(TradingData)



## Selecting required columns from ModelData1 and keeping unique data points

ModelData2 <- ModelData1 %>% select(c(Sales_Season,Sub_Brand,Material_Type,Style_Code,Category_Group,Sleeve,Fit,Rise,ColorGroup,Pattern,MRP,Collar,Gender,Weave,Wash,Melange_Flag,Lumionosity,DD_Median,Fabric_Category,Primary_Fabric,Secondary_Fabric,Third_Fabric,Brand_presence))
ModelData2 <- unique(ModelData2)
ModelData3 <- ModelData2
ModelData3 <- ModelData3 %>% left_join(sumQtyData, by = c("Style_Code"))
ModelData3 <- ModelData3 %>% left_join(TradingData, by = c("Style_Code"))



## 4. "RoS" Column Creation

ModelData4 <- ModelData3 %>% mutate(RoS = (sumQty*7)/TradDays)



## 5. Hit Calculation Column creation based on Days Differences

ModelData4 <- ModelData4 %>% mutate(Hit= if_else(DD_Median <=30, "Hit-1",if_else(DD_Median > 30 & DD_Median <= 60,"Hit-2","Hit-3")))



### 6. "OPTION_SEASON_TREND" and "OPTION_ANNUAL_TREND" Varibale Creation (Season and Annual Trend Recoding)

ModelData4$OPTION_SEASON_TREND = car::recode(ModelData4$Sales_Season, "'AW16'= 1;'AW17'= 2;'AW18'=3;'SS16'=1;'SS17'=2;'SS18'=3")
ModelData4$OPTION_ANNUAL_TREND = car::recode(ModelData4$Sales_Season, "'SS16'= 1;'AW16'= 2;'SS17'=3;'AW17'=4;'SS18'=5;'AW18'=6")
ModelData4 <- ModelData4 %>% select(c(Sales_Season,Sub_Brand,Material_Type,Style_Code,Category_Group,Sleeve,Fit,Rise,ColorGroup,Pattern,MRP,Collar,Gender,Weave,Wash,Melange_Flag,Lumionosity,DD_Median,Hit,OPTION_SEASON_TREND,OPTION_ANNUAL_TREND,RoS,Fabric_Category,Primary_Fabric,Secondary_Fabric,Third_Fabric,Brand_presence))


## Replacing missing value in "Melange_flag" and "Lumionosity" column with 0

ModelData4$Lumionosity[is.na(ModelData4$Lumionosity)]<- 0
ModelData4$Melange_Flag[is.na(ModelData4$Melange_Flag)]<- 0
ModelData4$ColorGroup[is.na(ModelData4$ColorGroup)] <- "UNKNOWN"
ModelData5<- ModelData4



## 7. "ROS_capped" Column Creation (ROS Capping)

ROS_cap = quantile(ModelData5$RoS,c(0.02,0.99))
perc_lower_cap_ROS = ROS_cap[1][[1]]
perc_upper_cap_ROS = ROS_cap[2][[1]]
ModelData5$ROS_capped <- replace(ModelData5$RoS, ModelData5$RoS <= perc_lower_cap_ROS, perc_lower_cap_ROS)
ModelData5$ROS_capped <- replace(ModelData5$ROS_capped, ModelData5$RoS >= perc_upper_cap_ROS, perc_upper_cap_ROS)



## 8. "SeasonType" Column Creation (with value of "SS" and "AW")

ModelData5<- ModelData5  %>% mutate(Season_type = if_else(grepl("AW",Sales_Season),"AW","SS"))



## 9. "Mean RoS" Column Creation (for Different Combination/Level)

mismatchAttributes <- function(CombData, season, subBrand, materialType){
MeanROSFiltered <- MeanROS %>% filter(Season_type == season) %>% filter(Sub_Brand == subBrand) %>% filter(Material_Type == materialType)
  
matchIndex <- any((MeanROSFiltered$Season_type == CombData$Season_type) & (MeanROSFiltered$Sub_Brand == CombData$Sub_Brand) 
                    & (MeanROSFiltered$Material_Type == CombData$Material_Type) & (MeanROSFiltered$MRP == CombData$MRP) 
                    & (MeanROSFiltered$ColorGroup == CombData$ColorGroup) & (MeanROSFiltered$Fabric_Category == CombData$Fabric_Category))
  
  
  if(!matchIndex){
matchIndexMRP <- any((MeanROSFiltered$Season_type == CombData$Season_type) & (MeanROSFiltered$Sub_Brand == CombData$Sub_Brand) 
                         & (MeanROSFiltered$Material_Type == CombData$Material_Type) & (MeanROSFiltered$ColorGroup == CombData$ColorGroup) 
                         & (MeanROSFiltered$Fabric_Category == CombData$Fabric_Category))
    
matchIndexFabric <- any((MeanROSFiltered$Season_type == CombData$Season_type) & (MeanROSFiltered$Sub_Brand == CombData$Sub_Brand) 
                            & (MeanROSFiltered$Material_Type == CombData$Material_Type) & (MeanROSFiltered$MRP == CombData$MRP) 
                            & (MeanROSFiltered$ColorGroup == CombData$ColorGroup))
    
matchIndexColor <- any((MeanROSFiltered$Season_type == CombData$Season_type) & (MeanROSFiltered$Sub_Brand == CombData$Sub_Brand) 
                           & (MeanROSFiltered$Material_Type == CombData$Material_Type) & (MeanROSFiltered$MRP == CombData$MRP) 
                           & (MeanROSFiltered$Fabric_Category == CombData$Fabric_Category))
    
matchIndexMRPFabric <- any((MeanROSFiltered$Season_type == CombData$Season_type) & (MeanROSFiltered$Sub_Brand == CombData$Sub_Brand) 
                               & (MeanROSFiltered$Material_Type == CombData$Material_Type) & (MeanROSFiltered$ColorGroup == CombData$ColorGroup))
    
matchIndexMRPColor <- any((MeanROSFiltered$Season_type == CombData$Season_type) & (MeanROSFiltered$Sub_Brand == CombData$Sub_Brand) 
                              & (MeanROSFiltered$Material_Type == CombData$Material_Type) & (MeanROSFiltered$Fabric_Category == CombData$Fabric_Category))
    
matchIndexColorFabric <- any((MeanROSFiltered$Season_type == CombData$Season_type) & (MeanROSFiltered$Sub_Brand == CombData$Sub_Brand) 
                                 & (MeanROSFiltered$Material_Type == CombData$Material_Type) & (MeanROSFiltered$MRP == CombData$MRP))
    
matchIndexMRPColorFabric <- any((MeanROSFiltered$Season_type == CombData$Season_type) & (MeanROSFiltered$Sub_Brand == CombData$Sub_Brand) 
                                    & (MeanROSFiltered$Material_Type == CombData$Material_Type))
  }
  
  if(matchIndex == TRUE){
    CombData$Flag <- 0
  }else if(matchIndexMRP == TRUE && matchIndexColor == TRUE && matchIndexFabric == TRUE){
    CombData$Flag <- 1
  }else if(matchIndexFabric == TRUE && matchIndexColor == TRUE){
    CombData$Flag <- 2
  }else if(matchIndexColor == TRUE && matchIndexMRP == TRUE){
    CombData$Flag <- 3
  }else if(matchIndexFabric == TRUE && matchIndexMRP == TRUE){
    CombData$Flag <- 4
  }else if(matchIndexColorFabric == TRUE && matchIndexMRPFabric == TRUE && matchIndexMRPColor == TRUE){
    CombData$Flag <- 5
  }else if(matchIndexColorFabric == TRUE && matchIndexMRPFabric == TRUE){
    CombData$Flag <- 6
  }else if(matchIndexMRPFabric == TRUE && matchIndexMRPColor == TRUE){
    CombData$Flag <- 7
  }else if(matchIndexColorFabric == TRUE && matchIndexMRPColor == TRUE){
    CombData$Flag <- 8
  }else{
    CombData$Flag <- 9
  }
  
  finalData <- CombData
  return(finalData)  
}


# Reading Mean ROS input file

sheetNamesList <- c("All","Fabric_Unknown","Color_Unknown","MRP_Unknown","ColorFabric_Unknown","ColorMRP_Unknown","MRPFabric_Unknown","All_Unknown")

meanROSList <- vector("list", length(sheetNamesList))
names(meanROSList) <- sheetNamesList

for(sheetName in sheetNamesList){
meanROSData <- read.xlsx(paste0(inputLocation,"Mean_ROS_Lookup_v3_Nation.xlsx"), sheet = sheetName)
meanROSList[[sheetName]] <- meanROSData
}


MeanROS <- meanROSList$All
dataComb <- unique(paste0(ModelData5 $Season_type, ModelData5 $Sub_Brand, ModelData5 $Material_Type))
meanROSComb <- unique(paste0(MeanROS$Season_type, MeanROS$Sub_Brand, MeanROS$Material_Type))

uniqueCombData <- unique(ModelData5 [,c("Season_type","Sub_Brand","Material_Type","MRP","ColorGroup","Fabric_Category")])
uniqueCombData <- uniqueCombData %>% mutate_if(is.factor,as.character)

finalFlagData <- data.frame()
if(length(setdiff(dataComb, meanROSComb)) == 0){
  for(i in 1:nrow(uniqueCombData)){
uniqueCombDataFiltered <- uniqueCombData[i,]
season <- uniqueCombDataFiltered$Season_type
subBrand <- uniqueCombDataFiltered$Sub_Brand
materialType <- uniqueCombDataFiltered$Material_Type
matchFlagData <- mismatchAttributes(uniqueCombDataFiltered, season, subBrand, materialType)
finalFlagData <- rbind(finalFlagData, matchFlagData)
    
  }
}

finalFlagData$MRP_Join <- 0
finalFlagData$MRP_Join <- as.numeric(finalFlagData$MRP_Join)
finalFlagData$MRP <- as.numeric(finalFlagData$MRP)
finalFlagData$Fabric_Category_Join <- NA
finalFlagData$ColorGroup_Join <- NA

meanROSfinalData <- data.frame()

for(i in 1:nrow(finalFlagData)){
  filteredFlagData <- finalFlagData[i,]
  if(filteredFlagData$Flag == 0){
    filteredFlagData$Fabric_Category_Join <- filteredFlagData$Fabric_Category
    filteredFlagData$MRP_Join <- filteredFlagData$MRP
    filteredFlagData$ColorGroup_Join <- filteredFlagData$ColorGroup
    
    filteredFlagData <- filteredFlagData %>% left_join(meanROSList$All, 
                                                       by = c("Season_type" = "Season_type",
                                                              "Sub_Brand" = "Sub_Brand",
                                                              "Material_Type" = "Material_Type",
                                                              "MRP_Join" = "MRP",
                                                              "ColorGroup_Join" = "ColorGroup",
                                                              "Fabric_Category_Join" = "Fabric_Category"))
    
  }else if(filteredFlagData$Flag == 1 ||filteredFlagData$Flag == 2 || filteredFlagData$Flag == 4){
    filteredFlagData$Fabric_Category_Join <- "UNKNOWN"
    filteredFlagData$MRP_Join <- filteredFlagData$MRP
    filteredFlagData$ColorGroup_Join <- filteredFlagData$ColorGroup
    
    filteredFlagData <- filteredFlagData %>% left_join(meanROSList$Fabric_Unknown, 
                                                       by = c("Season_type" = "Season_type",
                                                              "Sub_Brand" = "Sub_Brand",
                                                              "Material_Type" = "Material_Type",
                                                              "MRP_Join" = "MRP",
                                                              "ColorGroup_Join" = "ColorGroup",
                                                              "Fabric_Category_Join" = "Fabric_Category"))
    
  }else if(filteredFlagData$Flag == 3){
    filteredFlagData$Fabric_Category_Join <- filteredFlagData$Fabric_Category
    filteredFlagData$MRP_Join <- filteredFlagData$MRP
    filteredFlagData$ColorGroup_Join <- "UNKNOWN"
    
    filteredFlagData <- filteredFlagData %>% left_join(meanROSList$Color_Unknown, 
                                                       by = c("Season_type" = "Season_type",
                                                              "Sub_Brand" = "Sub_Brand",
                                                              "Material_Type" = "Material_Type",
                                                              "MRP_Join" = "MRP",
                                                              "ColorGroup_Join" = "ColorGroup",
                                                              "Fabric_Category_Join" = "Fabric_Category"))
    
  }else if(filteredFlagData$Flag == 5 ||filteredFlagData$Flag == 6 ||filteredFlagData$Flag == 8){
    filteredFlagData$Fabric_Category_Join <- "UNKNOWN"
    filteredFlagData$MRP_Join <- filteredFlagData$MRP
    filteredFlagData$ColorGroup_Join <- "UNKNOWN"
    
    filteredFlagData <- filteredFlagData %>% left_join(meanROSList$ColorFabric_Unknown, 
                                                       by = c("Season_type" = "Season_type",
                                                              "Sub_Brand" = "Sub_Brand",
                                                              "Material_Type" = "Material_Type",
                                                              "MRP_Join" = "MRP",
                                                              "ColorGroup_Join" = "ColorGroup",
                                                              "Fabric_Category_Join" = "Fabric_Category"))
    
  }else if(filteredFlagData$Flag == 7){
    filteredFlagData$Fabric_Category_Join <- "UNKNOWN"
    filteredFlagData$MRP_Join <- 0
    filteredFlagData$ColorGroup_Join <- filteredFlagData$ColorGroup
    
    filteredFlagData <- filteredFlagData %>% left_join(meanROSList$MRPFabric_Unknown, 
                                                       by = c("Season_type" = "Season_type",
                                                              "Sub_Brand" = "Sub_Brand",
                                                              "Material_Type" = "Material_Type",
                                                              "MRP_Join" = "MRP",
                                                              "ColorGroup_Join" = "ColorGroup",
                                                              "Fabric_Category_Join" = "Fabric_Category"))
    
  }else{
    filteredFlagData$Fabric_Category_Join <- "UNKNOWN"
    filteredFlagData$MRP_Join <- 0
    filteredFlagData$ColorGroup_Join <- "UNKNOWN"
    
    filteredFlagData <- filteredFlagData %>% left_join(meanROSList$All_Unknown, 
                                                       by = c("Season_type" = "Season_type",
                                                              "Sub_Brand" = "Sub_Brand",
                                                              "Material_Type" = "Material_Type",
                                                              "MRP_Join" = "MRP",
                                                              "ColorGroup_Join" = "ColorGroup",
                                                              "Fabric_Category_Join" = "Fabric_Category"))
    
  }
  meanROSfinalData <- rbind(meanROSfinalData, filteredFlagData)
}

meanROSfinalTable <- meanROSfinalData
meanROSfinalTable$MRP_Join <- NULL
meanROSfinalTable$Fabric_Category_Join <- NULL
meanROSfinalTable$ColorGroup_Join <- NULL

ModelData5  <- ModelData5  %>% mutate_if(is.factor,as.character)
ModelData5$MRP <- as.numeric(ModelData5$MRP)
meanROSfinalTable$MRP <- as.numeric(meanROSfinalTable$MRP)
ModelData6 <- ModelData5  %>% left_join(meanROSfinalTable, by = c("Season_type","Sub_Brand","Material_Type","MRP","ColorGroup","Fabric_Category"))



## Taking unique value

ModelDataFinal<- unique(ModelData6)
ModelDataFinal$Brand_presence <- as.numeric(ModelDataFinal$Brand_presence)


## Removing Column Third Fabric

ModelDataFinal <- ModelDataFinal[,!names(ModelDataFinal) %in% ("Third_Fabric")]



## Removing the unnecessary columns (removing column with level less than 1)
    
columnstoberemoved<-vector()
    
columnstoberemoved<- colnames(ModelDataFinal)[sapply(ModelDataFinal, function(x) length(unique(x))<=1)]
    
columnstoberemoved<- columnstoberemoved[ !(columnstoberemoved=="Sales_Season") ]
    
if (length(columnstoberemoved)!=0 & ((length(columnstoberemoved)+1) != length(colnames(ModelDataFinal))) )
{
  ModelDataFinal<- ModelDataFinal[,!(names(ModelDataFinal) %in% columnstoberemoved)]
}
    
    

## 

outcome_variable <- "ROS_capped"
      
      
      
# Model Training

# Train data

model_train_data = ModelDataFinal
      
      

## Converting Character datatype variable to Factor datatype Variable & Adding one additional level "Unknown" 
## for all Factor level data type
      
model_train_data <- model_train_data %>% mutate_if (is.character, as.factor)
      
factorColumnList <- lapply(model_train_data,is.factor)
cond <- lapply(factorColumnList, function(x) x == TRUE)
factorColumns <- names(factorColumnList[unlist(cond)])
      
for(t in factorColumns){
levels(model_train_data[,t]) <- c(levels(model_train_data[,t]), "UNKNOWN")
}
      
      
## Removing "RoS","Style_Code","Sales_Season",'Fabric_Category','DD_Median','Fabric_Category', & 'Flag'column from model_train_data

model_train_data <- model_train_data[,!names(model_train_data) %in% c('RoS','Style_Code','Sales_Season','Fabric_Category','DD_Median','Fabric_Category','Flag')]




## Building Random Forest model
      
set.seed(1234)
caret_train_model <- randomForest(x= model_train_data[ , !(names(model_train_data) %in% c(outcome_variable))],
                                  y= model_train_data[, outcome_variable], ntree = 500, mtry = c(7,3,4,10,12), nodesize = c(2,3,5,7,9), do.trace = TRUE)




## Ouput File Creation
      
rds_file_path     = paste0("RF"," ","Overall Nation"," ", "Model", ".rds")
model_path = paste0(modelRDSLocation,rds_file_path)
saveRDS(caret_train_model,model_path)
      
      

# Metrics for Performance

outcome_variable <- "RoS"
actual_output = c(ModelDataFinal[outcome_variable]) %>% magrittr::extract2(outcome_variable)
prediction_results<-caret_train_model$predicted
RMSE = Metrics::rmse(actual_output, prediction_results)
MAE = Metrics::mae(actual_output, prediction_results)
MSE = Metrics::mse(actual_output, prediction_results)
MAPE = Metrics::mape(actual_output, prediction_results)
R_Sq = cor(actual_output, prediction_results)^2



# Output Results
      

# 1. Training Vars
      
Predictor_Variable<- data.frame(Predictors=names(model_train_data[ , !(names(model_train_data) %in% c(outcome_variable, 'Style_Code','RoS','Fabric'))]))
importance1<- as.data.frame(importance(caret_train_model))
Importance_Value1 <- importance1$IncNodePurity
Predictor_Variable1 <- row.names(importance1)
Importance_Value <- as.vector(Importance_Value1)
Predictor_Variable <-as.vector(Predictor_Variable1)
Importance_feature <- data.frame(Predictor_Variable, Importance_Value)
Importance_feature <- Importance_feature[order(- Importance_feature$Importance_Value),]  



# 2. Training results

TrainingResults<- data.frame(R.M.S.E=RMSE,M.A.E=MAE,M.S.E=MSE,M.A.P.E=MAPE,R_Sqd=R_Sq)




# 3. Prediction difference

Diffr<- data.frame(Sub_Brand=ModelDataFinal$Sub_Brand,
                   Material_Type=ModelDataFinal$Material_Type,
                   Style_Code=ModelDataFinal$Style_Code,
                   Actual_ROS=actual_output,
                   Predicted_ROS=prediction_results,
                   Differece= abs(actual_output-prediction_results),
                   Percentage= percent(abs(actual_output-prediction_results)/actual_output))


## 


predictor_file_path = paste0("RF"," ","Overall Nation"," ", "Model", ".xlsx")
predictors_path = paste0(predictionLocation,predictor_file_path)
wb <- createWorkbook()
addWorksheet(wb, "Predictor_variables")
addWorksheet(wb, "Importance Features")
addWorksheet(wb, "Training Results")
addWorksheet(wb, "Actual and Predicted comparison")
writeData(wb, 1, Predictor_Variable)
writeData(wb, 2, Importance_feature)
writeData(wb, 3, TrainingResults)
writeData(wb, 4, Diffr)
saveWorkbook(wb, file = predictors_path, overwrite = TRUE)
