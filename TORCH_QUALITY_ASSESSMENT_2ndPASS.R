rm(list=ls())
options(digits = 12)
options(warn=1) #options(warn=2, error=recover)

library(lidR) #
library(rgdal) #
library(stats) #
library (pastecs)
library (maptools) #
library(raster) #
library(spatstat) #
library(dplyr) #
library(rgeos) #
library(MASS) #
library(stringr) #
library(alphashape3d)
library(EBImage) #
library(tidyr) #
library(fields) #
library(data.table) # 
library(sf)
library(lwgeom)
library(spdep) #
library(units)
library(randomcoloR)
library(oceanmap)
library(geosphere)
library(pracma)


Train_Flights <- c(3, 12, 44,  1,  4, 27, 19,  6, 40, 52, 18, 29, 37, 17, 20, 11, 53, 22, 32, 26, 33,  9, 43, 41, 36,  2, 28, 51, 21, 30,  8, 15, 46, 35)
Valid_Flights <- c(5, 7, 10, 13, 34, 39, 47, 48, 49)

Comp_Name <- "//rcgdata/dj806/"
SEGMENTATION_ALGO <- "_ORIG_CODE_REDO_T3"

FOLDER_PROJECT <- "/R_Code/CNN_R_Code/" # ITCD_CNN/
FOLDER_PRE_MASTER_CODE <- paste(Comp_Name, FOLDER_PROJECT, "/PreMASTER_CNN", sep="") # "/PRE_MASTER_CODE"
source(paste(FOLDER_PRE_MASTER_CODE, "/TORCH_FUNCTIONS_PreMASTER.R", sep=""))
#############################################################################################################################################################
#############################################################################################################################################################
#############################################################################################################################################################

DF_SPLIT_TID <- data.frame(Split_PlotID = numeric(),
                           Split_TID = numeric(),
                           Split_PlotID_To = numeric(), 
                           Split_TID_To = numeric(), 
                           Split_Trees_Retain = character())

DF_MERGE_TID <- data.frame(Merge_From_PlotID = numeric(), 
                           Merge_From_TID = numeric(), 
                           Merge_To_PlotID = numeric(),
                           Merge_To_TID =  numeric() )

DF_REMOVE_TID <- data.frame(Remove_Tree_PlotID = numeric(), 
                            Remove_Tree_TID = numeric() )

DF_BASE_SPLIT_TID  <- data.frame(Base_Split_PlotID = numeric(), 
                                 Base_Split_TID = numeric(),
                                 Max_TID_ID = numeric(),
                                 Base_Count = numeric())  

DF_BASE_FILL_ZERO  <- data.frame(Fill_Zero_PlotID = numeric(), 
                                 Fill_Zero_TID = numeric())

DF_DIAM_SPLIT_TID<- data.frame(Diam_Split_PlotID = numeric(), 
                               Diam_Split_TID = numeric())



#############################################################################################################################################################
#############################################################################################################################################################
#############################################################################################################################################################

###################
# CHANGE AND RE-RUN
###################

# THE BELOW PROCEDURE ONLY GENERATES THE CSV FILES OUTLINING THE REQUIRED CHANGES THAT YOU WILL THEN NEED TO APPLY IN TORCH_QUALITY_ASSESSMENT

#FID <- c(12)
#oneP_PlotID <- 63
flag <- 888
browser()
######################
if(FID %in% Valid_Flights){
  FOLDER_O <- paste(Comp_Name, "CNN/THINNED_DATA", SEGMENTATION_ALGO, "/VALIDATE_DATA/Flight_" , FID, sep="")
}
if(FID %in% Train_Flights){
  FOLDER_O <- paste(Comp_Name, "CNN/THINNED_DATA", SEGMENTATION_ALGO, "/TRAIN_DATA/Flight_", FID, sep="")
}

LAS_FILE <- paste(FOLDER_O, "/LAS/LAS_MP1/LAS_P_ALL/F", FID, "_P", oneP_PlotID,"_allT_AttTIDallLAS.laz", sep="")
LAS_oneP <-  readLAS(LAS_FILE,  select = "xyzp0")
Unique_TID <- unique(LAS_oneP@data$TID)
Unique_TID <- Unique_TID[which(Unique_TID > 1)]
print(cat(paste(oneP_PlotID, "_", c(0, sort(Unique_TID), -9999), sep=""), sep= '" , "'))
######################


Merge_Trees_From <- c(  "63_103") 
Merge_Trees_To <-   c("63_10" )    #   "217_338" ,  "217_345"  , "217_381" ,  , "217_384"
#
Split_Trees_From <- c("" )  
Split_Trees_Retain <- c("Y") 
Split_Trees_To <- c( list(c(""  )))  
#
Diam_Split <- c( "")
#
Fill_Zeros_Below <- c("63_10" )
#
Base_Split <- c("63_10")
Base_Count <- c(2)
#
Remove_Tree <- c( "") #  
#
#
print(paste("CHECK ............................................................P",oneP_PlotID, sep=""))

browser()
#
#############################################################################################################################################################
#############################################################################################################################################################
#############################################################################################################################################################

##########################################
# PREPARING THE DIRECTORIES FOR PROCESSING
##########################################



dir.create(file.path(FOLDER_O,  "UPDATE_QA_INFO_2nD_Pass"), showWarnings = FALSE)
FOLDER_QA_INFO <- "UPDATE_QA_INFO_2nD_Pass"
FOLDER_UPDATE_QA <- paste(FOLDER_O, "/",FOLDER_QA_INFO, sep="") 

MAX_TID <- read.csv(paste(FOLDER_O, "/F", FID, "_Max_TID_ID.csv", sep=""))
MAX_TID <- MAX_TID$x

#######
# MERGE
#######

if(Merge_Trees_From != ""){
  for(MM in 1:length(Merge_Trees_From)){
    oneMerge_Trees_From <- Merge_Trees_From[MM]
    oneMerge_Trees_To <-Merge_Trees_To[MM]
    From_Merge_ID <- as.numeric(numextract_all(oneMerge_Trees_From))
    To_Merge_ID <- as.numeric(numextract_all(oneMerge_Trees_To))
    DF_MERGE_TID[(nrow(DF_MERGE_TID)+1),] <- c(From_Merge_ID[1], From_Merge_ID[2], To_Merge_ID[1],  To_Merge_ID[2]  ) 
  }
}

#######
# SPLIT
#######
if(Split_Trees_From != ""){
  for(SS in 1:length(Split_Trees_From)){
    oneSplit_Trees_Retain <- Split_Trees_Retain[SS]
    oneSplit_ID_From <- numextract_all(Split_Trees_From[SS])
    oneSplit_PlotID_From <- oneSplit_ID_From[1]
    oneSplit_TID_From <- oneSplit_ID_From[2]
    
    Split_Trees_To_UNLIST <- unlist(Split_Trees_To[[SS]])
    all_PlotID_To <- c()
    for(SSS in 1:length(Split_Trees_To_UNLIST)){
      oneSplit_Tree_To <- Split_Trees_To_UNLIST[SSS]
      oneN_oneSplit_To <- numextract_all(oneSplit_Tree_To)
      oneN_oneSplit_PlotID_To <- oneN_oneSplit_To[1]
      all_PlotID_To <- c(all_PlotID_To, oneN_oneSplit_PlotID_To)
      oneN_oneSplit_TID_To <- oneN_oneSplit_To[2]
      
      DF_SPLIT_TID[(nrow(DF_SPLIT_TID)+1),] <- c(oneSplit_PlotID_From, oneSplit_TID_From, oneN_oneSplit_PlotID_To, oneN_oneSplit_TID_To, oneSplit_Trees_Retain)
    }
  }
}

############
# DIAM SPLIT
############
if(Diam_Split != ""){
  for(DD in 1:length(Diam_Split)){
    Diam_Split_TID <- as.numeric(numextract_all(Diam_Split[DD])[c(FALSE,TRUE)])
    oneP_PlotID <- as.numeric(numextract_all(Diam_Split[DD])[c(TRUE, FALSE)])
    DF_DIAM_SPLIT_TID[(nrow(DF_DIAM_SPLIT_TID)+1),] <- c(oneP_PlotID, Diam_Split_TID)
  }
}

###########
# FILL ZERO
###########
if(Fill_Zeros_Below != ""){
  for(FZ in 1:length(Fill_Zeros_Below)){
    FILL_ZERO_TID <- as.numeric(numextract_all(Fill_Zeros_Below[FZ])[c(FALSE,TRUE)])
    oneP_PlotID <- as.numeric(numextract_all(Fill_Zeros_Below[FZ])[c(TRUE, FALSE)])
    DF_BASE_FILL_ZERO[(nrow(DF_BASE_FILL_ZERO)+1),] <- c(oneP_PlotID, FILL_ZERO_TID)
  }
}

############
# BASE SPLIT
############
if(Base_Split != ""){
  for(BB in 1:length(Base_Split)){
    Base_Split_TID <- as.numeric(numextract_all(Base_Split[BB])[c(FALSE,TRUE)])
    oneP_PlotID <- as.numeric(numextract_all(Base_Split[BB])[c(TRUE, FALSE)])
    for(MX in 1:(Base_Count[BB]-1)){
      MAX_TID <- MAX_TID + 1
    }
    # UPDATE  Max_TID_ID
    DIR_Max_TID_ID <- paste(FOLDER_O, "/F",FID, "_Max_TID_ID.csv", sep="")
    write.csv(MAX_TID, DIR_Max_TID_ID, row.names = FALSE)
    DF_BASE_SPLIT_TID[(nrow(DF_BASE_SPLIT_TID)+1),] <- c(oneP_PlotID, Base_Split_TID, MAX_TID, Base_Count[BB])
  }
}

#############
# REMOVE TREE
#############
if(Remove_Tree != ""){
  if(Remove_Tree == "ALL"){
    Remove_Tree <- paste(oneP_PlotID, "_", c(0,Unique_TID), sep="")
  }
  for(RR in 1:length(Remove_Tree)){
    oneRemove_Tree <- Remove_Tree[RR]
    Remove_PlotID <- numextract_all(oneRemove_Tree)[[1]]
    Remove_TID <- numextract_all(oneRemove_Tree)[[2]]
    DF_REMOVE_TID[(nrow(DF_REMOVE_TID)+1), ] <- c(Remove_PlotID, Remove_TID )  
  }
}

print(paste("DONE ............................................................P",oneP_PlotID, sep=""))

write.csv(DF_MERGE_TID, paste(FOLDER_UPDATE_QA, "/F",FID,"_MP1_P",oneP_PlotID , "_Track_Changes_MERGE.csv", sep=""), row.names = FALSE) # Plot_",oneP_PlotID,  "_",
write.csv(DF_SPLIT_TID, paste(FOLDER_UPDATE_QA, "/F",FID,"_MP1_P",oneP_PlotID , "_Track_Changes_SPLIT.csv", sep=""), row.names = FALSE) # Plot_",oneP_PlotID, "_", 
write.csv(DF_REMOVE_TID, paste(FOLDER_UPDATE_QA, "/F",FID,"_MP1_P",oneP_PlotID , "_Track_Changes_REMOVE_TID.csv", sep=""), row.names = FALSE)
write.csv(DF_BASE_SPLIT_TID, paste(FOLDER_UPDATE_QA, "/F",FID,"_MP1_P",oneP_PlotID , "_Track_Changes_BASE_SPLIT.csv", sep=""), row.names = FALSE)
write.csv(DF_BASE_FILL_ZERO, paste(FOLDER_UPDATE_QA, "/F",FID,"_MP1_P",oneP_PlotID , "_Track_Changes_FILL_ZERO.csv", sep=""), row.names = FALSE)
write.csv(DF_DIAM_SPLIT_TID, paste(FOLDER_UPDATE_QA, "/F",FID,"_MP1_P",oneP_PlotID , "_Track_Changes_DIAM_SPLIT.csv", sep=""), row.names = FALSE)
