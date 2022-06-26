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

Proj_Sys <- "+proj=utm +zone=55 +south +ellps=WGS84 +units=m +no_defs" # st_crs(4326) 

FUN_Dir <- "//rcgdata/dj806/R_Code/ITCD_OPEN_SOURCE_R_Code/GITHUB/ITCD_UAS_LIDAR/" # CHANGED 3/02/21
source(paste(FUN_Dir, "FUNCTIONS_ITCD.R", sep="")) #FUN

Para_Vx_R_Res <- 0.2
Para_Sl_Z <- Para_Vx_R_Res*2
Para_d_clust <- Para_Vx_R_Res*8 + 0.01
Para_C_Base_BW <- 0.5
Para_C_Base_Thresh <- 0.2

Comp_Name <- "//rcgdata/dj806/"
FOLDER_PROJECT <- "/R_Code/CNN_R_Code/ITCD_CNN/"
FOLDER_PRE_MASTER_CODE <- paste(Comp_Name,  "/R_Code/CNN_R_Code/PreMASTER_CNN", sep="") #Y:\R_Code\CNN_R_Code\PreMASTER_CNN
# Algo_Type <- c("THINNED_DATA_ORIG_CODE_REDO") #c("THINNED_DATA_ORIG_CODE", "THINNED_SAMPLES_GIT_CODE")

R_Code_Version <- "_V1"
source(paste(Comp_Name, FOLDER_PROJECT, "TORCH_FUNCTIONS_CNN_MASTER",R_Code_Version, ".R", sep=""))
source(paste(FOLDER_PRE_MASTER_CODE, "/TORCH_FUNCTIONS_PreMASTER.R", sep=""))


###################################################################################################
###################################################################################################
###################################################################################################
# CHECK FILES THAT CAN BE ASSESSED FOR ACCURACY
SEGMENTATION_ALGO <- "_ORIG_CODE_REDO_T3"
Folder_Input <- paste("//rcgdata/dj806/CNN/THINNED_DATA", SEGMENTATION_ALGO, "/", sep="")

D_Type <- c("TRAIN_DATA", "VALIDATE_DATA")

for(DD in 1:length(D_Type)){
  Flights <- list.files(paste(Folder_Input, D_Type[DD], sep=""), pattern="Flight_")
  All_Done_LAS_ALL <- c()
  All_Folder_Check_LAS_ALL <- c()
  All_Done_LAS_THIN <- c()
  All_Folder_Check_LAS_THIN <- c()
  for(CC in 1:length(Flights)){
    Folder_Check_LAS_ALL <- paste(Folder_Input, D_Type[DD], "/",Flights[CC], "/LAS/LAS_MP1/LAS_P_ALL", sep="")
    All_Folder_Check_LAS_ALL <- c(All_Folder_Check_LAS_ALL, Folder_Check_LAS_ALL)
    
    Done <- length(list.files(Folder_Check_LAS_ALL))
    All_Done_LAS_ALL <- c(All_Done_LAS_ALL, Done)
    
    Folder_Check_LAS_THIN <- paste(Folder_Input, D_Type[DD], "/",Flights[CC], "/LAS/LAS_MP1/LAS_P", sep="")
    All_Folder_Check_LAS_THIN <- c(All_Folder_Check_LAS_THIN, Folder_Check_LAS_THIN)
    
    Done <- length(list.files(Folder_Check_LAS_THIN))
    All_Done_LAS_THIN <- c(All_Done_LAS_THIN, Done)
  }
  Output <- data.frame(Flights, All_Done_LAS_ALL, All_Folder_Check_LAS_ALL, All_Done_LAS_THIN, All_Folder_Check_LAS_THIN)
  if(DD == 1){
    Output_ALL <-Output
  }else{
    Output_ALL <-rbind(Output_ALL, Output)
  }
}
# browser()
# Output_ALL$Flights[which(Output_ALL$All_Done_LAS_ALL > 0)]
Flights_ReadyToDo_THIN_AVAILABLE <- c(12, 17, 18, 19, 20, 21, 22, 26, 27, 28, 30, 32, 33, 35, 36, 37, 40, 41, 34, 39)
Flights_Ready <- Output_ALL$Flights[which(Output_ALL$All_Done_LAS_ALL > 0)]
Flights_Ready <- as.numeric(numextract_all(Flights_Ready))
Flights_Done <- c(1,2, 4, 5,6, 7, 10, 11, 13, 15, 46, 47, 48, 49)
Flights_Done_1stPass <- c(12, 17, 18, 19, 20, 21, 22, 26, 27, 28, 30, 32, 33, 35, 36, 37, 40, 41, 34, 39) 
Flights <- Flights_Ready[-which(Flights_Ready %in% Flights_Done)]
# SEE WHAT FLIGHTS ARE READY FOR CLEANING ... Output_ALL[,c(1,2,4)]
#browser()
###################################################################################################
###################################################################################################
###################################################################################################

# Flights <- c(2, 1,  3, 4,5,6,7,8,9,10,11,12,13,15,17,18,19,20,21,22,26,27,28,29,30,32,33,34,35,36,37,39,40,41,43,44,46,47,48,49, 51, 52, 53)  # c( 40, 41, 43, 44, 46)
# Flights <- Flights_Ready
Train_Flights <- c(3, 12, 44,  1,  4, 27, 19,  6, 40, 52, 18, 29, 37, 17, 20, 11, 53, 22, 32, 26, 33,  9, 43, 41, 36,  2, 28, 51, 21, 30,  8, 15, 46, 35)
Valid_Flights <- c(5, 7, 10, 13, 34, 39, 47, 48, 49)

# DONE: 1,2, 4, 5,6, 7, 10, 11, 13, 15, 46, 47, 48, 49
# NOT_AVAILABLE: 3, 9, 51, 52, 53
#Flights <- c(1, 2, 4, 5,6, 7, 10, 11, 13, 15, 46, 47, 48, 49) #c(2)
#Flights <- c(1, 2, 4, 5, 6, 7, 10, 11, 13, 15, 46, 47, 48, 49) #LAS ALL DONE FOR REST 

MP <- 1

########################################################################################################################
########################################################################################################################
########################################################################################################################
# THINGS TO RUN
RUN_PLOT_FLIGHT_FUN <- "No"
RUN_MANUAL_CHANGES <- "No"      # "Yes" INVOLVES PUTTING IN DATA FOR CSVs (Folder: UPDATE_QA_INFO) AND PERFORMING LAS UPDATE AND "No" MEANS IT READS THE CSV AND PERFORMS UPDATES TO LAS FILES
  # IF No THEN MAKE SURE Start_Flight and Start_Plot IS UPDATED
RUN_2nd_PASS_CHANGES <- "Yes"    # IF YOU HAVE ASSESSED ALL THE PLOTS AND HAVE ABSOLUTE LAST CHANGES TO UNDERTAKE.
  # "Yes" FOR ACTUALLY RUNNING THE SECOND SET OF CSV FILES LISTING REQUIRED CHANGES.
    # "No" If RUNNING FIRST SET OF CHANGES. (WHEN RUN_MANUAL_CHANGES == "No")
RUN_VISUALISE_END_PLOT <- "No"
RUN_RE_SAVE <-  "No"
########################################################################################################################
########################################################################################################################
########################################################################################################################

# WHEN RUNNING THE CSV FILES (FILES WITH STORED CHANGES AND IT CRASHES THE BELOW INFORMATION MAKES IT START RUNNING FROM CORRECT PLACE)
Start_Flight <- 39 #19  # CHECK WITH FID
UPTO_Flight <- which(Flights == Start_Flight)
Start_Plot <- 227 #"First" # ACTUAL PLOT ID # OR "First"  # CHECK WITH oneP_PlotID (NOTE THAT YOU MAY NEED TO COPY THE PLOT OVER BEFORE YOU RUN IF IT WAS ALTERED HALFWAY THROUGH LOOP)

########################################################################################################################
########################################################################################################################
########################################################################################################################
if(RUN_2nd_PASS_CHANGES == "Yes"){
  FOLDER_QA_INFO <- "UPDATE_QA_INFO_2nD_Pass"
  # Flights_TO_RUN <- c()
  # for (ff in 1:length(Flights)){
  #   
  #   FID <- numextract(Flights[ff])
  #   
  #   if(FID %in% Valid_Flights){
  #     FOLDER_O <- paste(Comp_Name, "CNN/THINNED_DATA", SEGMENTATION_ALGO, "/VALIDATE_DATA/Flight_" , FID, sep="")
  #   }
  #   if(FID %in% Train_Flights){
  #     FOLDER_O <- paste(Comp_Name, "CNN/THINNED_DATA", SEGMENTATION_ALGO, "/TRAIN_DATA/Flight_", FID, sep="")
  #   }
  #   # browser()
  #   Exists_FOLDER_QA_INFO <- list.files(FOLDER_O, pattern=FOLDER_QA_INFO)
  #   if(length(Exists_FOLDER_QA_INFO) == 1){
  #     Flights_TO_RUN <- c(Flights_TO_RUN, Flights[ff])
  #   }
  # }
}else{
  FOLDER_QA_INFO <- "UPDATE_QA_INFO" 
}

Flights_TO_RUN <- UPTO_Flight:length(Flights)

# LOOP FLIGHTS
for (ff in Flights_TO_RUN){
  
  # START WORKING FROM PLOT ONE IF YOU GO TO NEXT FLIGHT
  if(UPTO_Flight != ff){ Start_Plot <- "First"}

  FID <- numextract(Flights[ff])
  
  #####################################################################################################################################################################
  # DETERMINE WHERE TO OUTPUT THE RESULTS
  #######################################c
  if(FID %in% Valid_Flights){
    FOLDER_O <- paste(Comp_Name, "CNN/THINNED_DATA", SEGMENTATION_ALGO, "/VALIDATE_DATA/Flight_" , FID, sep="")
  }
  if(FID %in% Train_Flights){
    FOLDER_O <- paste(Comp_Name, "CNN/THINNED_DATA", SEGMENTATION_ALGO, "/TRAIN_DATA/Flight_", FID, sep="")
  }
  FOLDER_LAS <- paste(FOLDER_O, "/LAS/LAS_MP",MP, sep="")
  
  #####################################################################################################################################################################
  # VISUALISE WHOLE FLIGHT AND GET POLYGONS FOR ALL PLOTS
  ########################################################  
  Poly_allP <- st_read(
    dsn= paste(FOLDER_O, "/SHP", sep=""), # paste(FOLDER_O, "/SHP/SHP_MP",MP, sep="") ,_MovPos1_allPlots
    layer=paste("F" ,  FID, "_MovPos1_allPlots", sep=""))  %>% st_set_crs(Proj_Sys)
  Neighbour_mtx <- st_intersects(Poly_allP, Poly_allP)
  Centroid_allP <- as.data.frame(st_coordinates(st_centroid(Poly_allP)))
  Centroid_allP$Grid_ID <- Poly_allP$layer
  
  # VISUALISE THE WHOLE FLIGHT 
  if(RUN_PLOT_FLIGHT_FUN == "Yes"){
    Plot_Flight_Output <- PLOT_FLIGHT_FUN(FOLDER_O, FID, Poly_allP, Neighbour_mtx)
    Flight_Dev <- Plot_Flight_Output[[1]]
    SHifT_X<- Plot_Flight_Output[[2]]
    SHifT_Y<- Plot_Flight_Output[[3]]
  }
  
  ###################################################################################################################################################################
  # GET Max_TID_ID (MAKING SURE NEW TID DOES NOT DUPLICATE ACCIDENTLY)
  ################
  
  LAS_ALS_FLIGHT <- readLAS(paste(FOLDER_O, "/LAS", "/All_ALS_F",FID,".laz", sep=""), select = "xyzp0")
  #maxZ_ALS_Flight <- max(LAS_ALS_FLIGHT$Z)
  
  # RUN FOR FIRST TIME SO Max_TID_ID STARTS FROM FIRST AVAILABLE NUMBER (i.e. max(LAS_ALS_FLIGHT@data$TID) + 100)
  if(RUN_MANUAL_CHANGES == "No" & RUN_2nd_PASS_CHANGES == "No"){
    File_Max_TID_ID <- NULL
  }else{
    File_Max_TID_ID <- list.files(FOLDER_O, pattern = "Max_TID_ID")
  }
  
  if(length(File_Max_TID_ID) == 0){
    Max_TID_ID <- max(LAS_ALS_FLIGHT@data$TID) + 100
    write.csv(Max_TID_ID, paste(FOLDER_O, "/F",FID, "_Max_TID_ID.csv", sep=""), row.names = FALSE)
  }else{
    Max_TID_ID <- read.csv(paste(FOLDER_O, "/F",FID, "_Max_TID_ID.csv", sep=""))
    Max_TID_ID<- Max_TID_ID$x
  }
#####################################################################################################################################################################

  # STARTS RUNNING FROM THE LAST PLOT THAT WAS COMPLETED (THIS IS SET AT THE START OF FLIGHT LOOP)
  if(RUN_2nd_PASS_CHANGES == "Yes"){
    Files_To_Change <- list.files(paste(FOLDER_O, "/", FOLDER_QA_INFO, sep=""))
    Plot_IDS <- unique(sort(as.numeric(numextract_all(Files_To_Change))[c(F,F,T)]))
    
  }else{
    Plot_LAZ <- list.files(paste(FOLDER_LAS, "/LAS_P_ALL" , sep=""), pattern = paste("F",FID,"_P", sep="")) #"/LAS_P"
    Plot_IDS<-  numextract_all(Plot_LAZ)
    Plot_IDS <- sort(as.numeric(Plot_IDS[c(F,T)])) 
    Plot_IDS <-unique(Plot_IDS)
  }
  if(Start_Plot == "First"){Start_Plot  <-  Plot_IDS[1]}
  UPTO_Plot <- which(Plot_IDS == Start_Plot)
  PLOTS_TO_RUN <- UPTO_Plot:length(Plot_IDS)

  # GOOD EXAMPLES OF WRONG PLOTTING:   F15_P69,  F15_P80
  for(PP in PLOTS_TO_RUN){ # 
    print(paste("F",FID, ":................................ Start PP:",which(PLOTS_TO_RUN == PP), " out of ", length(PLOTS_TO_RUN)  ))
    #####################################################################################################################################################################
    #####################
    # GET PLOT NEIGHBOURS
    #####################
    
    oneP_PlotID <- Plot_IDS[PP]
    Poly_oneP <- Poly_allP[which(Poly_allP$layer == oneP_PlotID), ]
    XY_oneP <- data.frame(st_coordinates(Poly_oneP))
    Poly_Neigh <- Poly_allP[Neighbour_mtx[[which(Poly_allP$layer == oneP_PlotID)]], "layer"]
    
    # PUT BLUE POLYGON OVER PLOT IN FLIGHT 
    if(RUN_PLOT_FLIGHT_FUN == "Yes"){
      set3d(Flight_Dev, silent = FALSE)
      polygon3d(XY_oneP[,1]-SHifT_X, XY_oneP[,2]-SHifT_Y, rep(max(LAS_ALS_FLIGHT$Z+1), nrow(XY_oneP)), fill = T,  color="blue", lwd=2)
      }

    # GET ALL GRIDS AND NEIGHBOURS.
    oneP_Neigh <- Poly_Neigh$layer[which(Poly_Neigh$layer != oneP_PlotID)]

    #####################################################################################################################################################################
    #####################################################################################################################################################################
    
    BAD_Plot <- c("F1_P112", "F1_P142", "F1_P162")
    # Flight 47 195_415 needs base split
    
    if(RUN_MANUAL_CHANGES == "Yes"){
      #browser()
      ####################
      # VISUALISE THE PLOT 
      ####################
      THIN_FOLDER <- paste(FOLDER_LAS, "/LAS_P", sep="")  # Dir_oneLAS <- paste(FOLDER_LAS, Dir_Prefix, oneN_PlotID, Dir_Suffix, sep="")v
      ALL_FOLDER <-  paste(FOLDER_LAS, "/LAS_P_ALL", sep="")
      TREE_CODE <- VISUALISE_PLOT_START_FUN(THIN_FOLDER, ALL_FOLDER, oneP_PlotID, oneP_Neigh, Poly_Neigh, FID)
      print(cat(c(TREE_CODE, -9999), sep= '" , "'))
      
      ###################
      # CHANGE AND RE-RUN
      ###################
      #
      REMOVE_Plot <- "N"           # Y or N
      #
      Merge_Trees_From <- c( "207_507", "207_514" )     #ALL "" c("")
      Merge_Trees_To <- c("207_493" ,"207_493"  )    #93_223     #ALL  "" c("") 214_536
      #
      Split_Trees_From <- c("" )        #  DOM DOM DOM ALWAYS NEEDS TO BE SUBJECT PLOT
      Split_Trees_Retain <- c("Y")       #
      Split_Trees_To <- c(list(c("" )))  #, , "184_410"
      #
      Diam_Split <- c("") 
      #
      Fill_Zeros_Below <- c( "207_493")   #ALL WHEN MERGE ALL Merge_Trees_To[1]
      #
      Base_Split <- c( "") 
      Base_Count <- c(2)
      #
      Remove_Tree <- c("" ) 
      #
      # NO SEGMENTATION IN PLOT: 149, 150, 158, 159, 160, 161, 162, 163, 268, 169, 170, 177, 178, 179, 180, 181, 182, 
      # 187, 188, 189, 197, 198, 206, 207, 219, 220- END
      
      # Check_PlotID <- 108
      # LAS_Test <- readLAS(paste(FOLDER_LAS, "/LAS_P_ALL", "/F",FID,"_P", Check_PlotID, "_allT_AttTIDallLAS.laz", sep=""),  select = "xyzp0")#  LAS_oneP
      # PLOT_COL_FUN(LAS_Test, Title="NEW TID", size = 2, TID_Number = "Yes", ShowGridID="Yes", Check_Plot= "Yes")
      browser()
      
      print(paste("...........................................................P",oneP_PlotID,": ...............CHECK AND RUN INPUT !!!!!!!!", sep=""))
      browser()
    }else{
      # browser()
      REMOVE_Plot <- "N" 
      
      # UPDATING LAS_ALL FOR EACH PLOT

      FOLDER_UPDATE_QA_INPUT <- paste(FOLDER_O, "/", FOLDER_QA_INFO, sep="")
      Files_To_Change <- list.files(FOLDER_UPDATE_QA_INPUT)
      CSV_Plot_ID <- as.numeric(numextract_all(Files_To_Change))[c(F,F,T)]
      #browser()
      Files_To_Change <- Files_To_Change[which(CSV_Plot_ID %in% oneP_PlotID)]
      INPUT_LAS_ALL_UPDATE <- LAS_ALL_UPDATE_FUN(FOLDER_UPDATE_QA_INPUT, oneP_PlotID, Files_To_Change)
      Merge_Trees_From <- INPUT_LAS_ALL_UPDATE[[1]]
      Merge_Trees_To <- INPUT_LAS_ALL_UPDATE[[2]] 
      Diam_Split <- INPUT_LAS_ALL_UPDATE[[3]]
      Split_Trees_From <- INPUT_LAS_ALL_UPDATE[[4]] 
      Split_Trees_Retain <- INPUT_LAS_ALL_UPDATE[[5]] 
      Split_Trees_To <- INPUT_LAS_ALL_UPDATE[[6]] 
      Base_Split <- INPUT_LAS_ALL_UPDATE[[7]]
      Base_Count <- INPUT_LAS_ALL_UPDATE[[8]] 
      Fill_Zeros_Below <- INPUT_LAS_ALL_UPDATE[[9]] 
      Remove_Tree <- INPUT_LAS_ALL_UPDATE[[10]] 

      #################### START
      print(INPUT_LAS_ALL_UPDATE)
      
      LAS_oneChanged <- readLAS(paste(FOLDER_LAS, "/LAS_P_ALL", "/F",FID,"_P" ,oneP_PlotID, "_allT_AttTIDallLAS.laz", sep=""), select = "xyzp0")
      PLOT_COL_FUN(LAS_oneChanged, Title=paste("F:", FID, "P:", oneP_PlotID), size = 5, TID_Number = "Yes", ShowGridID="Yes", Check_Plot= "Yes")

      UNIQUE_TID <- unique(LAS_oneChanged@data$TID)[which(unique(LAS_oneChanged@data$TID) > 1)]
      print(cat(c(paste(oneP_PlotID, "_", UNIQUE_TID, sep=""), -9999), sep= '" , "'))
      #browser()
      
      #################### END

 
    }
    #Diam_Split <- c("153_436")  Remove_Tree <- c(  "153_472")
    ###################################################################################################################################################################
    
    if(!all(c(length(Merge_Trees_From) == length(Merge_Trees_To),
              length(Split_Trees_Retain) == length(Split_Trees_From),
              length(Split_Trees_To) == length(Split_Trees_From)))){
      print("INPUT NOT CORRECT RE-ASSESS!!!!!")
      browser()
    }
    
    # MAKING SURE MERGE PLOTS ARE CORRECT
    From_Merge_ID <- as.numeric(numextract_all(Merge_Trees_From))[c(T,F)]
    INdex_MergeFromIncorrect <- which(From_Merge_ID != oneP_PlotID)
    if(length(INdex_MergeFromIncorrect) > 0){
      print("MERGE INPUT FROM IS NOT FROM SUBJECT PLOT!!!!!")
      browser()
    }
    
    ###################################################################################################################################################################
    ###################################################################################################################################################################
    ###################################################################################################################################################################
    # PARAMETRES FOR RUNNING 
    ########################
    
    THIN_OR_ALL_LAS <- c("All") # , "Thin" DOM DOM DOM !!! DON'T RUN THIN ANY MORE (VOX CLUSTERING WOULD NOT WORK HERE). BEST TO PERFORM THE SOCK METHOD FOR THIN LATER!!!
    
    Check_Plot <- "No"
    Dir_Prefix <- paste("/LAS_P_ALL", "/F",FID,"_P", sep="")
    Folder_Prefix <- "/LAS_P_ALL"
    File_Prefix <- paste("F",FID,"_P", sep="")
    Dir_Suffix <- "_allT_AttTIDallLAS.laz"  #
    
    TRUE_FALSE <-  c(F,T)
    
    # FOLDER_QA_INFO_INPUT <- "UPDATE_QA_INFO"
    # if(THIN_OR_ALL_LAS == "Thin"){
    #   # Check_Plot <- "No"
    #   # Dir_Prefix <- paste("/LAS_P", "/F", FID, "_MP", MP,"_P" , sep="")
    #   # Folder_Prefix <- "/LAS_P"
    #   # File_Prefix <- paste("F", FID, "_MP", MP,"_P", sep="")
    #   # Dir_Suffix <- ".laz"
    #   # TRUE_FALSE <-  c(F,F,T)
    #   # #FOLDER_QA_INFO <- "UPDATE_QA_INFO"
    #   
    # }else{
    #   Check_Plot <- "Yes"
    #   Dir_Prefix <- paste("/LAS_P_ALL", "/F",FID,"_P", sep="")
    #   Folder_Prefix <- "/LAS_P_ALL"
    #   File_Prefix <- paste("F",FID,"_P", sep="")
    #   Dir_Suffix <- "_allT_AttTIDallLAS.laz"  #
    #   
    #   FOLDER_QA_INFO_INPUT <- "UPDATE_QA_INFO"
    #   TRUE_FALSE <-  c(F,T)
    # }
    
    # OPEN LAS ONE PLOT
    LAS_oneP <-  readLAS(paste(FOLDER_LAS, Dir_Prefix,oneP_PlotID, Dir_Suffix, sep=""),  select = "xyzp0")
    LAS_oneP@data$Grid_ID <- oneP_PlotID
    
    UNIQUE_TID <-  unique(LAS_oneP@data$TID)[which(unique(LAS_oneP@data$TID) > 1)]
    
    dir.create(file.path(FOLDER_O,  FOLDER_QA_INFO), showWarnings = FALSE)
    FOLDER_UPDATE_QA <- paste(FOLDER_O, "/",FOLDER_QA_INFO, sep="") 

    ####################################
    # EMPTY DATA_FRAMES TRACKING CHANGES
    ####################################

    DF_REMOVE_PLOT <- data.frame(Remove_Plot_ID= numeric())
    
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

    
    #####################################################################################################################################################################
    #####################################################################################################################################################################
    #####################################################################################################################################################################
    #####################################################################################################################################################################
    #####################################################################################################################################################################
    #####################################################################################################################################################################
    #####################################################################################################################################################################
    #####################################################################################################################################################################
    #####################################################################################################################################################################
    #####################################################################################################################################################################
    
    #####################################################################################################################################################################
    
    ##########################
    # MERGE TREES TO NEW PLOT
    ##########################
    if(Merge_Trees_From == "ALL"){
      Merge_Trees_From <- paste(oneP_PlotID, "_", c(UNIQUE_TID), sep="")[2:length(UNIQUE_TID)]
      Merge_Trees_To <- rep(paste(oneP_PlotID, "_", c(UNIQUE_TID), sep="") [1], length(Merge_Trees_From))
      if( Fill_Zeros_Below == "ALL"){
        Fill_Zeros_Below <- Merge_Trees_To[1]
      }
    }
    
    if(Merge_Trees_From != ""){
      MERGE_OUTPUT<- MERGE_TID_FUN(LAS_oneP, Merge_Trees_From, REMOVE_Plot = "N", oneP_PlotID, MP, FID, Check_Plot= Check_Plot, THIN_OR_ALL_LAS)
      DF_MERGE_TID <- MERGE_OUTPUT[[1]]
      LAS_oneP <- MERGE_OUTPUT[[2]]
    } # END MERGE_TO if-STATEMENT
    
    ##############################################################################################################################################################
    #############
    # SPLIT TREES 
    #############
    
    if(Split_Trees_From != ""){
      SPLIT_OUTPUT<- SPLIT_TREES_FUN(Method = "Vox", LAS_oneP, Split_Trees_From, Split_Trees_To, Split_Trees_Retain, 
                                     MP, FID, oneP_PlotID, FOLDER_LAS, Check_Plot= "Yes", THIN_OR_ALL_LAS, Para_Vx_R_Res) 
      DF_SPLIT_TID <- SPLIT_OUTPUT[[1]]
      LAS_oneP <- SPLIT_OUTPUT[[2]]
    }
    
    ##############################################################################################################################################################
    #############
    # DIAM SPLIT 
    #############
    if(Diam_Split != ""){
      DIAM_SPLIT_OUTPUT <- DIAM_SPLIT_FUN(LAS_oneP, Diam_Split, MP, FID, oneP_PlotID, FOLDER_LAS, Check_Plot= Check_Plot, THIN_OR_ALL_LAS)
      DF_DIAM_SPLIT_TID <- DIAM_SPLIT_OUTPUT[[1]]
      LAS_oneP <- DIAM_SPLIT_OUTPUT[[2]]
    }
    
    ##############################################################################################################################################################
    ##################
    # FILL_ZEROS TREES 
    ##################
    if(Fill_Zeros_Below != ""){
      ZEROS_OUTPUT <- FILL_ZEROS_FUN(LAS_oneP, Fill_Zeros_Below, MP, FID, oneP_PlotID, FOLDER_LAS, Check_Plot= Check_Plot, THIN_OR_ALL_LAS, Para_C_Base_Thresh, Para_C_Base_BW)
      DF_BASE_FILL_ZERO <- ZEROS_OUTPUT[[1]]
      LAS_oneP  <- ZEROS_OUTPUT[[2]]
      }
    
    ##############################################################################################################################################################
    ##################
    # BASE SPLIT TREES 
    ##################
    if(Base_Split != ""){
      Max_TID_ID <- read.csv(paste(FOLDER_O, "/F",FID, "_Max_TID_ID.csv", sep=""))
      Max_TID_ID<- Max_TID_ID$x
      BASE_SPLIT_OUTPUT <- BASE_SPLIT_FUN(Method = "No", LAS_oneP, Base_Split, Base_Count, Max_TID_ID, MP, FID, oneP_PlotID, FOLDER_LAS, Check_Plot= Check_Plot, 
                                          THIN_OR_ALL_LAS, Para_Vx_R_Res, Para_C_Base_BW, Para_C_Base_Thresh)
      DF_BASE_SPLIT_TID <- BASE_SPLIT_OUTPUT[[1]]
      LAS_oneP <- BASE_SPLIT_OUTPUT[[2]]
      Max_TID_ID <- BASE_SPLIT_OUTPUT[[3]]
      }
    
    ##############################################################################################################################################################
    ##########################
    # Remove_Tree
    ##########################
    if(Remove_Tree != ""){
      # REMOVE ALL TREES IF IT IS STRUB WITH NOT OBVIOUS TREES.
      if(Remove_Tree == "ALL"){
        Remove_Tree <- paste(oneP_PlotID, "_", c(0,UNIQUE_TID), sep="")
      }
      print("IN  ... Remove_Tree")
      for(RR in 1:length(Remove_Tree)){
        oneRemove_Tree <- Remove_Tree[RR]
        Remove_PlotID <- numextract_all(oneRemove_Tree)[[1]]
        Remove_TID <- numextract_all(oneRemove_Tree)[[2]]
        
        LAS_oneP <- filter_poi(LAS_oneP, TID != Remove_TID)
        DF_REMOVE_TID[(nrow(DF_REMOVE_TID)+1), ] <- c(Remove_PlotID, Remove_TID )   # <-  Remove_Tree_PlotID Remove_Tree_TID
      }
      one_DIR <-   paste(FOLDER_LAS, Dir_Prefix, oneP_PlotID, Dir_Suffix, sep="")
      print(paste("UPDATING: ", one_DIR))
      writeLAS(LAS_oneP, one_DIR)
      }

    #############################
    # STORING CHANGES IN CSV FILE
    #############################
    if(RUN_MANUAL_CHANGES == "Yes"){
      write.csv(DF_MERGE_TID, paste(FOLDER_UPDATE_QA, "/F",FID,"_MP",MP,"_P",oneP_PlotID , "_Track_Changes_MERGE.csv", sep=""), row.names = FALSE) # Plot_",oneP_PlotID,  "_",
      write.csv(DF_SPLIT_TID, paste(FOLDER_UPDATE_QA, "/F",FID,"_MP",MP,"_P",oneP_PlotID , "_Track_Changes_SPLIT.csv", sep=""), row.names = FALSE) # Plot_",oneP_PlotID, "_", 
      write.csv(DF_REMOVE_TID, paste(FOLDER_UPDATE_QA, "/F",FID,"_MP",MP,"_P",oneP_PlotID , "_Track_Changes_REMOVE_TID.csv", sep=""), row.names = FALSE)
      write.csv(DF_BASE_SPLIT_TID, paste(FOLDER_UPDATE_QA, "/F",FID,"_MP",MP,"_P",oneP_PlotID , "_Track_Changes_BASE_SPLIT.csv", sep=""), row.names = FALSE)
      write.csv(DF_BASE_FILL_ZERO, paste(FOLDER_UPDATE_QA, "/F",FID,"_MP",MP,"_P",oneP_PlotID , "_Track_Changes_FILL_ZERO.csv", sep=""), row.names = FALSE)
      write.csv(DF_DIAM_SPLIT_TID, paste(FOLDER_UPDATE_QA, "/F",FID,"_MP",MP,"_P",oneP_PlotID , "_Track_Changes_DIAM_SPLIT.csv", sep=""), row.names = FALSE)
      
      paste("...............................................................................DF_MERGE_TID"); print(DF_MERGE_TID);
      paste("...............................................................................DF_SPLIT_TID"); print(DF_SPLIT_TID)
      paste("...............................................................................DF_REMOVE_TID"); print(DF_REMOVE_TID)
    }

    if(RUN_RE_SAVE == "Yes"){
      write.csv(DF_MERGE_TID, paste(FOLDER_UPDATE_QA, "/F",FID,"_MP",MP,"_P",oneP_PlotID , "_Track_Changes_MERGE.csv", sep=""), row.names = FALSE) # Plot_",oneP_PlotID,  "_",
      write.csv(DF_SPLIT_TID, paste(FOLDER_UPDATE_QA, "/F",FID,"_MP",MP,"_P",oneP_PlotID , "_Track_Changes_SPLIT.csv", sep=""), row.names = FALSE) # Plot_",oneP_PlotID, "_", 
      write.csv(DF_REMOVE_TID, paste(FOLDER_UPDATE_QA, "/F",FID,"_MP",MP,"_P",oneP_PlotID , "_Track_Changes_REMOVE_TID.csv", sep=""), row.names = FALSE)
      write.csv(DF_BASE_SPLIT_TID, paste(FOLDER_UPDATE_QA, "/F",FID,"_MP",MP,"_P",oneP_PlotID , "_Track_Changes_BASE_SPLIT.csv", sep=""), row.names = FALSE)
      write.csv(DF_BASE_FILL_ZERO, paste(FOLDER_UPDATE_QA, "/F",FID,"_MP",MP,"_P",oneP_PlotID , "_Track_Changes_FILL_ZERO.csv", sep=""), row.names = FALSE)
      write.csv(DF_DIAM_SPLIT_TID, paste(FOLDER_UPDATE_QA, "/F",FID,"_MP",MP,"_P",oneP_PlotID , "_Track_Changes_DIAM_SPLIT.csv", sep=""), row.names = FALSE)
    }
    ##############################################################################################################################################################
    ##############################################################################################################################################################
    ##############################################################################################################################################################
    
    Flag <- 7; print(paste("FINISHED PLOT: ", oneP_PlotID));  print(paste("Flag: ", Flag))   
 #}#else{
    ####################################################
    # CHECKING FINAL RESULTS FOR PLOTS THAT HAVE CHANGED
    #################################################### 
    
    # VISUALISE EACH CHANGED PLOT
    if(RUN_VISUALISE_END_PLOT == "Yes"){
      
      # IF MANUAL CHANGES TAKE PLACE THEN PLOT THE END RESULTS OF ALL THE PLOTS (OTHERWISE ONLY VISUALISE THE SUBJECT PLOT)
      if(RUN_MANUAL_CHANGES == "Yes"){ 
        # GET LIST OF PLOTS (NEIGHBOURS) THAT HAVE BEEN CHANGED DURING THIS PLOT LOOP
        if(Merge_Trees_To != ""){
          all_Merge_PLOTS <- as.numeric(c(unique(numextract_all(Merge_Trees_To)[c(TRUE,FALSE)])))
        }else{
          all_Merge_PLOTS <- ""
          }
        
        if(all(Split_Trees_To != "")){
          Split_Trees_To <- unlist(Split_Trees_To)
          all_Split_PLOTS <- as.numeric(c(unique(numextract_all(Split_Trees_To)[c(TRUE,FALSE)])))
        }else{ 
          all_Split_PLOTS <- ""
          }
        CHANGED_PLOTS <- unique(c(oneP_PlotID, all_Merge_PLOTS, all_Split_PLOTS))
        CHANGED_PLOTS <- CHANGED_PLOTS[which(CHANGED_PLOTS != "")]
      }else{
        # IF JUST RUNNING THE CSV FILES FOR CHANGES THEN ONLY VISUALISE oneP_PlotID
        CHANGED_PLOTS <- oneP_PlotID
      }
  
      if(length(CHANGED_PLOTS) > 0){
        for(NN in 1:length(CHANGED_PLOTS)){
          LAS_oneChanged <- readLAS(paste(FOLDER_LAS, Dir_Prefix ,CHANGED_PLOTS[NN], Dir_Suffix, sep=""), select = "xyzp0")
          PLOT_COL_FUN(LAS_oneChanged, Title=paste("F:", FID, "P:", CHANGED_PLOTS[NN]), size = 5, TID_Number = "Yes", ShowGridID="Yes", Check_Plot= "Yes")
          }
        }
    }
    print(paste("........................................FLIGHT: ", FID , "FINISHED PLOT:", oneP_PlotID))

    } # # LOOP PLOT

  ################################################################################################################################################
  ################################################################################################################################################
  ################################################################################################################################################
  ################################################################################################################################################
  # BACK UP EVERYTHING THAT IS GENERATED IN FOREST_WATER FOLDER
  #############################################################
  
  # if(PP == length(Plot_IDS)){
  #  
  #   Folder_Flight_Backup <- paste( "//uncle/entities/research/ForestWater/CNN", "/Flight_", FID, sep="")
  #   dir.create(Folder_Flight_Backup)
  # 
  #   Folder_Results <- paste("//rcgdata/dj806/CNN/THINNED_DATA_ORIG_CODE_REDO_T3/TRAIN_DATA/Flight_", FID, sep="")
  #   
  #   File_Max_ID_From <- paste(Folder_Results, "/F", FID, "_Max_TID_ID.csv", sep="")
  #   File_Max_ID_TO <- paste(Folder_Flight_Backup, "/F", FID, "_Max_TID_ID.csv", sep="")
  #   file.copy(File_Max_ID_From, File_Max_ID_TO, recursive=FALSE)
  #   
  #   FOLDER_UPDATE_QA <- paste(Folder_Results, "/", FOLDER_QA_INFO, sep="")
  #   file.copy(FOLDER_UPDATE_QA, Folder_Flight_Backup, recursive=TRUE)
  #   
  #   Folder_LAS_From <- paste(Folder_Results, "/LAS/LAS_MP1/",Folder_Prefix,  sep="")
  #   file.copy(Folder_LAS_From, Folder_Flight_Backup, recursive=TRUE)
  #   }
    
  # # CHECK THE RESULTS
  # Folder_Results <- paste("//rcgdata/dj806/CNN/THINNED_DATA_ORIG_CODE_REDO_T3/TRAIN_DATA/Flight_", FID, sep="")
  # Folder_Prefix <- "/LAS_P_ALL"
  # #Dir_Suffix <- "_allT_AttTIDallLAS.laz"
  # 
  # Folder_LAS_From <- paste(Folder_Results, "/LAS/LAS_MP1/",Folder_Prefix,  sep="")
  # FILES_LAS <- list.files(Folder_LAS_From, pattern = Dir_Suffix)
  # for(PP in 1:length(FILES_LAS)){
  #   LAS_ALS_FLIGHT <- readLAS(paste(Folder_LAS_From, "/",FILES_LAS[PP], sep=""), select = "xyzp0")
  #   #maxZ_ALS_Flight <- max(LAS_ALS_FLIGHT$Z)
  #   SHifT_X <- min(LAS_ALS_FLIGHT$X)
  #   SHifT_Y <-   min(LAS_ALS_FLIGHT$Y)
  #   Title <- paste("FLIGHT:", FID, sep="")
  #   PLOT_COL_FUN(LAS_ALS_FLIGHT, Title, size=2, TID_Number = "Yes", ShowGridID="Yes", Check_Plot=  "Yes" )
  #   browser()
  # }
  
  ###########
  #COMMENTS
  ###########
  #FLIGHT 1
  # DOM DOM DOM !!!
  # LAS_ALL FILES NAMES MAY NEED TO BE CHANGED SO THAT  Dir_Suffix <- "_allT_AttTIDallLAS.laz"
  
  #P100 couple missing trees, 100_285 (fill_Z)
  # 101_257 Split(Neigh)
  # 103_320 (Base Sploit)
  # 105_238 (remove)
  # 0_290 (Merge 0_283)
  # split (125_269 to 125_296)
  # Base split (126_310)
  # Base split (127_347)
    # MERGE NEW to 127_499
  # Base split 139_432
  # split (140_332 to 140_335)
  # 141 missed tree (removed and only 0)
  # 153_450 Neigh split
  # 154_469 Neigh split
  # 162_451 Fill Zero
  # 40_28 (Split Neig), 40_23 (Fill Zero)
  # Split 41_14  to 41_10
  # Split 43_40  to 43_63
  # Split (57_100 to Neigh)
  # Remove 69_250 (or Merge Neigh)
  # Base split (88_122), Split (88_122 to 88_119)
  
  #FLIGHT 2
  # 128_247 (Split Neigh)
  
  print(paste("........................................FINISHED FLIGHT:", FID))
  #browser()
} # LOOP FLIGHT


# LAS_Folder <- "//rcgdata/dj806/CNN/THINNED_DATA_ORIG_CODE_REDO/TRAIN_DATA/Flight_2/LAS/LAS_MP2/LAS_P"
# LAS_PLOTS <- list.files(LAS_Folder)
# for(p in 1:length(LAS_PLOTS)){
#   LAS_oneP <- readLAS(paste(LAS_Folder,"/", LAS_PLOTS[p], sep=""), select = "xyzp0")
#   Title = LAS_PLOTS[p]
#   PLOT_COL_FUN(LAS_oneP, Title, size=5)
#   browser()
# }



##############################################################################################################################################################
##############################################################################################################################################################
##############################################################################################################################################################
# UPDATING CSV WITH CHANGES
###########################

# RUN_ADD_CHANGES_TO_EXISTING_CLEANED_CHANGES <- "Yes" # ONLY WHEN RUN_MANUAL_CHANGES == "Yes" WILL THE ADD CHANGES (i.e. RUN_ADD_CHANGES_TO_EXISTING_CLEANED_CHANGES) HAPPEN WITH "Yes" 


# if(RUN_ADD_CHANGES_TO_EXISTING_CLEANED_CHANGES == "Yes" & RUN_MANUAL_CHANGES == "Yes"){
#   ### DOM DOM DOM !!! COME UP WITH A WAY TO ADD TO THE EXISTING CHANGES.
#   DF_MERGE_TID_EXISTING <- read.csv( paste(FOLDER_UPDATE_QA, "/F",FID,"_MP",MP,"_P",oneP_PlotID , "_Track_Changes_MERGE.csv", sep="")) # Plot_",oneP_PlotID,  "_",
#   DF_MERGE_TID <- rbind(DF_MERGE_TID_EXISTING, DF_MERGE_TID)
#   
#   DF_SPLIT_TID_EXISTING <- read.csv(paste(FOLDER_UPDATE_QA, "/F",FID,"_MP",MP,"_P",oneP_PlotID , "_Track_Changes_SPLIT.csv", sep="")) # Plot_",oneP_PlotID, "_", 
#   if(ncol(DF_SPLIT_TID_EXISTING) == 4){
#     DF_SPLIT_TID_EXISTING$Split_Trees_Retain <- "Y"
#   }
#   DF_SPLIT_TID <- rbind(DF_SPLIT_TID_EXISTING, DF_SPLIT_TID)
#   
#   DF_REMOVE_TID_EXISTING <- read.csv(paste(FOLDER_UPDATE_QA, "/F",FID,"_MP",MP,"_P",oneP_PlotID , "_Track_Changes_REMOVE_TID.csv", sep=""))
#   DF_REMOVE_TID <- rbind(DF_REMOVE_TID_EXISTING, DF_REMOVE_TID)
#   
#   DF_BASE_SPLIT_TID_EXISTING <- read.csv(paste(FOLDER_UPDATE_QA, "/F",FID,"_MP",MP,"_P",oneP_PlotID , "_Track_Changes_BASE_SPLIT.csv", sep=""))
#   if(ncol(DF_BASE_SPLIT_TID_EXISTING) == 3){
#     DF_BASE_SPLIT_TID_EXISTING$Base_Count <- 2
#   }
#   DF_BASE_SPLIT_TID <- rbind(DF_BASE_SPLIT_TID_EXISTING, DF_BASE_SPLIT_TID)
#                          
#   DF_BASE_FILL_ZERO_EXISTING <- read.csv(paste(FOLDER_UPDATE_QA, "/F",FID,"_MP",MP,"_P",oneP_PlotID , "_Track_Changes_FILL_ZERO.csv", sep=""))
#   DF_BASE_FILL_ZERO <- rbind(DF_BASE_FILL_ZERO_EXISTING, DF_BASE_FILL_ZERO)
#   
#   DF_DIAM_SPLIT_TID_EXISTING <- read.csv(paste(FOLDER_UPDATE_QA, "/F",FID,"_MP",MP,"_P",oneP_PlotID , "_Track_Changes_DIAM_SPLIT.csv", sep=""))
#   DF_DIAM_SPLIT_TID <- rbind(DF_DIAM_SPLIT_TID_EXISTING, DF_DIAM_SPLIT_TID)
#   
#   # MERGE THEM SO THAT YOU ADD TO CHANGES
# }


