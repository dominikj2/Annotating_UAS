
# Comp <- "//rcgdata/dj806/"
# Packages_Required <- list.functions.in.file(paste(Comp, "R_Code/FUNCTIONS_CNN.R", sep=""), alphabetic = TRUE)
##########################################################################################################################################################################################
###FUNCTIONS IN THIS SCRIPT
##########################################################################################################################################################################################
# Morph_reconstruct
# FUNCTION FOR WEIGHTED MSE LOSS
# XYZ_Extent_XYZWLHR_FUN
# XYZ_Extent_Summary_FUN
# XYZWLHR_To_XYZWLHR_N_FUN
# XYZWLHR_To_Goffset_FUN
# Goffset_To_XYZWLHR_FUN
# SCALE_PLOT2RoI_VERT_FUN
# SCALE_RoI2PLOT_FUN
# SCALE_RoI2Plot_Coord_FUN
# XYZWLHR_NORM_to_COORD_FUN
# RoI_NORM_to_COORD_FUN
# PLOT_LAS_FUN
# PLOT_LAS_XYZWLHR_FUN
# PLOT_Ver1_Vert2_VOX_FUN
# PLOT_GT_PRIOR_VOX_FUN
# PLOT_GT_PRIOR_VOX_RoI_FUN
# SCALE_LAS_FUN
# EMPTY_VOX_FUN
# EMPTY_VOX_FUN2
# EMPTY_RoI_VOX_FUN
# TENSOR_LAS_RoI_FUN
# TENSOR_LAS_PLOT_FUN
# REVERSE_ENG_TENSOR_FUN
# DICE_LOSS_FUN
# VOX_IoU_LOSS_FUN
# FSCORE_FUN
# FSCORE_FUN
# BINARY_ACCURACY_FUN
# Peak_Dip_FUN
# GAP_DENSITY_FUNCTION_NEW2
# TRIANG_ID_ALL_FUN
# LIST_ROI_NORM_FUN
# rad2deg
# deg2rad
# XYZWLHR_FUN
# GEN_ROI_VERTICES_FUN
# XYZWHR_TO_VERT_GPU_FUN
# XYZWHR_TO_CORNER_GPU_FUN
# BBox_Sqr_FUN
# VERT_To_XYZWLHR_FUN
# INTERSECT_TRI_FUN
# index_Order_Function
# TRI_ID_NODES_FUN
# BBOX_PNTS_FUN
# VERTICIES_ORDER_FUN
# PLOT_CNN_OUTPUT_FUN

#############################################################################################################################################
# SCALE_PLOT2RoI_VERT_GPU_FUN
SCALE_PLOT2RoI_VERT_GPU_FUN <- function(RoI_Dec, XYZWLHR, OUT_VERT_or_XYZWLHR = "XYZWLHR", Plot_N=b, Para_Cnt = 10, use_Tensor = "Yes", Para_Base_WL = 0.5, Para_Target_Base=16,
                                        Col_Name = Colnames_XYZWLHR, Normalise= "Yes", device=device, LAS_oneP_ExtP_T, Title_Plot){
  RoI_XYZWLHR_T = list()
  
  for(r in 1:RoI_Dec$size(2)){  # dim(RoI_Dec)[2]
    
    Vertices_ExtP <- XYZWHR_TO_VERT_GPU_FUN(XYZWLHR[r,], Base_WL = Para_Base_WL/Para_Target_Base, device=device) #  GPU
    
    oneRoI_Dec = RoI_Dec[Plot_N,r,]
    xxB <- oneRoI_Dec[2]
    yyB <- oneRoI_Dec[4]
    zzB <- oneRoI_Dec[6]
    xxT <- oneRoI_Dec[3]
    yyT <- oneRoI_Dec[5]
    zzT <- oneRoI_Dec[7]
    
    x_oneP <- Vertices_ExtP[,1] #$X # USING THE PLOT EXTENT HERE     # X_Base	X_BotBox	 X_TopBox 		 
    x_New <- (x_oneP-xxB)/(xxT-xxB)
    
    y_oneP <- Vertices_ExtP[,2] #$Y                                  # Y_Base Y_BotBox	 Y_TopBox 	 (Length_Y)	 
    y_New <-  (y_oneP-yyB)/(yyT-yyB)
    
    z_oneP <- Vertices_ExtP[,3] #$Z                                   # Z_Base Z_BotBox Z_TopBox H_TopBox  
    z_New <-  (z_oneP-zzB)/(zzT-zzB)
    
    Vertices_ExtR <- torch_stack(c(x_New, y_New, z_New), dim=2) # $to(device = device)
  
    if(OUT_VERT_or_XYZWLHR == "Vert"){
      browser() # DOM DOM DOM THIS NEEDS TO BE CHANGED FOR GPU
      RoI_XYZWLHR_T <- list.append(RoI_XYZWLHR_T, Vertices_ExtR)
    }else{
      #browser()
      # print(paste("R:", r))
      XYZWLHR_NewBox <- VERT_To_XYZWLHR_GPU_FUN(Vertices_ExtR, Para_Cnt = Para_TriShpParaCnt, Col_Name, Normalise = "Yes", device=device, RoI2Plot_or_Plot2RoI = "Plot2RoI" ) #  torch_zeros(1, dtype = torch_bool()) for "No" ....or .....torch_ones(1, dtype = torch_bool(), device=device) for "yes"
      
      XYZWLHR_NewBox <- torch_unsqueeze(XYZWLHR_NewBox, dim=1)
      if(r== 1){
        RoI_XYZWLHR_T <- XYZWLHR_NewBox
      }else{
        RoI_XYZWLHR_T <- torch_cat(c(RoI_XYZWLHR_T, XYZWLHR_NewBox), dim=1L)   
      }
    }
  } # R LOOP
  return(RoI_XYZWLHR_T)
}

#############################################################################################################################################
# SCALE_RoI2PLOT_GPU_FUN
SCALE_RoI2PLOT_GPU_FUN <- function(RoI_Dec, XYZWLHR, Plot_N=b, Para_Cnt = 10,  Normalised = "Yes",
                                   IN_VERT_or_XYZWLHR = "XYZWLHR", OUT_VERT_or_XYZWLHR = "XYZWLHR", device=device, LAS_oneP_ExtP_T, Title_Plot){
  for(r in 1:RoI_Dec$size(2) ){ # dim(RoI_Dec)[2]
    oneR_XYZWLHR <- XYZWLHR[r,]
    if(IN_VERT_or_XYZWLHR == "XYZWLHR"){
      Vertices_ExtR <- XYZWHR_TO_VERT_GPU_FUN(oneR_XYZWLHR, Base_WL = Para_Base_WL/Para_Target_Base, Normalised = Normalised, Para_Cnt = Para_Cnt, device=device)
    }else{
      Vertices_ExtR <- oneR_XYZWLHR  
    }

    oneRoI_Dec = RoI_Dec[Plot_N,r,]

    xxB <- oneRoI_Dec[2] 
    yyB <- oneRoI_Dec[4] 
    zzB <- oneRoI_Dec[6] 
    xxT <- oneRoI_Dec[3] 
    yyT <- oneRoI_Dec[5] 
    zzT <- oneRoI_Dec[7]
    #
    x_oneP <- Vertices_ExtR[,1] #$X # USING THE PLOT EXTENT HERE     # X_Base	X_BotBox	 X_TopBox 		  c(1, 4,  10)   
    x_New <- x_oneP*(xxT-xxB)+xxB
    y_oneP <- Vertices_ExtR[,2] #$Y                                  # Y_Base Y_BotBox	 Y_TopBox 	 (Length_Y)	 c(2, 5, 6, 11, 12) 
    y_New <-   y_oneP*(yyT-yyB)+yyB
    z_oneP <- Vertices_ExtR[,3] #$Z                                   # Z_Base Z_BotBox Z_TopBox H_TopBox  c(3, 8, 14, 16)
    z_New <-  z_oneP*(zzT-zzB)+zzB
    #
    Vertices_ExtP <- torch_stack(c(x_New, y_New, z_New), dim=2) # $to(device = "cpu")
    
    if(OUT_VERT_or_XYZWLHR == "Vert"){
      RoI_XYZWLHR_T <- list.append(RoI_XYZWLHR_T, Vertices_ExtP) ### DOM DOM DOM !!! YOU NEED TO FIX THIS FOR GPU ... 
    }else{

      # WORK AROUND MAKING SURE DEVICES ARE CONSISTEN IN COMPUTATIONS 09/05/2022
        # MOVE DEVICE TO CUDA AND THEN BACK TO CPU
      device_Temp <- torch_device(paste("cuda:", cuda_current_device())) 
      Vertices_ExtP <-Vertices_ExtP$to(device = device_Temp)
      XYZWLHR_NewBox <- VERT_To_XYZWLHR_GPU_FUN(Vertices_ExtP, Para_Cnt = Para_TriShpParaCnt, Col_Name, Normalise= "Yes", device = device_Temp, RoI2Plot_or_Plot2RoI = "RoI2Plot" ) # "Yes" is torch_ones(1, dtype = torch_bool(), device=device)
      XYZWLHR_NewBox <- XYZWLHR_NewBox$to(device = "cpu")
      if(r== 1){
        RoI_XYZWLHR_T <- XYZWLHR_NewBox$unsqueeze(1)
      }else{
        RoI_XYZWLHR_T <- torch_cat(c(RoI_XYZWLHR_T, XYZWLHR_NewBox$unsqueeze(1)), dim=1L)
      }
    }
  }
  return(RoI_XYZWLHR_T)
}

#############################################################################################################################################
# XYZWHR_TO_VERT_GPU_FUN
XYZWHR_TO_VERT_GPU_FUN  <- function(XYZWHRT_Data = oneTriShp, Base_WL = 0.5, Normalised = "Yes", Para_Cnt = 10, device=device){
  
  for(BB in 1:2) { # BB_loop

    if(BB == 1){ 
      Cent_X <- XYZWHRT_Data[1] 
      Cent_Y <- XYZWHRT_Data[2] 
      Length_Y <- torch_tensor(Base_WL, device=device) 
      Width_X <-torch_tensor(Base_WL, device=device) 
      Rotate_Deg <-torch_tensor(0, device=device) 
      Height <-XYZWHRT_Data[3] 
    }else{
      # CHANGDE FOR H_TopBox (24/09/2021)
      #  "X_Base"    "Y_Base"    "Z_Base"    "X_TopBox"  "Y_TopBox"  "Z_TopBox" (Centre of canopy)  "L_TopBox"  "W_TopBox"  "H_TopBox"(Depth of canopy) "R_TopBox" 
      Cent_X <- XYZWHRT_Data[4] 
      Cent_Y <- XYZWHRT_Data[5] 
      Length_Y <- XYZWHRT_Data[7] # [6] 
      Width_X <-XYZWHRT_Data[8] #[7] 
      Height <- XYZWHRT_Data[6] - 0.5*XYZWHRT_Data[9] # OLDER VERSION: XYZWHRT_Data[8]  
      Rotate_Deg <-XYZWHRT_Data[10] # [9] 
      # 
      if(BB==2){ #BB_loop
        Tree_Height <- XYZWHRT_Data[6] + 0.5*XYZWHRT_Data[9]  # XYZWHRT_Data[10] 
      }
      if(Normalised == "Yes"){Rotate_Deg <- Rotate_Deg*180}
    }
    
    # translate point to origin
    tempX_noRotate <- torch_stack(c(Cent_X-(0.5*Width_X), Cent_X-(0.5*Width_X), Cent_X+(0.5*Width_X),  Cent_X+(0.5*Width_X)))
    tempY_noRotate <- torch_stack(c(Cent_Y-(0.5*Length_Y), Cent_Y+ (0.5*Length_Y), Cent_Y+(0.5*Length_Y), Cent_Y- (0.5*Length_Y)))
    
    # MOVE BOX TO CENTRE BEFORE APPLYING ROTATION CALCULATION
    tempX_noRot_ZeroOrigin = torch_sub(tempX_noRotate, other=Cent_X) 
    tempY_noRot_ZeroOrigin = torch_sub(tempY_noRotate, other=Cent_Y) 
    
    Rotate_Rad <- Rotate_Deg$deg2rad() 
    
    rotatedX_ZeroOrigin = tempX_noRot_ZeroOrigin*torch_squeeze(torch_cos(Rotate_Rad)) - tempY_noRot_ZeroOrigin*torch_squeeze(torch_sin(Rotate_Rad));
    rotatedY_ZeroOrigin = tempX_noRot_ZeroOrigin*torch_squeeze(torch_sin(Rotate_Rad)) + tempY_noRot_ZeroOrigin*torch_squeeze(torch_cos(Rotate_Rad));

    # translate back
    XNew <- torch_squeeze(torch_add(rotatedX_ZeroOrigin,Cent_X))
    YNew <- torch_squeeze(torch_add(rotatedY_ZeroOrigin,Cent_Y))

    Rep_Cnt <- torch_tensor(dim(tempX_noRot_ZeroOrigin)[1], dtype = torch_long(), device = device) #$to() # $to(device = device)
    ZNew <- torch_squeeze(torch_repeat_interleave(torch_unsqueeze(Height, dim=1), Rep_Cnt)) # $to(device = device)

    XYZ <- torch_stack(c(XNew, YNew, ZNew), dim=2)
    # browser()
    if(BB == 1){
      Vertices <- torch_clone(XYZ)
    }else{
      Vertices <- torch_cat(c(Vertices, XYZ))
    }
  }

  # ADD THE VERY TOP BOX
  Rep_Cnt <- torch_tensor(dim(tempX_noRotate)[1], dtype = torch_long(), device = device) # $to(dtype = torch_long())$to(device = device)
  ZNew <- torch_squeeze(torch_repeat_interleave(torch_unsqueeze(Tree_Height, dim=1), Rep_Cnt)) # $to(device = device)
  XYZ <- torch_stack(c(XNew, YNew, ZNew), dim=2)
  Vertices <- torch_cat(c(Vertices, XYZ))

  return(Vertices)
}

#############################################################################################################################################
# XYZWHR_TO_VERT_GPU_FUN     ..... DOM DOM DOM !!! NOT SURE IF YOU NEED TO ORDER THESE VERTICIES FoR CORRECT REPRESENTATION ... SEE "VERTICIES_ORDER_FUN"
XYZWHR_TO_CORNER_GPU_FUN  <- function(XYZWHRT_Data = oneTriShp, Normalised = "Yes", device=device){
    # "X_TopBox",  "Y_TopBox",  "Z_TopBox",  "L_TopBox",  "W_TopBox" , "H_TopBox", "R_TopBox"
    B = XYZWHRT_Data$size(1)
    #XYZWHRT_Data <- XYZWHRT_Data[1,]
    XYZWHRT_Data <- torch_squeeze(XYZWHRT_Data)
    Cent_X <- XYZWHRT_Data[..,1] 
    Cent_Y <- XYZWHRT_Data[..,2] 
    Length_Y <- XYZWHRT_Data[..,4] # [6] 
    Width_X <-XYZWHRT_Data[..,5] #[7] 
    Rotate_Deg <-XYZWHRT_Data[..,7] # [9] 

    if(Normalised == "Yes"){Rotate_Deg <- Rotate_Deg*180}

    
    # translate point to origin
    tempX_noRotate <- torch_stack(c(Cent_X-(0.5*Width_X), Cent_X-(0.5*Width_X), Cent_X+(0.5*Width_X),  Cent_X+(0.5*Width_X)))
    tempY_noRotate <- torch_stack(c(Cent_Y-(0.5*Length_Y), Cent_Y+ (0.5*Length_Y), Cent_Y+(0.5*Length_Y), Cent_Y- (0.5*Length_Y)))
    
    # MOVE BOX TO CENTRE BEFORE APPLYING ROTATION CALCULATION
    tempX_noRot_ZeroOrigin = torch_sub(tempX_noRotate, other=Cent_X) 
    tempY_noRot_ZeroOrigin = torch_sub(tempY_noRotate, other=Cent_Y) 
    
    Rotate_Rad <- Rotate_Deg$deg2rad() 
    
    rotatedX_ZeroOrigin = tempX_noRot_ZeroOrigin*torch_squeeze(torch_cos(Rotate_Rad)) - tempY_noRot_ZeroOrigin*torch_squeeze(torch_sin(Rotate_Rad));
    rotatedY_ZeroOrigin = tempX_noRot_ZeroOrigin*torch_squeeze(torch_sin(Rotate_Rad)) + tempY_noRot_ZeroOrigin*torch_squeeze(torch_cos(Rotate_Rad));
    
    # translate back
    XNew <- torch_squeeze(torch_add(rotatedX_ZeroOrigin,Cent_X))
    YNew <- torch_squeeze(torch_add(rotatedY_ZeroOrigin,Cent_Y))
    Vertices <- torch_stack(c(XNew, YNew), dim=2)
    
    # TROUBLE SHOOT WHILST TESTING CODE
    if(length(dim(Vertices)) == 2){
      Vertices <- Vertices$unsqueeze(3)
    }
    # print(paste(c("PERMUTE DIM:", dim(Vertices))))
    # browser()
    Vertices <- Vertices$permute(c(3,1,2))
    Vertices <- Vertices$view(c(B,-1,4,2))
    #Vertices = Vertices$view(c(B,-1,4,2)) # REPLACED THIS ALONE
    
    
    # ### doM DOM DOM YOU NEED TO GENERATE 2D PLOTS TO SEE IF THEY ARE GOING IN CORRECT ORDER.....
    # dim(Vertices)    
    # for(VV in 1:dim(Vertices)[2]){
    #   oneVertices <- as.data.frame(as.array(Vertices[,VV,,]$to(device= "cpu")$squeeze()))
    #   oneVertices$Index <- 1:4
    #   plot(oneVertices$V1, oneVertices$V2, pch='.')
    #   text(oneVertices$V1, oneVertices$V2, oneVertices$Index)
    #   browser()
    # }
    
# #################    
# # TESTING RESULTS  
# #################  
#   
# oneP_Vertices <- Vertices[,1,,]
# oneP_XYZWHRT_Data <- XYZWHRT_Data[1,]
# oneP_XYZWHRT_Data2 <-torch_cat(c(torch_tensor(c(0,0,0), device=device),c(oneP_XYZWHRT_Data)), dim= 1)
# browser()
# oneP_Vertices_Test <- XYZWHR_TO_VERT_GPU_FUN(oneP_XYZWHRT_Data2, Base_WL = 0.5, Normalised = "Yes", Para_Cnt = 10, device=device)
    
    
    
  return(Vertices)
}

#############################################################################################################################################
# VERT_To_XYZWLHR_GPU_FUN
VERT_To_XYZWLHR_GPU_FUN  <- function(Vertices, Para_Cnt = 10, Col_Name= Colnames_XYZWLHR, Normalise = "Yes", device = device, RoI2Plot_or_Plot2RoI= "Plot2RoI"){ #torch_ones(1, dtype = torch_bool(), device=device)
  
  Levl_Vec <- c(1, 2) # , 3 # (CHANGED 24/09/2021) FOR H_TopBox
  Level = torch_repeat_interleave(torch_tensor(c(1, 2, 3), device=device, dtype = torch_long()), torch_tensor(4L, device=device))

  # List_Vert_CIoU <- list(as.array(Vertices$to(device ="cpu"))) # ,as.array(Vert_onePred$to(device ="cpu"))
  # #
  # Title_Plot <- paste("LA", sep="")
  # #
  # LAS_Test <- LAS(data.frame(X = 0.5, Y=0.5, Z=0.5))
  # LAS_Test@data$TID <- 1
  # #
  # PLOT_Ver1_Vert2_VOX_FUN(LAS_Test, List_Vert_CIoU, Title_Plot, colours="red", Normalised = "No", Plot_Colour_Att = "TID")
  
  for(L in 1:max(Levl_Vec)){
    #browser()
    #print(paste("L: ",L))
    Index = torch_where( Level == L, 1, 0)$to(dtype = torch_bool()) 
    oneLevel_Vertices_Paralal <- Vertices[Index,]
    
    oneLevel_Vertices <- torch_clone(oneLevel_Vertices_Paralal)
    # browser()
    # SEE IF VERTICES MAKE UP RIGHT ANGLE RECTANGLE
    Bearing1 <- BEARING_GPU_FUN(oneLevel_Vertices_Paralal, P1=c(1,2))
    Bearing2 <- BEARING_GPU_FUN(oneLevel_Vertices_Paralal, P1=c(2,3)) 
    Angle_B1_B2 <- torch_abs(Bearing2 - Bearing1)
    if(!torch_equal(Angle_B1_B2, torch_tensor(90, device=device)$squeeze())){
      if(!torch_equal(Angle_B1_B2, torch_tensor(0, device=device)$squeeze())){
        Distance <- torch_cdist(oneLevel_Vertices[1:3,1:2],oneLevel_Vertices[1:3,1:2])$to(device = "cpu") 
        if(as.logical(Distance[1,2] < Distance[1,3])){
          oneLevel_Vertices <- oneLevel_Vertices[c(2,3,4,1),]
          oneLevel_Vertices <- PARALLELOGRAM_TO_SQUARE_RECTANGLE_FUN(oneLevel_Vertices)
          oneLevel_Vertices <- oneLevel_Vertices[c(4,1,2,3),]
        }else{
          oneLevel_Vertices <- PARALLELOGRAM_TO_SQUARE_RECTANGLE_FUN(oneLevel_Vertices)
        }
      }
    }

    if(L == 1){ 
      Centre_X <- torch_mean(oneLevel_Vertices[,1]) 
      Centre_Y <- torch_mean(oneLevel_Vertices[,2]) 
      Centre_Z <- oneLevel_Vertices[1,3] 
      BBox <- torch_stack(c(Centre_X, Centre_Y, Centre_Z)) 
      All_BBox <- torch_clone(BBox)
    }else{  
      # if(L == max(Levl_Vec)){   # (CHANGED 24/09/2021) FOR H_TopBox
      #   Tree_Top <- oneLevel_Vertices[1,3] 
      #   All_BBox <- torch_cat(c(All_BBox, torch_unsqueeze(Tree_Top,1))) 
      #   
      # }else{
      #
     
      if(RoI2Plot_or_Plot2RoI == "Plot2RoI"){
        bearing_deg <- BEARING_GPU_FUN(oneLevel_Vertices, P1 = c(1,2))
        Length_Y  <- torch_cdist(oneLevel_Vertices[1,1:2]$view(c(1,2)), oneLevel_Vertices[2,1:2]$view(c(1,2)))$squeeze()
        Width_X  <- torch_cdist(oneLevel_Vertices[2,1:2]$view(c(1,2)), oneLevel_Vertices[3,1:2]$view(c(1,2)))$squeeze()
      }else{
        bearing_deg <- BEARING_GPU_FUN(oneLevel_Vertices, P1 = c(2,3))
        Length_Y  <- torch_cdist(oneLevel_Vertices[2,1:2]$view(c(1,2)), oneLevel_Vertices[3,1:2]$view(c(1,2)))$squeeze()
        Width_X <- torch_cdist(oneLevel_Vertices[1,1:2]$view(c(1,2)), oneLevel_Vertices[2,1:2]$view(c(1,2)))$squeeze()
      }
      Box_R = bearing_deg$view(1)  
      #

      #
      if(Normalise == "Yes"){ Box_R <- Box_R/180 }

      BoxCent_X <- torch_mean(oneLevel_Vertices[,1])
      BoxCent_Y <- torch_mean(oneLevel_Vertices[,2])
      #Box_Z <- oneLevel_Vertices[1,3] # (CHANGED 24/09/2021) FOR H_TopBox
      BoxCent_Z <- torch_mean(Vertices[5:12,3])
      Height_Z <- torch_mean(Vertices[9:12,3]) - torch_mean(Vertices[5:8,3])

      # All_BBox<- torch_cat(c(All_BBox, torch_unsqueeze(BoxCent_X,1),  # (CHANGED 24/09/2021) FOR H_TopBox
      #                        torch_unsqueeze(BoxCent_Y,1), 
      #                        torch_unsqueeze(Length_Y,1), 
      #                        torch_unsqueeze(Width_X,1), 
      #                        torch_unsqueeze(Box_Z,1), 
      #                        Box_R))
      All_BBox<- torch_cat(c(All_BBox, torch_unsqueeze(BoxCent_X,1),
                             torch_unsqueeze(BoxCent_Y,1),
                             torch_unsqueeze(BoxCent_Z,1),
                             torch_unsqueeze(Length_Y,1),
                             torch_unsqueeze(Width_X,1),
                             torch_unsqueeze(Height_Z,1),
                             Box_R))
      
      #}
    } # LOOP L > 2
  } # L LOOP (THROUGH LEVELS)
  return(All_BBox) 
}

#############################################################################################################################
# BEARING_GPU_FUN: CALCULATE THE BEARING/ANGLE BETWEEN TWO INTERSECTING LINES IN BOUNDING BOX  # https://www.igismap.com/formula-to-find-bearing-or-heading-angle-between-two-points-latitude-longitude/

BEARING_GPU_FUN <- function(XY_Vertices, P1){  #, P2 = c(1,3)
  dL = XY_Vertices[P1[2],1] - XY_Vertices[P1[1],1]
  X = torch_cos(XY_Vertices[P1[2],2]*pi/180)*torch_sin(dL*pi/180) # =COS(39.099912*pi/180)
  Y = (torch_cos(XY_Vertices[P1[1],2]*pi/180)*torch_sin(XY_Vertices[P1[2],2]*pi/180)) - 
    (torch_sin(XY_Vertices[P1[1],2]*pi/180)*torch_cos(XY_Vertices[P1[2],2]*pi/180)*torch_cos(dL*pi/180))
  bearing_rad <- torch_atan2(X,Y)
  bearing_deg <- (bearing_rad *180)/pi
  bearing_deg <- torch_round(bearing_deg)   # DOM DOM DOM !!! SEE torch_angle
  # browser()
  if(as.logical((bearing_deg < torch_tensor(0, device=device))$to(device="cpu"))){bearing_deg <- bearing_deg + 180 }
  return(bearing_deg) 
}

#############################################################################################################################
# PARALALAGRAM_TO_SQUARE_RECTANGLE_FUN: CALCULATE NEW VERTICES THAT ARE A BOX USING PARALLELOGRAM VERICES
PARALLELOGRAM_TO_SQUARE_RECTANGLE_FUN <- function(oneLevel_Vertices){
  End_Points <- list(c(1,2,4), c(3, 4,2))
  
  for(a in 1:length(End_Points)){
    
    P1 <- oneLevel_Vertices[End_Points[[a]][1],]
    P2 <- oneLevel_Vertices[End_Points[[a]][2],]
    P3 <- oneLevel_Vertices[End_Points[[a]][3],]
    #
    b1 <- 1
    b2 <- 1
    m1 <- (P2[2]-P1[2])/(P2[1]-P1[1]) 
    a1 <- -m1
    c1 <- -b1*P2[2] - a1*P2[1]
    m2 <- -1/(m1) 
    a2 <- -m2
    c2 <- -b2*P3[2] - a2*P3[1]

    # INTERCEPT OF PERPENDICULAR LINE (POINT P3 TO LINE)
    XO = (b1*c2 - b2*c1)/(a1*b2 - a2*b1)
    YO = (c1*a2 - a1*c2)/(a1*b2 - a2*b1)
    #
    Y1 <- P1[2] - 0.5*(P1[2] - YO)
    X1 <- P1[1] - 0.5*(P1[1] - XO)
    #
    Y2 <- P3[2] + 0.5*(P1[2] - YO) 
    X2 <- P3[1] + 0.5*(P1[1] - XO)  
    
    oneLevel_Vertices[End_Points[[a]][1],c(1,2)] <- torch_cat(c(X1, Y1))  #(a*(a-1)+1)
    oneLevel_Vertices[End_Points[[a]][3],c(1,2)] <- torch_cat(c(X2, Y2)) # (a*(a-1)+2)
  }
  return(oneLevel_Vertices)
}

#############################################################################################################################################
# PLOT_Ver1_Vert2_VOX_FUN
PLOT_Ver1_Vert2_VOX_FUN <- function( LAS_Vox, List_Vert, Title_Plot, colours = c( "white",  "green", "red"), 
                                     Normalised = "Yes", Para_Cnt = 10, Plot_Colour_Att){

  LAS_oneP_Vox <- filter_poi(LAS_Vox, TID > 0)
  unique_TID <- unique(LAS_oneP_Vox@data$TID)
  
  if(length(unique_TID) > 0){
    palette <- sample(distinctColorPalette(length(unique_TID)))
    
    # ADD ONE POINT WITH XYZ ALL ZERO.
    LAS_one000_Vox <- LAS_oneP_Vox
    LAS_one000_Vox@data <- LAS_one000_Vox@data[1,]
    LAS_one000_Vox@data$X <- 0; LAS_one000_Vox@data$Y <- 0; LAS_one000_Vox@data$Z <- 0; 
    LAS_oneP_Vox@data <- rbind(LAS_oneP_Vox@data, LAS_one000_Vox@data)
    
    n <- length(unique(LAS_oneP_Vox@data$TID))
    palette <- sample(distinctColorPalette(n))
    # browser()
    # GENERATE ATTRIBUTE FOR COLOURING EACH POINT
    LAS_oneP_Vox@data$Color <- as.character("")
    Color_ID <- data.frame(Unique_TID = unique(LAS_oneP_Vox@data$TID),
                           ID_TID = palette)
    Index_Color_TID_1 <- which(LAS_oneP_Vox@data$TID %in% Color_ID$Unique_TID)
    Index_Color_TID_2 <- match(LAS_oneP_Vox@data$TID[Index_Color_TID_1],
                               Color_ID$Unique_TID)
    LAS_oneP_Vox@data$Color[Index_Color_TID_1] <- as.character(Color_ID$ID_TID[Index_Color_TID_2])
    
    if(Plot_Colour_Att == "TID"){
      plot(LAS_oneP_Vox, color="Color", size=3)
    } else{
      plot(LAS_oneP_Vox, color=Plot_Colour_Att, size=6)
    }
    
    Line_Width <- rep(3, length(List_Vert))
    # browser()
    for(LL in 1:length(List_Vert)){
      one_List_Vert <- List_Vert[[LL]]
      polygon3d(one_List_Vert[1:4,], fill=FALSE, col=colours[[LL]], lwd  = Line_Width[LL])
      polygon3d(one_List_Vert[5:8,], fill=FALSE, col=colours[[LL]], lwd  = Line_Width[LL])
      polygon3d(one_List_Vert[9:12,], fill=FALSE, col=colours[[LL]], lwd  = Line_Width[LL])
      # print(LL)
    }
    text3d(max(LAS_oneP_Vox$X), max(LAS_oneP_Vox$X), max(LAS_oneP_Vox@data$Z),
           col="white",
           size = 8,
           Title_Plot)
    if(Normalised == "Yes"){
      lines3d(x=c(0, 1), y=c(0,0), z=c(0,0), col="purple", lwd = 3)
      lines3d(x=c(0, 1), y=c(0,0), z=c(1,1), col="purple", lwd = 3, lty=3)
      lines3d(x=c(0, 0), y=c(0,1), z=c(0,0), col="blue", lwd = 3)
      lines3d(x=c(0, 0), y=c(0,1), z=c(1,1), col="blue", lwd = 3, lty=3)
    }
  }
} # END PLOT_Ver1_Vert2_VOX_FUN
############################################################################################################################################
# XYZWLHR_To_FROM_Goffset_GPU_FUN ....  REPLACES .... XYZWLHR_To_Goffset_GPU_FUN   Goffset_To_XYZWLHR_GPU_FUN

# SEE file:///D:/DOCUMENTS/PDF_FILES/Xu_Jun_3d_2019_GIoU.pdf   
# 3D-GIoU: 3D Generalized Intersection over Union for Object Detection in Point Cloud
# GEORGE: LETS LOOK AT THE LOSS FUNCTION HERE AND SEE IF I AM USING IT APPROPRIATELY

# Goffset_To_XYZWLHR_GPU_FUN  ... , To_FROM="FROM"
# XYZWLHR_To_Goffset_GPU_FUN ... , To_FROM="TO"
XYZWLHR_To_FROM_Goffset_GPU_FUN <- function(Input_XYZWLHR, Prior_XYZWLHR, Para_Cnt = Para_Cnt, device=device, To_FROM="TO"){

  if(Para_Cnt == 7){
    Index_XY_Box <- torch_tensor(as.integer(c(1,2))) 
    Index_Z_Box <- torch_tensor(as.integer(c(3))) 
    Index_L <- torch_tensor(as.integer(4))
    Index_W <- torch_tensor(as.integer(5)) 
    Index_H <- torch_tensor(as.integer(6))
    Index_R <- torch_tensor(as.integer(7)) 

  }else{
    Index_XYZ_Base <- torch_tensor(as.integer(c(1,2,3))) 
    # Index_XYZ_Box <- torch_tensor(as.integer(c(4,5,6)))
    Index_XY_Box <- torch_tensor(as.integer(c(4,5))) 
    Index_Z_Box <- torch_tensor(as.integer(c(6))) 
    Index_L <- torch_tensor(as.integer(7))
    Index_W <- torch_tensor(as.integer(8)) 
    Index_H <- torch_tensor(as.integer(9))
    Index_R <- torch_tensor(as.integer(10)) 
  }
  
  # YOLO 9000 uses sigmoid to deal with centroid location https://stats.stackexchange.com/questions/403741/what-is-sigma-function-in-the-yolo-object-detector and https://arxiv.org/abs/1612.08242
  # XYZWLHR_XYZ_Base_Box <- torch_sigmoid(Input_XYZWLHR_XYZWLHR_[..,Index_XYZ_Base_Box]) + Prior_XYZWLHR[..,Index_XYZ_Base_Box] # SOME HAVE MULTIPLY XYZWLHR_ by Prior_L and Prior_W (but we do not have aligned boxes)
  # Goffset_XYZ_Base_Box <- torch_sigmoid(Input_XYZWLHR[..,Index_XYZ_Box]) - Prior_XYZWLHR[..,Index_XYZ_Box]

  # GEORGE: THE REGRESSION TARGETS (t*) FOR THE TRAINING PAIR (P,G) ARE DEFINED AS: 
    # file:///D:/DOCUMENTS/PDF_FILES/CNN_MEETING/Girshick_2014.pdf ... EQUATION 6,7,8,9
    # DOES THAT MEAN THAT THOSE EQUATIONS ARE 
  
  # GEORGE: file:///D:/DOCUMENTS/PDF_FILES/Xu_Jun_3d_2019_GIoU.pdf EQUATION 3 IS CAPTURED HERE
  
  #Prior_Diagnal <- torch_sqrt(torch_pow(torch_sqrt(Prior_XYZWLHR[..,Index_L]*Prior_XYZWLHR[..,Index_L] + Prior_XYZWLHR[..,Index_W]*Prior_XYZWLHR[..,Index_W]),2)+Prior_XYZWLHR[..,Index_H]*Prior_XYZWLHR[..,Index_H])
  # browser()
  Prior_Diagnal <- torch_sqrt((Prior_XYZWLHR[..,Index_L]*Prior_XYZWLHR[..,Index_L]) + (Prior_XYZWLHR[..,Index_W]*Prior_XYZWLHR[..,Index_W]))
  # Ha <- Prior_XYZWLHR[..,Index_H] #  (*Prior_XYZWLHR[..,Index_H]))
  if(To_FROM == "TO"){
    XYZWLHR_XY_Box <- (Input_XYZWLHR[..,Index_XY_Box] - Prior_XYZWLHR[..,Index_XY_Box])/Prior_Diagnal
    XYZWLHR_Z_Box <- (Input_XYZWLHR[..,Index_Z_Box] - Prior_XYZWLHR[..,Index_Z_Box])/Prior_XYZWLHR[..,Index_H]
    XYZWLHR_L <- torch_log(Input_XYZWLHR[..,Index_L]/ Prior_XYZWLHR[..,Index_L]) # (Input_XYZWLHR[..,Index_L]/ Prior_XYZWLHR[..,Index_L]) #
    XYZWLHR_W <- torch_log(Input_XYZWLHR[..,Index_W]/ Prior_XYZWLHR[..,Index_W])  # (Input_XYZWLHR[..,Index_W]/ Prior_XYZWLHR[..,Index_W]) #
    XYZWLHR_H <- torch_log(Input_XYZWLHR[..,Index_H]/Prior_XYZWLHR[..,Index_H]) # (Input_XYZWLHR[..,Index_H]/ Prior_XYZWLHR[..,Index_H]) # 
    XYZWLHR_R <- (Input_XYZWLHR[..,Index_R] - Prior_XYZWLHR[..,Index_R])
    
    if(Para_Cnt == 10){
      XYZWLHR_XYZ_Base <- (Input_XYZWLHR[..,Index_XYZ_Base] - Prior_XYZWLHR[..,Index_XYZ_Base])
      }
    }
  if(To_FROM == "FROM"){
    # browser()
    # XYZWLHR_XYZ_Box <- (Input_XYZWLHR[..,Index_XYZ_Box]*Prior_Diagnal) + Prior_XYZWLHR[..,Index_XYZ_Box]   # [ CUDAFloatType{64,6} ][ grad_fn = <AddBackward0> ]
    XYZWLHR_XY_Box <- (Input_XYZWLHR[..,Index_XY_Box]*Prior_Diagnal) + Prior_XYZWLHR[..,Index_XY_Box]
    XYZWLHR_Z_Box <- Input_XYZWLHR[..,Index_Z_Box]*Prior_XYZWLHR[..,Index_H]+ Prior_XYZWLHR[..,Index_Z_Box]
    XYZWLHR_L <- torch_exp(Input_XYZWLHR[..,Index_L])*Prior_XYZWLHR[..,Index_L]     # DOM DOM DOM !!! TESTING THE EXPONENTIALITY FOR WHOLE THINK AND NOT JUST PART OF IT!!! 11/05/2022
    XYZWLHR_W <- torch_exp(Input_XYZWLHR[..,Index_W])*Prior_XYZWLHR[..,Index_W]
    XYZWLHR_H <- torch_exp(Input_XYZWLHR[..,Index_H])*Prior_XYZWLHR[..,Index_H]                                                                     # [ CUDAFloatType{64,1} ][ grad_fn = <MulBackward0> ]
    XYZWLHR_R <- (Input_XYZWLHR[..,Index_R] + Prior_XYZWLHR[..,Index_R])
    
    if(Para_Cnt == 10){
      XYZWLHR_XYZ_Base <- (Input_XYZWLHR[..,Index_XYZ_Base] + Prior_XYZWLHR[..,Index_XYZ_Base])
      }
  }
  
  # CONCATENATE OUTPUT
  if(Para_Cnt == 7){
    Output_all <- torch_cat(list(#XYZWLHR_XYZ_Box, 
                                  XYZWLHR_XY_Box,
                                  XYZWLHR_Z_Box,
                                  XYZWLHR_L,
                                  XYZWLHR_W,
                                  XYZWLHR_H,
                                  XYZWLHR_R),length(dim(Input_XYZWLHR))) # 
    }else{
      Output_all <- torch_cat(list(XYZWLHR_XYZ_Base,
                                    #XYZWLHR_XYZ_Box, 
                                   XYZWLHR_XY_Box,
                                   XYZWLHR_Z_Box,
                                    XYZWLHR_L,
                                    XYZWLHR_W,
                                    XYZWLHR_H,
                                    XYZWLHR_R),length(dim(Input_XYZWLHR)))
    }
  return(Output_all) 
}


# Z_Range <- torch_unsqueeze(torch_subtract(torch_amax(Prior_XYZWLHR[,Index_Z],1), torch_amin(Prior_XYZWLHR[,Index_Z],1)),2)
# Z_Range_View <- Z_Range$view(c(1,3))
# Z_Range_3Zs <- torch_repeat_interleave(Z_Range_View, torch_tensor(64L, device=device), dim = 1)
# Goffset_Z <- (Input_XYZWLHR[,Index_Z] - Prior_XYZWLHR[,Index_Z])/Z_Range_3Zs
# 
# Goffset_XY <- (Input_XYZWLHR[,Index_XY] - Prior_XYZWLHR[,Index_XY])
# 
# Diagonal_XY <- torch_sqrt(torch_square(Prior_XYZWLHR[,Index_W]) + torch_square(Prior_XYZWLHR[,Index_L]))
# Diagonal_XYZ <- torch_sqrt(torch_square(Diagonal_XY) + torch_square(Prior_XYZWLHR[,Index_Z]))
# 
# Goffset_XBox <- (Input_XYZWLHR[,Index_X_Box] - Prior_XYZWLHR[,Index_X_Box]) / Diagonal_XYZ                         # (T-P)/sqrt((PW)^2 + (PL)^2)
# Goffset_YBox <- (Input_XYZWLHR[,Index_Y_Box] - Prior_XYZWLHR[,Index_Y_Box]) / Diagonal_XYZ                         # (T-P)/PL
# Goffset_ZBox <- (Input_XYZWLHR[,Index_Z_Box] - Prior_XYZWLHR[,Index_Z_Box]) / Diagonal_XYZ  
# Goffset_L <-torch_log((Input_XYZWLHR[,Index_L]/ Prior_XYZWLHR[,Index_L])) 
# Goffset_W <-torch_log((Input_XYZWLHR[,Index_W]/ Prior_XYZWLHR[,Index_W])) 
# Goffset_H <-torch_log((Input_XYZWLHR[,Index_H]/ Prior_XYZWLHR[,Index_H])) 
# Goffset_R <- (Input_XYZWLHR[,Index_R] - Prior_XYZWLHR[,Index_R])
# 
# GOffset_all <- torch_cat(list(Goffset_Z, 
#                               Goffset_XY,
#                               Goffset_XBox, 
#                               Goffset_YBox, 
#                               Goffset_ZBox,
#                               Goffset_L,
#                               Goffset_W,
#                               Goffset_H,
#                               Goffset_R),2)[,Index_all_Order] 

# ############################################################################################################################################
# # Goffset_To_XYZWLHR_GPU_FUN
# 
# 
# Goffset_To_XYZWLHR_GPU_FUN <- function(Input_XYZWLHR_GOffset, Prior_XYZWLHR, Para_Cnt = Para_Cnt, use_Tensor = "Yes", device=device){
# 
#   if(Para_Cnt == 7){
#     index_XYZ_Base_Box <- torch_tensor(as.integer(c(1,2,3))) 
#     Index_L <- torch_tensor(as.integer(4))
#     Index_W <- torch_tensor(as.integer(5)) 
#     Index_H <- torch_tensor(as.integer(6))
#     Index_R <- torch_tensor(as.integer(7)) 
#   }else{
#     index_XYZ_Base_Box <- torch_tensor(as.integer(c(1,2,3,4,5,6))) 
#     Index_L <- torch_tensor(as.integer(7))
#     Index_W <- torch_tensor(as.integer(8)) 
#     Index_H <- torch_tensor(as.integer(9))
#     Index_R <- torch_tensor(as.integer(10)) 
#   }
# 
#   # YOLO 9000 uses sigmoid to deal with centroid location https://stats.stackexchange.com/questions/403741/what-is-sigma-function-in-the-yolo-object-detector and https://arxiv.org/abs/1612.08242
#   # XYZWLHR_XYZ_Base_Box <- torch_sigmoid(Input_XYZWLHR_GOffset[,index_XYZ_Base_Box]) + Prior_XYZWLHR[,index_XYZ_Base_Box] # SOME HAVE MULTIPLY GOffset by Prior_L and Prior_W (but we do not have aligned boxes)
#   
#   Prior_Diagnal <- torch_sqrt(torch_pow(torch_sqrt((Prior_XYZWLHR[,Index_L]*Prior_XYZWLHR[,Index_L]) + (Prior_XYZWLHR[,Index_W]*Prior_XYZWLHR[,Index_W])),2)+(Prior_XYZWLHR[,Index_H]*Prior_XYZWLHR[,Index_H]))
#   XYZWLHR_XYZ_Base_Box <- (Input_XYZWLHR_GOffset[,index_XYZ_Base_Box]*Prior_Diagnal) + Prior_XYZWLHR[,index_XYZ_Base_Box]   # [ CUDAFloatType{64,6} ][ grad_fn = <AddBackward0> ]
#   
#   # DOM DOM DOM !!! TESTING THE EXPONENTIALITY FOR WHOLE THINK AND NOT JUST PART OF IT!!! 11/05/2022
#   XYZWLHR_L <- torch_exp(Input_XYZWLHR_GOffset[,Index_L])*Prior_XYZWLHR[,Index_L]
#   XYZWLHR_W <- torch_exp(Input_XYZWLHR_GOffset[,Index_W])*Prior_XYZWLHR[,Index_W]
#   XYZWLHR_H <- torch_exp(Input_XYZWLHR_GOffset[,Index_H])*Prior_XYZWLHR[,Index_H]                                                                     # [ CUDAFloatType{64,1} ][ grad_fn = <MulBackward0> ]
#   # XYZWLHR_L <- torch_exp((Input_XYZWLHR_GOffset[,Index_L]*Prior_XYZWLHR[,Index_L])) # (Input_XYZWLHR_GOffset[,Index_L]*Prior_XYZWLHR[,Index_L]) #
#   # XYZWLHR_W <- torch_exp((Input_XYZWLHR_GOffset[,Index_W]*Prior_XYZWLHR[,Index_W])) # (Input_XYZWLHR_GOffset[,Index_W]*Prior_XYZWLHR[,Index_W]) #
#   # XYZWLHR_H <- torch_exp((Input_XYZWLHR_GOffset[,Index_H]*Prior_XYZWLHR[,Index_H])) # (Input_XYZWLHR_GOffset[,Index_H]*Prior_XYZWLHR[,Index_H]) #
#   XYZWLHR_R <- (Input_XYZWLHR_GOffset[,Index_R] + Prior_XYZWLHR[,Index_R])
#   
#   XYZWLHR_all <- torch_cat(list(XYZWLHR_XYZ_Base_Box, 
#                                 XYZWLHR_L,
#                                 XYZWLHR_W,
#                                 XYZWLHR_H,
#                                 XYZWLHR_R),2)
#   
#   return(XYZWLHR_all)
# }
# 
# # REMOVED FROM Goffset_To_XYZWLHR_GPU_FUN
# #
# # if(use_Tensor == "Yes"){
# #   #1"X_Base", 2"Y_Base", 3"Z_Base", 4"X_TopBox", 5"Y_TopBox", 6"L_TopBox", 7"W_TopBox", 8"Z_TopBox", 9"R_TopBox",  10"Z_TopTree"
# #   Index_Z <- torch_tensor(c(3, 8, 10), dtype = torch_long(), device=device)
# #   Index_XY <- torch_tensor(c(1,2), dtype = torch_long(), device=device)
# #   Index_X_Box <- torch_tensor(c(4), dtype = torch_long(), device=device)
# #   Index_Y_Box <- torch_tensor(c(5), dtype = torch_long(), device=device)
# #   Index_L <- torch_tensor(c(6), dtype = torch_long(), device=device)
# #   Index_W <- torch_tensor(c(7), dtype = torch_long(), device=device)
# #   Index_R <- torch_tensor(c(9), dtype = torch_long(), device=device)
# #   Index_all_Order <- torch_tensor(order(c(3, 8, 10, 1, 2, 4,  5, 6,  7,  9)), dtype = torch_long(), device=device)
# # }else{
# #   Index_Z <- torch_tensor(c(3, 14, 16), dtype = torch_long(), device=device)
# #   Index_XY <- torch_tensor(c(1,2), dtype = torch_long(), device=device)
# #   Index_X_Box <- torch_tensor(c(10), dtype = torch_long(), device=device)
# #   Index_Y_Box <- torch_tensor(c(11), dtype = torch_long(), device=device)
# #   Index_L <- torch_tensor(c(12), dtype = torch_long(), device=device)
# #   Index_W <- torch_tensor(c(13), dtype = torch_long(), device=device)
# #   Index_R <- torch_tensor(c( 15), dtype = torch_long(), device=device)
# #   Index_all_Order <- torch_tensor(order(c( 3, 14, 16, 1, 2, 10, 11, 12,  13,  15)), dtype = torch_long(), device=device)
# # }
# #
# # Z_Range <- torch_unsqueeze(torch_subtract(torch_amax(Prior_XYZWLHR[,Index_Z],1), torch_amin(Prior_XYZWLHR[,Index_Z],1)),2)
# # Z_Range_View <- Z_Range$view(c(1,3))
# # Z_Range_3Zs <- torch_repeat_interleave(Z_Range_View, torch_tensor(64L, device=device), dim = 1)
# # XYZWLHR_Z <- (Input_XYZWLHR_GOffset[,Index_Z]*Z_Range_3Zs)  + Prior_XYZWLHR[,Index_Z] 
# # 
# # XYZWLHR_XY <- (Input_XYZWLHR_GOffset[,Index_XY] + Prior_XYZWLHR[,Index_XY])
# # 
# # Diagonal <- torch_sqrt(torch_square(Prior_XYZWLHR[,Index_W]) + torch_square(Prior_XYZWLHR[,Index_L]))
# # XYZWLHR_XBox <- (Input_XYZWLHR_GOffset[,Index_X_Box]*Diagonal)  + Prior_XYZWLHR[,Index_X_Box]                       
# # XYZWLHR_YBox <- (Input_XYZWLHR_GOffset[,Index_Y_Box]*Diagonal)  + Prior_XYZWLHR[,Index_Y_Box]                        
# # XYZWLHR_L <-torch_exp(Input_XYZWLHR_GOffset[,Index_L])* Prior_XYZWLHR[,Index_L]
# # XYZWLHR_W <-torch_exp(Input_XYZWLHR_GOffset[,Index_W])* Prior_XYZWLHR[,Index_W]
# # XYZWLHR_R <- (Input_XYZWLHR_GOffset[,Index_R] + Prior_XYZWLHR[,Index_R])
# # 
# # XYZWLHR_all <- torch_cat(list(XYZWLHR_Z,
# #                               XYZWLHR_XY,
# #                               XYZWLHR_XBox,
# #                               XYZWLHR_YBox,
# #                               XYZWLHR_L,
# #                               XYZWLHR_W,
# #                               XYZWLHR_R),2)[,Index_all_Order]

############################################################################################################################################
# TRIANG_ID_ALL_FUN
TRIANG_ID_ALL_FUN <- function (Para_Faces_Used = 4, Box_Levels = 4){
  Triangles_ID <- matrix(c(c(1,2,3),
                           c(1,3,4),
                           c(1,2,5),
                           c(5,6,2),
                           c(2,3,6),
                           c(3,7,6),
                           c(3,4,7),
                           c(4,7,8),
                           c(1,4,8),
                           c(1,8,5),
                           c(5,6,7),
                           c(5,7,8)), ncol=3, byrow= TRUE)
  if(Box_Levels == 4){
    # BOTH BOUNDING BOXES AND BASE LOCATION (MATRIX FORM)
    Triangles_ID_All <- c(as.vector(t(Triangles_ID[1:10,])),
                          as.vector(t((Triangles_ID+Para_Faces_Used)[3:10,])),
                          as.vector(t((Triangles_ID+(Para_Faces_Used*2))[3:12,])))
  }else{ # THIS ONE HAS Box_Level == 3
    # BOTH BOUNDING BOXES AND BASE LOCATION (MATRIX FORM)
    Triangles_ID_All <- c(as.vector(t(Triangles_ID[1:10,])),
                          as.vector(t((Triangles_ID+Para_Faces_Used)[3:12,])))
  }
  return(Triangles_ID_All)
}

##############################################################################################################################################################################################################
# FUNCTION FOR WEIGHTED MSE LOSS

#
smooth_l1_Weighted_FUN <- function(input, target, weight){
  mse_loss_W  <-  torch_mean(weight * (input - target) ** 2)
  return (mse_loss_W)
}

mse_loss <- function(input, target){
  mse_loss <-  torch_mean((input - target) ** 2)
  return (mse_loss)
}

##############################################################################################################################################################################################################
# DICE_LOSS_FUN
DICE_LOSS_FUN <- function(Mask_Pred, Mask_Obs) {

  smooth <- 0.00001 # RATHER THAN 1
  Mask_Pred <- Mask_Pred$view(-1)
  Mask_Obs <- Mask_Obs$view(-1)
  intersection <- (Mask_Pred * Mask_Obs)$sum()
  #Dice <- 1 - ((2 * intersection + smooth) / (Mask_Pred$sum() + Mask_Obs$sum() + smooth))
  Dice <- torch_squeeze(1 - ((2 * intersection + smooth) / (torch_sum(torch_square(Mask_Pred)) + torch_sum(torch_square(Mask_Obs)) + smooth))) # use 1- to minimise
  return(Dice)
}
############################################################################################################################################

EMPTY_RoI_VOX_FUN <- function(para_RoI_Pool_Dim_X,para_RoI_Pool_Dim_Y, para_RoI_Pool_Dim_Z) {
  # browser()
  X_Seq <- seq(0, 1, length.out = para_RoI_Pool_Dim_X)
  Y_Seq <- seq(0, 1, length.out = para_RoI_Pool_Dim_Y)
  Z_Seq <- seq(0, 1, length.out = para_RoI_Pool_Dim_Z)
  XY_Seq <- as.data.frame(crossing(X_Seq, Y_Seq))
  XYZ_Seq <- XY_Seq[rep(seq_len(nrow(XY_Seq)), each = para_RoI_Pool_Dim_Z), ] ## XYZ_Seq <- XY_Seq[rep(seq_len(nrow(XY_Seq)), each = Para_MaxZ_Shrink+1), ]
  XYZ_Seq$Z_Seq <- as.double(rep(Z_Seq, nrow(XY_Seq))) ## XYZ_Seq$Z_Seq <- as.double(rep(0:Para_MaxZ_Shrink, nrow(XY_Seq)))
  
  # SORT BY Z THEN Y AND FINALLY X (SO MOVES ACROSS X FIRST, THEN Y AND THEN Z)
  XYZ_Seq <- XYZ_Seq[order( XYZ_Seq[,1], XYZ_Seq[,2], XYZ_Seq[,3]),] # DOM DOM DOM !!! CHANGED 17/05/2022
  colnames(XYZ_Seq) <-  c("X", "Y", "Z")
  # colnames(XYZ_Seq) <-  c("X", "Y", "Z") # DOM DOM DOM !!! CHANGED 17/05/2022
  # colnames(XYZ_Seq) <-  c("Z", "Y", "X")
  return(XYZ_Seq)
}

############################################################################################################################################
TENSOR_LAS_RoI_FUN <- function(oneB_oneRoI_Vox_Den, oneB_oneRoI_MASK_ExtR,
                               para_RoI_Pool_Dim_XY,
                               para_RoI_Pool_Dim_Z) {
  XYZ_RoI_Empty_N <- EMPTY_RoI_VOX_FUN(para_RoI_Pool_Dim_XY,para_RoI_Pool_Dim_XY, para_RoI_Pool_Dim_Z)
  
  XYZ_RoI_Empty_N$Tensor_Count_Norm <- round(as.vector(as.array(oneB_oneRoI_Vox_Den)), 4) #Shift_Data$Shift_Vox_X[1]
  XYZ_RoI_Empty_N$Mask <- round(as.vector(as.array(oneB_oneRoI_MASK_ExtR)), 4) #Shift_Data$Shift_Vox_X[1]
  LAS_Tensor_Vox_Den <- LAS(XYZ_RoI_Empty_N)
  # browser()
  return(LAS_Tensor_Vox_Den)
}
############################################################################################################################################

# FUNCTIONS FOR GENERATING VERTICES FROM XYZWHRT
GEN_ROI_VERTICES_FUN <- function(best_RoI_Vox){
X1 <- best_RoI_Vox[1]
X2 <- best_RoI_Vox[2]
Y1 <- best_RoI_Vox[3]
Y2 <- best_RoI_Vox[4]
Z1 <- best_RoI_Vox[5]
Z2 <- best_RoI_Vox[6]

XYZ <- data.frame(X = c(X1,  X1, X2, X2),
                  Y = c(Y1,  Y2, Y2, Y1),
                  Z = rep(Z1, 4))  # THESES ARE ORDERED SO THAT THE VERTICES ARE LISTED IN AN ODER THAT IS CLOCKWISE WITHOUT MAKING A CROSS
XYZ2 <- data.frame(X = c(X1,  X1, X2, X2),
                   Y = c(Y1,  Y2, Y2, Y1),
                   Z = rep(Z2, 4))

Vertices <- rbind(XYZ, XYZ2)
return(Vertices)
}
############################################################################################################################################

numextract <- function(string){
  str_extract(string, "\\-*\\d+\\.*\\d*")
}
numextract_all <- function(string){
  unlist(regmatches(string,gregexpr("[[:digit:]]+\\.*[[:digit:]]*",string)))
}

mround_Up <- function(x,base){
  base*ceiling(x/base)
}
mround_Down <- function(x,base){
  base*floor(x/base)
} 

##########################################################################################################################################
##########################################################################################################################################

MODEL_PARA_COUNT_FUN <- function(model_Saved){
  
  #STATE DICT OUTPUT
  List_state_dict<- length(model_Saved$model_state_dict)
  List_names <-names(model_Saved$model_state_dict)
  
  Output_Model_Names <- c()
  Output_Model_Parameters <- c()
  Model_Training <- c()
  for(LC in 1:List_state_dict){
    one_name <- List_names[[LC]]
    One_State_Dict_Para <- prod(model_Saved$model_state_dict[[LC]]$size())
    one_Train <- model_Saved$model_state_dict[[LC]]$requires_grad
    
    Output_Model_Names <- c(Output_Model_Names, one_name)
    Output_Model_Parameters <- c(Output_Model_Parameters, One_State_Dict_Para)
    Model_Training <- c(Model_Training, one_Train)
  }
  
  Output_MODEL_STATE_ALL <- data.frame(Output_Model_Names, Output_Model_Parameters, Model_Training)
  
  # OPTIMISER STATE DICT OUTPUT
  List_Optim_state_dict1 <- length(model_Saved$optimizer_state_dict[[1]][[1]])
  List_Optim_state_dict1_Para <- length(model_Saved$optimizer_state_dict[[1]][[1]][[1]])
  List_Optim_state_dict2 <- length(model_Saved$optimizer_state_dict[[2]])
  
  List_Optim_names <- names(model_Saved$optimizer_state_dict[[2]])
  
  Output_Optim_Names <- c("Optim_Hyper_1", "Optim_Hyper_2")
  Output_Optim_Parameters <- c(List_Optim_state_dict1, List_Optim_state_dict1_Para)
  Optim_Training <- c("Not_Sure", "Not_Sure")
  
  for(OSD in 1:List_Optim_state_dict2){
    one_Optim_name <-  names(model_Saved$optimizer_state_dict[[2]][[OSD]])
    One_Optim_Para <- prod(model_Saved$optimizer_state_dict[[2]][[OSD]][[1]]$size())
    one_Optim_Train <- model_Saved$optimizer_state_dict[[2]][[OSD]][[1]]$requires_grad
    
    Output_Optim_Names <- c(Output_Optim_Names, one_Optim_name)
    Output_Optim_Parameters <- c(Output_Optim_Parameters, One_Optim_Para)
    Optim_Training <- c(Optim_Training, one_Optim_Train)
  }
  
  Output_OPTIM_ALL <- data.frame(Output_Optim_Names, Output_Optim_Parameters, Optim_Training)
  
  # SUMMARISE ALL THE PARAMETERS
  
  TOTAL_PARA_MODEL_STATE <- as.data.frame(Output_MODEL_STATE_ALL %>%
                                            dplyr::group_by(Model_Training) %>%
                                            dplyr::summarise(Total_Para = sum(Output_Model_Parameters), .groups = 'drop'))
  TOTAL_PARA_MODEL_STATE$Type_Para <- colnames(TOTAL_PARA_MODEL_STATE)[1]
  colnames(TOTAL_PARA_MODEL_STATE)[1] <- "Training"
  TOTAL_PARA_MODEL_STATE <- TOTAL_PARA_MODEL_STATE[,c(3,1,2)]
  
  TOTAL_PARA_OPTIM_ALL <- as.data.frame(Output_OPTIM_ALL %>%
                                          dplyr::group_by(Optim_Training) %>%
                                          dplyr::summarise(Total_Para = sum(Output_Optim_Parameters), .groups = 'drop'))
  
  TOTAL_PARA_OPTIM_ALL$Type_Para <- colnames(TOTAL_PARA_OPTIM_ALL)[1]
  colnames(TOTAL_PARA_OPTIM_ALL)[1] <- "Training"
  TOTAL_PARA_OPTIM_ALL <- TOTAL_PARA_OPTIM_ALL[,c(3,1,2)]
  
  Summary_Total_Parmeter <- rbind(TOTAL_PARA_MODEL_STATE, TOTAL_PARA_OPTIM_ALL)
  
  return(list(Output_MODEL_STATE_ALL, Output_OPTIM_ALL, Summary_Total_Parmeter))
}

##############################################################################################################################################################################################################
##############################################################################################################################################################################################################
# XYZ_Extent_XYZWLHR_FUN
########################

XYZ_Extent_XYZWLHR_FUN <- function(Data_DF = XYZWLHR_plotGT_N[,-2], ID_Col = "TID", Normalised = "No", Para_Cnt = 10, device=device){
  #browser()
  unique_ID <- Data_DF[,which(colnames(Data_DF) == ID_Col)]
  Data_DF <- Data_DF[,-which(colnames(Data_DF) == ID_Col)]
  Data_T <- torch_tensor(as.matrix(Data_DF), device=device)
  for(i in 1:length(unique_ID)){
    #browser()
    oneData_T <- Data_T[which(unique_ID == unique_ID[i])[1],] # added [1]  CHANGED THIS 24/01/2022 
    
    oneB_Vert <- as.array(XYZWHR_TO_VERT_GPU_FUN(oneData_T, Base_WL = Para_Base_WL/Para_Target_Base, Normalised = Normalised, Para_Cnt = Para_Cnt, device=device))
    oneB_Vert <- as.data.frame(oneB_Vert)
    colnames(oneB_Vert) <- c("X", "Y", "Z")
    oneExtent_Summary <- XYZ_Extent_Summary_FUN(oneB_Vert)
    
    oneB_Vert <- data.frame(unique_ID[i], oneB_Vert)
    colnames(oneB_Vert)[1] <- ID_Col
    #
    oneExtent_Summary <- data.frame(unique_ID[i], oneExtent_Summary)
    colnames(oneExtent_Summary)[1] <- ID_Col
    #
    mins <- oneExtent_Summary[,c(1, 2, 4, 6)]
    colnames(mins) <- c(	"PriorID","X", "Y", "Z")
    maxs <- oneExtent_Summary[,c(1, 3, 5, 7)]
    colnames(maxs) <- c("PriorID","X", "Y", "Z")
    oneExtent_Summary_FORMAT <- rbind(mins, maxs)


    if(i == 1){
      all_Vert <- oneB_Vert
      Extent_Summary <- oneExtent_Summary
      Extent_Summary_FORMAT <- oneExtent_Summary_FORMAT
    }else{
      all_Vert <- rbind(all_Vert, oneB_Vert)
      Extent_Summary <- rbind(Extent_Summary, oneExtent_Summary)
      Extent_Summary_FORMAT <- rbind(Extent_Summary_FORMAT, oneExtent_Summary_FORMAT)
    }
  }
  return(list(all_Vert, Extent_Summary, Extent_Summary_FORMAT))
}

##############################################################################################################################################################################################################
##############################################################################################################################################################################################################
# XYZ EXTENT SUMMARY
####################
XYZ_Extent_Summary_FUN <- function(Data_DF){
  Extent_Summary <- as.data.frame(Data_DF  %>%
                                    dplyr::summarise(MinX = min(X),
                                                     MaxX = max(X),
                                                     MinY = min(Y),
                                                     MaxY = max(Y),
                                                     MinZ = min(Z),
                                                     MaxZ = max(Z),
                                                     RangeX = MaxX-MinX,
                                                     RangeY = MaxY-MinY,
                                                     RangeZ = MaxZ-MinZ, .groups = 'drop'))
  return(Extent_Summary)
}

##############################################################################################################################################################################################################
##############################################################################################################################################################################################################
# LIST_ROI_NORM_FUN
####################
LIST_ROI_NORM_FUN <- function(BestWorst_Summary_Prior_TID, Summary_TID_Extent,
                              XYZWLHR_plotGT_N, XYZWLHR_plotPriors_N,
                              Ext_allPrior, LAS_Vox,
                              para_IoU_XYRes, para_IoU_ZRes, Normalised= "Yes", Para_Cnt = Para_Cnt){
  #
  Decimal_Vox_XY <- sort(unique(LAS_Vox$X))
  Decimal_Vox_Z <- sort(unique(LAS_Vox$Z))
  List_RoI_Dec <- list()
  List_RoI_Vox <- list()
  
  List_RoI_Dec_XYZWLH <- list()
  List_RoI_Vox_XYZWLH <- list()
  
  for(TIU in 1:nrow(BestWorst_Summary_Prior_TID)){
    
    onePriorID <- BestWorst_Summary_Prior_TID[TIU,]
    
    # GET GT TID
    oneSummary_TID_Extent <- Summary_TID_Extent[which(Summary_TID_Extent$TID == onePriorID$TID),]
    
    # # ROUND PRIOR EXTENT TO VOXEL POSITION
    onePrior_Ext <- Ext_allPrior[which(Ext_allPrior$PriorID == onePriorID$Prior),]
    # print(onePrior_Ext)
    #
    # GET OVERALL MIN MAX OF PRIOR AND GT
    minX_GT_Prior <- min(oneSummary_TID_Extent$MinX, onePrior_Ext$X[1])
    maxX_GT_Prior <- max(oneSummary_TID_Extent$MaxX, onePrior_Ext$X[2])
    minY_GT_Prior <- min(oneSummary_TID_Extent$MinY, onePrior_Ext$Y[1])
    maxY_GT_Prior <- max(oneSummary_TID_Extent$MaxY, onePrior_Ext$Y[2])
    minZ_GT_Prior <- min(oneSummary_TID_Extent$MinZ, onePrior_Ext$Z[1])
    maxZ_GT_Prior <- max(oneSummary_TID_Extent$MaxZ, onePrior_Ext$Z[2])
    #
    ####################################################
    # GET EXTENT VALUES THAT REPRESENT THE CLOSEST VOXEL
    ####################################################
    #
    #X1
    Nearest_Below_X1 <- Decimal_Vox_XY[which((Decimal_Vox_XY-minX_GT_Prior)<=0)]
    Nearest_Below_X1 <- Nearest_Below_X1[which.min(abs(minX_GT_Prior-Nearest_Below_X1))]
    if(length(Nearest_Below_X1) > 1){browser()}
    minX_GT_Prior <-ifelse(length(Nearest_Below_X1) == 1, Nearest_Below_X1, 0) # onePrior_Ext$X[1] <- Nearest_Below_X1
    #X2
    Nearest_Above_X2 <- Decimal_Vox_XY[which((Decimal_Vox_XY-maxX_GT_Prior)>0)]
    Nearest_Above_X2 <- Nearest_Above_X2[which.min(abs(maxX_GT_Prior-Nearest_Above_X2))]
    if(length(Nearest_Above_X2) > 1){browser()}
    maxX_GT_Prior <-ifelse(length(Nearest_Above_X2) == 1, Nearest_Above_X2, 1) #  onePrior_Ext$X[2] <- Nearest_Above_X2
    #Y1
    Nearest_Below_Y1 <- Decimal_Vox_XY[which((Decimal_Vox_XY-minY_GT_Prior)<0)]
    Nearest_Below_Y1 <- Nearest_Below_Y1[which.min(abs(minY_GT_Prior-Nearest_Below_Y1))]
    if(length(Nearest_Below_Y1) > 1){browser()}
    minY_GT_Prior <-ifelse(length(Nearest_Below_Y1) == 1, Nearest_Below_Y1, 0) # onePrior_Ext$Y[1] <- Nearest_Below_Y1
    #
    #Y2
    Nearest_Above_Y2 <- Decimal_Vox_XY[which((Decimal_Vox_XY-maxY_GT_Prior)>0)]
    Nearest_Above_Y2 <- Nearest_Above_Y2[which.min(abs(maxY_GT_Prior-Nearest_Above_Y2))]
    if(length(Nearest_Above_Y2) > 1){browser()}
    maxY_GT_Prior <-ifelse(length(Nearest_Above_Y2) == 1, Nearest_Above_Y2, 1) # onePrior_Ext$Y[2] <- Nearest_Above_Y2
    #Z1
    Nearest_Below_Z1 <- Decimal_Vox_Z[which((Decimal_Vox_Z-minZ_GT_Prior)<0)]
    Nearest_Below_Z1 <- Nearest_Below_Z1[which.min(abs(minZ_GT_Prior-Nearest_Below_Z1))]
    if(length(Nearest_Below_Z1) > 1){browser()}
    minZ_GT_Prior <-ifelse(length(Nearest_Below_Z1) == 1, Nearest_Below_Z1, 0) # onePrior_Ext$Z[1] <- Nearest_Below_Z1
    #Z2
    Nearest_Above_Z2 <- Decimal_Vox_Z[which((Decimal_Vox_Z-maxZ_GT_Prior)>0)]
    Nearest_Above_Z2 <- Nearest_Above_Z2[which.min(abs(maxZ_GT_Prior-Nearest_Above_Z2))]
    if(length(Nearest_Above_Z2) > 1){browser()}
    maxZ_GT_Prior <-ifelse(length(Nearest_Above_Z2) == 1, Nearest_Above_Z2, 1) #onePrior_Ext$Z[2] <- Nearest_Above_Z
    
    ######################
    # GET VOXEL PLOT RANGE
    ######################
    Vox_minX <- range(LAS_Vox$X)[1] # 0
    Vox_maxX <- range(LAS_Vox$X)[2] # 1
    Vox_minY <- range(LAS_Vox$Y)[1] # 0
    Vox_maxY <- range(LAS_Vox$Y)[2] # 1
    Vox_minZ <- range(LAS_Vox$Z)[1] # 0
    Vox_maxZ <- range(LAS_Vox$Z)[2] # 1
    #
    # CALCULATE TABLE_IoU
    #
    # oneList_RoI_Dec <- list( TIU,
    #                      c(max(0,(minX_GT_Prior-Vox_minX)), max(0,(Vox_maxX-maxX_GT_Prior))),
    #                      c(max(0,(minY_GT_Prior-Vox_minY)), max(0,(Vox_maxY-maxY_GT_Prior))),
    #                      c(max(0,(minZ_GT_Prior-Vox_minZ)), max(0,(Vox_maxZ-maxZ_GT_Prior))))
    oneList_RoI_Dec <- list( TIU,
                             c(minX_GT_Prior, maxX_GT_Prior),
                             c(minY_GT_Prior, maxY_GT_Prior),
                             c(minZ_GT_Prior, maxZ_GT_Prior))
    #
    oneList_RoI_Orig <- oneList_RoI_Dec
    #
    # DEALING WITH EDGE CASES WHEN EXPANDING THE SMALL TREES
    for(RS in 2:4){
      # GET PRESENT RANGE OF GT_PSID
      if(RS == 2){GT_PSID_Range <- (maxX_GT_Prior-minX_GT_Prior)}
      if(RS == 3){GT_PSID_Range <- (maxY_GT_Prior-minY_GT_Prior)}
      if(RS == 4){GT_PSID_Range <- (maxZ_GT_Prior-minZ_GT_Prior)}
      if(RS <= 3){
        #LAS_Range <- para_LAS_XY_Range
        IoU_Range <- para_IoU_XYRes
        Decimal_Vox_Shift <-Decimal_Vox_XY
      }else{
        #LAS_Range <-para_LAS_Z_Range
        IoU_Range <-para_IoU_ZRes
        Decimal_Vox_Shift <-Decimal_Vox_Z
      }
      #
      #######################################################################################################
      # IF RANGE IS SMALLER THAN FINAL ROI RANGE THEN YOU NEED TO INCLUDE VOXELS SO ITS ABSOLUTE MINIMUM SIZE
      # NOTE: IF RANGE IS LARGER THAN RoI POOLING WILL BE ADOPTED IN ALGORITHM
      #######################################################################################################
      #
      if(GT_PSID_Range < IoU_Range){ # IF RANGE NEEDS EXPANDING
        Need_Shift <- IoU_Range - GT_PSID_Range
        #
        # ADUST BOTTOM SIDE .... (EXPAND ON BOTH SIDES (FIRST BOT))
        Bot_Shift <- Need_Shift/2
        if ((oneList_RoI_Dec[[RS]][1] - Bot_Shift) > 0){ # SEE IF 1/2 THE SHIFT CAN FIT IN BOTTOM
          oneList_RoI_Dec[[RS]][1] <- oneList_RoI_Dec[[RS]][1] - Bot_Shift
          Need_Shift <- Need_Shift- Bot_Shift
        }
        # else{
        #   Need_Shift <-  Need_Shift - oneList_RoI_Dec[[RS]][1]
        #   oneList_RoI_Dec[[RS]][1] <- 0
        # }
        #
        # ADUST TOP SIDE .... (EXPAND ON BOTH SIDES (SECOND TOP))
        Top_Shift <-ceiling(Need_Shift/2)
        if ((oneList_RoI_Dec[[RS]][2] + Top_Shift) < 1){
          oneList_RoI_Dec[[RS]][2] <- oneList_RoI_Dec[[RS]][2] + Top_Shift
          Need_Shift <- Need_Shift- Top_Shift
        }
        # else{
        #   # REDUCE THE Need_Shift BY AMOUNT SHIFTED (first below line) AND SHIFT TO OUTER EDGE
        #   Need_Shift <-  Need_Shift - (1-oneList_RoI_Dec[[RS]][2])
        #   oneList_RoI_Dec[[RS]][2] <- 1
        # }
        
        # IF THERE IS STILL SOME SHIFTING NECESSARY
        if(Need_Shift != 0){
          Index_Change <- which.max(c(oneList_RoI_Dec[[RS]][1], (1-oneList_RoI_Dec[[RS]][2])))
          
          if(Index_Change == 1){
            oneList_RoI_Dec[[RS]][Index_Change] <- oneList_RoI_Dec[[RS]][Index_Change] - Need_Shift
          }else{
            oneList_RoI_Dec[[RS]][Index_Change] <- oneList_RoI_Dec[[RS]][Index_Change] + Need_Shift
          }
        }
      }
      #
      ###############################################
      # MAKING SURE EXPANDED RoI IS ANCHORED TO VOXEL
      ###############################################
      Nearest_Below <- Decimal_Vox_Shift[which((Decimal_Vox_Shift-oneList_RoI_Dec[[RS]][1])<=0)]
      Nearest_Below <- Nearest_Below[which.min(abs(Nearest_Below-oneList_RoI_Dec[[RS]][1]))]
      oneList_RoI_Dec[[RS]][1] <-ifelse(length(Nearest_Below) == 1, Nearest_Below, 0) # onePrior_Ext$Z[1] <- Nearest_Below_Z1
      #
      Nearest_Above <- Decimal_Vox_Shift[which((Decimal_Vox_Shift-oneList_RoI_Dec[[RS]][2])>=0)]
      Nearest_Above <- Nearest_Above[which.min(abs(Nearest_Above -oneList_RoI_Dec[[RS]][2]))]
      oneList_RoI_Dec[[RS]][2] <-ifelse(length(Nearest_Above) == 1, Nearest_Above, 1) #onePrior_Ext$Z[2] <- Nearest_Above_Z
      if(oneList_RoI_Dec[[RS]][1] > oneList_RoI_Dec[[RS]][2]){browser()}
    }
    if(oneList_RoI_Dec[[RS]][1] > oneList_RoI_Dec[[RS]][2]){browser()}
    
    #browser()
    # APPENDING ROI_Dec TO LIST WITH ALL PRIORS
    List_RoI_Dec <- list.append(List_RoI_Dec, oneList_RoI_Dec)
    
    
    ###################################################
    # CONVERT THE DECIMAL POSITION TO VOXEL ID POSITION
    ###################################################
    
    oneList_RoI_Vox <- oneList_RoI_Dec
    oneList_RoI_Vox[[2]][1] <- which(round(Decimal_Vox_XY,4) == round(oneList_RoI_Dec[[2]][1],4))
    oneList_RoI_Vox[[2]][2] <- which(round(Decimal_Vox_XY,4) == round(oneList_RoI_Dec[[2]][2],4))
    oneList_RoI_Vox[[3]][1] <- which(round(Decimal_Vox_XY,4) == round(oneList_RoI_Dec[[3]][1],4))
    oneList_RoI_Vox[[3]][2] <- which(round(Decimal_Vox_XY,4) == round(oneList_RoI_Dec[[3]][2],4))
    oneList_RoI_Vox[[4]][1] <- which(round(Decimal_Vox_Z,4) == round(oneList_RoI_Dec[[4]][1],4))
    oneList_RoI_Vox[[4]][2] <- which(round(Decimal_Vox_Z,4) == round(oneList_RoI_Dec[[4]][2],4))
    
    # APPENDING ROI_Vox TO LIST WITH ALL PRIORS
    List_RoI_Vox <- list.append(List_RoI_Vox, oneList_RoI_Vox)
    
    # browser()
    
    # DOM DOM DOM !!! oneList_RoI_Dec_XYZWLH IS MISLEADING AND INCORRECT. ROI IS EXTENT OF BOTH PRIOR AND GT. YOU CANT REFORMAT IT TO W (X width), L (Y Length), and H (Z HEIGHT).
    # ############################################################################################
    # # CHANGE FORMAT OF RoI TO BE BOTTOM LEFT XYZ, W (X width), L (Y Length), and H (Z HEIGHT).
    # ############################################################################################
    #
    # # FIRST MAKE IT SAME AS ORIGINAL FORMAT AND THEN CHANGE EACH LIST ELEMENT
    # oneList_RoI_Dec_XYZWLH <- unlist(oneList_RoI_Dec)
    # oneList_RoI_Dec_XYZWLH[2] <- oneList_RoI_Dec[[2]][1] # BOT LEFT X
    # oneList_RoI_Dec_XYZWLH[3] <- oneList_RoI_Dec[[3]][1] # BOT LEFT Y
    # oneList_RoI_Dec_XYZWLH[4] <- oneList_RoI_Dec[[4]][1] # BOT LEFT Z
    # # oneList_RoI_Dec_XYZWLH[5] <- (Vox_maxX - oneList_RoI_Dec[[2]][2]) - oneList_RoI_Dec[[2]][1]  # W
    # # oneList_RoI_Dec_XYZWLH[6] <- (Vox_maxY - oneList_RoI_Dec[[3]][2]) - oneList_RoI_Dec[[3]][1]  # L
    # # oneList_RoI_Dec_XYZWLH[7] <- (Vox_maxZ - oneList_RoI_Dec[[4]][2]) - oneList_RoI_Dec[[4]][1]  # H
    #
    # #### DOM DOM DOM !!! BELOW IS INCORRECT.... THEY ARE RANGES AND NOT WLH
    # oneList_RoI_Dec_XYZWLH[5] <- (oneList_RoI_Dec[[2]][2]) - oneList_RoI_Dec[[2]][1]  # W
    # oneList_RoI_Dec_XYZWLH[6] <- (oneList_RoI_Dec[[3]][2]) - oneList_RoI_Dec[[3]][1]  # L
    # oneList_RoI_Dec_XYZWLH[7] <- (oneList_RoI_Dec[[4]][2]) - oneList_RoI_Dec[[4]][1]  # H
    #
    # if(length(which(unlist(oneList_RoI_Dec_XYZWLH) < 0)) > 0){browser()}
    #
    # # APPENDING RoI_Dec_XYZWLH TO LIST WITH ALL PRIORS
    # List_RoI_Dec_XYZWLH <- list.append(List_RoI_Dec_XYZWLH, oneList_RoI_Dec_XYZWLH)
    #
    # oneList_RoI_Vox_XYZWLH <- oneList_RoI_Vox
    # oneList_RoI_Vox_XYZWLH[2] <- oneList_RoI_Vox[[2]][1] # BOT LEFT X
    # oneList_RoI_Vox_XYZWLH[3] <- oneList_RoI_Vox[[3]][1] # BOT LEFT Y
    # oneList_RoI_Vox_XYZWLH[4] <- oneList_RoI_Vox[[4]][1] # BOT LEFT Z
    # # oneList_RoI_Vox_XYZWLH[5] <- (length(Decimal_Vox_XY) - oneList_RoI_Vox[[2]][2]) - oneList_RoI_Vox[[2]][1] # W
    # # oneList_RoI_Vox_XYZWLH[6] <- (length(Decimal_Vox_XY) -oneList_RoI_Vox[[3]][2]) - oneList_RoI_Vox[[3]][1]  # L
    # # oneList_RoI_Vox_XYZWLH[7] <- (length(Decimal_Vox_Z) -oneList_RoI_Vox[[4]][2]) - oneList_RoI_Vox[[4]][1]  # H
    # oneList_RoI_Vox_XYZWLH[5] <- (oneList_RoI_Vox[[2]][2]) - oneList_RoI_Vox[[2]][1] # W
    # oneList_RoI_Vox_XYZWLH[6] <- (oneList_RoI_Vox[[3]][2]) - oneList_RoI_Vox[[3]][1]  # L
    # oneList_RoI_Vox_XYZWLH[7] <- (oneList_RoI_Vox[[4]][2]) - oneList_RoI_Vox[[4]][1]  # H
    #
    # # APPENDING ROI_Vox_XYZWLH TO LIST WITH ALL PRIORS
    # List_RoI_Vox_XYZWLH <-  list.append(List_RoI_Vox_XYZWLH, oneList_RoI_Vox_XYZWLH)
    
    # DOM DOM DOM ... KEEP IN MIND THAT YOU ARE CLIPPING ONE EXTRA VOXEL USING THE VoxID BECAUSE THE FIRST ONE IS 1 not 0.
    # YOU HAVE DECIDED TO IGNORE THIS AS IN GENERAL CASE THIS WILL CLIP OFF A BIT OF CANOPY WHICH IS NOT AN ISSUE. REDUCING RoI AREA IS PROBABLY BETTER THAN OVER-SIZING
    
    
    # # TEST
    # XRange_inGrids <- (para_LAS_XY_Range-oneList_RoI_Dec[[2]][2]) - oneList_RoI_Dec[[2]][1]
    # YRange_inGrids <- (para_LAS_XY_Range-oneList_RoI_Dec[[3]][2]) - oneList_RoI_Dec[[3]][1]
    # ZRange_inGrids <- (para_LAS_Z_Range-oneList_RoI_Dec[[4]][2]) - oneList_RoI_Dec[[4]][1]
    #
    # print(paste("oneList_RoI_Orig_X: ", XRange_inGrids_Orig, "Y: ", YRange_inGrids_Orig, "Z: ", ZRange_inGrids_Orig))
    # print(oneList_RoI_Orig)
    #
    # print(paste("oneList_RoI_X............: ", XRange_inGrids, "Y: ", YRange_inGrids, "Z: ", ZRange_inGrids))
    # print(oneList_RoI_Dec)
    # # if(flag== 1){browser()}
    # # if(XRange_inGrids < para_IoU_XYRes | YRange_inGrids < para_IoU_XYRes | ZRange_inGrids < para_IoU_ZRes){browser()}
    # # END TEST
    #browser()
    
  }
  return(list(List_RoI_Dec, List_RoI_Vox))     #,  List_RoI_Dec_XYZWLH, List_RoI_Vox_XYZWLH
}

##############################################################################################################################################################################################################
##############################################################################################################################################################################################################
# PLOT_GT_PRIOR_VOX_RoI_FUN
####################
PLOT_GT_PRIOR_VOX_RoI_FUN <- function(LAS_Vox_N, Vert_oneGT, Vert_onePriors, Vert_RoI, unique_GT, Para_Cnt = 10){ # IoU_BestWorst_allGT, Vert_oneGT, Vert_onePriors, LAS_Vox_N, oneList_RoI_Dec
  
  LAS_oneP_Vox_N <- filter_poi(LAS_Vox_N, TID > 0)
  palette <- sample(distinctColorPalette(length(unique(LAS_oneP_Vox_N@data$TID))))
  Shift_X_Plot <- min(LAS_oneP_Vox_N$X)
  Shift_Y_Plot <- min(LAS_oneP_Vox_N$Y)
  LAS_oneP_Vox_N$X  <-  LAS_oneP_Vox_N$X- Shift_X_Plot
  LAS_oneP_Vox_N$X  <-  LAS_oneP_Vox_N$X- Shift_Y_Plot
  n <- length(unique(LAS_oneP_Vox_N@data$TID))
  palette <- sample(distinctColorPalette(n))
  
  # GENERATE ATTRIBUTE FOR COLOURING EACH POINT
  LAS_oneP_Vox_N@data$Color <- as.character("")
  Color_ID <- data.frame(Unique_TID = unique(LAS_oneP_Vox_N@data$TID),
                         ID_TID = palette)
  Index_Color_TID_1 <- which(LAS_oneP_Vox_N@data$TID %in% Color_ID$Unique_TID)
  Index_Color_TID_2 <- match(LAS_oneP_Vox_N@data$TID[Index_Color_TID_1],
                             Color_ID$Unique_TID)
  LAS_oneP_Vox_N@data$Color[Index_Color_TID_1] <- as.character(Color_ID$ID_TID[Index_Color_TID_2])
  
  plot(LAS_oneP_Vox_N, color="Color", size=5)
  
  text3d(mean(LAS_oneP_Vox_N$X), mean(LAS_oneP_Vox_N$X), max(LAS_oneP_Vox_N@data$Z),
         col="white",
         size = 2,
         paste("TID:", unique_GT))
  
  polygon3d(Vert_oneGT[1:4,], fill=FALSE, col="green", lwd=3)
  polygon3d(Vert_oneGT[5:8,], fill=FALSE, col="green", lwd=3)
  polygon3d(Vert_oneGT[9:12,], fill=FALSE, col="green", lwd=3)
  #if(Para_Cnt == 16){ polygon3d(Vert_oneGT[13:16,], fill=FALSE, col="green", lwd=3)}
  
  polygon3d(Vert_onePriors[1:4,], fill=FALSE, col="red", lwd=3)
  polygon3d(Vert_onePriors[5:8,], fill=FALSE, col="red", lwd=3)
  polygon3d(Vert_onePriors[9:12,], fill=FALSE, col="red", lwd=3)
  #if(Para_Cnt == 16){ polygon3d(Vert_onePriors[13:16,], fill=FALSE, col="red", lwd=3)}
  
  # PLOT ROI
  polygon3d(Vert_RoI[1:4,], fill=FALSE, col="yellow")
  polygon3d(Vert_RoI[5:8,], fill=FALSE, col="yellow")
  
  LAS_oneP_Vox_N_Trees_oneT <- filter_poi(LAS_oneP_Vox_N, TID > 1)
  LAS_oneP_Vox_N_Trees_oneT@data$TID[which(LAS_oneP_Vox_N_Trees_oneT@data$TID != unique_GT)] <- 1
  plot(LAS_oneP_Vox_N_Trees_oneT, color="TID")
  
}
##################################################################################################################################################################################################################################
PLOT_GT_PRIOR_VOX_FUN <- function(IoU_BestWorst_allGT, XYZWLHR_plotGT_N, XYZWLHR_plotPriors_N, LAS_Vox_N, Normalised = "Yes", Para_Cnt = 10, device=device){
  
  palette <- sample(distinctColorPalette(length(unique(LAS_oneP_Vox_N@data$TID))))
  Shift_X_Plot <- min(LAS_oneP_Vox_N$X)
  Shift_Y_Plot <- min(LAS_oneP_Vox_N$Y)
  LAS_oneP_Vox_N$X  <-  LAS_oneP_Vox_N$X- Shift_X_Plot
  LAS_oneP_Vox_N$X  <-  LAS_oneP_Vox_N$X- Shift_Y_Plot
  n <- length(unique(LAS_oneP_Vox_N@data$TID))
  palette <- sample(distinctColorPalette(n))
  # GENERATE ATTRIBUTE FOR COLOURING EACH POINT
  LAS_oneP_Vox_N@data$Color <- as.character("")
  Color_ID <- data.frame(Unique_TID = unique(LAS_oneP_Vox_N@data$TID),
                         ID_TID = palette)
  Index_Color_TID_1 <- which(LAS_oneP_Vox_N@data$TID %in% Color_ID$Unique_TID)
  Index_Color_TID_2 <- match(LAS_oneP_Vox_N@data$TID[Index_Color_TID_1],
                             Color_ID$Unique_TID)
  LAS_oneP_Vox_N@data$Color[Index_Color_TID_1] <- as.character(Color_ID$ID_TID[Index_Color_TID_2])
  #plot(LAS_oneP_Vox_N, color="A")
  # browser()
  unique_GT <- unique(IoU_BestWorst_allGT$TID)
  for(j in 1:length(unique_GT)){
    plot(LAS_oneP_Vox_N, color="Color", size=5)
    
    IoU_BestWorst_oneGT <- IoU_BestWorst_allGT[which(IoU_BestWorst_allGT$TID == unique_GT[j]),]
    text3d(mean(LAS_oneP_Vox_N$X), mean(LAS_oneP_Vox_N$X), max(LAS_oneP_Vox_N@data$Z),
           col="white",
           size = 2,
           paste("P:", IoU_BestWorst_oneGT$Plot_ID[1],  "TID:", unique_GT[j]))
    #
    oneGT_XYZWLHR_N <- XYZWLHR_plotGT_N[which(XYZWLHR_plotGT_N$TID == unique_GT[j]),-1]
    oneGT_XYZWLHR_N_T <- torch_squeeze(torch_tensor(as.matrix(oneGT_XYZWLHR_N), device="cpu")) # [,-c(1,2,6:11),]
    Vert_oneGT <- as.array(XYZWHR_TO_VERT_GPU_FUN(oneGT_XYZWLHR_N_T, Base_WL = Para_Base_WL/Para_Target_Base, Normalised = Normalised, Para_Cnt = Para_Cnt, device="cpu"))
    polygon3d(Vert_oneGT[1:4,], fill=FALSE, col="white", lwd=3)
    polygon3d(Vert_oneGT[5:8,], fill=FALSE, col="white", lwd=3)
    polygon3d(Vert_oneGT[9:12,], fill=FALSE, col="white", lwd=3)
    # if(Para_Cnt == 16){ polygon3d(Vert_oneGT[13:16,], fill=FALSE, col="white", lwd=3)}
    
    
    for(i in 1:nrow(IoU_BestWorst_oneGT)){
      oneIoU_BestWorst_oneGT_onePrior <- IoU_BestWorst_oneGT[i,]
      onePriors_XYZWLHR_N <- XYZWLHR_plotPriors_N[which(XYZWLHR_plotPriors_N$PriorID == oneIoU_BestWorst_oneGT_onePrior$Prior),-1]
      onePriors_XYZWLHR_N_T <- torch_squeeze(torch_tensor(as.matrix(onePriors_XYZWLHR_N), device="cpu")) #[,-c(1,5:10),]
      Vert_onePriors <- as.array(XYZWHR_TO_VERT_GPU_FUN(onePriors_XYZWLHR_N_T, Base_WL = Para_Base_WL/Para_Target_Base, Normalised = Normalised, Para_Cnt = Para_Cnt, device="cpu") )
      
      if(oneIoU_BestWorst_oneGT_onePrior$Prior_Type == 0){ Colour_Type <- "red"}else{ Colour_Type <- "green"}
      
      polygon3d(Vert_onePriors[1:4,], fill=FALSE, col=Colour_Type, lwd=3)
      polygon3d(Vert_onePriors[5:8,], fill=FALSE, col=Colour_Type, lwd=3)
      polygon3d(Vert_onePriors[9:12,], fill=FALSE, col=Colour_Type, lwd=3)
      #if(Para_Cnt == 16){ polygon3d(Vert_onePriors[13:16,], fill=FALSE, col=Colour_Type, lwd=3)}
      #browser()
    }
    # browser()
  }
}

##################################################################################################################################################################################################################################
##################################################################################################################################################################################################################################

PLOT_CNN_OUTPUT_FUN <- function(INPUT_LIST, TARGET_LIST, VNET_OUTPUT_LIST, Para_Target_Base, Para_Base_WL, Para_Target_Z_Height, Para_TriShpParaCnt){
  
  
  
  # DOM DOM DOM !!! YOU DON'T USE BELOW (POS only from LOSS_POS_TENSORS_LIST) FOR PLOTTING BELOW BUT WORTH CONSIDERING AND REPLACING VNET_OUTPUT_LIST 
  # # LOSS_POS_TENSORS_LIST 
  # pred_RegFUN_7Pos <- LOSS_POS_TENSORS_LIST[[1]]
  # PRED_Goffset_allP_allR_ExtR_7_Pos <- LOSS_POS_TENSORS_LIST[[2]]
  # TARGET_XYZWLHR_allP_allR_ExtR_7_Pos <- LOSS_POS_TENSORS_LIST[[3]]
  # ACTUAL_Goffset_allP_allR_ExtR_7_Pos <- LOSS_POS_TENSORS_LIST[[4]]
  # PRIOR_XYZWLHR_allP_allR_ExtR_7_Pos <- LOSS_POS_TENSORS_LIST[[5]]
  # PRED_XYZ_Goffset_allP_allR_ExtR_3_Pos <- LOSS_POS_TENSORS_LIST[[6]]
  # TARGET_XYZ_allR_ExtR_3_Pos <- LOSS_POS_TENSORS_LIST[[7]]
  # ACTUAL_XYZ_Goffset_allP_allR_ExtR_3_Pos <- LOSS_POS_TENSORS_LIST[[8]]
  # PRIOR_XYZ_allP_allR_ExtR_3_Pos <- LOSS_POS_TENSORS_LIST[[9]]
  # PLOT_N_Pos <- LOSS_POS_TENSORS_LIST[[10]]
  # TARGET_TID_Pos <- LOSS_POS_TENSORS_LIST[[11]]
  # LOSS_BCE_Pos <- LOSS_POS_TENSORS_LIST[[12]]
  
  
  ##################
  # ROI LEVEL OUTPUT
  ##################
  
  #VNET_OUTPUT_out4
  VNET_OUTPUT_out4 <- VNET_OUTPUT_LIST[[1]]
  TARGET_XYZWLHR_allP_allR_ExtR <- torch_clone(VNET_OUTPUT_out4[[1]])$to(device = "cpu")         # 16 64 16     ### DOM DOM DOM THIS IS THE SCALED ??? 
  ACTUAL_Goffset_allP_allR_ExtR <- torch_clone(VNET_OUTPUT_out4[[2]])$to(device = "cpu")         # 16 64 16     ### DOM DOM DOM THIS IS THE SCALED ??? 
  PRIOR_XYZWLHR_allP_allR_ExtR <- torch_clone(VNET_OUTPUT_out4[[3]])$to(device = "cpu")          # 16 64 16     ### DOM DOM DOM THIS IS THE SCALED ??? 
  # OUTPUT_Vox_Den_RoI <- torch_clone(VNET_OUTPUT_out4[[4]])$to(device = "cpu") 
  # OUTPUT_RoI_32Ch <- torch_clone(VNET_OUTPUT_out4[[5]])$to(device = "cpu")                       # 16 64 32  8  8 16     # 64 ROIS, EACH WITH 16 CHANNELS FOR ALL VOXELS IN ROI
  # pred_VoxDice <- torch_clone(VNET_OUTPUT_out4[[6]])$to(device = "cpu")                          # 16 64 1  8  8 16      # 64 ROIS, EACH WITH  1 "softmax" CHANNELS FOR ALL VOXELS IN ROI
  # TARGET_MASK <- torch_clone(VNET_OUTPUT_out4[[7]])$to(device = "cpu")                           # 16 64 8  8 16         # 64 ROIS, BINARY MASK FOR ALL VOXELS IN ROI
  
  # VNET_OUTPUT_out5
  VNET_OUTPUT_out5 <- VNET_OUTPUT_LIST[[2]]
  Binary_Score <- torch_clone(VNET_OUTPUT_out5[[1]])$to(device = "cpu")                          # 16 64  2        # 64 ROIS, 2 Scores for background or tree
  PRED_Goffset_allP_allR_ExtR <- torch_clone(VNET_OUTPUT_out5[[2]])$to(device = "cpu")
  
  
  Number_Plots <- TARGET_LIST[[2]]$size(1)
  for(p in 1:length(Number_Plots)){ # Number_Plots
    
    # PLOT INFO
    oneP_BestWorst_IoU <- torch_clone(TARGET_LIST[[2]][p, .., drop = TRUE])$to(device ="cpu")  # DOM DOM DOM !!! SEE Indexing Error when using ..  https://github.com/mlverse/torch/issues/695
    Plot_ID <- as.array(oneP_BestWorst_IoU[1,2]) 
    TID_RoI <- as.array(oneP_BestWorst_IoU[,3])
    PRIOR_Type <- as.array(oneP_BestWorst_IoU[,17])
    
    ###############################
    # GET PLOT SPECIFIC VOX TENSORS   # DOM DOM DOM !!! SEE Indexing Error when using .. https://github.com/mlverse/torch/issues/695
    ###############################
    
    # INPUT LISTS
    oneP_Vox_Den_ExtP <- torch_clone(INPUT_LIST[[1]][p, .., drop = FALSE])$to(device ="cpu")          # 1 1 16 16 40  INPUT_VOX_Den_T           1 CHANNEL PROVIDING DENSITY OF POINTS IN EACH VOXEL
    oneP_RoI_Vox <- torch_clone(INPUT_LIST[[2]][p, .., drop = FALSE])$to(device ="cpu")               # 1 64  7       INPUT_ROI_T               64 ROI LOCATOINS AND ID
    oneP_RoI_Dec <- torch_clone(INPUT_LIST[[3]][p, .., drop = FALSE])$to(device ="cpu")               # 1 64  7       INPUT_ROI_T               64 ROI LOCATOINS AND ID
    oneP_PRIOR_XYZWLHR_ExtP  <- torch_clone(INPUT_LIST[[4]][p, .., drop = FALSE])$to(device ="cpu")   # 1 64 16       INPUT_PRIOR_XYZWLHR_T     64 UNIQUE PRIORS WITH XYZWLHR LCOATION
    
    # TARGET LISTS
    oneP_Vox_TID_ExtP <- torch_clone(TARGET_LIST[[1]][p, .., drop = FALSE])$to(device ="cpu")         # 1 16 16 40       TARGET_VOX_TID_T   
    oneP_TARGET_XYZWLHR_ExtP <- torch_clone(TARGET_LIST[[3]][p, .., drop = FALSE])$to(device ="cpu") 
    oneP_TID <- torch_clone(TARGET_LIST[[4]][p, .., drop = FALSE])$to(device ="cpu")                  # 1 64             TARGET_TID_T       
    oneP_STOCKING <- torch_clone(TARGET_LIST[[5]][p, .., drop = FALSE])$to(device ="cpu")             #  1 64            TARGET_STOCKING      
    
    ##################################
    # GENERATING LAS FILE  FROM TENSOR
    ##################################
    LAS_oneP_ExtP <- TENSOR_LAS_RoI_FUN(oneP_Vox_Den_ExtP, oneP_Vox_TID_ExtP,  Para_Target_Base,  Para_Target_Z_Height)
    LAS_oneP_Trees_ExtP <- filter_poi(LAS_oneP_ExtP, Tensor_Count_Norm > 0)
    LAS_oneP_Trees_ExtP@data$TID <- LAS_oneP_Trees_ExtP@data$Mask
    LAS_oneP_Trees_ExtP@data$GT <- 0
    LAS_oneP_Trees_ExtP@data$Pred <- 0
    
    #################
    # GET RoI OUTPUTS
    #################
    oneP_Pred_XYZWLHR_Goffset_ExtR <- torch_squeeze(torch_clone(PRED_Goffset_allP_allR_ExtR[p, .., drop = FALSE])$to(device ="cpu")) 
    oneP_TARGET_XYZWLHR_allR_ExtR <- torch_squeeze(torch_clone(TARGET_XYZWLHR_allP_allR_ExtR[p, .., drop = FALSE])$to(device ="cpu"))    # torch_clone(VNET_OUTPUT_out4[[1]])#$to(device = device))$to(device = device)             # 16 64 16     ### DOM DOM DOM THIS IS THE SCALED ??? 
    oneP_ACTUAL_Goffset_allR_ExtR <- torch_squeeze(torch_clone(ACTUAL_Goffset_allP_allR_ExtR[p, .., drop = FALSE])$to(device ="cpu"))   # torch_clone(VNET_OUTPUT_out4[[2]])#$to(device = device))$to(device = device)     # 16 64 16     ### DOM DOM DOM THIS IS THE SCALED ??? 
    oneP_PRIOR_XYZWLHR_allR_ExtR  <- torch_squeeze(torch_clone(PRIOR_XYZWLHR_allP_allR_ExtR[p, .., drop = FALSE])$to(device ="cpu"))    # torch_clone(VNET_OUTPUT_out4[[3]])#$t
    
    ######################################################################
    # CONVERT GoffSET TO PREDICT_XYZWLHR (FOR ROI) AND SCALE TO PLOT LEVEL       ### DOM DOM DOM !!! Loc_Loss (smooth_l1) uses TARGET THAT IS XYZWLHR_Goffset # decoded_locs = cxcy_to_xy( gcxgcy_to_cxcy(predicted_locs[i], self.priors_cxcy))
    ######################################################################
    
    oneP_Pred_XYZWLHR_ExtR  <- XYZWLHR_To_FROM_Goffset_GPU_FUN(oneP_Pred_XYZWLHR_Goffset_ExtR, oneP_PRIOR_XYZWLHR_allR_ExtR, 
                                                               Para_Cnt = Para_TriShpParaCnt, device=cpu, To_FROM="FROM")  # oneP_Pred_XYZWLHR_RevGoffset_ExtR
    
    oneP_Pred_XYZWLHR_ExtP <-  SCALE_RoI2PLOT_GPU_FUN(RoI_Dec = oneP_RoI_Dec, oneP_Pred_XYZWLHR_ExtR, 
                                                      Plot_N=1, Para_Cnt = Para_TriShpParaCnt, Normalised = "Yes",
                                                      IN_VERT_or_XYZWLHR = "XYZWLHR", OUT_VERT_or_XYZWLHR = "XYZWLHR", device="cpu")
    
    # LOOP THROUGH ROIS
    for(rrr in 1:oneP_RoI_Dec$size(2)){
      
      #####################################################
      # GENERATE VERTICES OF PLOT SCALE GT, PRIOR, and PRED
      #####################################################
      oneGT_Vert_ExtP <- XYZWHR_TO_VERT_GPU_FUN(oneP_TARGET_XYZWLHR_ExtP[,rrr,]$squeeze(), Base_WL = Para_Base_WL/Para_Target_Base, Normalised = "Yes", Para_Cnt = Para_TriShpParaCnt, device="cpu")
      oneGT_Vert_ExtP <- as.array(oneGT_Vert_ExtP)
      onePrior_Vert_ExtP <- XYZWHR_TO_VERT_GPU_FUN(oneP_PRIOR_XYZWLHR_ExtP[,rrr,]$squeeze(), Base_WL = Para_Base_WL/Para_Target_Base, Normalised = "Yes", Para_Cnt = Para_TriShpParaCnt, device="cpu")
      onePrior_Vert_ExtP <- as.array(onePrior_Vert_ExtP)
      onPred_Vert_ExtP <- XYZWHR_TO_VERT_GPU_FUN(oneP_Pred_XYZWLHR_ExtP[rrr,]$squeeze(), Base_WL = Para_Base_WL/Para_Target_Base, Normalised = "Yes", Para_Cnt = Para_TriShpParaCnt, device="cpu")
      onPred_Vert_ExtP <- as.array(onPred_Vert_ExtP)
      
      ###########################
      # PLOT BBOX AND ROI IN EXTP
      ###########################
      colours <- c( "red","green", "white")
      List_Vert_ExtP <- list(onePrior_Vert_ExtP, oneGT_Vert_ExtP, onPred_Vert_ExtP)
      Title_Plot <- paste("E:", epoch, " P:", Plot_ID, " R:", rrr, " PT:", PRIOR_Type[rrr], sep="")
      PLOT_Ver1_Vert2_VOX_FUN(LAS_oneP_Trees_ExtP, List_Vert_ExtP, Title_Plot, colours, Normalised = "No", Plot_Colour_Att = "Tensor_Count_Norm")
      lines3d(x=c(0, 1), y=c(0,0), z=c(0,0), col="purple", lwd = 3)
      lines3d(x=c(0, 1), y=c(0,0), z=c(1,1), col="purple", lwd = 3, lty=3) # X is purple
      lines3d(x=c(0, 0), y=c(0,1), z=c(0,0), col="blue", lwd = 3)
      lines3d(x=c(0, 0), y=c(0,1), z=c(1,1), col="blue", lwd = 3, lty=3) # Y is blue
      
      oneP_oneRoI_Dec_Vec <- as.array(oneP_RoI_Dec[1,rrr,])[-1]
      Vert_RoI <- GEN_ROI_VERTICES_FUN(oneP_oneRoI_Dec_Vec) # POLYGON OF ROI AREA
      polygon3d(Vert_RoI[1:4,], fill=FALSE, col="yellow")
      polygon3d(Vert_RoI[5:8,], fill=FALSE, col="yellow")
    } # LOOP rrr
  } # LOOP THROUGH PLOTS
}

##################################################################################################################################################################################################################################
##################################################################################################################################################################################################################################

### DOM DOM DOM !!! YOU NEED TO INVESTIGATE THIS AND MAKE SURE IT WORKS
PLOT_NMS_KEPT_FUN <- function(NMS_KEEP_PredBBox_GTBBox_TID){

  ############################################################################################################################################################
  ############################################################################################################################################################
  
  TARGET_TID_Pos
  ############################################################################################################################################################
  ############################################################################################################################################################
  
  
  # Unique <- as.array(NMS_FSc_N[[1]]$to(device="cpu"))
  # Unique <- Unique[which(Unique > 0)]
  Number_Plots <- length(NMS_KEEP_PredBBox_GTBBox_TID)
  
  for(p in 1:Number_Plots ){ # 
    oneKEEP_PredBBox_GTBBox_TID <- NMS_KEEP_PredBBox_GTBBox_TID[[p]]
    PRED_boxes_Keep <- oneKEEP_PredBBox_GTBBox_TID[[1]]
    TARGET_Boxes_Keep <- oneKEEP_PredBBox_GTBBox_TID[[2]]
    oneTARGET_TID_Pos_Keep <- oneKEEP_PredBBox_GTBBox_TID[[3]]
    #
    Unique_Trees <- torch_unique_consecutive(torch_sort(oneTARGET_TID_Pos_Keep)[[1]])[[1]]
    #browser()
    #
    TreeList_Pred_Vert_CIoU <- c()
    TreeList_TARGET_Vert_CIoU <- c()
    Pred_colours <- c()
    n_Target <- dim(Unique_Trees)
    
    Target_colours <- sample(distinctColorPalette(n_Target))
    for(t in 1:dim(Unique_Trees)){
      
      Index_tree <- torch_where(oneTARGET_TID_Pos_Keep == Unique_Trees[t], 1, 0)$to(dtype=torch_bool())
      oneT_PRED_boxes <- PRED_boxes_Keep$squeeze()[Index_tree]
      oneT_TARGET_Boxes <- TARGET_Boxes_Keep$squeeze()[torch_sort(Index_tree$to(dtype = torch_long()), descending =TRUE)[[2]][1]]$unsqueeze(1) # Fixed 0.7.0 +1L  # DOM DOM DOM !!! WORK AROUND WHILST TORCH_SORT IS 0-BASED
      #
      one_T_XYZ <- torch_tensor(c(0.5,0.5,0.5), device=device)$view(c(1,3))$'repeat'(c(dim(oneT_PRED_boxes)[1], 1)) # dim(oneT_PRED_boxes)[1]]
      oneT_Pred_XYZWLHR <- torch_cat(c(one_T_XYZ, oneT_PRED_boxes), dim=2)
      for(k in 1:dim(oneT_Pred_XYZWLHR)[1]){
        Vert_onePred<- XYZWHR_TO_VERT_GPU_FUN( oneT_Pred_XYZWLHR[k,], Base_WL = Para_Base_WL/Para_Target_Base, device=device)
        TreeList_Pred_Vert_CIoU <- c(TreeList_Pred_Vert_CIoU, list(as.array(Vert_onePred$to(device ="cpu")))) 
        Pred_colours <- c(Pred_colours, Target_colours[t])
      }
      
      one_T_XYZ <- torch_tensor(c(0.5,0.5,0.5), device=device)$view(c(1,3))$'repeat'(c(dim(oneT_TARGET_Boxes)[1], 1)) 
      oneT_TARGET_XYZWLHR <- torch_cat(c(one_T_XYZ, oneT_TARGET_Boxes), dim=2)
      for(k in 1:dim(oneT_TARGET_XYZWLHR)[1]){
        Vert_oneTARGET<- XYZWHR_TO_VERT_GPU_FUN( oneT_TARGET_XYZWLHR[k,], Base_WL = Para_Base_WL/Para_Target_Base, device=device)
        TreeList_TARGET_Vert_CIoU <- c(TreeList_TARGET_Vert_CIoU, list(as.array(Vert_oneTARGET$to(device ="cpu")))) 
      }
    }
    
    TIDs <- as.array(Unique_Trees$to(device ="cpu"))
    for(a in 1:length(TreeList_TARGET_Vert_CIoU)){
      TreeList_TARGET_Vert_CIoU[[a]] <- cbind(rep(Batch_Count, nrow(TreeList_TARGET_Vert_CIoU[[a]]) ),
                                              rep(p, nrow(TreeList_TARGET_Vert_CIoU[[a]]) ),
                                              rep(TIDs[[a]], nrow(TreeList_TARGET_Vert_CIoU[[a]]) ), 
                                              TreeList_TARGET_Vert_CIoU[[a]])
    }
    TreeList_TARGET_Vert_CIoU_DF <- data.frame(do.call(rbind, TreeList_TARGET_Vert_CIoU))
    colnames(TreeList_TARGET_Vert_CIoU_DF) <- c("Batch","P_Identifier", "TID", "X", "Y", "Z")
    
    for(aa in 1:length(TreeList_Pred_Vert_CIoU)){
      TreeList_Pred_Vert_CIoU[[aa]] <- cbind(rep(Batch_Count, nrow(TreeList_Pred_Vert_CIoU[[aa]]) ),
                                             rep(p, nrow(TreeList_Pred_Vert_CIoU[[aa]]) ), 
                                             TreeList_Pred_Vert_CIoU[[aa]])
    }
    TreeList_Pred_Vert_CIoU_DF <- data.frame(do.call(rbind, TreeList_Pred_Vert_CIoU))
    colnames(TreeList_Pred_Vert_CIoU_DF) <- c("Batch", "P_Identifier", "X", "Y", "Z")
    
    
    if(p == 1){
      PlotList_Pred_Vert_CIoU <- TreeList_Pred_Vert_CIoU_DF
      PlotList_TARGET_Vert_CIoU <- TreeList_TARGET_Vert_CIoU_DF
    }else{
      PlotList_Pred_Vert_CIoU <- rbind(PlotList_Pred_Vert_CIoU, TreeList_Pred_Vert_CIoU_DF)
      PlotList_TARGET_Vert_CIoU <- rbind(PlotList_TARGET_Vert_CIoU,TreeList_TARGET_Vert_CIoU_DF)
    }
    
    # 
    # PlotList_Pred_Vert_CIoU <- c(PlotList_Pred_Vert_CIoU, TreeList_Pred_Vert_CIoU)
    # PlotList_TARGET_Vert_CIoU <- c(PlotList_TARGET_Vert_CIoU, TreeList_TARGET_Vert_CIoU)
    
    
    
    # # n_Target <- dim(Unique_Trees)
    # # colours <- sample(distinctColorPalette(n))
    # LAS_Test <- LAS(data.frame(X = 0.5, Y=0.5, Z=0.5))
    # LAS_Test@data$TID <- 1
    # Title_Plot <- paste("P", epoch , sep="")
    # PLOT_Ver1_Vert2_VOX_FUN(LAS_Test, TreeList_Pred_Vert_CIoU, Title_Plot, Pred_colours, Normalised = "No", Plot_Colour_Att = "TID")
    # n <- length(TreeList_TARGET_Vert_CIoU)
    # colours <- sample(distinctColorPalette(n))
    # Title_Plot <- paste("T", epoch, sep="")
    # PLOT_Ver1_Vert2_VOX_FUN(LAS_Test, TreeList_TARGET_Vert_CIoU, Title_Plot, Target_colours, Normalised = "No", Plot_Colour_Att = "TID")
    # # browser()
  } # LOOP p
  write.csv(PlotList_TARGET_Vert_CIoU, paste(Folder_TRAIN_OUTPUT, "/train_TARGET_Vert_EPOCH", epoch,"_Batch", Batch_Count, ".csv", sep=""))
  write.csv(PlotList_Pred_Vert_CIoU, paste(Folder_TRAIN_OUTPUT, "/train_Pred_Vert_EPOCH", epoch,"_Batch", Batch_Count, ".csv", sep="")) 
  return(list(PlotList_TARGET_Vert_CIoU, PlotList_Pred_Vert_CIoU))  
}

##################################################################################################################################################################################################################################
##################################################################################################################################################################################################################################



PLOT_PID_IN_CNN <- function(LAS_oneF_allP_Trees_PLOT, allTID_BBox,allTID_LOC, PID){
  for(UPP in 1:length(PID)){
    # browser()
    Unique_PID <- as.numeric(PID[UPP])
    LAS_oneF_oneP_Trees_PLOT <- filter_poi(LAS_oneF_allP_Trees_PLOT, PID %in% Unique_PID ) 
    Unique_TID <- unique(LAS_oneF_oneP_Trees_PLOT@data$TID)
    Unique_TID <- Unique_TID[which(Unique_TID > 1)]
    PLOT_OFFSET_X <- min(LAS_oneF_oneP_Trees_PLOT@data$X)
    PLOT_OFFSET_Y <-min(LAS_oneF_oneP_Trees_PLOT@data$Y)
    LAS_oneF_oneP_Trees_PLOT$X <- LAS_oneF_oneP_Trees_PLOT$X - PLOT_OFFSET_X
    LAS_oneF_oneP_Trees_PLOT$Y <- LAS_oneF_oneP_Trees_PLOT$Y - PLOT_OFFSET_Y
    
    allTID_BBox_PLOT <- allTID_BBox[which(allTID_BBox$TID %in% Unique_TID),]
    allTID_BBox_PLOT$X <-  allTID_BBox_PLOT$X - PLOT_OFFSET_X
    allTID_BBox_PLOT$Y <-  allTID_BBox_PLOT$Y - PLOT_OFFSET_Y
    
    allTID_LOC_PLOT <- allTID_LOC[which(allTID_LOC$TID %in% Unique_TID),]
    allTID_LOC_PLOT$X <-  allTID_LOC_PLOT$X - PLOT_OFFSET_X
    allTID_LOC_PLOT$Y <-  allTID_LOC_PLOT$Y - PLOT_OFFSET_Y
    
    # PLOT THE FLIGHT AND ALL THE BBOXES AND STEM LOCATIONS.
    PLOT_COL_FUN(LAS_oneF_oneP_Trees_PLOT, Title=paste("PID:", PID[UPP]), size = 5, TID_Number = "No", ShowGridID="No", Check_Plot= "Yes")
    points3d(allTID_LOC_PLOT$X,allTID_LOC_PLOT$Y,allTID_LOC_PLOT$Z, color = "black", size = 20 )
    Unique_TID_Poly <- unique(allTID_BBox_PLOT$TID)
    for(BB in 1:length(Unique_TID_Poly)){
      oneBBOx <- allTID_BBox_PLOT[which(allTID_BBox_PLOT$TID %in% Unique_TID_Poly[BB]),]
      polygon3d(oneBBOx$X,
                oneBBOx$Y,
                oneBBOx$Z,
                fill = FALSE, col="blue", lwd=2)
    }
  }
  
}


#
# }
# 
# # #################################################################################################################
# # # MORPHOLOGICAL RECONSTRUCTION USING R (mmand)
# # ##############################################  
# # 
# Morph_reconstruct <- function (marker, mask, kernel)
# {
#   previous <- marker
#   repeat{
#     marker <- pmin(dilate(marker,kernel), mask)
#     if (all(marker == previous))
#       return (marker)
#     previous <- marker
#   }
# }
# 
# 
# 
# 
# ##############################################################################################################################################################################################################
# ##############################################################################################################################################################################################################
# # XYZWHR_TO_VERT_GPU_FUN
# ##############################################################################################################################################################################################################
# ########################################################################################################################################################################
# 
# # XYZWHR_TO_VERT_GPU_FUN_Old  <- function(XYZWHRT_Data = oneTriShp, Base_WL = 0.5, Normalised = "Yes", Para_Cnt = 10){
# #   
# #   if(Para_Cnt == 16){
# #     Prefix <- c("", "Bot", "Top")
# #     BB_loop <- 3
# #   }else{
# #     Prefix <- c("", "Top")
# #     BB_loop <- 2
# #   }
# #   
# #   for(BB in 1:BB_loop) {
# #     
# #     if(BB == 1){ 
# #       Cent_X <- XYZWHRT_Data[,which(colnames(XYZWHRT_Data) == "X_Base")]
# #       Cent_Y <- XYZWHRT_Data[,which(colnames(XYZWHRT_Data) == "Y_Base")]
# #       Length_Y <- Base_WL
# #       Width_X <-Base_WL
# #       Rotate_Deg <-0
# #       Height <-XYZWHRT_Data[,which(colnames(XYZWHRT_Data) == "Z_Base")]
# #     }else{
# #       
# #       Cent_X <- XYZWHRT_Data[,which(colnames(XYZWHRT_Data) == paste( "X_", Prefix[BB],"Box", sep=""))]
# #       Cent_Y <- XYZWHRT_Data[,which(colnames(XYZWHRT_Data) == paste( "Y_", Prefix[BB],"Box", sep=""))]
# #       Length_Y <- XYZWHRT_Data[,which(colnames(XYZWHRT_Data) == paste("L_", Prefix[BB],"Box", sep=""))]
# #       Width_X <-XYZWHRT_Data[,which(colnames(XYZWHRT_Data) == paste( "W_", Prefix[BB],"Box", sep=""))]
# #       Rotate_Deg <-XYZWHRT_Data[,which(colnames(XYZWHRT_Data) == paste( "R_", Prefix[BB], "Box",sep=""))]
# #       Height <-XYZWHRT_Data[,which(colnames(XYZWHRT_Data) == paste( "Z_", Prefix[BB],"Box", sep=""))]
# #       
# #       if(BB==BB_loop){
# #         Tree_Height <- XYZWHRT_Data[,which(colnames(XYZWHRT_Data) == "Z_TopTree")] 
# #       }
# #       
# #       if(Normalised == "Yes"){Rotate_Deg <- Rotate_Deg*180}
# #     }
# #     
# #     Hyp <- sqrt((0.5*Length_Y)^2+ (0.5*Width_X)^2)
# #     Deg1 <-Rotate_Deg
# #     Deg2 <- rad2deg(asin((0.5*Width_X)/Hyp))
# #     Deg3 <- Deg1-Deg2
# #     
# #     X1_Offset <- sin(deg2rad(Deg3))*Hyp
# #     Y1_Offset <- cos(deg2rad(Deg3))*Hyp
# #     
# #     X1 <- Cent_X -X1_Offset 
# #     X2 <- Cent_X +X1_Offset 
# #     Y1 <- Cent_Y -Y1_Offset 
# #     Y2 <- Cent_Y +Y1_Offset 
# # 
# #     Deg4 <-90-Deg1-Deg2
# #     Y2_Offset <- sin(deg2rad(Deg4))*Hyp
# #     X2_Offset <- cos(deg2rad(Deg4))*Hyp
# #     
# #     X3 <- Cent_X + X2_Offset 
# #     X4 <- Cent_X - X2_Offset 
# #     Y3 <- Cent_Y + Y2_Offset 
# #     Y4 <- Cent_Y - Y2_Offset 
# #     
# #     XYZ <- data.frame(X = c(X1,  X3, X2, X4),
# #                       Y = c(Y1,  Y3, Y2, Y4),
# #                       Z = rep(Height, 4))  # THESES ARE ORDERED SO THAT THE VERTICES ARE LISTED IN AN ODER THAT IS CLOCKWISE WITHOUT MAKING A CROSS
# #     
# #     if(BB == 1){
# #       Vertices <- XYZ
# #     }else{
# #       Vertices <- rbind(Vertices, XYZ)
# #     }
# #     
# #   }
# #   XYZ$Z <- Tree_Height 
# #   
# #   Vertices <- rbind(Vertices, XYZ)
# #   Vertices <- as.matrix(Vertices)
# #   Vertices <- na.omit(Vertices)
# # 
# #   return(Vertices)
# # }
# XYZWHR_TO_VERT_FUN  <- function(XYZWHRT_Data = oneTriShp, Base_WL = 0.5, Normalised = "Yes", Para_Cnt = 10){
# 
#   if(Para_Cnt == 16){
#     Prefix <- c("", "Bot", "Top")
#     BB_loop <- 3
#   }else{
#     Prefix <- c("", "Top")
#     BB_loop <- 2
#   }
# 
#   for(BB in 1:2) { # BB_loop
# 
#     if(BB == 1){
#       Cent_X <- XYZWHRT_Data[,which(colnames(XYZWHRT_Data) == "X_Base")]
#       Cent_Y <- XYZWHRT_Data[,which(colnames(XYZWHRT_Data) == "Y_Base")]
#       Length_Y <- Base_WL
#       Width_X <- Base_WL
#       Rotate_Deg <- 0
#       Height <-XYZWHRT_Data[,which(colnames(XYZWHRT_Data) == "Z_Base")]
#     }else{
# 
#       Cent_X <- XYZWHRT_Data[,which(colnames(XYZWHRT_Data) == paste( "X_", Prefix[BB],"Box", sep=""))]
#       Cent_Y <- XYZWHRT_Data[,which(colnames(XYZWHRT_Data) == paste( "Y_", Prefix[BB],"Box", sep=""))]
#       Length_Y <- XYZWHRT_Data[,which(colnames(XYZWHRT_Data) == paste("L_", Prefix[BB],"Box", sep=""))]
#       Width_X <-XYZWHRT_Data[,which(colnames(XYZWHRT_Data) == paste( "W_", Prefix[BB],"Box", sep=""))]
#       Rotate_Deg <-XYZWHRT_Data[,which(colnames(XYZWHRT_Data) == paste( "R_", Prefix[BB], "Box",sep=""))]
#       Height <-XYZWHRT_Data[,which(colnames(XYZWHRT_Data) == paste( "Z_", Prefix[BB],"Box", sep=""))]
# 
#       if(BB==2){ #BB_loop
#         Tree_Height <- XYZWHRT_Data[,which(colnames(XYZWHRT_Data) == "Z_TopTree")]
#       }
# 
#       if(Normalised == "Yes"){Rotate_Deg <- Rotate_Deg*180}
# 
#     }
# 
#     Rotate_Deg <- deg2rad(Rotate_Deg) # Rotate_Deg$deg2rad # 
# 
#     # translate point to origin
#     tempX <- c(Cent_X-(0.5*Width_X), Cent_X-(0.5*Width_X), Cent_X+(0.5*Width_X),  Cent_X+(0.5*Width_X))
#     tempY <- c(Cent_Y-(0.5*Length_Y), Cent_Y+ (0.5*Length_Y), Cent_Y+(0.5*Length_Y), Cent_Y- (0.5*Length_Y))
#     # browser()
#     tempX = tempX - Cent_X #c(tempX,Cent_X) #- Cent_X;
#     tempY = tempY - Cent_Y #c(tempY,Cent_Y) #- Cent_Y;
# 
#     # now apply rotation
#     rotatedX = tempX*cos(Rotate_Deg) - tempY*sin(Rotate_Deg);
#     rotatedY = tempX*sin(Rotate_Deg) + tempY*cos(Rotate_Deg);
# 
#     # translate back
#     XNew <- rotatedX + Cent_X
#     YNew <- rotatedY + Cent_Y
# 
#     XYZ <- data.frame(X= XNew, Y = YNew, Z = rep(Height, 4))
# 
#     # # ORDER SO SMALLEST X IS ALWAYS FIRST AND THEN MOVE AROUND CONSISTENTLY FROM THERE
#     # browser()
#     # XYZ <- XYZ[order(XYZ$X, XYZ$Y),]
# 
#     if(BB == 1){
#       Vertices <- XYZ
#     }else{
#       # browser()
#       Vertices <- rbind(Vertices, XYZ)
#     }
# 
#   }
#   XYZ$Z <- Tree_Height
# 
#   Vertices <- rbind(Vertices, XYZ)
#   Vertices <- as.matrix(Vertices)
#   Vertices <- na.omit(Vertices)
# 
#   # # PLOTTING TO SEE RESULTS
#   # browser()
#   # Vertices_Plot <- data.frame(Vertices)
#   # Shift_X <- min(Vertices_Plot$X)
#   # Shift_Y <-   min(Vertices_Plot$Y)
#   # Vertices_Plot$X <- Vertices_Plot$X - Shift_X
#   # Vertices_Plot$Y <- Vertices_Plot$Y - Shift_Y
#   # plot(Vertices_Plot$X[1:4], Vertices_Plot$Y[1:4], col="red", xlim = c(range(Vertices_Plot$X)), ylim=(range(Vertices_Plot$Y)))
#   # par(new=TRUE)
#   # plot(Vertices_Plot$X[5:8], Vertices_Plot$Y[5:8], col="blue", xlim = c(range(Vertices_Plot$X)), ylim=(range(Vertices_Plot$Y)))
#   # XYZWHRT_Data_N <- XYZWHRT_Data  .. , ylim = c(range(Vertices_Plot)), xlim = c(range(Vertices_Plot))
# 
#   return(Vertices)
# }
# 
# 
# ##############################################################################################################################################################################################################
# ##############################################################################################################################################################################################################
# # XYZWLHR_To_XYZWLHR_N_FUN
# ##############################################################################################################################################################################################################
# ########################################################################################################################################################################
# 
# XYZWLHR_To_XYZWLHR_N_FUN <- function(XYZWLHR = XYZWLHR_oneF_oneMP_oneP_allPrior, Shift, Para_Cnt = Para_Cnt){
#   
#   XYZWLHR$X_Base <- (XYZWLHR$X_Base- Shift$Shift_Vox_X)/Shift$Shift_Vox_N_X
#   XYZWLHR$Y_Base  <- (XYZWLHR$Y_Base- Shift$Shift_Vox_Y)/Shift$Shift_Vox_N_Y
#   
#   # if(Para_Cnt == 16){
#   #   XYZWLHR$X_BotBox <- (XYZWLHR$X_BotBox- Shift$Shift_Vox_X)/Shift$Shift_Vox_N_X
#   #   XYZWLHR$Y_BotBox <- (XYZWLHR$Y_BotBox- Shift$Shift_Vox_Y)/Shift$Shift_Vox_N_Y
#   #   
#   #   XYZWLHR$L_BotBox <- (XYZWLHR$L_BotBox)/Shift$Shift_Vox_N_Y   
#   #   XYZWLHR$W_BotBox <- (XYZWLHR$W_BotBox)/Shift$Shift_Vox_N_X     
#   #   XYZWLHR$R_BotBox <- (XYZWLHR$R_BotBox)/180
#   #   XYZWLHR$Z_BotBox  <- (XYZWLHR$Z_BotBox - Shift$Shift_Vox_Z)/Shift$Shift_Vox_N_Z  
#   # }
# 
#   
#   XYZWLHR$X_TopBox <- (XYZWLHR$X_TopBox- Shift$Shift_Vox_X)/Shift$Shift_Vox_N_X    
#   XYZWLHR$Y_TopBox <- (XYZWLHR$Y_TopBox- Shift$Shift_Vox_Y)/Shift$Shift_Vox_N_Y 
#   
#   XYZWLHR$L_TopBox <- (XYZWLHR$L_TopBox)/Shift$Shift_Vox_N_Y  
#   XYZWLHR$W_TopBox <- (XYZWLHR$W_TopBox)/Shift$Shift_Vox_N_X   
#   XYZWLHR$R_TopBox <- (XYZWLHR$R_TopBox)/180
#   
#   XYZWLHR$Z_Base <- (XYZWLHR$Z_Base- Shift$Shift_Vox_Z)/Shift$Shift_Vox_N_Z  
#  
#   XYZWLHR$Z_TopBox <- (XYZWLHR$Z_TopBox- Shift$Shift_Vox_Z)/Shift$Shift_Vox_N_Z 
#   XYZWLHR$Z_TopTree <- (XYZWLHR$Z_TopTree- Shift$Shift_Vox_Z)/Shift$Shift_Vox_N_Z 
#   #browser()
#   return(XYZWLHR)
# }
# 
# ##############################################################################################################################################################################################################
# ##############################################################################################################################################################################################################
# # XYZWLHR_To_GXYZWLHR_FUN
# # ##############################################################################################################################################################################################################
# # ##############################################################################################################################################################################################################
# # ### DOM DOM DOM !!! TO SPEED UP CODE MAYBE WORTH IMPROVING FUNCTION XYZWLHR_To_Goffset_FUN SO IT ACCEPTS MANY BATCHES AND THEN 
# # # UNDERTAKING THE COMPUTATION FOR ALL BATCHES AT ONCE...
# # XYZWLHR_To_Goffset_FUN <- function(Input_XYZWLHR, Prior_XYZWLHR, Plot_Output = "No", Normalised = "Yes", Para_Cnt = Para_Cnt, use_Tensor = "Yes", Batch_Count){
# #  
# #   # if(Para_Cnt == 16){
# #   #   Index_Z <- torch_tensor(c(3, 8, 14, 16), dtype = torch_long())     
# #   #   Index_XY <- torch_tensor(c(1,2), dtype = torch_long())
# #   #   Index_X_Box <- torch_tensor(c(4,10), dtype = torch_long())      
# #   #   Index_Y_Box <- torch_tensor(c(5,11), dtype = torch_long())    
# #   #   Index_L <- torch_tensor(c(6,12), dtype = torch_long())            
# #   #   Index_W <- torch_tensor(c(7,13), dtype = torch_long())            
# #   #   Index_R <- torch_tensor(c(9, 15), dtype = torch_long())                
# #   #   Index_all_Order <- torch_tensor(order(c(3, 8, 14, 16, 1, 2, 4, 10, 5, 11, 6, 12, 7, 13, 9, 15)), dtype = torch_long()) 
# #   #   Colnames_XYZWLHR <- c("X_Base", "Y_Base", "Z_Base", 
# #   #                         "X_BotBox", "Y_BotBox", "L_BotBox", "W_BotBox", "Z_BotBox", "R_BotBox", 
# #   #                         "X_TopBox", "Y_TopBox", "L_TopBox", "W_TopBox", "Z_TopBox ", "R_TopBox", 
# #   #                         "Z_TopTree")
# #   #   
# #   # }else{
# #   
# #     # DOM DOM DOM !!! IN THE TORCH_INITIALISE_V18 YOU RUN THIS CODE WITH TENSORS AND BELOW ARE THE RESULTS THAT ARE REQUIRED
# #     # "X_TopBox 4 was 10", "Y_TopBox 5 was 11", "L_TopBox 6 was 12", "W_TopBox 7 was 13", "Z_TopBox 8 was 14", "R_TopBox 9 was 15", 
# #     # "Z_TopTree 10 was 16"
# #     
# #     if(use_Tensor == "Yes"){
# #       #1"X_Base", 2"Y_Base", 3"Z_Base", 4"X_TopBox", 5"Y_TopBox", 6"L_TopBox", 7"W_TopBox", 8"Z_TopBox", 9"R_TopBox",  10"Z_TopTree"
# #       Index_Z <- torch_tensor(c(3,  8, 10), dtype = torch_long())
# #       Index_XY <- torch_tensor(c(1,2), dtype = torch_long())
# #       Index_X_Box <- torch_tensor(c(4), dtype = torch_long())
# #       Index_Y_Box <- torch_tensor(c(5), dtype = torch_long())
# #       Index_L <- torch_tensor(c(6), dtype = torch_long())
# #       Index_W <- torch_tensor(c(7), dtype = torch_long())
# #       Index_R <- torch_tensor(c( 9), dtype = torch_long())
# #       Index_all_Order <- torch_tensor(order(c(3, 8, 10, 1,2,  4,   5, 6,  7,  9)), dtype = torch_long())
# #       
# #     }else{
# #       Index_Z <- torch_tensor(c(3, 14, 16), dtype = torch_long())
# #       Index_XY <- torch_tensor(c(1,2), dtype = torch_long())
# #       Index_X_Box <- torch_tensor(c( 10), dtype = torch_long())
# #       Index_Y_Box <- torch_tensor(c(11), dtype = torch_long())
# #       Index_L <- torch_tensor(c(12), dtype = torch_long())
# #       Index_W <- torch_tensor(c(13), dtype = torch_long())
# #       Index_R <- torch_tensor(c( 15), dtype = torch_long())
# #       Index_all_Order <- torch_tensor(order(c(3, 14, 16,  1, 2, 10,  11, 12,  13,  15)), dtype = torch_long())
# #     }
# # 
# #     Colnames_XYZWLHR <- c("X_Base", "Y_Base", "Z_Base", 
# #                           "X_TopBox", "Y_TopBox", "L_TopBox", "W_TopBox", "Z_TopBox", "R_TopBox", 
# #                           "Z_TopTree")
# #   #}
# #   
# #   # browser()
# #   #if(Batch_Count == 4) {browser()}
# #   # Z RANGE IS USED TO NORMALISE THE Goffset_Z
# #   Z_Range <- torch_unsqueeze(torch_subtract(torch_amax(Prior_XYZWLHR[,Index_Z],1), torch_amin(Prior_XYZWLHR[,Index_Z],1)),2)
# #   Z_Range_View <- Z_Range$view(c(1,3))
# #   # Z_Range_3Zs <- torch_cat(list(Z_Range, Z_Range,Z_Range),2)
# #   # Z_Range_3Zs <- torch_cat(3*list(Z_Range))
# #   Z_Range_3Zs <- torch_repeat_interleave(Z_Range_View, torch_tensor(64L), dim = 1)
# #   Goffset_Z <- (Input_XYZWLHR[,Index_Z] - Prior_XYZWLHR[,Index_Z])/Z_Range_3Zs
# #   
# #   Goffset_XY <- (Input_XYZWLHR[,Index_XY] - Prior_XYZWLHR[,Index_XY])
# # 
# #   Diagonal <- torch_sqrt(torch_square(Prior_XYZWLHR[,Index_W]) + torch_square(Prior_XYZWLHR[,Index_L]))
# #   Goffset_XBox <- (Input_XYZWLHR[,Index_X_Box] - Prior_XYZWLHR[,Index_X_Box]) / Diagonal                         # (T-P)/sqrt((PW)^2 + (PL)^2)
# #   Goffset_YBox <- (Input_XYZWLHR[,Index_Y_Box] - Prior_XYZWLHR[,Index_Y_Box]) / Diagonal                         # (T-P)/PL
# #   Goffset_L <-torch_log((Input_XYZWLHR[,Index_L]/ Prior_XYZWLHR[,Index_L])) 
# #   Goffset_W <-torch_log((Input_XYZWLHR[,Index_W]/ Prior_XYZWLHR[,Index_W])) 
# #   Goffset_R <- (Input_XYZWLHR[,Index_R] - Prior_XYZWLHR[,Index_R])
# # 
# #   GOffset_all <- torch_cat(list(Goffset_Z, 
# #                                 Goffset_XY,
# #                                 Goffset_XBox, 
# #                                 Goffset_YBox, 
# #                                 Goffset_L,
# #                                 Goffset_W,
# #                                 Goffset_R),2)[,Index_all_Order] 
# #   # browser()
# #   FINITE_Count <- length(which(is.finite(as.vector(as.array(GOffset_all)))))
# #   if(FINITE_Count != length(as.vector(as.array(GOffset_all)))){browser()} # not one of the values NA, NaN, Inf or -Inf
# #   
# #   # # REMOVING ALL NA/NaN/Inf/-Inf with 0 VALUE  .... log(0/0) = NaN .... 1/0 = Inf .... -1/0 = -Inf      
# #   # Finite_Bool <- torch_isfinite(GOffset_all)
# #   # ### DOM DOM DOM !!! UN HASHTAG BELOW LINE ONCE TESTING COMPLETED
# #   # GOffset_all <- torch_where(Finite_Bool, GOffset_all, Finite_Bool$to(dtype = torch_float()))
# #   
# #   if(Plot_Output == "Yes"){
# # 
# #     
# #     XYZ_RoI_Empty_N <- EMPTY_RoI_VOX_FUN(para_RoI_Pool_Dim_XY, para_RoI_Pool_Dim_Z)
# #     LAS_RoI_Empty_N <- LAS(XYZ_RoI_Empty_N)
# #     LAS_RoI_Empty_N@data$TID <- 1
# #     
# #     Test <- as.data.frame(as.array(GOffset_all))
# #     colnames(Test) <- Colnames_XYZWLHR
# #     
# #     GOffset_all2 <- torch_where(Finite_Bool, GOffset_all, Finite_Bool$to(dtype = torch_float()))
# #     Test2 <- as.data.frame(as.array(GOffset_all2))
# #     colnames(Test2) <- Colnames_XYZWLHR
# #     
# #     # Input_XYZWLHR, Prior_XYZWLHR
# #     oneP_PRIOR_XYZWLHR_ExtR_DF <- Prior_XYZWLHR
# #     oneP_PRIOR_XYZWLHR_ExtR_DF <-as.data.frame(as.array(oneP_PRIOR_XYZWLHR_ExtR_DF))
# #     colnames(oneP_PRIOR_XYZWLHR_ExtR_DF) <- Colnames_XYZWLHR
# #     
# #     oneP_TARGET_XYZWLHR_ExtR_DF <- Input_XYZWLHR
# #     oneP_TARGET_XYZWLHR_ExtR_DF <-as.data.frame(as.array(oneP_TARGET_XYZWLHR_ExtR_DF))
# #     colnames(oneP_TARGET_XYZWLHR_ExtR_DF) <- Colnames_XYZWLHR
# #     
# #     for(PR in 1:nrow(Input_XYZWLHR)){
# #       
# #       oneP_onePR_PRIOR_XYZWLHR_ExtR_DF <- oneP_PRIOR_XYZWLHR_ExtR_DF[PR,]
# #       
# #       
# #       
# #       oneP_onePR_TARGET_XYZWLHR_ExtR_DF <- oneP_TARGET_XYZWLHR_ExtR_DF[PR,]
# #       
# #       # FOR PLOTTING PURPOSES MAKE THE ROTATION BETWEEN 0 and 180
# #       # if(Para_Cnt == 16){
# #       #   oneP_onePR_PRIOR_XYZWLHR_ExtR_DF$R_TopBox <- oneP_onePR_PRIOR_XYZWLHR_ExtR_DF$R_TopBox*180
# #       #   oneP_onePR_PRIOR_XYZWLHR_ExtR_DF$R_BotBox <- oneP_onePR_PRIOR_XYZWLHR_ExtR_DF$R_BotBox*180
# #       #   oneP_onePR_TARGET_XYZWLHR_ExtR_DF$R_TopBox <- oneP_onePR_TARGET_XYZWLHR_ExtR_DF$R_TopBox*180
# #       #   oneP_onePR_TARGET_XYZWLHR_ExtR_DF$R_BotBox <- oneP_onePR_TARGET_XYZWLHR_ExtR_DF$R_BotBox*180
# #       # }else{
# #         oneP_onePR_PRIOR_XYZWLHR_ExtR_DF$R_TopBox <- oneP_onePR_PRIOR_XYZWLHR_ExtR_DF$R_TopBox*180
# #         oneP_onePR_TARGET_XYZWLHR_ExtR_DF$R_TopBox <- oneP_onePR_TARGET_XYZWLHR_ExtR_DF$R_TopBox*180
# #       #}
# #       #oneP_onePR_Pred_XYZWLHR_ExtR_DF<- oneP_Pred_XYZWLHR_ExtR_DF[PR,]
# #       #browser()
# #       
# #       ######################################################
# #       # CONVERT XYZWLHR (TARGET AND PREDICTED) INTO VERTICES
# #       ######################################################
# #       oneP_PRIOR_Vert <- as.data.frame(XYZWHR_TO_VERT_GPU_FUN(oneP_onePR_PRIOR_XYZWLHR_ExtR_DF, Base_WL = Para_Base_WL/Para_Target_Base, Normalised = Normalised, Para_Cnt = Para_Cnt))
# #       oneP_TARGET_Vert <- as.data.frame(XYZWHR_TO_VERT_GPU_FUN(oneP_onePR_TARGET_XYZWLHR_ExtR_DF, Base_WL = Para_Base_WL/Para_Target_Base, Normalised = Normalised, Para_Cnt = Para_Cnt))
# #       #oneP_Pred_Vert <- as.data.frame(XYZWHR_TO_VERT_GPU_FUN(oneP_onePR_Pred_XYZWLHR_ExtR_DF, Base_WL = Para_Base_WL/Para_Target_Base))
# #       
# #       Prior_Type <-  "NA" #BestWorst_IoU$Prior_Type[PR]
# #       Plot_ID <- "NA"
# #       Flight <- "NA"
# #       Title_Plot <- paste("F:", Flight,  "P:", Plot_ID, "PT:", Prior_Type, sep="")
# #       
# #       colours <- c( "red","green")
# #       List_Vert <- list(oneP_PRIOR_Vert, oneP_TARGET_Vert) # , oneP_Pred_Vert
# #       
# #       ### DOM DOM DOM !!!! UNHASHTAG BELOW AND CHECK RESULTS AFTER LUNCH!!!!!
# #       # here here here
# #       
# #       # PLOT_Ver1_Vert2_VOX_FUN(LAS_RoI_Empty_N, List_Vert, Title_Plot, colours)
# #       # 
# #       # # PRINTING RESULTS
# #       # oneTest <- Test[PR,]
# #       # oneTest2 <- Test2[PR,]
# #       # Comparison <- rbind(oneP_onePR_PRIOR_XYZWLHR_ExtR_DF, oneP_onePR_TARGET_XYZWLHR_ExtR_DF, oneTest, oneTest2)
# #       # Comparison <-Comparison %>%  mutate_if(is.numeric, round, digits=3)
# #       # Comparison <- data.frame(OBJECT= c("PRIOR", "TARGET", "GOffset","GOffset Fill 0"),  Comparison)
# #       # print(Comparison)
# #       
# #       #### DOM DOM DOM !!! THE R ROTATE NEEDS TO BE CONVERTED TO A DEGREE BEFORE IT IS PLOTTED. 
# #       # browser()
# #     }
# #   }
# # 
# #   return(GOffset_all) 
# # }
# 
# 
# # ##############################################################################################################################################################################################################
# # ##############################################################################################################################################################################################################
# # # Goffset_To_XYZWLHR_FUN
# # ##############################################################################################################################################################################################################
# # ##############################################################################################################################################################################################################
# # NEEDED FOR THE FINAL VISuALISATION (I THINK)
# 
# # Goffset_To_XYZWLHR_FUN <- function(Input_XYZWLHR_GOffset, Prior_XYZWLHR, Para_Cnt = Para_Cnt, use_Tensor = "Yes"){
# # 
# #   # if(Para_Cnt == 16){
# #   #   Index_Z <- torch_tensor(c(3, 8, 14, 16), dtype = torch_long(), device=device)
# #   #   Index_XY <- torch_tensor(c(1,2), dtype = torch_long(), device=device)
# #   #   Index_X_Box <- torch_tensor(c(4,10), dtype = torch_long(), device=device)
# #   #   Index_Y_Box <- torch_tensor(c(5,11), dtype = torch_long(), device=device)
# #   #   Index_L <- torch_tensor(c(6,12), dtype = torch_long(), device=device)
# #   #   Index_W <- torch_tensor(c(7,13), dtype = torch_long(), device=device)
# #   #   Index_R <- torch_tensor(c(9, 15), dtype = torch_long(), device=device)
# #   #   Index_all_Order <- torch_tensor(order(c(3, 8, 14, 16, 1, 2, 4, 10,  5, 11, 6, 12, 7, 13, 9, 15)), dtype = torch_long(), device=device)
# #   # 
# #   # }else{
# #     if(use_Tensor == "Yes"){
# #       #1"X_Base", 2"Y_Base", 3"Z_Base", 4"X_TopBox", 5"Y_TopBox", 6"L_TopBox", 7"W_TopBox", 8"Z_TopBox", 9"R_TopBox",  10"Z_TopTree"
# #       Index_Z <- torch_tensor(c(3, 8, 10), dtype = torch_long(), device=device)
# #       Index_XY <- torch_tensor(c(1,2), dtype = torch_long(), device=device)
# #       Index_X_Box <- torch_tensor(c(4), dtype = torch_long(), device=device)
# #       Index_Y_Box <- torch_tensor(c(5), dtype = torch_long(), device=device)
# #       Index_L <- torch_tensor(c(6), dtype = torch_long(), device=device)
# #       Index_W <- torch_tensor(c(7), dtype = torch_long(), device=device)
# #       Index_R <- torch_tensor(c(9), dtype = torch_long(), device=device)
# #       Index_all_Order <- torch_tensor(order(c(3, 8, 10, 1, 2, 4,  5, 6,  7,  9)), dtype = torch_long(), device=device)
# #     }else{
# #       Index_Z <- torch_tensor(c(3, 14, 16), dtype = torch_long(), device=device)
# #       Index_XY <- torch_tensor(c(1,2), dtype = torch_long(), device=device)
# #       Index_X_Box <- torch_tensor(c(10), dtype = torch_long(), device=device)
# #       Index_Y_Box <- torch_tensor(c(11), dtype = torch_long(), device=device)
# #       Index_L <- torch_tensor(c(12), dtype = torch_long(), device=device)
# #       Index_W <- torch_tensor(c(13), dtype = torch_long(), device=device)
# #       Index_R <- torch_tensor(c( 15), dtype = torch_long(), device=device)
# #       Index_all_Order <- torch_tensor(order(c( 3, 14, 16, 1, 2, 10, 11, 12,  13,  15)), dtype = torch_long(), device=device)
# #     }
# #   #}
# # 
# #   ### DOM DOM DOM !!! THIS NEEDS UPDATING!!!
# #   #XYZWLHR_Z <- (Input_XYZWLHR_GOffset[,Index_Z] + Prior_XYZWLHR[,Index_Z])
# #   # Z_Range <- torch_unsqueeze(torch_subtract(torch_amax(Prior_XYZWLHR[,Index_Z],1), torch_amin(Prior_XYZWLHR[,Index_Z],1)),2)
# #   # Z_Range_3Zs <- torch_cat(list(Z_Range, Z_Range,Z_Range),2)
# #   # XYZWLHR_Z <- (Input_XYZWLHR_GOffset[,Index_Z]*Z_Range_3Zs)  + Prior_XYZWLHR[,Index_Z]    # (Off*ZRange) + Pr
# #   
# #   Z_Range <- torch_unsqueeze(torch_subtract(torch_amax(Prior_XYZWLHR[,Index_Z],1), torch_amin(Prior_XYZWLHR[,Index_Z],1)),2)
# #   Z_Range_View <- Z_Range$view(c(1,3))
# #   # Z_Range_3Zs <- torch_cat(list(Z_Range, Z_Range,Z_Range),2)
# #   # Z_Range_3Zs <- torch_cat(3*list(Z_Range))
# #   Z_Range_3Zs <- torch_repeat_interleave(Z_Range_View, torch_tensor(64L), dim = 1)
# #   XYZWLHR_Z <- (Input_XYZWLHR_GOffset[,Index_Z]*Z_Range_3Zs)  + Prior_XYZWLHR[,Index_Z] 
# #   
# #   XYZWLHR_XY <- (Input_XYZWLHR_GOffset[,Index_XY] + Prior_XYZWLHR[,Index_XY])
# # 
# #   Diagonal <- torch_sqrt(torch_square(Prior_XYZWLHR[,Index_W]) + torch_square(Prior_XYZWLHR[,Index_L]))
# #   XYZWLHR_XBox <- (Input_XYZWLHR_GOffset[,Index_X_Box]*Diagonal)  + Prior_XYZWLHR[,Index_X_Box]                       # (Off*Diag) + Pr
# #   XYZWLHR_YBox <- (Input_XYZWLHR_GOffset[,Index_Y_Box]*Diagonal)  + Prior_XYZWLHR[,Index_Y_Box]                        # (Off*Diag) + Pr
# #   XYZWLHR_L <-torch_exp(Input_XYZWLHR_GOffset[,Index_L])* Prior_XYZWLHR[,Index_L]
# #   XYZWLHR_W <-torch_exp(Input_XYZWLHR_GOffset[,Index_W])* Prior_XYZWLHR[,Index_W]
# #   XYZWLHR_R <- (Input_XYZWLHR_GOffset[,Index_R] + Prior_XYZWLHR[,Index_R])
# # 
# #   GOffset_all <- torch_cat(list(XYZWLHR_Z,
# #                                 XYZWLHR_XY,
# #                                 XYZWLHR_XBox,
# #                                 XYZWLHR_YBox,
# #                                 XYZWLHR_L,
# #                                 XYZWLHR_W,
# #                                 XYZWLHR_R),2)[,Index_all_Order]
# #   return(GOffset_all)
# #   # #browser()
# #   # # REMOVING ALL NA/NaN/Inf/-Inf with 0 VALUE  .... log(0/0) = NaN .... 1/0 = Inf .... -1/0 = -Inf
# #   # Finite_Bool <- torch_isfinite(GOffset_all)
# #   #
# #   # ### DOM DOM DOM !!! UN HASHTAG BELOW LINE ONCE TESTING COMPLETED
# #   # GOffset_all <- torch_where(Finite_Bool, GOffset_all, Finite_Bool$to(dtype = torch_float()))
# # 
# # }
# 
# ##############################################################################################################################################################################################################
# ##############################################################################################################################################################################################################
# # NMS_LOSS_FUN
# ##############################################################################################################################################################################################################
# ##############################################################################################################################################################################################################
# 
# 
# NMS_LOSS_FUN <- function(    Empty_Vox, Triangles_ID_All_Mx, Para_Threshold_IoU, Para_Threshold_Prob,
#                              INPUT_PRIOR_XYZWLHR,
#                              pred_XYZWLHR, pred_Score, pred_VoxDice, 
#                              TARGET_XYZWLHR, TARGET_CLASS, TARGET_MASK, TARGET_STOCK,
#                              Normalised = "Yes", Para_Cnt = Para_Cnt){
#   
#   ### DOM DOM DOM WONDERIN IF OUTPUT_RoI_16Ch <- OUTPUT_out4[[1]]  # 16 64 16  8  8 16     # 64 ROIS, EACH WITH 16 CHANNELS FOR ALL VOXELS IN ROI
#   
#   ### DOM DOM DOM ACTUAL PREDICTED XYZWLHR MAY BE INCORRECT
#   ActualPred_XYZWLHR <- INPUT_PRIOR_XYZWLHR - pred_XYZWLHR  # DOM DOM DOM !!! PRIOR - OFFSET (MAYBE SHOULD BE +)
#   
#   ### DOM DOM DOM !!! BELOW YOU USED THE CLASS PRED_SCORE TO WORK OUT WHICH ONES WHERE PREDICTED TREES (NOTE THAT THIS ELEMENT DOES NOT LEARN SO ITS PROBLEMATIC.)
#   ### DOM DOM DOM !!! MAYBE ALL THE ActualPred_XYZWLHR(pred_XYZWLHR IS OFFSET)  NEED TO BE USED AND WORK OUT THE FINAL ONES THAT OVERLAP THE OBSERVED TREES
#   ### DOM DOM DOM !!! BELOW IS ALL WRONG ... 
#   
#   # LOOP THROUGH BATCHES
#   Para_Threshold_Prob <- 0.5
#   for(Plot_N in 1:pred_Score$size(1)){ #  dim(pred_Score)[1]
#     
#     # IDENTIFY PRIORS WITH HIGH PROBABILITY OF TREE
#     Binary_Prior_HighProbTree <- pred_Score[Plot_N,..] > Para_Threshold_Prob
#     # PredProb <- as.data.frame(as.array(torch_nonzero(Binary_Prior_HighProbTree)+1)) # +1 to make sure its R and not Python Index
#     PredProb <- as.data.frame(as.array(torch_nonzero(Binary_Prior_HighProbTree))) # CHANGED WITH VERSION 0.4.
#     colnames(PredProb) <- c("Prior", "Binary")
#     
#     print(table(PredProb$Binary))
#     
#     if(nrow(PredProb) > 0){
#       Prob <- c()
#       # ORDER HIGH PROBABILITY PRIORS FROM HIGHEST PROBABILITY TO LOWEST PROBABILITY
#       for(j in 1:nrow(PredProb)){
#         oneProb <- as.array(pred_Score[Plot_N,PredProb[j,1],PredProb[j,2]])
#         Prob <- c(Prob, oneProb)
#       }
#       Order_Descend_Prob <- rev(order(Prob))
#       PredProb <- PredProb[Order_Descend_Prob,]
#       
#       # EXTRACT THE PRIOR WITH HIGHEST PROBABILITY OF TREE
#       Pred_NMS <- PredProb[1,] # THE VERY TOP ONE IS USED
#       ### DOM DOM DOM !!! SHOULD YOU GET THE DICE COEFFICIENT FOR THIS BEST PRIOR ... 
#       PredProb <- PredProb[-1,]
#       
#       count <- 0
#       
#       # if(Para_Cnt == 16){
#       #   Colnames_XYZWLHR <- c("X_Base", "Y_Base", "Z_Base", 
#       #                         "X_BotBox", "Y_BotBox", "L_BotBox", "W_BotBox", "Z_BotBox", "R_BotBox", 
#       #                         "X_TopBox", "Y_TopBox", "L_TopBox", "W_TopBox", "Z_TopBox", "R_TopBox", 
#       #                         "Z_TopTree")
#       # }else{
#         Colnames_XYZWLHR <- c("X_Base", "Y_Base", "Z_Base",  
#                               "X_TopBox", "Y_TopBox", "L_TopBox", "W_TopBox", "Z_TopBox", "R_TopBox", 
#                               "Z_TopTree")
#       #}
# 
#       
#       # WHILE LOOP PERFORMS NMS... GETS ALL PRIORS THAT HAVE HIGHEST PROBABILITY OF TREE AND LEAST OVERLAP WITH OTHER PRIORS WITH HIGH PROBABILITY OF TREE
#       Cnt_Prior_Prob <- dim(PredProb)[1]
#       while(Cnt_Prior_Prob > 0){ # WHILE THERE ARE INDICES WITH VALUES
#         
#         for(PN in 1:nrow(Pred_NMS)){
#           
#           # COMPUTED THE LOCATION OF PREDICTIONS
#           # one_Prior_INPUT <- as.data.frame(t(as.matrix(INPUT_PRIOR_XYZWLHR[Plot_N,PredProb[PN,1],]))) #INPUT_PRIOR_XYZWLHR[Pred_NMS[[PN]][1],Pred_NMS[[PN]][2],]
#           # one_Prior_NMS_Predicted <- as.data.frame(t(as.matrix(pred_XYZWLHR[Plot_N,PredProb[PN,1],])))
#           # onePred_XYZWLHR <- one_Prior_INPUT - one_Prior_NMS_Predicted
#           onePred_XYZWLHR <- as.data.frame(t(as.matrix(ActualPred_XYZWLHR[Plot_N,PredProb[PN,1],])))
#           
#           
#           # SECOND EXTRACT ALL THE VOXELS FROM iNMS[PN] PRIOR THAT FALL WITHIN EXISTING PRIOR... IF COUNT IS LESS THAN SOMETHING THEN DISCARD 
#           colnames(onePred_XYZWLHR) <- Colnames_XYZWLHR
#           
#           # FIRST FOR EACH PredProb PRIOR EXTRACT ALL THE VOXELS  (USING WHOLE CUBE SPACE) WITHIN TRISHAPE
#           ############################################################################################
#           Vert_onePred <- XYZWHR_TO_VERT_GPU_FUN(onePred_XYZWLHR, Base_WL = Para_Base_WL, Normalised = Normalise, Para_Cnt = Para_Cnt, device=device) # [,-1] NOT THAT THIS PARAMETERS IS NORMALISED
#           #browser()
#           nbIntersect = INTERSECT_TRI_FUN(Triangles_ID_All_Mx, Vert_onePred, Empty_Vox)
#           insideVect_NMS_Pred <- which(nbIntersect%%2 != 0)
#           Vox_NMS_Pred <- Empty_Vox[insideVect_NMS_Pred,] 
#           
#           # LOOP THROUGH EACH PRIOR THAT IS NOT IN FINAL STOCKING 
#           index_remove_PredProb <- c()
#           for(PP in 1:nrow(PredProb)){
#             one_Pred_Check_IoU <- as.data.frame(t(as.matrix(ActualPred_XYZWLHR[Plot_N,PredProb[PP,1],])))
#             colnames(one_Pred_Check_IoU) <- Colnames_XYZWLHR
#             
#             # FIRST FOR EACH PredProb PRIOR EXTRACT ALL THE VOXELS  (USING WHOLE CUBE SPACE) WITHIN TRISHAPE
#             ############################################################################################
#             Vert_onePred <- XYZWHR_TO_VERT_GPU_FUN(one_Pred_Check_IoU, Base_WL = Para_Base_WL, Normalised = Normalise, Para_Cnt = Para_Cnt, device=device) # [,-1] NOT THAT THIS PARAMETERS IS NORMALISED
#             nbIntersect = INTERSECT_TRI_FUN(Triangles_ID_All_Mx, Vert_onePred, Vox_NMS_Pred)
#             insideVect_i_Pred <- which(nbIntersect%%2 != 0) # LAS_one_i_Prior <- lasfilter(LAS_Empty_Vox, unlist(nbIntersect%%2 != 0))  
#             
#             ############################################################################################
#             
#             # LOOP FINAL STOCKING PRIORS (Pred_NMS) TO SEE IF REMAINING PRIORS (PredProb) OVERLAPS STOCKING PRIORS
#             
#             Overlap_Percent <- length(insideVect_NMS_Pred)/length(insideVect_i_Pred)
#             # IF THERE IS LARGE OVERLAP STORE THE INDEX OF OVERLAPPING PriorProb
#             if(Overlap_Percent > Para_Threshold_IoU){
#               index_remove_PredProb <- c(index_remove_PredProb, PP)
#             }
#           } # PP LOOP WHICH LOOPS THROUGH PredProb (potential Priors)
#           
#           # UPDATE THE Pred_NMS ONLY IF IT DOES NOT ALSO OVERLAP a1 PP
#           index_remove_PredProb <- unique(index_remove_PredProb) # NOT SURE IF THIS IS NECESSARY
#           PredProb <- PredProb[-index_remove_PredProb,]
#           # IF THERE ARE NO MORE PredProb BREAK OUT OF PN LOOP
#           if(nrow(PredProb) == 0){
#             break() 
#           }
#         }# PN LOOP WHICH LOOPS THROUGH Pred_NMS 
#         
#         if(nrow(PredProb) > 0){
#           
#           browser()
#           # IF YOU ARE IN THIS LOOP THEN YOU HAVE ADDED ANOTHER STEM TO THE RESULTS (I THINK)
#           
#           Pred_NMS <- rbind(Pred_NMS, PredProb[1,]) # ADD TOP PRIOR PROB THAT DIDN'T OVERLAP ANY Pred_NMS ...  cTO Pred_NMS
#           PredProb <- PredProb[-1,]  # REMOVE THE PRIOR THAT IS BEST AND THEREFORE IN Pred_NMS
#         }
#         Cnt_Prior_Prob <- dim(PredProb)[1]
#       } # LOOP WHILE THERE ARE PP 
#       
#     }else{
#       # IF THERA ARE NO PRIORS ABOVE THE PROBABILITY THRESHOLD, THERE ARE ALSO NO NMS PRIORS
#       Pred_NMS <- PredProb
#       # browser()
#     }
#   }
#   
#   
#   
#   return(list(Pred_NMS))
# }
# 
# 
# 
# ########################################################################################################################################################################################################
# #######################################################################################################################################################################
# #######################################################################################################################################################################
# 
# # losses <- c()
# # log_lrs <- c()
# # 
# # FIND_LEARN_RATE_FUN <- function(Trees_dl, init_value = 1e-8, final_value = 10, beta = 0.98) {
# # 
# #   num <- Trees_dl$.length()
# #   mult = (final_value/init_value)^(1/num)
# #   lr <- init_value
# #   optimizer$param_groups[[1]]$lr <- lr
# #   avg_loss <- 0
# #   best_loss <- 0
# #   batch_num <- 0
# # 
# #   for (b in enumerate(Trees_dl)) {
# # 
# #     batch_num <- batch_num + 1
# #     optimizer$zero_grad()
# #     output <- model(b[[1]]$to(device = device))
# #     loss <- criterion(output, b[[2]]$to(device = device))
# # 
# #     #Compute the smoothed loss
# #     avg_loss <- beta * avg_loss + (1-beta) * loss$item()
# #     smoothed_loss <- avg_loss / (1 - beta^batch_num)
# #     #Stop if the loss is exploding
# #     if (batch_num > 1 && smoothed_loss > 4 * best_loss) break
# #     #Record the best loss
# #     if (smoothed_loss < best_loss || batch_num == 1) best_loss <- smoothed_loss
# # 
# #     #Store the values
# #     losses <<- c(losses, smoothed_loss)
# #     log_lrs <<- c(log_lrs, (log(lr, 10)))
# # 
# #     loss$backward()
# #     optimizer$step()
# # 
# #     #Update the lr for the next step
# #     lr <- lr * mult
# #     optimizer$param_groups[[1]]$lr <- lr
# #   }
# # }
# 
# 
# ########################################################################################################################################################################################################
# #######################################################################################################################################################################
# #######################################################################################################################################################################
# # DOM DOM DOM !!! IN THE TORCH_INITIALISE_V18 YOU RUN THIS CODE WITH TENSORS AND BELOW ARE THE RESULTS THAT ARE REQUIRED
# # "X_TopBox 4 was 10", "Y_TopBox 5 was 11", "L_TopBox 6 was 12", "W_TopBox 7 was 13", "Z_TopBox 8 was 14", "R_TopBox 9 was 15", 
# # "Z_TopTree 10 was 16"
# 
# # SCALE_PLOT2RoI_FUN_OLD <- function(RoI_Dec, XYZWLHR, Plot_N=b, Para_Cnt = 10, use_Tensor = "Yes"){
# #   
# #   # Colnames_XYZWLHR <- c("X_Base", "Y_Base", "Z_Base",  
# #   #                       "X_TopBox", "Y_TopBox", "L_TopBox", "W_TopBox", "Z_TopBox", "R_TopBox", 
# #   #                       "Z_TopTree")
# #   
# #   if(Para_Cnt == 16){
# #     Index_X <- torch_tensor(c(1,4,10), dtype = torch_long()) 
# #     Index_Y <- torch_tensor(c(2, 5, 11), dtype = torch_long()) 
# #     Index_Z <- torch_tensor(c(3, 8, 14, 16), dtype = torch_long()) 
# #     Index_L <- torch_tensor(c(6, 12), dtype = torch_long()) 
# #     Index_W <- torch_tensor(c(7, 13), dtype = torch_long()) 
# #     Index_R <- torch_tensor(c(9, 15), dtype = torch_long()) 
# #     Index_all_Order <- torch_tensor(order(c(1, 4, 10, 2, 5, 11, 3, 8, 14, 16,6, 12,7, 13, 9, 15)), dtype = torch_long()) 
# #   }else{
# #     if(use_Tensor == "Yes"){
# #       Index_X <- torch_tensor(c(1,4), dtype = torch_long()) 
# #       Index_Y <- torch_tensor(c(2,  5), dtype = torch_long()) 
# #       Index_Z <- torch_tensor(c(3,  8, 10), dtype = torch_long()) 
# #       Index_L <- torch_tensor(c( 6), dtype = torch_long()) 
# #       Index_W <- torch_tensor(c( 7), dtype = torch_long()) 
# #       Index_R <- torch_tensor(c( 8), dtype = torch_long()) 
# #       Index_all_Order <- torch_tensor(order(c(1,  4, 2,  5, 3,  8, 10, 6, 7,  8)), dtype = torch_long()) 
# #     }else{ 
# #       Index_X <- torch_tensor(c(1,10), dtype = torch_long()) 
# #       Index_Y <- torch_tensor(c(2,  11), dtype = torch_long()) 
# #       Index_Z <- torch_tensor(c(3,  14, 16), dtype = torch_long()) 
# #       Index_L <- torch_tensor(c( 12), dtype = torch_long()) 
# #       Index_W <- torch_tensor(c( 13), dtype = torch_long()) 
# #       Index_R <- torch_tensor(c( 15), dtype = torch_long()) 
# #       Index_all_Order <- torch_tensor(order(c(1,  10, 2,  11, 3,  14, 16, 12, 13,  15)), dtype = torch_long()) 
# #       }
# # 
# #   }
# # 
# #   XYZWLHR <- torch_squeeze(XYZWLHR, dim=1L)
# #   List_RoI_XYZWLHR = list()
# #   
# #   ### DOM DOM DOM !!! I WONDER IF YOU CAN NORMALISE FOR RoI WITHOUT LOOPING THROUGH RoIs (r)
# #   for(r in 1:dim(RoI_Dec)[2]){
# # 
# #     oneRoI_Dec = RoI_Dec[Plot_N,r,]
# #     xxB <- as.array(oneRoI_Dec[2])
# #     yyB <- as.array(oneRoI_Dec[4])
# #     zzB <- as.array(oneRoI_Dec[6])
# #     xxT <- as.array(oneRoI_Dec[3])
# #     yyT <- as.array(oneRoI_Dec[5])
# #     zzT <- as.array(oneRoI_Dec[7])
# # 
# # 
# #     x_oneP <- as.array(XYZWLHR[r,Index_X]) # USING THE PLOT EXTENT HERE     # X_Base	X_BotBox	 X_TopBox 		  c(1, 4,  10)   
# #     x_New <- (x_oneP-xxB)/(xxT-xxB)
# # 
# #     y_oneP <- as.array(XYZWLHR[r,Index_Y])                                   # Y_Base Y_BotBox	 Y_TopBox 	 (Length_Y)	 c(2, 5, 6, 11, 12) 
# #     y_New <-  (y_oneP-yyB)/(yyT-yyB)
# # 
# #     z_oneP <- as.array(XYZWLHR[r,Index_Z])                                   # Z_Base Z_BotBox Z_TopBox Z_TopTree  c(3, 8, 14, 16)
# #     z_New <-  (z_oneP-zzB)/(zzT-zzB)
# # 
# #     L_oneP <- as.array(XYZWLHR[r,Index_L])                                     # L_BotBox L_TopBox c(6, 12)
# #     L_New <-L_oneP/(yyT-yyB) # CHANGED FROM * to * 28/4/21
# # 
# #     W_oneP <- as.array(XYZWLHR[r,Index_W])                                   # W_BotBox W_TopBox (Width_X) c(7, 13)
# #     W_New <-W_oneP/(xxT-xxB)  # CHANGED FROM * to * 28/4/21
# #     
# #     r_new <- as.array(XYZWLHR[r,Index_R]) # R_BotBox R_TopBox	c(9, 15)
# #     
# #     # REORDER THE SCALLED 16 PARAMETERS AND PUT IN TENSOR FORMAT
# #     oneR_XYZWLH <- torch_tensor(c(x_New, y_New, z_New,L_New,W_New, r_new))[Index_all_Order] 
# #     # browser()
# #     List_RoI_XYZWLHR <- list.append(List_RoI_XYZWLHR, oneR_XYZWLH)
# #   }
# # 
# #   oneB_allR_XYZWLH_ExtR <- torch_stack(List_RoI_XYZWLHR)
# #   return(oneB_allR_XYZWLH_ExtR)
# # }
# 
# 
# 
# # SCALE_PLOT2RoI_VERT_FUN <- function(RoI_Dec, XYZWLHR, OUT_VERT_or_XYZWLHR = "XYZWLHR", Plot_N=b, Para_Cnt = 10, use_Tensor = "Yes", Para_Base_WL = 0.5, Para_Target_Base=16,
# #                                     Col_Name = Colnames_XYZWLHR, Normalise= "Yes"){
# #   List_RoI_XYZWLHR = list()
# #   for(r in 1:RoI_Dec$size(2)){ # dim(RoI_Dec)[2]
# #     XYZWLHR_Vert <- as.data.frame(XYZWHR_TO_VERT_GPU_FUN(XYZWLHR[r,], Base_WL = Para_Base_WL/Para_Target_Base))
# #     
# #     oneRoI_Dec = RoI_Dec[Plot_N,r,]
# #     xxB <- as.array(oneRoI_Dec[2])
# #     yyB <- as.array(oneRoI_Dec[4])
# #     zzB <- as.array(oneRoI_Dec[6])
# #     xxT <- as.array(oneRoI_Dec[3])
# #     yyT <- as.array(oneRoI_Dec[5])
# #     zzT <- as.array(oneRoI_Dec[7])
# #      
# #     x_oneP <- XYZWLHR_Vert$X # USING THE PLOT EXTENT HERE     # X_Base	X_BotBox	 X_TopBox 		  c(1, 4,  10)   
# #     x_New <- (x_oneP-xxB)/(xxT-xxB)
# #     y_oneP <- XYZWLHR_Vert$Y                                  # Y_Base Y_BotBox	 Y_TopBox 	 (Length_Y)	 c(2, 5, 6, 11, 12) 
# #     y_New <-  (y_oneP-yyB)/(yyT-yyB)
# #     z_oneP <- XYZWLHR_Vert$Z                                   # Z_Base Z_BotBox Z_TopBox Z_TopTree  c(3, 8, 14, 16)
# #     z_New <-  (z_oneP-zzB)/(zzT-zzB)
# #     XYZWLHR_Vert_New <- as.matrix(data.frame(X =x_New, Y = y_New, Z = z_New)) 
# # 
# #     oneXYZWLHR_Vert_New1 <- data.frame(BBOX_PNTS_FUN(XYZWLHR_Vert_New[1:4,1:2]))
# #     oneXYZWLHR_Vert_New1 <- data.frame(oneXYZWLHR_Vert_New1[-5,], Z = rep(XYZWLHR_Vert_New[1,3],4))
# #     oneXYZWLHR_Vert_New2 <- data.frame(BBOX_PNTS_FUN(XYZWLHR_Vert_New[5:8,1:2]))
# #     oneXYZWLHR_Vert_New2 <- data.frame(oneXYZWLHR_Vert_New2[-5,], Z = rep(XYZWLHR_Vert_New[5,3],4))
# #     oneXYZWLHR_Vert_New3 <- data.frame(BBOX_PNTS_FUN(XYZWLHR_Vert_New[9:12,1:2]))
# #     oneXYZWLHR_Vert_New3 <- data.frame(oneXYZWLHR_Vert_New3[-5,], Z = rep(XYZWLHR_Vert_New[9,3],4))
# #     XYZWLHR_Vert_NewBox <- rbind(oneXYZWLHR_Vert_New1, oneXYZWLHR_Vert_New2, oneXYZWLHR_Vert_New3 )
# #    
# #     if(OUT_VERT_or_XYZWLHR == "Vert"){
# #       List_RoI_XYZWLHR <- list.append(List_RoI_XYZWLHR, XYZWLHR_Vert_NewBox)
# #     }else{
# #       # browser()
# #       XYZWLHR_NewBox <- VERT_To_XYZWLHR_FUN(XYZWLHR_Vert_NewBox, Para_Cnt = Para_TriShpParaCnt, Col_Name, Normalise= "Yes")
# #       if(r== 1){
# #         List_RoI_XYZWLHR <- XYZWLHR_NewBox
# #       }else{
# #         List_RoI_XYZWLHR <- rbind(List_RoI_XYZWLHR, XYZWLHR_NewBox)
# #         }
# #     }
# #   } # R LOOP
# #   # if(OUT_VERT_or_XYZWLHR == "Vert"){ browser()}
# #   #browser()
# #   if(class(List_RoI_XYZWLHR) == "data.frame"){List_RoI_XYZWLHR <- list(List_RoI_XYZWLHR)}
# #   RoI_XYZWLHR_T <- torch_tensor(as.matrix(List_RoI_XYZWLHR[[1]]), dtype = torch_float())
# #   
# #   return(RoI_XYZWLHR_T)
# # }
# ##################################################################################################################################################################
# 
# # SCALE_RoI2PLOT_FUN <- function(RoI_Dec, XYZWLHR, Plot_N=b, Para_Cnt = 10,  Normalised = "Yes",
# #                                IN_VERT_or_XYZWLHR = "XYZWLHR", OUT_VERT_or_XYZWLHR = "XYZWLHR"){
# #   # use_Tensor = "Yes",
# #   # # Colnames_XYZWLHR <- c("X_Base", "Y_Base", "Z_Base", "X_TopBox", "Y_TopBox", "L_TopBox", "W_TopBox", "Z_TopBox", "R_TopBox", "Z_TopTree")
# #   # if(Para_Cnt == 16){
# #   #   Index_X <- torch_tensor(c(1,4,10), dtype = torch_long()) 
# #   #   Index_Y <- torch_tensor(c(2, 5, 11), dtype = torch_long()) 
# #   #   Index_Z <- torch_tensor(c(3, 8, 14, 16), dtype = torch_long()) 
# #   #   Index_L <- torch_tensor(c(6, 12), dtype = torch_long()) 
# #   #   Index_W <- torch_tensor(c(7, 13), dtype = torch_long()) 
# #   #   Index_R <- torch_tensor(c(9, 15), dtype = torch_long()) 
# #   #   Index_all_Order <- torch_tensor(order(c(1, 4, 10, 2, 5, 11, 3, 8, 14, 16,6, 12,7, 13, 9, 15)), dtype = torch_long()) 
# #   # }else{
# #   #   if(use_Tensor == "Yes"){
# #   #     Index_X <- torch_tensor(c(1, 4), dtype = torch_long())      # "X_Base", "X_TopBox"
# #   #     Index_Y <- torch_tensor(c(2, 5), dtype = torch_long())      # "Y_Base", "Y_TopBox"
# #   #     Index_Z <- torch_tensor(c(3,  8, 10), dtype = torch_long()) # "Z_Base", "Z_TopBox", "Z_TopTree" 
# #   #     Index_L <- torch_tensor(c(6), dtype = torch_long())         # "L_TopBox"
# #   #     Index_W <- torch_tensor(c(7), dtype = torch_long())         # "W_TopBox"
# #   #     Index_R <- torch_tensor(c( 9), dtype = torch_long())        #   
# #   #     Index_all_Order <- torch_tensor(order(c(1, 4, 2, 5, 3,  8,  10, 6,  7,  9)), dtype = torch_long())
# #   #   }else{
# #   #     Index_X <- torch_tensor(c(1,10), dtype = torch_long())  
# #   #     Index_Y <- torch_tensor(c(2,  11), dtype = torch_long()) 
# #   #     Index_Z <- torch_tensor(c(3,  14, 16), dtype = torch_long()) 
# #   #     Index_L <- torch_tensor(c( 12), dtype = torch_long()) 
# #   #     Index_W <- torch_tensor(c( 13), dtype = torch_long()) 
# #   #     Index_R <- torch_tensor(c( 15), dtype = torch_long()) 
# #   #     Index_all_Order <- torch_tensor(order(c(1,  10, 2,  11, 3,  14, 16, 12, 13,  15)), dtype = torch_long()) 
# #   #     
# #   #     }
# #   #   }
# #   # browser()
# # 
# #   # #browser()
# #   # XYZWLHR <- torch_squeeze(XYZWLHR, dim=1L)
# #   XYZWLHR_DF <- as.data.frame(as.array(XYZWLHR))
# #   colnames(XYZWLHR_DF) <-Colnames_XYZWLHR
# #   List_RoI_XYZWLHR = list()
# #   
# #   ### DOM DOM DOM !!! I WONDER IF YOU CAN NORMALISE FOR RoI WITHOUT LOOPING THROUGH RoIs (r)
# #   # browser()
# #   for(r in 1:RoI_Dec$size(2) ){ # dim(RoI_Dec)[2]
# #     
# #     oneR_XYZWLHR_DF <- XYZWLHR_DF[r,]
# #     # browser()
# #     if(IN_VERT_or_XYZWLHR == "XYZWLHR"){
# #       # oneR_XYZWLHR <- as.data.frame(as.array(oneR_XYZWLHR))
# #       # colnames(oneR_XYZWLHR) <-Colnames_XYZWLHR
# #       XYZWLHR_Vert <- as.data.frame(XYZWHR_TO_VERT_GPU_FUN(oneR_XYZWLHR_DF, Base_WL = Para_Base_WL/Para_Target_Base, Normalised = Normalised, Para_Cnt = Para_Cnt))
# #     }else{
# #       XYZWLHR_Vert <- oneR_XYZWLHR_DF  
# #     }
# #     #browser()
# #     
# #     oneRoI_Dec = RoI_Dec[Plot_N,r,]
# #     xxB <- as.array(oneRoI_Dec[2])
# #     yyB <- as.array(oneRoI_Dec[4])
# #     zzB <- as.array(oneRoI_Dec[6])
# #     xxT <- as.array(oneRoI_Dec[3])
# #     yyT <- as.array(oneRoI_Dec[5])
# #     zzT <- as.array(oneRoI_Dec[7])
# #     
# #     x_oneP <- XYZWLHR_Vert$X # USING THE PLOT EXTENT HERE     # X_Base	X_BotBox	 X_TopBox 		  c(1, 4,  10)   
# #     x_New <- x_oneP*(xxT-xxB)+xxB
# #     y_oneP <- XYZWLHR_Vert$Y                                  # Y_Base Y_BotBox	 Y_TopBox 	 (Length_Y)	 c(2, 5, 6, 11, 12) 
# #     y_New <-   y_oneP*(yyT-yyB)+yyB
# #     z_oneP <- XYZWLHR_Vert$Z                                   # Z_Base Z_BotBox Z_TopBox Z_TopTree  c(3, 8, 14, 16)
# #     z_New <-  z_oneP*(zzT-zzB)+zzB
# # 
# #     # XYZWLHR_Vert_New <- as.matrix(data.frame(X =x_New, Y = y_New, Z = z_New)) 
# # 
# #     XYZWLHR_Vert_New <- torch_stack(c(x_New, y_New, z_New), dim=2)$to(device = "cpu")
# #     XYZWLHR_Vert_New <- as.array(XYZWLHR_Vert_New) 
# #     
# #     oneXYZWLHR_Vert_New1 <- data.frame(BBOX_PNTS_FUN(XYZWLHR_Vert_New[1:4,1:2]))
# #     oneXYZWLHR_Vert_New1 <- data.frame(oneXYZWLHR_Vert_New1[-5,], Z = rep(XYZWLHR_Vert_New[1,3],4))
# #     oneXYZWLHR_Vert_New2 <- data.frame(BBOX_PNTS_FUN(XYZWLHR_Vert_New[5:8,1:2]))
# #     oneXYZWLHR_Vert_New2 <- data.frame(oneXYZWLHR_Vert_New2[-5,], Z = rep(XYZWLHR_Vert_New[5,3],4))
# #     oneXYZWLHR_Vert_New3 <- data.frame(BBOX_PNTS_FUN(XYZWLHR_Vert_New[9:12,1:2]))
# #     oneXYZWLHR_Vert_New3 <- data.frame(oneXYZWLHR_Vert_New3[-5,], Z = rep(XYZWLHR_Vert_New[9,3],4))
# #     XYZWLHR_Vert_NewBox <- rbind(oneXYZWLHR_Vert_New1, oneXYZWLHR_Vert_New2, oneXYZWLHR_Vert_New3 )
# #     
# #     if(OUT_VERT_or_XYZWLHR == "Vert"){
# #       List_RoI_XYZWLHR <- list.append(List_RoI_XYZWLHR, XYZWLHR_Vert_NewBox)
# #     }else{
# #  
# #       XYZWLHR_NewBox <- VERT_To_XYZWLHR_FUN(XYZWLHR_Vert_NewBox, Para_Cnt = Para_TriShpParaCnt, Col_Name, Normalise= "Yes")
# #       if(r== 1){
# #         List_RoI_XYZWLHR <- XYZWLHR_NewBox
# #       }else{
# #         List_RoI_XYZWLHR <- rbind(List_RoI_XYZWLHR, XYZWLHR_NewBox)
# #       }
# #     }
# #     
# #     # x_oneP <- XYZWLHR_Vert$X
# #     # #x_oneP <- as.array(XYZWLHR[r,Index_X]) # USING THE PLOT EXTENT HERE     # X_Base	X_BotBox	 X_TopBox 		  c(1, 4,  10)   
# #     # x_New <- x_oneP*(xxT-xxB)+xxB
# #     # 
# #     # #y_oneP <- as.array(XYZWLHR[r,Index_Y])                                   # Y_Base Y_BotBox	 Y_TopBox 	 (Length_Y)	 c(2, 5, 6, 11, 12) 
# #     # y_New <- y_oneP*(yyT-yyB)+yyB
# #     # 
# #     # #z_oneP <- as.array(XYZWLHR[r,Index_Z])                                   # Z_Base Z_BotBox Z_TopBox Z_TopTree  c(3, 8, 14, 16)
# #     # z_New <- z_oneP*(zzT-zzB)+zzB
# # 
# #     # L_oneP <- as.array(XYZWLHR[r,Index_L])                                     # L_BotBox L_TopBox c(6, 12)
# #     # L_New <- L_oneP*(yyT-yyB) # CHANGED FROM / to * 28/4/21
# #     # 
# #     # W_oneP <- as.array(XYZWLHR[r,Index_W])                                   # W_BotBox W_TopBox (Width_X) c(7, 13)
# #     # W_New <- W_oneP*(xxT-xxB) # CHANGED FROM / to * 28/4/21
# #     # 
# #     # r_new <- as.array(XYZWLHR[r,Index_R]) # R_BotBox R_TopBox	c(9, 15)
# #     # 
# #     # # REORDER THE SCALLED 16 PARAMETERS AND PUT IN TENSOR FORMAT
# #     # oneR_XYZWLH <- torch_tensor(c(x_New, y_New, z_New, L_New, W_New, r_new))[Index_all_Order] 
# #     #
# #     #List_RoI_XYZWLHR <- list.append(List_RoI_XYZWLHR, oneR_XYZWLH)
# #   }
# #   
# #   if(class(List_RoI_XYZWLHR) == "data.frame"){List_RoI_XYZWLHR <- list(List_RoI_XYZWLHR)}
# #   RoI_XYZWLHR_T <- torch_tensor(as.matrix(List_RoI_XYZWLHR[[1]]), dtype = torch_float())
# #   return(RoI_XYZWLHR_T)
# # }
# 
# SCALE_RoI2Plot_Coord_FUN <- function(RoI_Dec, XYZWLHR, Plot_N=b, Shift=oneP_Shift_Parameters, ScaleRoI2Plot_First = "Yes", Para_Cnt = 10){
#   # browser()
#   if(ScaleRoI2Plot_First == "Yes"){
#     oneB_allR_XYZWLH_ExtP <- SCALE_RoI2PLOT_FUN(RoI_Dec, XYZWLHR, Plot_N=1, Para_Cnt = Para_Cnt, IN_VERT_or_XYZWLHR = "XYZWLHR", OUT_VERT_or_XYZWLHR = "XYZWLHR") # CHANGED FROM SCALE_PLOT2RoI_VERT_FUN TO SCALE_RoI2PLOT_FUN 18/03/21
#   }else{
#     oneB_allR_XYZWLH_ExtP <- XYZWLHR
#   }
#   ########################################
#   # DE-NORMALISING THE PREDICTED PARAMETERS
#   #########################################  
#   oneB_allR_XYZWLH_ExtP_Coord <- oneB_allR_XYZWLH_ExtP
# 
#   for(r in 1:oneB_allR_XYZWLH_ExtP_Coord$size(1)){ #dim(oneB_allR_XYZWLH_ExtP_Coord)[1]
# 
#     # X_Base (1)          Y_Base (2)            Z_Base (3)       X_BotBox (4)       Y_BotBox (5)         L_BotBox (6)        W_BotBox (7)       
#     # Z_BotBox (8)       R_BotBox (9)      X_TopBox (10)       Y_TopBox (11)         L_TopBox (12)        W_TopBox (13)        Z_TopBox (14)       
#     # R_TopBox (15)        Z_TopTree (16)
#     
#     #browser()  ### DOM DOM DOM ... HOW SHOULD L and W BE CONVERTED BACK ...
#     
#     # if(Para_Cnt == 16){
#     #   Index_X <- torch_tensor(c(1,4, 7, 10, 13), dtype = torch_long()) 
#     #   Index_Y <- torch_tensor(c(2, 5, 6, 11, 12), dtype = torch_long()) 
#     #   Index_Z <- torch_tensor(c(3, 8, 14, 16), dtype = torch_long()) 
#     #   Index_R <- torch_tensor(c(9, 15), dtype = torch_long()) 
#     # }else{
#       Index_X <- torch_tensor(c(1, 10, 13), dtype = torch_long(), device=device) 
#       Index_Y <- torch_tensor(c(2,  11, 12), dtype = torch_long(), device=device) 
#       Index_Z <- torch_tensor(c(3,  14, 16), dtype = torch_long(), device=device) 
#       Index_R <- torch_tensor(c(15), dtype = torch_long(), device=device) 
#     #}
#     
#       oneB_allR_XYZWLH_ExtP_Coord[r,Index_X] <-  oneB_allR_XYZWLH_ExtP_Coord[r,Index_X]*Shift$Shift_Vox_N_X 
#     oneB_allR_XYZWLH_ExtP_Coord[r,Index_Y] <-  oneB_allR_XYZWLH_ExtP_Coord[r,Index_Y]*Shift$Shift_Vox_N_Y 
#     oneB_allR_XYZWLH_ExtP_Coord[r,Index_Z] <-  oneB_allR_XYZWLH_ExtP_Coord[r,Index_Z]*Shift$Shift_Vox_N_Z 
#     oneB_allR_XYZWLH_ExtP_Coord[r,Index_R] <-  oneB_allR_XYZWLH_ExtP_Coord[r,Index_R]*180
#     
#     ###################
#     # ASSIGN COORDINATE 
#     ###################
#     # if(Para_Cnt == 16){
#     #   oneB_allR_XYZWLH_ExtP_Coord[r,Index_X[c(1,2,4)]] <-  oneB_allR_XYZWLH_ExtP_Coord[r,Index_X[c(1,2,4)]] + Shift$Shift_Vox_X 
#     #   oneB_allR_XYZWLH_ExtP_Coord[r,Index_Y[c(1,2,4)]] <-  oneB_allR_XYZWLH_ExtP_Coord[r,Index_Y[c(1,2,4)]] + Shift$Shift_Vox_Y 
#     #   oneB_allR_XYZWLH_ExtP_Coord[r,Index_Z[c(1,2,3,4)]] <-  oneB_allR_XYZWLH_ExtP_Coord[r,Index_Z[c(1,2,3,4)]] + Shift$Shift_Vox_Z 
#     # }else{
#       oneB_allR_XYZWLH_ExtP_Coord[r,Index_X[c(1,2)]] <-  oneB_allR_XYZWLH_ExtP_Coord[r,Index_X[c(1,2)]] + Shift$Shift_Vox_X 
#       oneB_allR_XYZWLH_ExtP_Coord[r,Index_Y[c(1,2)]] <-  oneB_allR_XYZWLH_ExtP_Coord[r,Index_Y[c(1,2)]] + Shift$Shift_Vox_Y 
#       oneB_allR_XYZWLH_ExtP_Coord[r,Index_Z[c(1,2,3)]] <-  oneB_allR_XYZWLH_ExtP_Coord[r,Index_Z[c(1,2,3)]] + Shift$Shift_Vox_Z 
#     #}
#   }
#   # PERFORM COORDINATE ADJUSTMENT
#   return(oneB_allR_XYZWLH_ExtP_Coord)
# }
# 
# XYZWLHR_NORM_to_COORD_FUN <- function(XYZWLHR_N = oneP_TARGET_XYZWLHR_ExtP_Orig_DF, Shift = oneP_Shift_Parameters, Para_Cnt = 10){
#   
#   # if(Para_Cnt == 16){
#   #   Index_X <- c(1,4, 10) #torch_tensor(c(1,3), dtype = torch_long()) 
#   #   Index_Y <- c(2,5, 11)    #torch_tensor(c(4,5), dtype = torch_long()) 
#   #   Index_Z <- c(3, 8, 14, 16)   #torch_tensor(c(6,7), dtype = torch_long()) 
#   #   Index_L <- c(6, 12)
#   #   Index_W <- c(7, 13)
#   #   Index_R <- c(9, 15)
#   # }else{
#     Index_X <- c(1, 10) #torch_tensor(c(1,3), dtype = torch_long()) 
#     Index_Y <- c(2, 11)    #torch_tensor(c(4,5), dtype = torch_long()) 
#     Index_Z <- c(3,  14, 16)   #torch_tensor(c(6,7), dtype = torch_long()) 
#     Index_L <- c( 12)
#     Index_W <- c( 13)
#     Index_R <- c( 15)
#   #}
#   
# 
#   
#   
#   XYZWLHR_Coord <- XYZWLHR_N
#   XYZWLHR_Coord[,Index_X] <-  XYZWLHR_Coord[,Index_X]* Shift$Shift_Vox_N_X 
#   XYZWLHR_Coord[,Index_Y] <-  XYZWLHR_Coord[,Index_Y]* Shift$Shift_Vox_N_Y 
#   XYZWLHR_Coord[,Index_Z] <-  XYZWLHR_Coord[,Index_Z]* Shift$Shift_Vox_N_Z 
# 
#   XYZWLHR_Coord[,Index_L] <-  XYZWLHR_Coord[,Index_L]*Shift$Shift_Vox_N_Z
#   XYZWLHR_Coord[,Index_W] <-  XYZWLHR_Coord[,Index_W]*Shift$Shift_Vox_N_Z
#   
#   XYZWLHR_Coord[,Index_R] <- XYZWLHR_Coord[,Index_R]*Shift$Shift_Rad 
#   
#   ###################
#   # ASSIGN COORDINATE 
#   ###################
#   XYZWLHR_Coord[,Index_X] <-  XYZWLHR_Coord[,Index_X] + Shift$Shift_Vox_X 
#   XYZWLHR_Coord[,Index_Y] <-  XYZWLHR_Coord[,Index_Y] + Shift$Shift_Vox_Y 
#   XYZWLHR_Coord[,Index_Z] <-  XYZWLHR_Coord[,Index_Z] + Shift$Shift_Vox_Z 
#   
#   # REVERSE SHRINK
#   XYZWLHR_Coord[,Index_X] <-  BBmisc::normalize(XYZWLHR_Coord[,Index_X], method = "range", range = c(Shift$X_origRng_Bot, Shift$X_origRng_Top)) #
#   XYZWLHR_Coord[,Index_Y] <-  BBmisc::normalize(XYZWLHR_Coord[,Index_Y], method = "range", range = c(Shift$Y_origRng_Bot, Shift$Y_origRng_Top)) #
#   XYZWLHR_Coord[,Index_Z] <-  BBmisc::normalize(XYZWLHR_Coord[,Index_Z], method = "range", range = c(Shift$Z_origRng_Bot, Shift$Z_origRng_Top)) #
#   
#   return(XYZWLHR_Coord)
# }
# 
# RoI_NORM_to_COORD_FUN <- function(oneP_RoI_Dec, Shift){
#   ########################################
#   # DE-NORMALISING THE PREDICTED PARAMETERS
#   #########################################  
#   oneP_RoI_Dec_Coord <- oneP_RoI_Dec
#   
#   for(r in 1:oneP_RoI_Dec_Coord$size(1)){ # dim(oneP_RoI_Dec_Coord)[1]
#     
#     Index_X <- torch_tensor(c(2,3), dtype = torch_long()) 
#     Index_Y <- torch_tensor(c(4,5), dtype = torch_long()) 
#     Index_Z <- torch_tensor(c(6,7), dtype = torch_long()) 
#     
#     oneP_RoI_Dec_Coord[r,Index_X] <-  oneP_RoI_Dec_Coord[r,Index_X]*Shift$Shift_Vox_N_X 
#     oneP_RoI_Dec_Coord[r,Index_Y] <-  oneP_RoI_Dec_Coord[r,Index_Y]*Shift$Shift_Vox_N_Y 
#     oneP_RoI_Dec_Coord[r,Index_Z] <-  oneP_RoI_Dec_Coord[r,Index_Z]*Shift$Shift_Vox_N_Z 
#     
#     ###################
#     # ASSIGN COORDINATE 
#     ###################
#     oneP_RoI_Dec_Coord[r,Index_X] <-  oneP_RoI_Dec_Coord[r,Index_X] + Shift$Shift_Vox_X 
#     oneP_RoI_Dec_Coord[r,Index_Y] <-  oneP_RoI_Dec_Coord[r,Index_Y] + Shift$Shift_Vox_Y 
#     oneP_RoI_Dec_Coord[r,Index_Z] <-  oneP_RoI_Dec_Coord[r,Index_Z] + Shift$Shift_Vox_Z 
#   }
#   return(oneP_RoI_Dec_Coord)
# }
# 
# PLOT_LAS_FUN <- function(LAS_PLOT, Title_Plot){
#   n <- length(unique(LAS_PLOT@data$TID))
#   palette <- sample(distinctColorPalette(n))
#   # GENERATE ATTRIBUTE FOR COLOURING EACH POINT
#   LAS_PLOT@data$Color <- as.character("")
#   Color_ID <- data.frame(Unique_TID = unique(LAS_PLOT@data$TID),
#                          ID_TID = palette)
#   Index_Color_TID_1 <- which(LAS_PLOT@data$TID %in% Color_ID$Unique_TID)
#   Index_Color_TID_2 <- match(LAS_Vox_Trees@data$TID[Index_Color_TID_1],
#                              Color_ID$Unique_TID)
#   LAS_PLOT@data$Color[Index_Color_TID_1] <- as.character(Color_ID$ID_TID[Index_Color_TID_2])
#   plot(LAS_PLOT, color="Color", size=3)
#   text3d(mean(LAS_PLOT$X), mean(LAS_PLOT$X), max(LAS_PLOT@data$Z),
#          col="white",
#          size = 4,
#          Title_Plot)
#   
# }
#   
# PLOT_LAS_XYZWLHR_FUN <- function(LAS_PLOT, XYZWLHR, Para_Base_WL, Normalised = "Yes", Para_Cnt = 10){
#   n <- length(unique(LAS_PLOT@data$TID))
#   palette <- sample(distinctColorPalette(n))
#   # GENERATE ATTRIBUTE FOR COLOURING EACH POINT
#   LAS_PLOT@data$Color <- as.character("")
#   Color_ID <- data.frame(Unique_TID = unique(LAS_PLOT@data$TID),
#                          ID_TID = palette)
#   index_Color_TID_1 <- which(LAS_PLOT@data$TID %in% Color_ID$Unique_TID)
#   index_Color_TID_2 <- match(LAS_PLOT@data$TID[index_Color_TID_1],
#                              Color_ID$Unique_TID)
#   LAS_PLOT@data$Color[index_Color_TID_1] <- as.character(Color_ID$ID_TID[index_Color_TID_2])
#   plot(LAS_PLOT, color="Color", size=2)
#   print(table(LAS_PLOT@data$TID))
#   Unique_T <- XYZWLHR[,1]
#   for(RT in 1:length(Unique_T)){ 
#     Index_T <- which(XYZWLHR[,1] ==Unique_T[RT])
#     which(colnames(XYZWLHR) == "X_Base")
#     #browser()
#     Vert_oneT <- XYZWHR_TO_VERT_GPU_FUN(XYZWLHR[Index_T,which(colnames(XYZWLHR) == "X_Base"):ncol(XYZWLHR)], Base_WL = Para_Base_WL, Normalised = Normalised, Para_Cnt = Para_Cnt, device=device)
#     Vert_oneT <- na.omit(Vert_oneT)
#     
#     Vert_oneT_DF <- as.data.frame(Vert_oneT)
#     Shift_X <- min(LAS_PLOT$X)
#     Shift_Y <- min(LAS_PLOT$Y)
#     
#     if(Normalised == "No"){
#       Vert_oneT_DF$X <- Vert_oneT_DF$X - Shift_X
#       Vert_oneT_DF$Y <- Vert_oneT_DF$Y - Shift_Y
#     }
#     
#     points3d(as.vector(Vert_oneT_DF$X[1:4]),
#              as.vector(Vert_oneT_DF$Y[1:4]),
#              as.vector(Vert_oneT_DF$Z[1:4]),
#              col="Yellow", size=10)
#     polygon3d(as.vector(Vert_oneT_DF$X[5:8]),
#               as.vector(Vert_oneT_DF$Y[5:8]),
#               as.vector(Vert_oneT_DF$Z[5:8]),
#               fill = FALSE, col="orange", lwd=2)
#     polygon3d(as.vector(Vert_oneT_DF$X[9:12]),
#               as.vector(Vert_oneT_DF$Y[9:12]),
#               as.vector(Vert_oneT_DF$Z[9:12]),
#               fill = FALSE, col="red", lwd=2)
#     # if(Para_Cnt == 16){
#     #   polygon3d(as.vector(Vert_oneT_DF$X[13:16]),
#     #             as.vector(Vert_oneT_DF$Y[13:16]),
#     #             as.vector(Vert_oneT_DF$Z[13:16]),
#     #             fill = FALSE, col="blue", lwd=2)
#     #   }
# 
#   }
# }
# 
# 
# 
# 
# #######################################################################################################
# #######################################################################################################
# # https://gis.stackexchange.com/questions/387535/shrinking-coordinates-of-las-file-to-fit-in-0-1-using-lidr
# 
# SCALE_LAS_FUN <- function(las, Scale_Factor, Offset_Factor_X, Offset_Factor_Y, Offset_Factor_Z) {
# 
#   las@header@PHB[["X scale factor"]] <- Scale_Factor
#   las@header@PHB[["Y scale factor"]] <- Scale_Factor
#   las@header@PHB[["Z scale factor"]] <- Scale_Factor
#   las@header@PHB[["X offset"]] <- Offset_Factor_X
#   las@header@PHB[["Y offset"]] <- Offset_Factor_Y
#   las@header@PHB[["Z offset"]] <- Offset_Factor_Z
#   
#   xmin <- las@header@PHB[["Min X"]]
#   ymin <- las@header@PHB[["Min Y"]]
#   zmin <- las@header@PHB[["Min Z"]]
#   xmax <- las@header@PHB[["Max X"]]
#   ymax <- las@header@PHB[["Max Y"]]
#   zmax <- las@header@PHB[["Max Z"]]
#   
#   projection(las) <- CRS()
#   las@header@VLR <- list()
#   
#   return(las)
#   }
# #######################################################################################################
# #######################################################################################################
# EMPTY_VOX_FUN <- function(Vox_TID_Count, Para_Vox_Res, Para_MaxZ_Shrink) {
# 
#   X_Seq <- seq(min(Vox_TID_Count$X), max(Vox_TID_Count$X), Para_Vox_Res)
#   Y_Seq <- seq(min(Vox_TID_Count$Y), max(Vox_TID_Count$Y), Para_Vox_Res)
#   XY_Seq <- as.data.frame(tidyr::crossing(X_Seq, Y_Seq))
#   XYZ_Seq <- XY_Seq[rep(seq_len(nrow(XY_Seq)), each = ((round(Para_MaxZ_Shrink)*(1/Para_Vox_Res))+1)), ] ## XYZ_Seq <- XY_Seq[rep(seq_len(nrow(XY_Seq)), each = Para_MaxZ_Shrink+1), ]
#   XYZ_Seq$Z_Seq <- as.double(rep(seq(0,round(Para_MaxZ_Shrink), Para_Vox_Res), nrow(XY_Seq))) ## XYZ_Seq$Z_Seq <- as.double(rep(0:Para_MaxZ_Shrink, nrow(XY_Seq)))
#   
#   # SORT BY Z THEN Y AND FINALLY X (SO MOVES ACROSS X FIRST, THEN Y AND THEN Z)
#   XYZ_Seq <- XYZ_Seq[order( XYZ_Seq[,3], XYZ_Seq[,2], XYZ_Seq[,1]),]
#   colnames(XYZ_Seq) <-  c("X", "Y", "Z")
#   
#   # FIND THE EMPY VOXELS THAT NEED TO BE ADDED TO LAS
#   Empty_Voxels <- anti_join(XYZ_Seq,Vox_TID_Count[,1:3])
#   Empty_Voxels$Count <- 0
#   Empty_Voxels$TID <- as.integer(0)
#   return(Empty_Voxels)
# }
# 
# # Para_Target_Base <- 16     # FOR V-NET OF EACH POTENTIAL TREE
# # Para_Target_Z_Height <- 40 
# 
# EMPTY_VOX_FUN2 <- function(Vox_TID_Count, Para_Target_Base, Para_Target_Z_Height, Para_Vox_Res) {
#   X_Seq <- seq(0, 1, length.out =Para_Target_Base)
#   Y_Seq <- seq(0, 1, length.out =Para_Target_Base)
#   XY_Seq <- as.data.frame(crossing(X_Seq, Y_Seq))
#   #browser()
#   XYZ_Seq <- XY_Seq[rep(seq_len(nrow(XY_Seq)), each =round(Para_Target_Z_Height)), ] ## XYZ_Seq <- XY_Seq[rep(seq_len(nrow(XY_Seq)), each = Para_MaxZ_Shrink+1), ]
#   XYZ_Seq$Z_Seq <- as.double(rep(seq(0,1, length.out =round(Para_Target_Z_Height)), nrow(XY_Seq))) ## XYZ_Seq$Z_Seq <- as.double(rep(0:Para_MaxZ_Shrink, nrow(XY_Seq)))
#   
#   # SORT BY Z THEN Y AND FINALLY X (SO MOVES ACROSS X FIRST, THEN Y AND THEN Z)
#   XYZ_Seq <- XYZ_Seq[order( XYZ_Seq[,3], XYZ_Seq[,2], XYZ_Seq[,1]),]
#   colnames(XYZ_Seq) <-  c("X", "Y", "Z")
#   Empty_Voxels <- data.frame(XYZ_Seq, Vox_TID_Count)
#   # FIND THE EMPY VOXELS THAT NEED TO BE ADDED TO LAS
#   # Empty_Voxels <- anti_join(XYZ_Seq,Vox_TID_Count[,1:3])
#   # Empty_Voxels$Count <- 0
#   # Empty_Voxels$TID <- as.integer(0)
#   return(Empty_Voxels)
# }
# 
# 
# 
# TENSOR_LAS_PLOT_FUN <- function(oneB_oneRoI_Vox_Den, Shift_Data, Vox_Den_Orig, LAS_Orig,
#                            Para_Vox_Res,
#                            Para_Target_Base,
#                            Para_MaxZ_Shrink) {
#   
#   # RECREATE THE LAS FILE USING THE SHIFT DATA
#   X_Seq <- seq(Shift_Data$Shift_Vox_X[1], (Shift_Data$Shift_Vox_X[1] + Para_Vox_Res*Para_Target_Base -1), Para_Vox_Res)
#   Y_Seq <- seq(Shift_Data$Shift_Vox_Y[1], (Shift_Data$Shift_Vox_Y[1] + Para_Vox_Res*Para_Target_Base -1), Para_Vox_Res) 
#   XY_Seq <- as.data.frame(crossing(X_Seq, Y_Seq))
#  
#   Empty_Voxels <- XY_Seq[rep(seq_len(nrow(XY_Seq)), each = ((round(Para_MaxZ_Shrink)*(1/Para_Vox_Res))+1)), ] ## Empty_Voxels <- XY_Seq[rep(seq_len(nrow(XY_Seq)), each = Para_MaxZ_Shrink+1), ]
#   Empty_Voxels$Z_Seq <- as.double(rep(seq(0,(round(Para_MaxZ_Shrink)), Para_Vox_Res), nrow(XY_Seq))) ## Empty_Voxels$Z_Seq <- as.double(rep(0:Para_MaxZ_Shrink, nrow(XY_Seq)))
#   
#   # SORT BY Z THEN Y AND FINALLY X (SO MOVES ACROSS X FIRST, THEN Y AND THEN Z)
#   Empty_Voxels <- Empty_Voxels[order( Empty_Voxels[,3], Empty_Voxels[,2], Empty_Voxels[,1]),]
#   colnames(Empty_Voxels) <-  c("X", "Y", "Z")
# 
#  
#   Empty_Voxels$Tensor_Count_Norm <- round(as.vector(as.array(oneB_oneRoI_Vox_Den)), 4) #Shift_Data$Shift_Vox_X[1]
#   Empty_Voxels$Tensor_Count_Adj <- round(as.vector(as.array(oneB_oneRoI_Vox_Den)) * Shift_Data$maxDen_N[1], 0)
# 
#   # SANITY CHECK TO MAKE SURE ALL WORKS
#   Empty_Voxels$Orig_Count_Norm <- Vox_Den_Orig$Count_Norm
#   Empty_Voxels$Orig_Count <- Vox_Den_Orig$Count
#   if(!all(round(Empty_Voxels$Orig_Count_Norm, 4) == round(Empty_Voxels$Tensor_Count_Norm, 4))){browser()}
#   if(!all(round(Empty_Voxels$Orig_Count, 4) == round(Empty_Voxels$Tensor_Count_Adj, 4))){browser()}
# 
#   #################################################
#   # CONVERT COORDINATES USING THE SHIFT CORRECTIONS
#   ################################################# 
# 
#   x_Orig <- BBmisc::normalize(Empty_Voxels$X, method = "range", range = c(Shift_Data$X_origRng_Bot, Shift_Data$X_origRng_Top))
#   Empty_Voxels$X <- x_Orig
#   y_Orig <- BBmisc::normalize(Empty_Voxels$Y, method = "range", range = c(Shift_Data$Y_origRng_Bot, Shift_Data$Y_origRng_Top))
#   Empty_Voxels$Y <- y_Orig
#   z_Orig <- BBmisc::normalize(Empty_Voxels$Z, method = "range", range = c(Shift_Data$Z_origRng_Bot, Shift_Data$Z_origRng_Top))
#   Empty_Voxels$Z <- z_Orig
#   
#   # # FIND THE EMPY VOXELS THAT NEED TO BE ADDED TO LAS
#   # Empty_Voxels$TID <- as.integer(0)
#   
#   # CONVERTING Empty Vox into LAS
#   LAS_Tensor_Vox_Den <- LAS(Empty_Voxels)
#   # LAS_Tensor_Trees <- filter_poi(LAS_Empty_Voxels, Tensor_Count_Norm > 0)
#   
#   # SANITY CHECK
#   # if(!all(round(range(LAS_Empty_Voxels_Trees$X),2) == round(range(LAS_Orig$X),2))){browser()}
#   # if(!all(round(range(LAS_Empty_Voxels_Trees$Y),2) == round(range(LAS_Orig$Y),2))){browser()}
#   # if(!all(round(range(LAS_Empty_Voxels_Trees$Z),2) == round(range(LAS_Orig$Z),2))){browser()}
#   
#   return(LAS_Tensor_Vox_Den)
# }
# 
# REVERSE_ENG_TENSOR_FUN <- function(INPUT_Vox_Den, DIR_LIST, FlightID_LIST, PlotID_LIST, batch_size,
#                                    INPUT_RoI, INPUT_PRIOR_XYZWLHR, pred_XYZWLHR, TARGET_XYZWLHR,
#                                    Para_Vox_Res, Para_Target_Base, Para_MaxZ_Shrink,
#                                    para_RoI_Pool_Dim_XY, para_RoI_Pool_Dim_Z) {
#   
#   print( "IN REVERSE ENGINEERING!" )
#   
#   List_LAS_Tensor_Vox_Den <- list()
#   
#   for(bd in 1:batch_size){
#     oneB_oneRoI_Vox_Den <- torch_squeeze(INPUT_Vox_Den[bd,])
#     
#     # CONVERT LAS TENSOR TO ARRAY 
#     oneB_oneRoI_Vox_Den_Array <- as.array(oneB_oneRoI_Vox_Den$view(c(-1)))
#     
#     # GET SHIFT INFORMATION
#     Shifts_LAS_XYZWLHR <- read.csv(paste(DIR_LIST[bd], "/CSV/F", as.array(FlightID_LIST)[bd], "_Shifts_LAS_XYZWLHR.csv", sep=""))
#     oneP_Shifts_LAS_XYZWLHR <- Shifts_LAS_XYZWLHR[which(Shifts_LAS_XYZWLHR$Plot_ID  == PlotID_LIST[bd]),]
#     
#     # write.csv(LAS_Vox_N_DF, paste(FOLDER_CSV_VOX_MP_O,"/F",  FID,"_P",   Plot_ID, "_LAS_Vox_N.csv",sep=''), row.names=FALSE) 
#     
#     LAS_Orig <- readLAS(paste(DIR_LIST[bd], "/LAS/LAS_MP1/LAS_P/F", as.array(FlightID_LIST)[bd], "_MP1_P", as.array(PlotID_LIST)[bd], ".laz", sep=""))
#     Vox_Den_Orig <- read.csv(paste(DIR_LIST[bd], "/CSV/CSV_MP1/VOX_DF/F", as.array(FlightID_LIST)[bd], "_P", as.array(PlotID_LIST)[bd],"_LAS_Vox_N.csv", sep=""))
#     LAS_Vox_Den_Orig <- LAS(Vox_Den_Orig)
#     LAS_Vox_Den_Orig_Tree <- filter_poi(LAS_Vox_Den_Orig, Count > 0)
#     
#     LAS_Tensor_Vox_Den <- TENSOR_LAS_FUN(oneB_oneRoI_Vox_Den, Shift_Data = oneP_Shifts_LAS_XYZWLHR, Vox_Den_Orig, LAS_Orig,
#                                    Para_Vox_Res,
#                                    Para_Target_Base,
#                                    Para_MaxZ_Shrink)
# 
#     List_LAS_Tensor_Vox_Den[[bd]] <- LAS_Tensor_Vox_Den
#     ##########################
#     # REVERSE ENGINEERING ROIS 
#     ##########################
#     
#     
#   }
# 
#   return(List_LAS_Tensor_Vox_Den)
# }
# 
# ##############################################################################################################################################################################################################
# ##############################################################################################################################################################################################################
# ##############################################################################################################################################################################################################
# ##############################################################################################################################################################################################################
# ##############################################################################################################################################################################################################
# ##############################################################################################################################################################################################################
# ##############################################################################################################################################################################################################
# ##############################################################################################################################################################################################################
# ##############################################################################################################################################################################################################
# ##############################################################################################################################################################################################################
# ##############################################################################################################################################################################################################
# ##############################################################################################################################################################################################################
# 
# #######################################################################################################
# 
# 
# 
# # INTERSECTION OF UNION FOR VOXEL WISE 
# VOX_IoU_LOSS_FUN <- function(Mask_Pred, Mask_Obs) {
#   Mask_Pred <- Mask_Pred$view(-1)
#   Mask_Obs <- Mask_Obs$view(-1)
#   intersection <- (Mask_Pred * Mask_Obs)$sum()
#   #Dice <- 1 - ((2 * intersection + smooth) / (Mask_Pred$sum() + Mask_Obs$sum() + smooth))
#   Union <- torch_squeeze(torch_sum(Mask_Pred + Mask_Obs - (Mask_Pred*Mask_Obs)))
#   VOX_IoU <- torch_squeeze(1 - (intersection/Union))
#   return(VOX_IoU)
# }
# 
# #######################################################################################################
# # # FUNCTION
# # FSCORE_FUN <- function(Empty_Vox, Triangles_ID_All_Mx, OUTPUT_CLASS, INPUT_PRIOR_XYZWLHR, Para_Threshold_IoU, Para_Threshold_Prob, Para_Cnt = 10){
# #   
# #   # IDENTIFY PRIORS WITH HIGH PROBABILITY OF TREE
# #   Binary_Prior_HighProbTree <- OUTPUT_CLASS > Para_Threshold_Prob
# #   Prior_Prob <- as.data.frame(as.array(torch_nonzero(Binary_Prior_HighProbTree)+1)) # +1 to make sure its R and not Python Index
# #   colnames(Prior_Prob) <- c("Batch", "Prior", "Binary")
# # 
# #   if(nrow(Prior_Prob) > 0){
# #     Prob <- c()
# #     # ORDER HIGH PROBABILITY PRIORS FROM HIGHEST PROBABILITY TO LOWEST PROBABILITY
# #     for(j in 1:nrow(Prior_Prob)){
# #       oneProb <- as.array(OUTPUT_CLASS[Prior_Prob[j,1],Prior_Prob[j,2],Prior_Prob[j,3]])
# #       Prob <- c(Prob, oneProb)
# #     }
# #     Order_Descend_Prob <- rev(order(Prob))
# #     Prior_Prob <- Prior_Prob[Order_Descend_Prob,]
# #     
# #     # EXTRACT THE PRIOR WITH HIGHEST PROBABILITY OF TREE
# #     Prior_NMS <- Prior_Prob[1,] # THE VERY TOP ONE IS USED
# #     Prior_Prob <- Prior_Prob[-1,]
# # 
# #     count <- 0
# #     
# #     if(Para_Cnt == 16){
# #       Colnames_XYZWLHR <- c("X_Base", "Y_Base", "Z_Base", 
# #                             "X_BotBox", "Y_BotBox", "L_BotBox", "W_BotBox", "Z_BotBox", "R_BotBox", 
# #                             "X_TopBox", "Y_TopBox", "L_TopBox", "W_TopBox", "Z_TopBox", "R_TopBox", 
# #                             "Z_TopTree")
# #     }else{
# #       Colnames_XYZWLHR <- c("X_Base", "Y_Base", "Z_Base",  
# #                             "X_TopBox", "Y_TopBox", "L_TopBox", "W_TopBox", "Z_TopBox", "R_TopBox", 
# #                             "Z_TopTree")
# #     }
# #     
# #     # WHILE LOOP PERFORMS NMS... GETS ALL PRIORS THAT HAVE HIGHEST PROBABILITY OF TREE AND LEAST OVERLAP WITH OTHER PRIORS WITH HIGH PROBABILITY OF TREE
# #     Cnt_Prior_Prob <- dim(Prior_Prob)[1]
# #     while(Cnt_Prior_Prob > 0){ # WHILE THERE ARE INDICES WITH VALUES
# #       
# #       for(PN in 1:nrow(Prior_NMS)){
# #         one_Prior_NMS <-as.data.frame(t(as.matrix(INPUT_PRIOR_XYZWLHR[Prior_Prob[PN,1],Prior_Prob[PN,2],]))) #INPUT_PRIOR_XYZWLHR[Prior_NMS[[PN]][1],Prior_NMS[[PN]][2],]
# #         
# #         # SECOND EXTRACT ALL THE VOXELS FROM iNMS[PN] PRIOR THAT FALL WITHIN EXISTING PRIOR... IF COUNT IS LESS THAN SOMETHING THEN DISCARD 
# #         colnames(one_Prior_NMS) <- Colnames_XYZWLHR
# #         
# #         # FIRST FOR EACH Prior_Prob PRIOR EXTRACT ALL THE VOXELS  (USING WHOLE CUBE SPACE) WITHIN TRISHAPE
# #         
# #         ############################################################################################
# #         Vert_onePrior <- XYZWHR_TO_VERT_GPU_FUN(one_Prior_NMS, Base_WL = Para_Base_WL, Normalised = Normalised, Para_Cnt = Para_Cnt) # [,-1] NOT THAT THIS PARAMETERS IS NORMALISED
# #         #browser()
# #         nbIntersect = INTERSECT_TRI_FUN(Triangles_ID_All_Mx, Vert_onePrior, Empty_Vox)
# #         insideVect_NMS_Prior <- which(nbIntersect%%2 != 0)
# #         Vox_NMS_Prior <- Empty_Vox[insideVect_NMS_Prior,] 
# # 
# #         # LOOP THROUGH EACH PRIOR THAT IS NOT IN FINAL STOCKING 
# #         index_remove_PriorProb <- c()
# #         
# #         for(PP in 1:nrow(Prior_Prob)){
# #           Test_Prior_IoU <- Prior_Prob[PP,]
# #           one_Prior_Check_IoU <- as.data.frame(t(as.matrix(INPUT_PRIOR_XYZWLHR[Prior_Prob[PP,1],Prior_Prob[PP,2],])))
# #           colnames(one_Prior_Check_IoU) <- Colnames_XYZWLHR
# #           
# #           # FIRST FOR EACH Prior_Prob PRIOR EXTRACT ALL THE VOXELS  (USING WHOLE CUBE SPACE) WITHIN TRISHAPE
# #           
# #           #Q###########################################################################################
# #           Vert_onePrior <- XYZWHR_TO_VERT_GPU_FUN(one_Prior_Check_IoU, Base_WL = Para_Base_WL, Normalised = Normalised, Para_Cnt = Para_Cnt) # [,-1] NOT THAT THIS PARAMETERS IS NORMALISED
# #           nbIntersect = INTERSECT_TRI_FUN(Triangles_ID_All_Mx, Vert_onePrior, Vox_NMS_Prior)
# #           insideVect_i_Prior <- which(nbIntersect%%2 != 0) # LAS_one_i_Prior <- lasfilter(LAS_Empty_Vox, unlist(nbIntersect%%2 != 0))  
# #           
# #           ############################################################################################
# #           
# #           # LOOP FINAL STOCKING PRIORS (Prior_NMS) TO SEE IF REMAINING PRIORS (Prior_Prob) OVERLAPS STOCKING PRIORS
# #          
# #           Overlap_Percent <- length(insideVect_NMS_Prior)/length(insideVect_i_Prior)
# #           # IF THERE IS LARGE OVERLAP STORE THE INDEX OF OVERLAPPING PriorProb
# #           if(Overlap_Percent > Para_Threshold_IoU){
# #             index_remove_PriorProb <- c(index_remove_PriorProb, PP)
# #           }
# #         } # PP LOOP WHICH LOOPS THROUGH Prior_Prob (potential Priors)
# # 
# #         # UPDATE THE Prior_NMS ONLY IF IT DOES NOT ALSO OVERLAP A PP
# #         index_remove_PriorProb <- unique(index_remove_PriorProb) # NOT SURE IF THIS IS NECESSARY
# #         Prior_Prob <- Prior_Prob[-index_remove_PriorProb,]
# #         # IF THERE ARE NO MORE PRIOR_PROB BREAK OUT OF PN LOOP
# #         if(nrow(Prior_Prob) == 0){
# #           break() 
# #         }
# #       }# PN LOOP WHICH LOOPS THROUGH Prior_NMS 
# # 
# #       if(nrow(Prior_Prob) > 0){
# #         Prior_NMS <- rbind(Prior_NMS, Prior_Prob[1,]) # ADD TOP PRIOR PROB THAT DIDN'T OVERLAP ANY Prior_NMS ...  cTO Prior_NMS
# #         Prior_Prob <- Prior_Prob[-1,]  # REMOVE THE PRIOR THAT IS BEST AND THEREFORE IN Prior_NMS
# #       }
# #       Cnt_Prior_Prob <- dim(Prior_Prob)[1]
# #     } # LOOP WHILE THERE ARE PP 
# # 
# #   }else{
# #     # IF THERA ARE NO PRIORS ABOVE THE PROBABILITY THRESHOLD, THERE ARE ALSO NO NMS PRIORS
# #     Prior_NMS <- Prior_Prob
# #     # browser()
# #   }
# #   return(list(Prior_NMS))
# # }
# 
# ###############################################################################################################################
# 
# # FINDING PEAKS WHEN THERE ARE NO ZEROS
# Peak_Dip_FUN = function(pdens)
# {
#   TurnPnts <- turnpoints(pdens$y)  
#   Index <- c(1, TurnPnts$tppos, TurnPnts$n)
#   # DATAFRAME OF TURNING POINTS IN KERNAL DENSITY
#   Z <- c(min(pdens$x), pdens$x [TurnPnts$tppos], max(pdens$x)) # Adding one extra turning point at start and end
#   Density <- c(0, pdens$y[TurnPnts$tppos], 0)    # Adding one extra turning point at start and end
#   Peak_Dip_Summary <- t(data.frame(rbind(Z, Density, Index)))
#   colnames(Peak_Dip_Summary) <- c("Z", "Density", "Index")
#   Peak_Dip_Summary<- data.frame( Peak_Dip_Summary, Peak_Dip = rep("Dip", nrow(Peak_Dip_Summary)), stringsAsFactors = FALSE) 
#   Peak_Dip_Summary$Peak_Dip [c(1, nrow(Peak_Dip_Summary))] <- c("Start_End", "Start_End")
#   
#   ##############################################################################    
#   # PEAKS AND DIPS
#   ##############################################################################  
#   
#   Y_Peak <- TurnPnts$points[TurnPnts$peaks]
#   X_Peak <- pdens$x[TurnPnts$pos[TurnPnts$peaks]]
#   
#   Peak_Dip_Summary$Peak_Dip[match(X_Peak,Peak_Dip_Summary$Z)] <- "Peak"
#   
#   Dip_DF <- Peak_Dip_Summary[which(Peak_Dip_Summary$Peak_Dip == "Dip"),]
#   minDip_DF <- Dip_DF[which.min(Dip_DF$Density),]
#   
#   Peak_DF <- Peak_Dip_Summary[which(Peak_Dip_Summary$Peak_Dip == "Peak"),]
#   maxPeak_DF <- Peak_DF[which.max(Peak_DF$Density),]
#   
#   Range_Den_minDip_maxPeak <- maxPeak_DF$Density - minDip_DF$Density
#   
#   return(list(Peak_Dip_Summary=Peak_Dip_Summary,
#               minDip_DF = minDip_DF,
#               maxPeak_DF = maxPeak_DF,
#               Range_Den_minDip_maxPeak = Range_Den_minDip_maxPeak))
# }
# 
# 
# GAP_DENSITY_FUNCTION_NEW2 = function(Z_Values, 
#                                      Para_BW = 0.4,
#                                      Para_Threshold_Percent = 0.2,
#                                      Plot = "No",
#                                      Plot_Heading = 1) #also include argument about average height for flight...
# {
#   
#   pdens <- density(Z_Values, bw=Para_BW)
# 
#   #browser()
#   # GET Peak_Dip_Summary
#   Peak_Dip_Summary <- Peak_Dip_FUN(pdens) # Peak_Dip_Summary$Peak_Dip_Summary
#   
#   # GET ALL SECTIONS THAT ARE WITHIN 20% ABOVE THE MINIMUM DIP
#   Den_Threshold <- Peak_Dip_Summary$minDip_DF$Density + Peak_Dip_Summary$Range_Den_minDip_maxPeak * Para_Threshold_Percent
#   #browser()
#   # IF THERE ARE NO MINIMUM DIPS
#   if(length(Den_Threshold) == 0){
#     Start_Largest_Gap <- max(Peak_Dip_Summary$Peak_Dip_Summary$Z)
#     End_Largest_Gap <- max(Peak_Dip_Summary$Peak_Dip_Summary$Z)
#     
#   }else{
#     
#     # Near_min_density <- which(pdens$y >= Peak_Dip_Summary$minDip_DF$Density & pdens$y < Den_Threshold)
#     Near_min_density <- which(pdens$y < Den_Threshold)
#     # REMOVE STRING OF LOW DENSITIES NEAR BOTTOM HEIGHT AND TOP CANOPY HEIGHT
#     
#     Index_PeakDip_inMinDen <- Peak_Dip_Summary$Peak_Dip_Summary$Index[which(Peak_Dip_Summary$Peak_Dip_Summary$Index %in% Near_min_density)]
#     
#     
#     # IDENTIFY THE SECTION THAT HAS THE LARGEST STRETCH AND USE THOSE BOUNDS TO DETERMINE
#     # UNDER TOP AND CANOPY BASE AS FIRST PASS
#     
#     # VEG_PROGILE_GAPS _WITH_LOW_LIDAR_DENSITY FUNCTION 
#     Near_min_Density_Function = function(Near_min_density) 
#     {
#       # CALCULATING WHERE THE GAP IS
#       
#       Zero_Density <- rle(diff(Near_min_density))
#       myZero_Density <- which(Zero_Density$values == TRUE & Zero_Density$lengths > 0)
#       Zero_Density.lengths.cumsum <- cumsum(Zero_Density$lengths)
#       ends <- Zero_Density.lengths.cumsum[myZero_Density]
#       newindex <- ifelse(myZero_Density>1, myZero_Density-1, 0)
#       starts <- Zero_Density.lengths.cumsum[newindex] + 1
#       starts_2 <- starts
#       #browser()
#       if(length(which(newindex == 0))>0){starts_2 <-  c(1,starts)} 
#       
#       starts <- starts_2
#       Start_Height <- pdens$x[Near_min_density[starts]]
#       End_Height <- pdens$x[Near_min_density[ends]]
#       
#       # REMOVING THE GAP BELOW THE GROUND
#       if(End_Height[1] < 0){
#         Start_Height <- Start_Height[-1]  
#         End_Height <- End_Height[-1] 
#       }
#       
#       return(list(Start_Height=Start_Height, 
#                   End_Height=End_Height))  
#     }
#     # END Near_min_Density_Function
#     
#     Near_Zero_Density_List <-Near_min_Density_Function(Near_min_density)
#     
#     # GET OUTPUT FROM ONE OF TWO METHODS ABOVE
#     Start_Height <- Near_Zero_Density_List$Start_Height
#     End_Height <- Near_Zero_Density_List$End_Height
#     #new_Max_Value <- Near_Zero_Density_List$new_Max_Value
#     
#     # plot(pdens$x, pdens$y, type="l", ylim=c(0, 0.6),
#     #      main= paste( "F:", PointSourceID),
#     #      ylab = paste("Start:", round(Start_Height, 2)),
#     #      xlab = paste("End:", round(End_Height, 2)))
#     # 
#     # abline(v = Start_Height, col="blue", lwd=4)
#     # abline(v = End_Height, col="dark green", lwd=4)
#     # abline(h = Den_Threshold, col= "red")
#     
#     # GET THE LARGEST GAP WITH DENSITY CLOSE TO ZERO AND THAT DETERMINES THE UNDER AND CANOPY BASE 
#     Gaps <- End_Height -Start_Height
#     Largest_Gap <- max(Gaps) # Second_Largest_Gap <- sort(Gaps,partial=length(Gaps)-1)[length(Gaps)-1]
#     Start_Largest_Gap <- Start_Height[which(Gaps == Largest_Gap)][1]
#     End_Largest_Gap <- End_Height[which(Gaps == Largest_Gap)]
#     
#     # GET ALL PEAKS AND DIPS WITHIN FIRST PASS OF START AND END
#     PeakDip_Within_minDen <- Peak_Dip_Summary$Peak_Dip_Summary[which(Peak_Dip_Summary$Peak_Dip_Summary$Index %in% Index_PeakDip_inMinDen),]
#     
#     # MAKE THE START OF GAP AT THE "FIRST MIN" WITHIN THAT GAP
#     # WHERE "FIRST MIN" IS THE FIRST MIN THAT IS LESS THAN THE 50 PERCENTILE OF ALL MINS (AVOIDS USING MINS THAT ARE NOT SIGNIFICANT MINS)
#     # DOM DOM! SOME MINS STAY MIN FOR LONGER WHEN THERE ARE NO POINTS OVER A HEIGHT... MAYBE THIS NEEDS EXPLOITING!
#     #browser()
#     
#     Density_Dips_Start <- PeakDip_Within_minDen$Density[ which(PeakDip_Within_minDen$Z > Start_Largest_Gap & 
#                                                                  PeakDip_Within_minDen$Peak_Dip == "Dip")]
#     
#     Constraint <- which(PeakDip_Within_minDen$Z > Start_Largest_Gap & 
#             PeakDip_Within_minDen$Peak_Dip == "Dip" & 
#             PeakDip_Within_minDen$Density <= median(Density_Dips_Start))
#     
#     if(length(Constraint) > 0){
#       Start_Largest_Gap <- min(PeakDip_Within_minDen$Z[Constraint])
#     } else{
#       Index_Dip_Below_Start <- which(PeakDip_Within_minDen$Peak_Dip == "Dip")
#       End_Largest_Gap <- PeakDip_Within_minDen$Z[min(Index_Dip_Below_Start)]
#       Index_Dip_Below_End <- which(PeakDip_Within_minDen$Z < End_Largest_Gap)
#       Start_Largest_Gap <- PeakDip_Within_minDen$Z[max(Index_Dip_Below_End)]
#     }
#     
#     
#     
#     # MAKE THE END OF GAP AT THE LAST MIN WITHIN THAT GAP
#     
#     Density_Dips_Ends <- PeakDip_Within_minDen$Density[ which(PeakDip_Within_minDen$Z < End_Largest_Gap & 
#                                                                 PeakDip_Within_minDen$Peak_Dip == "Dip")]
#     
#     Constraint <- which(PeakDip_Within_minDen$Z < End_Largest_Gap & 
#             PeakDip_Within_minDen$Peak_Dip == "Dip" & 
#             PeakDip_Within_minDen$Density <= median(Density_Dips_Ends))
#     
#     if(length(Constraint) > 0){
#       # ONLY WORK WITH DENSITIES THAT ARE BELOW THE MEDIAN OF ALL DIPS
#       End_Largest_Gap <- max(PeakDip_Within_minDen$Z[Constraint])
#     }else{
#       End_Largest_Gap <- Start_Largest_Gap
#     }
# 
#   }
#   
#   if(Plot == "Yes"){
#     
#     plot(pdens$x, pdens$y, type="l", ylim=c(0, 0.6),
#          main= paste( "F:", Plot_Heading),
#          ylab = "Density", #paste("Start:", round(Start_Largest_Gap, 2)),
#          xlab = "Height (m)", #paste("End:", round(End_Largest_Gap, 2)),
#          cex.lab = 1.5,
#          cex.axis = 2)
#     
#     abline(v = Start_Largest_Gap, col="blue", lwd = 3)
#     abline(v = End_Largest_Gap, col="dark green", lwd = 3)
#     
#     
#     #plot(pdens)
#     abline(h = Den_Threshold, col= "red", lwd = 2)
#     
#     legend("topright", legend = c("Threshold Height", "Bottom Minima", "Top Minima"),
#            lty= 1, col=c("red", "blue", "dark green"), lwd=3,
#            cex= 1.7)
#   }
#   
#   return(list(Start_Largest_Gap=Start_Largest_Gap, 
#               End_Largest_Gap=End_Largest_Gap,
#               Peak_Dip_Summary = Peak_Dip_Summary))
# }
# 
# 
# 
# 
# 
# ##################################################################################################
# # https://towardsdatascience.com/pytorch-tabular-binary-classification-a0368da5bb89
# BINARY_ACCURACY_FUN <-  function (y_pred = BINARY_SCORE_View, y_test = TARGET_Prior_Type){
#   y_pred_tag = torch_round(torch_sigmoid(y_pred))
#   correct_results_sum = sum((y_pred_tag == y_test))
#   acc = correct_results_sum/dim(y_test)
#   acc = torch_round(acc * 100)
#   return(list(acc, y_pred_tag))
# }
# 
# ##################################################################################################
# FSCORE_SUMMARY_FUN <- function(IoU_Vox_Rest_Subj, oneRestID, oneSubjID){
#   
#   Summary_oneRestoneSubj <- as.data.frame(IoU_Vox_Rest_Subj  %>%
#                                             dplyr::group_by(Rest, Subj) %>%
#                                             dplyr::summarise(Count = length(Rest), .groups = 'drop'))
#   
#   ###############################
#   # CALCULATE FSCORE (TP, FN, FP)
#   ###############################
#   Index_TP <- which(Summary_oneRestoneSubj$Rest > 0 & Summary_oneRestoneSubj$Subj > 0)
#   Summary_FSc_IoU_oneRestoneSubj <- Summary_oneRestoneSubj[Index_TP,]
#   
#   if(nrow(Summary_FSc_IoU_oneRestoneSubj) == 0){
#     Summary_FSc_IoU_oneRestoneSubj <- rbind(Summary_FSc_IoU_oneRestoneSubj, apply(Summary_oneRestoneSubj, 2, sum))
#     colnames(Summary_FSc_IoU_oneRestoneSubj) <- colnames(Summary_oneRestoneSubj)
#     Summary_FSc_IoU_oneRestoneSubj$Portion_IoU_CorrecIn <- 0
#     Summary_FSc_IoU_oneRestoneSubj$Portion_IoU_WrongIn <- 1
#     Summary_FSc_IoU_oneRestoneSubj$TP <- 0
#   }else{
#     Summary_FSc_IoU_oneRestoneSubj$Portion_IoU_CorrecIn <- Summary_FSc_IoU_oneRestoneSubj$Count/ sum(Summary_oneRestoneSubj$Count)
#     Summary_FSc_IoU_oneRestoneSubj$Portion_IoU_WrongIn <- (sum(Summary_oneRestoneSubj$Count) -Summary_FSc_IoU_oneRestoneSubj$Count)/ sum(Summary_oneRestoneSubj$Count)
#     Summary_FSc_IoU_oneRestoneSubj$TP <- Summary_oneRestoneSubj$Count[Index_TP]
#     }
#   
#   Summary_FSc_IoU_oneRestoneSubj$oneRest <- oneRestID
#   Summary_FSc_IoU_oneRestoneSubj$oneSubj <- oneSubjID
#   Summary_FSc_IoU_oneRestoneSubj$Total_Vox <- sum(Summary_oneRestoneSubj$Count)
# 
#   Index_FN <- which(Summary_oneRestoneSubj$Rest == 0 & Summary_oneRestoneSubj$Subj > 0)
#   Summary_FSc_IoU_oneRestoneSubj$FN <- 0
#   if(length(Index_FN) > 0){ Summary_FSc_IoU_oneRestoneSubj$FN <- Summary_oneRestoneSubj$Count[Index_FN] }
#   
#   Index_FP <- which(Summary_oneRestoneSubj$Rest == 1 & Summary_oneRestoneSubj$Subj == 0)
#   Summary_FSc_IoU_oneRestoneSubj$FP <- 0
#   if(length(Index_FP) > 0){ Summary_FSc_IoU_oneRestoneSubj$FP <- Summary_oneRestoneSubj$Count[Index_FP] }
#   
#   Summary_FSc_IoU_oneRestoneSubj$Recall <- Summary_FSc_IoU_oneRestoneSubj$TP/(Summary_FSc_IoU_oneRestoneSubj$TP + Summary_FSc_IoU_oneRestoneSubj$FN)
#   Summary_FSc_IoU_oneRestoneSubj$Precision <- Summary_FSc_IoU_oneRestoneSubj$TP/(Summary_FSc_IoU_oneRestoneSubj$TP + Summary_FSc_IoU_oneRestoneSubj$FP)
#   Summary_FSc_IoU_oneRestoneSubj$FScore <- 2*((Summary_FSc_IoU_oneRestoneSubj$Recall* Summary_FSc_IoU_oneRestoneSubj$Precision)/(Summary_FSc_IoU_oneRestoneSubj$Recall + Summary_FSc_IoU_oneRestoneSubj$Precision))
#   if(is.nan(Summary_FSc_IoU_oneRestoneSubj$FScore)){Summary_FSc_IoU_oneRestoneSubj$FScore <- 0}
#   # Summary_FSc_IoU_oneRestoneSubj$Rest_Z_TopBox <- oneP_oneR_Rest_XYZWLHR_ExtP$Z_TopBox
#   return(Summary_FSc_IoU_oneRestoneSubj)
# }
# 
# ##################################################################################################
# 
# 
# # FSCORE_FUN <- function(Index_Compute,    # Index_PosBinaryTARGET
# #                     LAS_Vox, 
# #                     LIST_Subj_VoxInTrishp, 
# #                     LIST_Subj_Vert_ExtP_Gnd,
# #                     oneP_SubjID = oneP_GT, 
# #                     oneP_RestID = oneP_PriorID,
# #                     Rest_XYZWLHR,     # oneP_Pred_XYZWLHR_ExtP_DF
# #                     Index_Rest,       # BB
# #                     TriShp_Nodes,     # Triangles_ID_All_Mx_L4
# #                     Para_Base_WL, Para_Target_Base, Para_TriShpParaCnt, Para_Target_Z_Height,
# #                     Plot_TriShp = "Yes"){
# #   
# #   unique_Subj <- unique(oneP_SubjID)
# #   oneSubj_Value <- Index_Compute[Index_Rest]
# #   
# #   # GET THE SUBJECT GT VOXELS
# #   oneSubjID <- oneP_SubjID[oneSubj_Value]
# #   oneSubj_VoxInTrishp <- LIST_Subj_VoxInTrishp[[which(unique_Subj == oneSubjID)]] # WORK OUT WHICH GT IT IS AND GET THAT INDEX
# #   
# #   #################
# #   #################
# # 
# #   # VERTICES FOR PREDICTION (Inc GND)
# #   oneP_oneR_Rest_XYZWLHR_ExtP <- Rest_XYZWLHR[Index_Rest,]
# #   oneRest_Vert_ExtP <- XYZWHR_TO_VERT_GPU_FUN(oneP_oneR_Rest_XYZWLHR_ExtP, Base_WL = Para_Base_WL/Para_Target_Base, Normalised = "Yes", Para_Cnt = Para_TriShpParaCnt, device=device) # [,-1] NOT THAT THIS PARAMETERS IS NORMALISED
# #   oneRest_Vert_ExtP_oneGnd <- oneRest_Vert_ExtP[1:4,]
# #   oneRest_Vert_ExtP_oneGnd[,3] <- 0
# #   oneRest_Vert_ExtP_Gnd <- rbind(oneRest_Vert_ExtP_oneGnd, oneRest_Vert_ExtP)
# #   oneRest_Vert_ExtP_Gnd <- VERTICIES_ORDER_FUN(oneRest_Vert_ExtP_Gnd, Box_Levels = 4)
# # 
# #   # GET THE PREDICTED TREES VOXELS 
# #   Empty_Vox <- LAS_Vox@data[,1:3]
# #   oneRest_IntSubj = INTERSECT_TRI_FUN(TriShp_Nodes, as.matrix(oneRest_Vert_ExtP_Gnd), Empty_Vox)
# #   oneRest_VoxInTrishp <- which(oneRest_IntSubj%%2 != 0)
# #   
# #   # INTERSECT VOXELS BETWEEN SUBJECT GT AND PREDICTED TREE (THAT IS POSITIVE BINARY)
# #   IoU_Vox_Rest_Subj <- data.frame(t(splitstackshape:::charMat(list(oneRest_VoxInTrishp, oneSubj_VoxInTrishp), 0)))
# #   colnames(IoU_Vox_Rest_Subj) <- c("Rest", "Subj")
# #   
# #   Binary_Vox_TriShp <- data.frame(bce_Output = IoU_Vox_Rest_Subj$Rest, bce_Target =  IoU_Vox_Rest_Subj$Subj)
# #   
# #   IoU_Vox_Rest_Subj$Subj[which(IoU_Vox_Rest_Subj$Subj == 1)] <- oneSubj_Value
# # 
# #   ######################
# #   # IoU (Rest AND FSubj)
# #   ######################
# #   oneRestID <- oneP_RestID[oneSubj_Value]
# #   Summary_FSc_IoU_oneRestoneSubj <- c(IoU_Vox_Rest_Subj, oneRestID, oneSubjID)
# # 
# #   ###############
# #   # PLOT TRISHAPE
# #   ###############
# #   if(Plot_TriShp == "Yes"){
# # 
# #     LAS_Vox@data$Subj[oneSubj_VoxInTrishp] <- oneSubj_Value # 1
# #     LAS_Vox@data$Rest[oneRest_VoxInTrishp] <- 1
# #     
# #     # GET ALL VOXELS THAT ARE PRIOR AND Subj
# #     Test_LAS_Empty_N_Zero <- filter_poi (LAS_Vox, (X == 0 | X == 1) & 
# #                                                   (Y == 0 | Y == 1) & 
# #                                                   (Z == 0 | Z == 1))
# #     Test_LAS_Empty_N_Zero@data$TID <- 1
# #     
# #     LAS_Vox_Subj_Rest <- filter_poi(LAS_Vox, Rest == 1 | Subj == oneSubj_Value) 
# #     LAS_Plot <- LAS_Vox_Subj_Rest
# #     if(nrow(LAS_Plot@data) > 0){
# #       LAS_Plot_Corners <- LAS_Plot
# # 
# #       LAS_Plot_Corners@data$TID  <- 0
# #       Test_LAS_Empty_N_Zero@data$TID <- 0; Test_LAS_Empty_N_Zero@data$Subj <- 0; Test_LAS_Empty_N_Zero@data$Rest <- 0;
# #       LAS_Plot_Corners <- rbind(LAS_Plot_Corners, Test_LAS_Empty_N_Zero)
# #       plot(LAS_Plot_Corners, size = 6)
# #       bg3d("white")
# #       
# #       oneSubj_Vert_ExtP_Gnd <- LIST_Subj_Vert_ExtP_Gnd[[which(unique_Subj == oneSubjID)]]
# #       oneSubj_Vert_ExtP_Gnd <- as.matrix(oneSubj_Vert_ExtP_Gnd)
# #       oneSubj_Vert_ExtP_Gnd <- na.omit(oneSubj_Vert_ExtP_Gnd)
# #       
# #       oneSubj_Vert_ExtP_Gnd <- VERTICIES_ORDER_FUN(oneSubj_Vert_ExtP_Gnd, Box_Levels = 4)
# #       
# #       triangles3d(oneSubj_Vert_ExtP_Gnd[Triangles_ID_All_L4,], col="green")
# #       triangles3d(oneRest_Vert_ExtP_Gnd[Triangles_ID_All_L4,], col="red")
# #       text3d(0.5,0.5,0.2, paste("F:",round(Summary_FSc_IoU_oneRestoneSubj$FScore, 2)))
# #       text3d(0.5,0.5,0.8, paste("GT:",Summary_FSc_IoU_oneRestoneSubj$GT, "Pr:", Summary_FSc_IoU_oneRestoneSubj$PriorID))
# #     } # IF Subj LAS HAS POINTS
# #   }
# # 
# #   return(list(Summary_FSc_IoU_oneRestoneSubj, Binary_Vox_TriShp, oneRest_Vert_ExtP_Gnd, oneRest_VoxInTrishp) )
# # }
# 
# FSCORE_GPU_FUN <- function(Index_Compute,    # Index_PosBinaryTARGET
#                     LAS_Vox, 
#                     LIST_Subj_VoxInTrishp, 
#                     LIST_Subj_Vert_ExtP_Gnd,
#                     oneP_SubjID = oneP_GT, 
#                     oneP_RestID = oneP_PriorID,
#                     Rest_XYZWLHR,     # oneP_Pred_XYZWLHR_ExtP_DF
#                     Index_Rest,       # BB
#                     TriShp_Nodes,     # Triangles_ID_All_Mx_L4
#                     Para_Base_WL, Para_Target_Base, Para_TriShpParaCnt, Para_Target_Z_Height,
#                     Plot_TriShp = "Yes"){
#   
#   unique_Subj <- unique(oneP_SubjID) 
#   oneSubj_Value <- Index_Compute[Index_Rest]
#   
#   # GET THE SUBJECT GT VOXELS
#   oneSubjID <- oneP_SubjID[oneSubj_Value]
#   oneSubj_VoxInTrishp <- LIST_Subj_VoxInTrishp[[which(unique_Subj == oneSubjID)]] # WORK OUT WHICH GT IT IS AND GET THAT INDEX
# 
#   #################
#   #################
#   # VERTICES FOR PREDICTION (Inc GND)
#   oneP_oneR_Rest_XYZWLHR_ExtP <- Rest_XYZWLHR[Index_Rest,]
#   oneRest_Vert_ExtP <- XYZWHR_TO_VERT_GPU_FUN(oneP_oneR_Rest_XYZWLHR_ExtP, Base_WL = Para_Base_WL/Para_Target_Base, Normalised = "Yes", Para_Cnt = Para_TriShpParaCnt, device=device) # [,-1] NOT THAT THIS PARAMETERS IS NORMALISED
#   # oneRest_Vert_ExtP_oneGnd <- oneRest_Vert_ExtP[1:4,]
#   # oneRest_Vert_ExtP_oneGnd[,3] <- 0
#   # oneRest_Vert_ExtP_Gnd <- rbind(oneRest_Vert_ExtP_oneGnd, oneRest_Vert_ExtP)
#   # oneRest_Vert_ExtP_Gnd <- VERTICIES_ORDER_FUN(oneRest_Vert_ExtP_Gnd, Box_Levels = 4)
#   oneRest_Vert_ExtP <- oneRest_Vert_ExtP$to(device = "cpu")
#   oneRest_Vert_ExtP_oneGnd <- torch_clone(oneRest_Vert_ExtP)
#   oneRest_Vert_ExtP_oneGnd <- oneRest_Vert_ExtP_oneGnd[1:4,]
#   oneRest_Vert_ExtP_oneGnd[,3] <- 0
#   oneRest_Vert_ExtP_Gnd <- as.array(torch_cat(c(oneRest_Vert_ExtP_oneGnd, oneRest_Vert_ExtP)))
#   
# 
#   # GET THE PREDICTED TREES VOXELS 
#   oneRest_Vert_ExtP_Gnd <- VERTICIES_ORDER_FUN(oneRest_Vert_ExtP_Gnd, Box_Levels = 4)
#   oneRest_IntSubj = INTERSECT_TRI_FUN(Triangles_ID_All_Mx_L4, as.matrix(oneRest_Vert_ExtP_Gnd), Empty_Vox)         # GIVE VOXELS IN Subj TRISHAPE TID VALUE
#   oneRest_VoxInTrishp <- which(oneRest_IntSubj%%2 != 0)
# 
#   # Empty_Vox <- LAS_Vox@data[,1:3]
#   # oneRest_IntSubj = INTERSECT_TRI_FUN(TriShp_Nodes, as.matrix(oneRest_Vert_ExtP_Gnd), Empty_Vox)
#   # oneRest_VoxInTrishp <- which(oneRest_IntSubj%%2 != 0)
#   
#   # INTERSECT VOXELS BETWEEN SUBJECT GT AND PREDICTED TREE (THAT IS POSITIVE BINARY)
#   IoU_Vox_Rest_Subj <- data.frame(t(splitstackshape:::charMat(list(oneRest_VoxInTrishp, oneSubj_VoxInTrishp), 0)))
#   colnames(IoU_Vox_Rest_Subj) <- c("Rest", "Subj")
#   
#   Binary_Vox_TriShp <- data.frame(bce_Output = IoU_Vox_Rest_Subj$Rest, bce_Target =  IoU_Vox_Rest_Subj$Subj)
#   IoU_Vox_Rest_Subj$Subj[which(IoU_Vox_Rest_Subj$Subj == 1)] <- oneSubj_Value
#   
#   ######################
#   # IoU (Rest AND FSubj)
#   ######################
#   # browser()
#   oneRestID <- oneP_RestID[oneSubj_Value] # as.array($unsqueeze(1)$to(device="cpu"))
#   Summary_FSc_IoU_oneRestoneSubj <- FSCORE_SUMMARY_FUN(IoU_Vox_Rest_Subj, oneRestID, oneSubjID)
# 
#   # ###############
#   # # PLOT TRISHAPE
#   # ###############
#   # if(Plot_TriShp == "Yes"){
#   #   
#   #   LAS_Vox@data$Subj[oneSubj_VoxInTrishp] <- oneSubj_Value # 1
#   #   LAS_Vox@data$Rest[oneRest_VoxInTrishp] <- 1
#   #   
#   #   # GET ALL VOXELS THAT ARE PRIOR AND Subj
#   #   Test_LAS_Empty_N_Zero <- filter_poi (LAS_Vox, (X == 0 | X == 1) & 
#   #                                          (Y == 0 | Y == 1) & 
#   #                                          (Z == 0 | Z == 1))
#   #   Test_LAS_Empty_N_Zero@data$TID <- 1
#   #   
#   #   LAS_Vox_Subj_Rest <- filter_poi(LAS_Vox, Rest == 1 | Subj == oneSubj_Value) 
#   #   LAS_Plot <- LAS_Vox_Subj_Rest
#   #   if(nrow(LAS_Plot@data) > 0){
#   #     LAS_Plot_Corners <- LAS_Plot
#   #     
#   #     LAS_Plot_Corners@data$TID  <- 0
#   #     Test_LAS_Empty_N_Zero@data$TID <- 0; Test_LAS_Empty_N_Zero@data$Subj <- 0; Test_LAS_Empty_N_Zero@data$Rest <- 0;
#   #     LAS_Plot_Corners <- rbind(LAS_Plot_Corners, Test_LAS_Empty_N_Zero)
#   #     plot(LAS_Plot_Corners, size = 6)
#   #     bg3d("white")
#   #     
#   #     oneSubj_Vert_ExtP_Gnd <- LIST_Subj_Vert_ExtP_Gnd[[which(unique_Subj == oneSubjID)]]
#   #     oneSubj_Vert_ExtP_Gnd <- as.matrix(oneSubj_Vert_ExtP_Gnd)
#   #     oneSubj_Vert_ExtP_Gnd <- na.omit(oneSubj_Vert_ExtP_Gnd)
#   #     
#   #     oneSubj_Vert_ExtP_Gnd <- VERTICIES_ORDER_FUN(oneSubj_Vert_ExtP_Gnd, Box_Levels = 4)
#   #     
#   #     triangles3d(oneSubj_Vert_ExtP_Gnd[Triangles_ID_All_L4,], col="green")
#   #     triangles3d(oneRest_Vert_ExtP_Gnd[Triangles_ID_All_L4,], col="red")
#   #     text3d(0.5,0.5,0.2, paste("F:",round(Summary_FSc_IoU_oneRestoneSubj$FScore, 2)))
#   #     text3d(0.5,0.5,0.8, paste("GT:",Summary_FSc_IoU_oneRestoneSubj$GT, "Pr:", Summary_FSc_IoU_oneRestoneSubj$PriorID))
#   #   } # IF Subj LAS HAS POINTS
#   # }
#   
#   return(list(Summary_FSc_IoU_oneRestoneSubj, Binary_Vox_TriShp, oneRest_Vert_ExtP_Gnd, oneRest_VoxInTrishp) )
# }
# 
# 
# CIoU_D_V_FUN <- function(Pred_XYZWLHR = oneP_oneR_Pred_XYZWLHR, 
#                           GT_XYZWLHR = oneP_oneR_TARGET_XYZWLHR,
#                           Para_Base_WL, Para_Target_Base, Para_TriShpParaCnt, device=device){
#  
#   #################
#   #################
#   # VERTICES FOR PREDICTION (Inc GND)
# 
#   Pred_Vert <- XYZWHR_TO_VERT_GPU_FUN(Pred_XYZWLHR, Base_WL = Para_Base_WL/Para_Target_Base, Normalised = "Yes", Para_Cnt = Para_TriShpParaCnt, device=device) # [,-1] NOT THAT THIS PARAMETERS IS NORMALISED
#   Pred_Vert <- Pred_Vert$to(device = "cpu")
#   Pred_Vert_oneGnd <- torch_clone(Pred_Vert)
#   Pred_Vert_oneGnd <- Pred_Vert_oneGnd[1:4,]
#   Pred_Vert_oneGnd[,3] <- 0
#   Pred_Vert_Gnd <- as.array(torch_cat(c(Pred_Vert_oneGnd, Pred_Vert)))
#   
#   # GET THE PREDICTED TREES VOXELS 
#   Pred_Vert_Gnd <- VERTICIES_ORDER_FUN(Pred_Vert_Gnd, Box_Levels = 4)
#   Pred_IntSubj = INTERSECT_TRI_FUN(Triangles_ID_All_Mx_L4, as.matrix(Pred_Vert_Gnd), Empty_Vox)         # GIVE VOXELS IN Subj TRISHAPE TID VALUE
#   Pred_VoxInTrishp <- which(Pred_IntSubj%%2 != 0)
#   
#   GT_Vert <- XYZWHR_TO_VERT_GPU_FUN(GT_XYZWLHR, Base_WL = Para_Base_WL/Para_Target_Base, Normalised = "Yes", Para_Cnt = Para_TriShpParaCnt, device=device)
#   GT_Vert <- GT_Vert$to(device = "cpu")
#   GT_Vert_oneGnd <- torch_clone(GT_Vert)
#   GT_Vert_oneGnd <- GT_Vert_oneGnd[1:4,]
#   GT_Vert_oneGnd[,3] <- 0
#   GT_Vert_Gnd <- as.array(torch_cat(c(GT_Vert_oneGnd, GT_Vert)))
#   
#   # GET THE PREDICTED TREES VOXELS 
#   GT_Vert_Gnd <- VERTICIES_ORDER_FUN(GT_Vert_Gnd, Box_Levels = 4)
#   GT_IntSubj = INTERSECT_TRI_FUN(Triangles_ID_All_Mx_L4, as.matrix(GT_Vert_Gnd), Empty_Vox)         # GIVE VOXELS IN Subj TRISHAPE TID VALUE
#   GT_VoxInTrishp <- which(GT_IntSubj%%2 != 0)
#   
#   # Empty_Vox <- LAS_Vox@data[,1:3]
#   # oneRest_IntSubj = INTERSECT_TRI_FUN(TriShp_Nodes, as.matrix(Pred_Vert_Gnd), Empty_Vox)
#   # oneRest_VoxInTrishp <- which(oneRest_IntSubj%%2 != 0)
#   
#   # INTERSECT VOXELS BETWEEN SUBJECT GT AND PREDICTED TREE (THAT IS POSITIVE BINARY)
#   IoU_Vox_Pred_GT <- data.frame(t(splitstackshape:::charMat(list(Pred_VoxInTrishp, GT_VoxInTrishp), 0)))
#   colnames(IoU_Vox_Pred_GT) <- c("Rest", "Subj")
# 
#   #Binary_Vox_TriShp <- data.frame(bce_Output = IoU_Vox_Pred_GT$Rest, bce_Target =  IoU_Vox_Pred_GT$Subj)
#   
#   ######################
#   # IoU (Rest AND FSubj)
#   ######################
#   # browser()
# 
#   Summary_FSc_IoU_oneRestoneSubj <- FSCORE_SUMMARY_FUN(IoU_Vox_Pred_GT, NA, NA)
# 
#   
# 
#   # Colnames_XYZWLHR <- c("X_Base", "Y_Base", "Z_Base",  
#   #                       "X_TopBox", "Y_TopBox", "L_TopBox", "W_TopBox", "Z_TopBox", "R_TopBox", 
#   #                       "Z_TopTree")
#   
#   # INTERSECTION OF UNION (AREA OF OVERLAP/AREA OF UNION)
#   IoU <-  torch_tensor(Summary_FSc_IoU_oneRestoneSubj$Portion_IoU_CorrecIn) # (i.e. TP/Total_Vox)
#   GT_XYZ <- torch_unsqueeze(GT_XYZWLHR[c(4,5,8)], dim=1L)
#   Pred_XYZ <- torch_unsqueeze(Pred_XYZWLHR[c(4,5,8)], dim=1L)
#   
#   # DISTANCE CALCULATIONS
#   Pred_GT_Vert <- rbind(GT_Vert_Gnd, Pred_Vert_Gnd)
#   Pred_GT_BOT_LEFT <- torch_unsqueeze(torch_tensor(apply(Pred_GT_Vert, 2, FUN=min)), dim=1L)    # c(min(Pred_GT_Vert[,1]), min(Pred_GT_Vert[,2]), min(Pred_GT_Vert[,3])) 
#   Pred_GT_TOP_RIGHT <-  torch_unsqueeze(torch_tensor(apply(Pred_GT_Vert, 2, FUN=max)), dim=1L)  # c(max(Pred_GT_Vert[,1]), max(Pred_GT_Vert[,2]), max(Pred_GT_Vert[,3])) 
#   Dist_cCube <- torch_cdist(Pred_GT_BOT_LEFT , Pred_GT_TOP_RIGHT)
#   Dist_XY_BBox <- torch_cdist(GT_XYZ , Pred_XYZ) # , p = 2L, compute_mode = NULL)    Pred_XYZWLHRR[c(4,5,8)] # THIS WILL HAVE TO BE NORMALISED LATER.
#   D <- torch_square(Dist_XY_BBox)/torch_square(Dist_cCube) # THIS NEEDS TO BE NORMALISED STILL
#   
#   # ASPECT RATIO CALCULATION
#   V <- torch_tensor(4/pi^2)*torch_square(torch_arctan(GT_XYZWLHR[6]/GT_XYZWLHR[7]) - torch_arctan(Pred_XYZWLHR[6]/Pred_XYZWLHR[7]))
# 
#   alpha_V <- torch_where(IoU < 0.5, torch_tensor(0), V/(1-IoU+V))
#     
#   return(list(IoU, D, V, alpha_V) )
# }
# 
# ##################################################################################################
# 
# # FUNCTIONS FOR BOUNDING BOX CALCULATIONS
# rad2deg <- function(rad) {(rad * 180) / (pi)}
# deg2rad <- function(deg) {(deg * pi) / (180)}
# 
# XYZWLHR_FUN  <- function(onePSID_BBox, Prefix, Height_Strata, Sample_Att){
#   
#   # REMOVE THE LAST POINT WHICH IS A REPLICA OF FIRST
#   onePSID_BBox <- onePSID_BBox[1:4,]
#   
#   # ORDER POINTS SO MOST X VALUE IS FIRST
#   Ord_First <- which.min(onePSID_BBox$X)
#   if(Ord_First != 1){
#     onePSID_BBox <- onePSID_BBox[c(Ord_First:nrow(onePSID_BBox), 1:(Ord_First-1)),]
#   }
#   onePSID_BBox$Order <- 1:nrow(onePSID_BBox)
#   
#   Index_Col_XY <- which(colnames(onePSID_BBox) %in% c("X", "Y"))
#   
#   Lengths <- raster::pointDistance(as.matrix(onePSID_BBox[1:4,Index_Col_XY]), lonlat =FALSE)
#   Length_both <- c(Lengths[1,2], Lengths[2,3])
#   
#   Length_Y <- Length_both[which.max(Length_both)]
#   Width_X <- Length_both[which.min(Length_both)]
#   
#   #=c(0, Length_Y*2), ylim=c(0, Length_Y*2)
#   
#   onePSID_BBox_N <- onePSID_BBox
#   X_Scale <- min(onePSID_BBox_N$X)
#   Y_Scale <- min(onePSID_BBox_N$Y)
#   onePSID_BBox_N$X <- onePSID_BBox_N$X - X_Scale
#   onePSID_BBox_N$Y <- onePSID_BBox_N$Y - Y_Scale
# 
#   Bearing_both <- bearing(onePSID_BBox_N[,Index_Col_XY]) # Box_R <- Box_R[1:2]
#   
#   Index_Longest <- which.max(Length_both)
#   if(Index_Longest == 1){
#     Rotate_Deg <- Bearing_both[1]
#   }else{
#     Rotate_Deg <- Bearing_both[2] 
#   }
#   if(Rotate_Deg < 0) {Rotate_Deg <- Rotate_Deg + 180}
#   
#   Centre_XY <-  c(sum(onePSID_BBox$X[1:4])/4, sum(onePSID_BBox$Y[1:4])/4)
#   Cent_X <- Centre_XY[1]
#   Cent_Y <- Centre_XY[2]
# 
#   # Rotate_Deg <- rad2deg(atan((onePSID_BBox$X[1] - onePSID_BBox$X[2]) / (onePSID_BBox$Y[1] - onePSID_BBox$Y[2])))
#   #
#   # onePSID_BBox_N <- onePSID_BBox
#   # onePSID_BBox_N$X <- onePSID_BBox_N$X - min(onePSID_BBox_N$X)
#   # onePSID_BBox_N$Y <- onePSID_BBox_N$Y - min(onePSID_BBox_N$Y)
#   # 
#   # Box_R_1 <- bearing(onePSID_BBox_N[1,4:5], onePSID_BBox_N[2,4:5])
#   # Box_R_2 <- bearing(onePSID_BBox_N[2,4:5], onePSID_BBox_N[3,4:5])
#   # Box_R_2 <- bearing(onePSID_BBox_N[3,4:5], onePSID_BBox_N[4,4:5]) 
#   #                  
#   # Width_X <- raster::pointDistance(as.matrix(oneLevel_Vertices[2,2:3]), oneLevel_Vertices[3,2:3], lonlat =FALSE)
#   # Box_R <- bearing(oneLevel_Vertices[1,2:3], oneLevel_Vertices[2,2:3])
#   # 
#   # if(Rotate_Deg < 0){
#   #   Rotate_Deg <- Rotate_Deg+180
#   # }
#   # Centre_XY <-  c(sum(onePSID_BBox$X[1:4])/4, sum(onePSID_BBox$Y[1:4])/4)
#   # coords.Rot <- Rotation(data.frame(X = c(onePSID_BBox$X-Centre_XY[1]) , Y = c(onePSID_BBox$Y-Centre_XY[2])), Rotate_Deg*pi/180)
#   # coords.Rot[,1] <- coords.Rot[,1]+Centre_XY[1]
#   # coords.Rot[,2] <- coords.Rot[,2]+Centre_XY[2]
#   # 
#   # Width_X <- max(coords.Rot[,1])-min(coords.Rot[,1])
#   # Length_Y <- max(coords.Rot[,2])-min(coords.Rot[,2])
#   # 
#   # Cent_X <- (max(coords.Rot[,1]) +min(coords.Rot[,1]))/2
#   # Cent_Y <- (max(coords.Rot[,2])+min(coords.Rot[,2]))/2
#   
#   Height <- onePSID_BBox$Z[1]
#   TID  <- onePSID_BBox$TID [1]  
#   TreeHeight <- max(onePSID_BBox$Z)
#   
#   Height_Strata <- Height_Strata
#   if(Sample_Att == "Yes"){
#     Sample  <- onePSID_BBox$Sample  [1]
#     BBox <- data.frame(TID = TID, Sample = Sample, Height_Strata = Height_Strata, 
#                        X_Box= Cent_X, Y_Box = Cent_Y, 
#                        L_Box= Length_Y, W_Box =Width_X, Z_Box =Height,
#                        R_Box = Rotate_Deg, 
#                        Z_Tree = TreeHeight)
#     
#   }else{
#     BBox <- data.frame(TID = TID, Height_Strata = Height_Strata, 
#                        X_Box = Cent_X, Y_Box = Cent_Y, 
#                        L_Box = Length_Y, W_Box = Width_X, Z_Box = Height,
#                        R_Box = Rotate_Deg, 
#                        Z_Tree = TreeHeight)
#   }
#   
#   
#   # "X_Base",  Cent_X
#   # "Y_Base", Cent_Y
#   # "Z_Base", 
#   #  "X_BotBox", 
#   # "Y_BotBox", 
#   # "L_BotBox", 
#   # "W_BotBox", 
#   # "Z_BotBox", 
#   # "Rot_BotBox",
#   #  "X_TopBox", 
#   # "Y_TopBox", 
#   # "L_TopBox", 
#   # "W_TopBox", 
#   # "Z_TopBox", 
#   # "Rot_TopBox", 
#   # "Z_Tree"
#   
#   # browser()
#   # REMOVE BOTTOM TREE HEIGHT AS NOT NECESSARY
#   if(Prefix == "Bot"){
#     BBox <- BBox[,-which(colnames(BBox) == "Z_Tree")]
#   }
#   
#   if(Sample_Att == "Yes"){
#     colnames(BBox)[4:ncol(BBox)] <- paste(substr(colnames(BBox)[4:ncol(BBox)],1,2), Prefix, 
#                                           substr(colnames(BBox)[4:ncol(BBox)],3, nchar(colnames(BBox)[4:ncol(BBox)])), sep="")
#   }else{
#     colnames(BBox)[3:ncol(BBox)] <- paste(substr(colnames(BBox)[3:ncol(BBox)],1,2), Prefix, 
#                                           substr(colnames(BBox)[3:ncol(BBox)],3, nchar(colnames(BBox)[3:ncol(BBox)])), sep="")  
#   }
#  
#   # # PLOT THE VISUALISATION OF THE RESULTS
#   # plot(onePSID_BBox_N$X, onePSID_BBox_N$Y, xlim=c(0, Length_Y*2), ylim=c(0, Length_Y*2))
#   # text(onePSID_BBox_N$X, onePSID_BBox_N$Y,  onePSID_BBox$Order, pos =2)
#   # par(new=TRUE)
#   # plot(onePSID_BBox_N$X[1:2], onePSID_BBox_N$Y[1:2], col = "red", 
#   #      xlim=c(0, Length_Y*2), ylim=c(0, Length_Y*2))
#   # Cent_X_Plot <- Cent_X - X_Scale
#   # Cent_Y_Plot <- Cent_Y - Y_Scale 
#   # par(new=TRUE)
#   # plot(Cent_X_Plot, Cent_Y_Plot, xlim=c(0, Length_Y*2), ylim=c(0, Length_Y*2), col="green" ) 
#   # text(Width_X,Length_Y, paste("L:", round(BBox[, c(6)],1), "  W:",round(BBox[, c(7)],1), "  R:",round(BBox[, c(9)],1)))
#   # browser() 
#   return(BBox) 
# }
# 
# 
# 
# #############################################################################################################################
# # CONVERT VERTICES INTO XYZWLHR FORMAT
# 
# 
# VERT_To_XYZWLHR_FUN  <- function(Vertices, Para_Cnt = 10, Col_Name= Colnames_XYZWLHR, Normalise = "Yes"){
#   #browser()
#   # if(Para_Cnt == 16){
#   #   Level <-  rep(c(1,2,3,4), times = 1, length.out = nrow(Vertices), each = 4)
#   #   Prefix = c("", "Bot_", "Top_", "Top_")
#   # }else{
#     Level <-  rep(c(1,2,3), times = 1, length.out = nrow(Vertices), each = 4)
#     Prefix = c("", "Top_", "Top_")
#   #}
# 
#   Vertices <- data.frame(Level = Level, Vertices)
#   colnames(Vertices) <- c("Level","X", "Y", "Z")
#   
#   for(L in 1:max(unique(Level))){
#     oneLevel_Vertices <- Vertices[which(Vertices$Level == L),]
#     if(L == 1){
#       Centre_X <- mean(oneLevel_Vertices$X)
#       Centre_Y <- mean(oneLevel_Vertices$Y)
#       Centre_Z <- oneLevel_Vertices$Z[1]
#       BBox <- data.frame(X= Centre_X, Y =Centre_Y, Z =Centre_Z)
#       All_BBox <- BBox
#       } # LOOP L == 1
# 
#     # EUCLIDEAN DISTANCE CALCULATION...
#     if(L >= 2){
#       
#       if(L == max(unique(Level))){
#         #browser()
#         Tree_Top <- oneLevel_Vertices$Z[1]
#         All_BBox <- data.frame(All_BBox, Tree_Top)  ### DOM DOM DOM !!! IS THERE SOMETHING WRONG WITH THIS....Tree_Top POSITION
#                                                     ### THIS IS IN THE WRONG POSITION!!!!
#       }else{
#         
#         # Length_Y <- raster::pointDistance(as.matrix(oneLevel_Vertices[1,2:3]), oneLevel_Vertices[2,2:3], lonlat =FALSE)
#         # Width_X <- raster::pointDistance(as.matrix(oneLevel_Vertices[2,2:3]), oneLevel_Vertices[3,2:3], lonlat =FALSE)
#         # Box_R <- bearing(oneLevel_Vertices[1,2:3], oneLevel_Vertices[2,2:3])
# 
#         ###########################################################################################
#         ###########################################################################################
#         ###########################################################################################
# 
#         # REMOVE THE LAST POINT WHICH IS A REPLICA OF FIRST
#         onePSID_BBox <- oneLevel_Vertices[,2:4]
# 
#         # ORDER POINTS SO MOST X VALUE IS FIRST
#         Ord_First <- which.min(oneLevel_Vertices$X)
#         if(Ord_First != 1){
#           oneLevel_Vertices <- oneLevel_Vertices[c(Ord_First:nrow(oneLevel_Vertices), 1:(Ord_First-1)),]
#         }
#         oneLevel_Vertices$Order <- 1:nrow(oneLevel_Vertices)
#         # browser()
# 
#         Lengths <- raster::pointDistance(as.matrix(oneLevel_Vertices[1:4,2:3]), lonlat =FALSE)
#         Length_both <- c(Lengths[1,2], Lengths[2,3])
# 
#         Length_Y <- Length_both[which.max(Length_both)]
#         Width_X <- Length_both[which.min(Length_both)]
# 
#         #=c(0, Length_Y*2), ylim=c(0, Length_Y*2)
# 
#         oneLevel_Vertices_N <- oneLevel_Vertices
#         X_Scale <- min(oneLevel_Vertices_N$X)
#         Y_Scale <- min(oneLevel_Vertices_N$Y)
#         oneLevel_Vertices_N$X <- oneLevel_Vertices_N$X - X_Scale
#         oneLevel_Vertices_N$Y <- oneLevel_Vertices_N$Y - Y_Scale
#         Bearing_both <- bearing(oneLevel_Vertices_N[,2:3]) # Box_R <- Box_R[1:2]
# 
#         Index_Longest <- which.max(Length_both)
#         if(Index_Longest == 1){
#           Box_R <- 180 - Bearing_both[1]
#         }else{
#           Box_R <- 180 - Bearing_both[2]
#         }
#         if(Box_R < 0) {Box_R <- Box_R + 180}
# 
#         if(Normalise == "Yes"){
#           Box_R <-Box_R/180
#         }
#         #browser()
#         BoxCent_X <- mean(oneLevel_Vertices$X)
#         BoxCent_Y <- mean(oneLevel_Vertices$Y)
#         Box_Z <- oneLevel_Vertices$Z[1]
#         #browser()
#         All_BBox<- data.frame(All_BBox, BoxCent_X, BoxCent_Y, Length_Y, Width_X, Box_Z, Box_R)
#         }
#       } # LOOP L > 2
#   } # L LOOP (THROUGH LEVELS)
#   colnames(All_BBox) <- Colnames_XYZWLHR
#   return(All_BBox)
# }
# 
# VERT_To_XYZWLHR_GPU_FUN_OLD_Switching_LW  <- function(Vertices, Para_Cnt = 10, Col_Name= Colnames_XYZWLHR, Normalise = "Yes", device = device){ #torch_ones(1, dtype = torch_bool(), device=device)
#   #browser()
#   # if(Para_Cnt == 16){
#   #   Levl_Vec <- c(1, 2, 3, 4)#torch_tensor(c(1, 2, 3, 4), device=device, dtype = torch_long())
#   # }else{
#     Levl_Vec <- c(1, 2, 3)#torch_tensor(c(1, 2, 3), device=device, dtype = torch_long())
#   #}
# 
#   Level = torch_repeat_interleave(torch_tensor(Levl_Vec, device=device, dtype = torch_long()), torch_tensor(4L, device=device))
#   #browser()
#   #Vertices <- data.frame(Level = Level, Vertices)
#   #torch_unique_consecutive(Level)
#   
#   #for(L in Levl_Vec){ # 1:max(Levl_Vec)
#   for(L in 1:max(Levl_Vec)){ # dim(Levl_Vec)[1]
#     # browser()
#     #oneLevel_Vertices <- Vertices[which(Vertices$Level == L),]
#     Index = torch_where( Level == L, 1, 0)$to(dtype = torch_bool()) #Levl_Vec[L]
#     
#     oneLevel_Vertices <- Vertices[Index,]
#     # browser()
#     if(L == 1){ # torch_equal(Levl_Vec[L], torch_tensor(1L, device=device)$squeeze())
#       Centre_X <- torch_mean(oneLevel_Vertices[,1]) #mean(oneLevel_Vertices$X) 
#       Centre_Y <- torch_mean(oneLevel_Vertices[,2]) # mean(oneLevel_Vertices$Y) 
#       Centre_Z <- oneLevel_Vertices[1,3] # oneLevel_Vertices$Z[1]
#       #browser()
#       BBox <- torch_stack(c(Centre_X, Centre_Y, Centre_Z)) # data.frame(X= Centre_X, Y =Centre_Y, Z =Centre_Z)
#       All_BBox <- torch_clone(BBox)
#     } # LOOP L == 1 
#     # browser()
#     # EUCLIDEAN DISTANCE CALCULATION...
#     if(L >= 2){  
#       if(L == max(Levl_Vec)){   # torch_equal(Levl_Vec[L], torch_tensor(dim(Levl_Vec)[1], device=device)$squeeze())) 
#         Tree_Top <- oneLevel_Vertices[1,3] # oneLevel_Vertices$Z[1]
#         All_BBox <- torch_cat(c(All_BBox, torch_unsqueeze(Tree_Top,1))) # data.frame(All_BBox, Tree_Top)
# 
#       }else{
# 
#         
#         # Length_Y <- raster::pointDistance(as.matrix(oneLevel_Vertices[1,2:3]), oneLevel_Vertices[2,2:3], lonlat =FALSE)
#         # Width_X <- raster::pointDistance(as.matrix(oneLevel_Vertices[2,2:3]), oneLevel_Vertices[3,2:3], lonlat =FALSE)
#         # Box_R <- bearing(oneLevel_Vertices[1,2:3], oneLevel_Vertices[2,2:3])
#         
#         ###########################################################################################
#         ###########################################################################################
#         ###########################################################################################
# 
#         # # REMOVE THE LAST POINT WHICH IS A REPLICA OF FIRST
#         # onePSID_BBox <- oneLevel_Vertices[,2:4]
#         
#         # ORDER POINTS SO MOST X VALUE IS FIRST
#         Ord_First <- torch_argmin(oneLevel_Vertices[,1])
#         if(!torch_equal(Ord_First, torch_tensor(1L, device=device)$squeeze())){
# 
#           Start <- torch_arange( Ord_First, dim(oneLevel_Vertices)[1], step = 1, device=device, dtype=torch_int())
#           End <- torch_arange(1, Ord_First, 1, device=device, dtype=torch_int())
#           Index = torch_repeat_interleave(torch_cat(c(Start, End))$view(c(4,1)), 
#                                           torch_tensor(dim(oneLevel_Vertices)[2], device=device), dim = 2)
#           oneLevel_Vertices <- oneLevel_Vertices$gather(1, Index)
#         }
#         
#         Lengths <- torch_cdist(oneLevel_Vertices[1:4,1:2],oneLevel_Vertices[1:4,1:2])
#         Length_both <- torch_stack(c(Lengths[1,2], Lengths[2,3]))
# 
#         
#         #### DOM DOM DOM !!! YOU HAVE TO THINK ABOUT THIS PROCEDURE AND MAKE SURE YOU ARE NOT MUDDLING LENGTHS AND WIDTHS
#         Length_Y <- Length_both$max() # Length_both[torch_argmax(Length_both)]  
#         Width_X <- Length_both$min() # Length_both[torch_argmin(Length_both)]
# 
#         oneLevel_Vertices_N <- torch_clone(oneLevel_Vertices)
#         X_Scale <- torch_min(oneLevel_Vertices_N[,1])
#         Y_Scale <- torch_min(oneLevel_Vertices_N[,2])
#         oneLevel_Vertices_N[,1] <- torch_sub(oneLevel_Vertices_N[,1], X_Scale)
#         oneLevel_Vertices_N[,2] <- torch_sub(oneLevel_Vertices_N[,2], Y_Scale)
#         
#         # # I COMPUTE THE BEARING USING AN ARRAY BUT GPU WILL CRASH AS IT WILL NOT HAVE FUNCTION BEARING...
#         # Bearing_both <- bearing(as.array(oneLevel_Vertices_N)[,2:3])[1:2] # Box_R <- Box_R[1:2]
#         # Bearing_both <- round(Bearing_both)
#         
#         # Test <- as.array(oneLevel_Vertices_N)[,2:3]
#         #Kansas City: 39.099912, -94.581213  https://www.igismap.com/formula-to-find-bearing-or-heading-angle-between-two-points-latitude-longitude/
#         #St Louis: 38.627089, -90.200203
#         # Test[1,1] <- 39.099912
#         # Test[1,2] <- -94.581213
#         # Test[2,1] <- 38.627089
#         # Test[2,2] <- -90.200203
#         # Test <- Test[1:2,1:2]
# 
#         # 
#         dL = oneLevel_Vertices_N[2:3,1] - oneLevel_Vertices_N[1:2,1]
#         X = torch_cos(oneLevel_Vertices_N[2:3,2]*pi/180)*torch_sin(dL*pi/180) # =COS(39.099912*pi/180)
#         Y = (torch_cos(oneLevel_Vertices_N[1:2,2]*pi/180)*torch_sin(oneLevel_Vertices_N[2:3,2]*pi/180)) - 
#           (torch_sin(oneLevel_Vertices_N[1:2,2]*pi/180)*torch_cos(oneLevel_Vertices_N[2:3,2]*pi/180)*torch_cos(dL*pi/180))
# 
#         bearing_rad <- torch_atan2(X,Y)
#         bearing_deg <- (bearing_rad *180)/pi
#         bearing_deg <- torch_round(bearing_deg)   # DOM DOM DOM !!! SEE torch_angle
# 
#         # browser()
#         
#         # torch_angle(torch_tensor(c(-1 + 1i, -2 + 2i, 3 - 3i)))*180/3.14159
#         
#         Index_Longest <- torch_unsqueeze(torch_argmax(Length_both),1)
#   
#         Box_R = torch_index_select(bearing_deg, dim=1, Index_Longest)
#         Box_R <- 180 - Box_R
#         Box_R = torch_where( Box_R < 0, Box_R + 180,  Box_R )
#         
#         #Box_R = torch_where( Box_R < 0, Box_R + 180,  Box_R )
#         
#         # Box_R = torch_where(torch_squeeze(Normalise == torch_ones(1, dtype = torch_bool(), device=device)) , Box_R/180,  Box_R )
#         if(Normalise == "Yes"){ Box_R <- Box_R/180 }
#         #browser()
#         BoxCent_X <- torch_mean(oneLevel_Vertices[,1])
#         BoxCent_Y <- torch_mean(oneLevel_Vertices[,2])
#         Box_Z <- oneLevel_Vertices[1,3]
# 
#         All_BBox<- torch_cat(c(All_BBox, torch_unsqueeze(BoxCent_X,1), 
#                                  torch_unsqueeze(BoxCent_Y,1), 
#                                  torch_unsqueeze(Length_Y,1), 
#                                  torch_unsqueeze(Width_X,1), 
#                                  torch_unsqueeze(Box_Z,1), 
#                                  Box_R))
#       }
#     } # LOOP L > 2
#   } # L LOOP (THROUGH LEVELS)
#   return(All_BBox) 
# }
# 
# 
# # browser()
# # W <- raster::pointDistance(as.matrix(oneLevel_Vertices[1,2:3]), oneLevel_Vertices[2,2:3], lonlat =FALSE)
# # L <- raster::pointDistance(as.matrix(oneLevel_Vertices[2,2:3]), oneLevel_Vertices[3,2:3], lonlat =FALSE)
# # X_Diff <- range(oneLevel_Vertices$X)[2] - range(oneLevel_Vertices$X)[1]
# # Y_Diff<- range(oneLevel_Vertices$Y)[2] - range(oneLevel_Vertices$Y)[1]
# # #browser()
# # if(X_Diff > Y_Diff){
# #   Width_X  <- max(W, L) 
# #   Length_Y <- min(W, L)
# #   Box_R <- bearing(oneLevel_Vertices[1,2:3], oneLevel_Vertices[2,2:3])
# # }else{
# #   Width_X  <- min(W, L)
# #   Length_Y <- max(W, L)
# #   Box_R <- bearing(oneLevel_Vertices[2,2:3], oneLevel_Vertices[3,2:3])
# #   #
# # }
# 
# 
# # ls1 <- st_linestring(as.matrix(oneLevel_Vertices[1:2,2:3]))
# # ls2 <- st_linestring(as.matrix(oneLevel_Vertices[2:3,2:3]))
# # line_bearing(ls2, bidirectional = FALSE)
# # 
# # bearing(oneLevel_Vertices[2,2:3], oneLevel_Vertices[3,2:3])
# # browser()
# 
# 
# # lines3d(x=c(oneLevel_Vertices$X[1], oneLevel_Vertices$X[2]), 
# #         y=c(oneLevel_Vertices$Y[1],oneLevel_Vertices$Y[2]), 
# #         z=c(oneLevel_Vertices$Z[1],oneLevel_Vertices$Z[2]), col="white", lwd = 6, lty=3)
# # 
# # lines3d(x=c(oneLevel_Vertices$X[2], oneLevel_Vertices$X[3]), 
# #         y=c(oneLevel_Vertices$Y[2],oneLevel_Vertices$Y[3]), 
# #         z=c(oneLevel_Vertices$Z[2],oneLevel_Vertices$Z[3]), col="yellow", lwd = 6, lty=3)
# # browser()
# # if(X_Diff > Y_Diff){
# #   Box_R <- 180 - rad2deg(tan(abs(oneLevel_Vertices$Y[1] - oneLevel_Vertices$Y[2])/abs(oneLevel_Vertices$X[1] - oneLevel_Vertices$X[2])))
# # }else{
# #   Box_R <- rad2deg(tan(abs(oneLevel_Vertices$X[1] - oneLevel_Vertices$X[2])/abs(oneLevel_Vertices$Y[1] - oneLevel_Vertices$Y[2])))
# # }
# 
# 
# #####################################################################################
# #####################################################################################
# 
# # if(Para_TriShpParaCnt == 16){
# #   Colnames_XYZWLHR <- c("X_Base", "Y_Base", "Z_Base", 
# #                         "X_BotBox", "Y_BotBox", "L_BotBox", "W_BotBox", "Z_BotBox", "R_BotBox", 
# #                         "X_TopBox", "Y_TopBox", "L_TopBox", "W_TopBox", "Z_TopBox", "R_TopBox", 
# #                         "Z_TopTree")
# # }else{
# #   Colnames_XYZWLHR <- c("X_Base", "Y_Base", "Z_Base",  
# #                         "X_TopBox", "Y_TopBox", "L_TopBox", "W_TopBox", "Z_TopBox", "R_TopBox", 
# #                         "Z_TopTree")
# 
# 
# # coords.Rot <- Rotation(data.frame(X = c(oneLevel_Vertices$X-Centre_XY[1]) , Y = c(oneLevel_Vertices$Y-Centre_XY[2])), Rotate_Deg*pi/180)
# # coords.Rot[,1] <- coords.Rot[,1]+Centre_XY[1]
# # coords.Rot[,2] <- coords.Rot[,2]+Centre_XY[2]
# # 
# # Width_X <- max(coords.Rot[,1])-min(coords.Rot[,1])
# # Length_Y <- max(coords.Rot[,2])-min(coords.Rot[,2])
# # 
# # Cent_X <- (max(coords.Rot[,1]) +min(coords.Rot[,1]))/2
# # Cent_Y <- (max(coords.Rot[,2])+min(coords.Rot[,2]))/2
# # 
# # Height <- oneLevel_Vertices$Z[1]
# #PSID  <- oneLevel_Vertices$PSID [1]  
# #TreeHeight <- max(oneLevel_Vertices$Z)
# 
# # if(L == 1){
# #   BBox <- data.frame(X= Cent_X, Y =Cent_Y, Z =Height)
# #   All_BBox <- BBox
# # }
# # 
# # if(L == 2 ){
# #   BBox <- data.frame(Cent_X= Cent_X, Cent_Y =Cent_Y, 
# #                      Length_Y= Length_Y, Width_X =Width_X, Height =Height,
# #                      Rotate_Deg =Rotate_Deg)
# #   colnames(BBox) <- paste(Prefix[L], colnames(BBox), sep="")
# #   All_BBox <- cbind(All_BBox, BBox)
# # }
# # 
# # if(Para_Cnt == 16){
# #   if(L == 3){
# #     BBox <- data.frame(Cent_X= Cent_X, Cent_Y =Cent_Y, 
# #                        Length_Y= Length_Y, Width_X =Width_X, Height =Height,
# #                        Rotate_Deg =Rotate_Deg)
# #     colnames(BBox) <- paste(Prefix[L], colnames(BBox), sep="")
# #     All_BBox <- cbind(All_BBox, BBox)
# #   }
# #   if(L == 4){
# #     BBox <- data.frame( TreeHeight = TreeHeight)
# #     colnames(BBox) <- paste(Prefix[L], colnames(BBox), sep="")
# #     All_BBox <- cbind(All_BBox, BBox)
# #   }
# # }else{
# #   if(L == 3){
# #     BBox <- data.frame( TreeHeight = TreeHeight)
# #     colnames(BBox) <- paste(Prefix[L], colnames(BBox), sep="")
# #     All_BBox <- cbind(All_BBox, BBox)
# #   }
# # }
# 
# 
# 
# BBox_Sqr_FUN <- function(Cent_X, Cent_Y, Cent_Z, Sqr_Offset = Para_Base_WL/2){
#   X1 <- Cent_X -Sqr_Offset 
#   X2 <- Cent_X +Sqr_Offset 
#   Y1 <- Cent_Y -Sqr_Offset 
#   Y2 <- Cent_Y +Sqr_Offset 
#   
#   X3 <- Cent_X + Sqr_Offset 
#   X4 <- Cent_X - Sqr_Offset 
#   Y3 <- Cent_Y + Sqr_Offset 
#   Y4 <- Cent_Y - Sqr_Offset 
#   
#   XYZ <- data.frame(X = c(X1,  X3, X2, X4, X1),
#                     Y = c(Y1,  Y3, Y2, Y4, Y1),
#                     Z = rep(Cent_Z, 5))
#   return(XYZ)
# }
# 
# 
# 
# 
# 
# 
# # FUNCTIONS FOR TRIANGULATED SHAPES
# INTERSECT_TRI_FUN <-function (triangles, coord, points) 
#   {
#     points = as.matrix(points, nc = 3)
#     inside <- integer(dim(points)[1])
#     teVFdir = as.numeric(2 * runif(3) - 1)
#     #browser()
#     retour <- .C("pointinashape", as.integer(triangles), dim(triangles)[1], 
#                  as.numeric(coord), dim(coord)[1], as.numeric(points), 
#                  dim(points)[1], as.integer(inside), tempdir)
#     return(retour[[7]])
#   }
# 
# # index_Order_Function <- function(dist_Index){
# #   if(dist_Index == -3){Order_Vert <- c(2,3,4,1)}
# #   if(dist_Index == -2){Order_Vert <- c(3,4,1,2)}
# #   if(dist_Index == -1){Order_Vert <- c(4,1,2,3)}
# #   if(dist_Index == 0){Order_Vert <- c(1,2,3,4)}
# #   if(dist_Index == 1){Order_Vert <- c(2,3,4,1)}
# #   if(dist_Index == 2){Order_Vert <- c(3,4,1,2)}
# #   if(dist_Index == 3){Order_Vert <- c(4,1,2,3)}
# #   return(Order_Vert)
# # }
# 
# # TRI_ID_NODES_FUN <- function (Para_Faces_Used = 4){
# #   Triangles_ID <- matrix(c(c(1,2,3),
# #                            c(1,3,4),
# #                            c(1,2,5), 
# #                            c(5,6,2), 
# #                            c(2,3,6),
# #                            c(3,7,6),
# #                            c(3,4,7),
# #                            c(4,7,8),
# #                            c(1,4,8),
# #                            c(1,8,5),
# #                            c(5,6,7),
# #                            c(5,7,8)), ncol=3, byrow= TRUE)
# #   
# #   # BOTH BOUNDING BOXES AND BASE LOCATION (MATRIX FORM)
# #   Triangles_ID_All <- c(as.vector(t(Triangles_ID[1:10,])), 
# #                         as.vector(t((Triangles_ID+Para_Faces_Used)[3:10,])), 
# #                         as.vector(t((Triangles_ID+(Para_Faces_Used*2))[3:12,])))
# #   return(Triangles_ID_All)
# # }
# 
# ########################################################################################################################################## 
# 
# BBOX_PNTS_FUN <- function(p) {
#   # Analyze the convex hull edges 
#   # browser()
#   a <- chull(p)                                   # Indexes of extremal points
#   a <- c(a, a[1])                                 # Close the loop
#   e <- p[a[-1],] - p[a[-length(a)], ]             # Edge directions
#   norms <- sqrt(rowSums(e^2))                     # Edge lengths
#   v <- e / norms                                  # Unit edge directions
#   w <- cbind(-v[,2], v[,1])                       # Normal directions to the edges
#   # browser()
#   # Find the MBR
#   vertices <- p[a, ]                              # Convex hull vertices
#   x <- apply(vertices %*% t(v), 2, range)         # Extremes along edges
#   y <- apply(vertices %*% t(w), 2, range)         # Extremes normal to edges
#   areas <- (y[1,]-y[2,])*(x[1,]-x[2,])            # Areas
#   k <- which.min(areas)                           # Index of the best edge (smallest area)
#   # Form a rectangle from the extremes of the best edge
#   cbind(x[c(1,2,2,1,1),k], y[c(1,1,2,2,1),k]) %*% rbind(v[k,], w[k,])
# }
# 
# ########################################################################################################################################## 
# ########################################################################################################################################## 
# # MAKING SURE THE TRI-SHAPE IS ACCURATELY REPRESENTED
# 
# VERTICIES_ORDER_FUN <- function(Vertices_Mx, Box_Levels = 4){
#   
#   # MAKE SURE BOX HAS VERTICES GOING IN CORRECT ORDER
#   all_Box <- Vertices_Mx[1:4,]
#   dist_oneBox <- rdist(all_Box[,1:2], all_Box[,1:2]) # rdist(row, col)
#   Order_dist_oneBox_Largest <- apply(dist_oneBox, 1, which.max)
#   if(!all(Order_dist_oneBox_Largest == c(3,4,1,2))){browser()} # DOM DOM DOM YOUR FIRST BOX IS NOT IN CORRECT ORDER
#  
#   CentX <- mean(all_Box[,1])
#   CentY <- mean(all_Box[,2]) 
#   Previous_Box <- all_Box
#   
#   for(BL in 2:Box_Levels){
#     Index <- seq((((BL-1)*4)+1), ((BL-1)*4) + 4, 1)
#     
#     one_Box <- Vertices_Mx[Index,]
#     one_Box <- cbind(BBOX_PNTS_FUN(Vertices_Mx[Index,1:2])[1:4,],Vertices_Mx[Index,3])
#     dist_oneBox <- rdist(one_Box[,1:2], one_Box[,1:2]) # rdist(row, col)
#     Order_dist_oneBox_Largest <- apply(dist_oneBox, 1, which.max)
#     if(!all(Order_dist_oneBox_Largest == c(3,4,1,2))){browser()} # DOM DOM DOM... BOX IS NOT IN CORRECT ORDER
# 
#     # BETWEEN BOXES ORDER  
#     Dif_CentX <- mean(one_Box[,1]) - CentX 
#     Dif_CentY <- mean(one_Box[,2]) - CentY
#     subBox_Adj <- one_Box
# 
#     subBox_Adj[,1] <- subBox_Adj[,1] - Dif_CentX
#     subBox_Adj[,2] <- subBox_Adj[,2] - Dif_CentY
# 
#     dist <- rdist(Previous_Box[,1:2], subBox_Adj[,1:2]) # rdist(row, col)
#     Order_dist_Smallest <- apply(dist, 1, which.min)
#     dist_Smallest <- apply(dist, 1, min)
#     
#     one_Box <- one_Box[Order_dist_Smallest,]
#     
#     all_Box <- rbind(all_Box,one_Box)}
#     Previous_Box <- one_Box
#     CentX <- mean(Previous_Box[,1])
#     CentY <- mean(Previous_Box[,2]) 
#     
#     return(all_Box)  
#     # browser() 
#   }
# 
# # GET PRIOR CENTRE AND PLOT OFFSET 
# PLOT_OFFSET_FUN <- function(TAR_OFF = oneP_onePR_TARGET_XYZWLHR_ExtR_DF, 
#                             PRED_OFF = oneP_onePR_Pred_XYZWLHR_ExtR_DF,
#                             PRIOR = oneP_onePR_PRIOR_XYZWLHR_ExtR_DF){
#   
#   # TARGET OFFSET FROM PRIOR
#   End_X <- TAR_OFF$X_TopBox
#   End_Y <- TAR_OFF$Y_TopBox
#   End_Z <-  TAR_OFF$Z_TopBox
#   lines3d(x=c(PRIOR$X_TopBox, End_X), y=c(PRIOR$Y_TopBox,End_Y), z=c(PRIOR$Z_TopBox,End_Z), col="green", lwd = 5)
# 
#   L_PRED <- PRIOR$L_TopBox - TAR_OFF$L_TopBox
#   W_PRED <- PRIOR$W_TopBox - TAR_OFF$W_TopBox
#   
#   lines3d(x=c(0, L_PRED), y=c(1.1, 1.1), z=c(PRIOR$Z_TopBox, PRIOR$Z_TopBox), col="lightblue", lwd = 5)
#   text3d(x=c(L_PRED), y=c(1.1+0.05), z=c(PRIOR$Z_TopBox), "L", col="white")
#   lines3d(x=c(0, W_PRED), y=c(1.3, 1.3), z=c(PRIOR$Z_TopBox,PRIOR$Z_TopBox), col="pink", lwd = 5)
#   text3d(x=c(W_PRED), y=c(1.3+0.05), z=c(PRIOR$Z_TopBox), "W", col="white")
#   text3d(x=0, y=c(1.2), z=c(PRIOR$Z_TopBox), "TargetOffset", col="white")
#   
#   # PRED OFFSET FROM PRIOR
#   End_X <- PRED_OFF$X_TopBox
#   End_Y <- PRED_OFF$Y_TopBox
#   End_Z <- PRED_OFF$Z_TopBox
#   lines3d(x=c(PRIOR$X_TopBox, End_X), y=c(PRIOR$Y_TopBox,End_Y), z=c(PRIOR$Z_TopBox,End_Z), col="white", lwd = 5)
#   
#   L_PRED <- PRIOR$L_TopBox - PRED_OFF$L_TopBox
#   W_PRED <- PRIOR$W_TopBox - PRED_OFF$W_TopBox
#   
#   lines3d(x=c(0, L_PRED), y=c(-0.1, -0.1), z=c(PRIOR$Z_TopBox, PRIOR$Z_TopBox), col="lightblue", lwd = 5)
#   text3d(x=c(L_PRED), y=c(-0.1-0.05), z=c(PRIOR$Z_TopBox), "L", col="white")
#   lines3d(x=c(0, W_PRED), y=c(-0.3, -0.3), z=c(PRIOR$Z_TopBox,PRIOR$Z_TopBox), col="pink", lwd = 5)
#   text3d(x=c(W_PRED), y=c(-0.3-0.05), z=c(PRIOR$Z_TopBox), "W", col="white")
#   text3d(x=0, y=c(-0.2), z=c(PRIOR$Z_TopBox), "PredOffset", col="white")
#   
# }
# 
# ########################################################################################################################################## 
# 
# # Vertices_From_BBox_Function <- function(Summary_oneVox, onePSID_onePlot_Data, PSID_ID, Base_Radius = 2) {
# #   
# #   # GET PSID HEIGHTS FOR EACH TRIANGULAR SECTION
# #   Top_Height <-Summary_oneVox$Zmax[which(Summary_oneVox$PointSourceID  == PSID_ID)]
# #   ### DOM DOM DOM !!! MAKE SURE ABOVE PointSourceID IS CORRECT COLUMN
# #   browser()
# #   Top_BoxHeight <- onePSID_onePlot_Data$Z[which(onePSID_onePlot_Data$Quantile == 3)][1]
# #   Bot_BoxHeight <- onePSID_onePlot_Data$Z[which(onePSID_onePlot_Data$Quantile == 2)][1]
# #   Bot_Height <- onePSID_onePlot_Data$Z[which(onePSID_onePlot_Data$Quantile == 0)][1]
# #   
# #   #######################
# #   # VERTICES FOR ONE PSID
# #   #######################
# #   
# #   # GROUND BOX
# #   Ground_Coord <- onePSID_onePlot_Data[which(onePSID_onePlot_Data$Quantile == 0), which(colnames(onePSID_onePlot_Data) %in% c("X", "Y", "Z"))]
# #   
# #   Ground_Vertices <- as.data.frame(matrix(c(c(Ground_Coord$X ,Ground_Coord$Y + Base_Radius,Ground_Coord$Z),
# #                                             c(Ground_Coord$X + Base_Radius,Ground_Coord$Y,Ground_Coord$Z),
# #                                             c(Ground_Coord$X,Ground_Coord$Y - Base_Radius,Ground_Coord$Z),
# #                                             c(Ground_Coord$X - Base_Radius,Ground_Coord$Y,Ground_Coord$Z)), ncol=3, byrow= TRUE))
# #   colnames(Ground_Vertices) <- c("X", "Y", "Z")
# #   Vertices <- Ground_Vertices
# #   
# #   # FIRST BOUNDING BOX
# #   BoundBox1_Vertices <- onePSID_onePlot_Data[which(onePSID_onePlot_Data$Quantile == 2), which(colnames(onePSID_onePlot_Data) %in% c("X", "Y", "Z"))]
# #   BoundBox1_Vertices  <- na.omit(BoundBox1_Vertices)
# #   if(nrow(BoundBox1_Vertices) == 5){
# #     BoundBox1_Vertices <- BoundBox1_Vertices[c(1,4,3,2),]
# #     dist <- rdist(Vertices[,1:2],BoundBox1_Vertices[1:4,1:2]) # rdist(row, col)
# #     dist_Index1 <- which(dist == min(dist), arr.ind = TRUE) # 4 is closest
# #     Index_Shift_1 <- as.vector(dist_Index1[,2]- dist_Index1[,1]) #print(paste("Shift", Index_Shift_1))
# #     BoundBox1_Vertices <- BoundBox1_Vertices[index_Order_Function(Index_Shift_1),]
# #     Vertices <- rbind(Vertices,BoundBox1_Vertices )
# #   }
# #   
# #   # SECOND BOUNDING BOX
# #   BoundBox2_Vertices <- onePSID_onePlot_Data[which(onePSID_onePlot_Data$Quantile == 3), which(colnames(onePSID_onePlot_Data) %in% c("X", "Y", "Z"))]
# #   BoundBox2_Vertices  <- na.omit(BoundBox2_Vertices)
# #   if(nrow(BoundBox2_Vertices) == 5){
# #     BoundBox2_Vertices <-  BoundBox2_Vertices[c(1,4,3,2),]
# #     if(nrow(BoundBox1_Vertices) != 4){
# #       dist <- rdist(Ground_Vertices[,1:2],BoundBox2_Vertices[1:4,1:2]) # rdist(row, 
# #     }else{
# #       dist <- rdist(BoundBox1_Vertices[,1:2],BoundBox2_Vertices[1:4,1:2]) # rdist(row, col)
# #     }
# #     # browser()
# #     dist_Index2 <- which(dist == min(dist), arr.ind = TRUE) # 4 is closest
# #     Index_Shift_2 <- as.vector(dist_Index2[,2]- dist_Index2[,1]) # print(paste("Shift", Index_Shift_2))
# #     BoundBox2_Vertices <- BoundBox2_Vertices[index_Order_Function(Index_Shift_2),]
# #     Vertices <- rbind(Vertices,BoundBox2_Vertices)
# #     # BoundBox2_Vertices[,3] <- Top_Height
# #     #Vertices <- rbind(Vertices,BoundBox2_Vertices )
# #   }
# #   
# #   #################################################################
# #   # SORT OUT THE VERTICES IF CERTAIN BOUNDING BOXES ARE NOT PRESENT
# #   # IF BBOX1 IS MISSING BUT BBOX2 EXIST THEN GIVE BBOX1 THE SAME SIZE AS GROUND WITH Z HALF WAY UP
# #   # IF BBOX2 IS MISSING BUT BBOX1 EXIST THEN MAKE BBOX2 SAME AS BBOX1 AND REDUCE BBOX1 Z AS GROUND WITH Z HALF WAY UP
# #   # IF GBo1 IS ONLY VERTICE THEN GIVE BBOX1 and BBOX2 SAME VERTICE BUT INCREASE EACH BY 0.5 M
# #   #################################################################
# #   flag <- 0
# #   # IF BBOX1 IS MISSING BUT BBOX2 EXIST THEN GIVE BBOX1 THE SAME SIZE AS GROUND WITH Z HALF WAY UP
# #   if(nrow(BoundBox1_Vertices) == 0 & nrow(BoundBox2_Vertices) != 0){
# #     BoundBox1_Vertices <- BoundBox2_Vertices
# #     BoundBox1_Vertices[,3] <- BoundBox2_Vertices[,3]- 0.5*(BoundBox2_Vertices[,3]-Ground_Vertices[,3]) 
# #     flag <- 1
# #   }
# #   
# #   # IF BBOX2 IS MISSING BUT BBOX1 EXIST THEN MAKE BBOX2 SAME AS BBOX1 AND REDUCE BBOX1 Z AS GROUND WITH Z HALF WAY UP
# #   if(nrow(BoundBox1_Vertices) != 0 & nrow(BoundBox2_Vertices) == 0){
# #     BoundBox2_Vertices <- BoundBox1_Vertices
# #     BoundBox1_Vertices[,3] <- BoundBox2_Vertices[,3]- 0.5*(BoundBox2_Vertices[,3]-Ground_Vertices[,3]) 
# #     flag <- 2
# #   }
# #   
# #   # IF BBOX2 IS MISSING BUT BBOX1 EXIST THEN MAKE BBOX2 SAME AS BBOX1 AND REDUCE BBOX1 Z AS GROUND WITH Z HALF WAY UP
# #   if(nrow(BoundBox1_Vertices) == 0 & nrow(BoundBox2_Vertices) == 0){
# #     BoundBox1_Vertices <- Ground_Vertices
# #     BoundBox2_Vertices <- Ground_Vertices
# #     BoundBox1_Vertices[,3] <- BoundBox1_Vertices[,3]+0.5
# #     BoundBox2_Vertices[,3] <- BoundBox2_Vertices[,3]+1
# #     flag <- 3
# #   }
# #   
# #   # ADD THE BOUNDING BOX AT THE TOP OF THE 
# #   BoundBoxTop_Vertices <- BoundBox2_Vertices
# #   BoundBoxTop_Vertices[,3] <- Top_Height
# #   
# #   Vertices <- rbind(Ground_Vertices, BoundBox1_Vertices, BoundBox2_Vertices, BoundBoxTop_Vertices)
# #   return(Vertices)
# # }
# 
# 
# 
# 
# 
# 
# 
# # 
# # List_RoI_Function  <- function(BestWorst_Summary_Prior_PSID, Summary_PSID_Extent,
# #                                Ext_allPrior, LAS_Vox,
# #                                para_IoU_XYRes, para_IoU_ZRes){
# #   
# #   List_RoI_Dec <- list()
# #   Decimal_Vox <- unique(LAS_Vox$X)
# #   for(TIU in 1:nrow(BestWorst_Summary_Prior_PSID)){
# #     
# #     onePSID_Prior <- BestWorst_Summary_Prior_PSID[TIU,]
# #     
# #     # GET GT PSID
# #     Summary_TID_Extent <- Summary_PSID_Extent[which(Summary_PSID_Extent$PointSourceID == onePSID_Prior$PSID),]
# #     ### DOM DOM DOM !!! MAKE SURE ABOVE PointSourceID IS CORRECT COLUMN
# # 
# #     # ROUND PRIOR EXTENT TO VOXEL POSITION
# #     onePrior_Ext <- Ext_allPrior[which(Ext_allPrior$PSID == onePSID_Prior$Prior),]
# #     
# #     #browser()
# #     if(onePrior_Ext$X[1]%%1 > Decimal_Vox){onePrior_Ext$X[1] <- ceiling(onePrior_Ext$X[1]) - Decimal_Vox }else{onePrior_Ext$X[1] <- floor(onePrior_Ext$X[1]) - Decimal_Vox}
# #     if(onePrior_Ext$X[2]%%1 > Decimal_Vox){onePrior_Ext$X[2] <- ceiling(onePrior_Ext$X[2]) + Decimal_Vox}else{onePrior_Ext$X[2] <- floor(onePrior_Ext$X[2]) + Decimal_Vox}
# #     if(onePrior_Ext$Y[1]%%1 > Decimal_Vox){onePrior_Ext$Y[1] <- ceiling(onePrior_Ext$Y[1]) - Decimal_Vox }else{onePrior_Ext$Y[1] <- floor(onePrior_Ext$Y[1]) - Decimal_Vox}
# #     if(onePrior_Ext$Y[2]%%1 > Decimal_Vox){onePrior_Ext$Y[2] <- ceiling(onePrior_Ext$Y[2]) + Decimal_Vox}else{onePrior_Ext$Y[2] <- floor(onePrior_Ext$Y[2]) + Decimal_Vox}
# #     onePrior_Ext$Z[1] <- floor(onePrior_Ext$Z[1])
# #     onePrior_Ext$Z[2] <- ceiling(onePrior_Ext$Z[2])
# #     
# #     # GET OVERALL MIN MAX OF PRIOR AND GT
# #     minX_GT_Prior <- min(Summary_TID_Extent$MinX, onePrior_Ext$X[1])
# #     maxX_GT_Prior <- max(Summary_TID_Extent$MaxX, onePrior_Ext$X[2])
# #     minY_GT_Prior <- min(Summary_TID_Extent$MinY, onePrior_Ext$Y[1])
# #     maxY_GT_Prior <- max(Summary_TID_Extent$MaxY, onePrior_Ext$Y[2])
# #     minZ_GT_Prior <- min(Summary_TID_Extent$MinZ, onePrior_Ext$Z[1])
# #     maxZ_GT_Prior <- max(Summary_TID_Extent$MaxZ, onePrior_Ext$Z[2])
# #     
# #     # GET VOXEL PLOT RANGE
# #     Vox_minX <- range(LAS_Vox$X)[1]
# #     Vox_maxX <- range(LAS_Vox$X)[2]
# #     Vox_minY <- range(LAS_Vox$Y)[1]
# #     Vox_maxY <- range(LAS_Vox$Y)[2]
# #     Vox_minZ <- range(LAS_Vox$Z)[1]
# #     Vox_maxZ <- range(LAS_Vox$Z)[2]
# #     
# #     # CALCULATE TABLE_IoU
# #     oneList_RoI_Dec <- list( TIU,
# #                          c(max(0,minX_GT_Prior-Vox_minX), max(0,Vox_maxX-maxX_GT_Prior)),
# #                          c(max(0,minY_GT_Prior-Vox_minY), max(0,Vox_maxY-maxY_GT_Prior)),
# #                          c(max(0,minZ_GT_Prior-Vox_minZ), max(0,Vox_maxZ-maxZ_GT_Prior)))
# #     
# #     
# #     # # TEST
# #     # oneList_RoI_Orig <- oneList_RoI_Dec
# #     # List_RoI_Orig <- list.append(List_RoI_Orig, oneList_RoI_Orig) 
# #     # XRange_inGrids_Orig <- (para_LAS_XY_Range-oneList_RoI_Dec[[1]][2]) - oneList_RoI_Dec[[1]][1]
# #     # YRange_inGrids_Orig <- (para_LAS_XY_Range-oneList_RoI_Dec[[2]][2]) - oneList_RoI_Dec[[2]][1]
# #     # ZRange_inGrids_Orig <- (para_LAS_Z_Range-oneList_RoI_Dec[[3]][2]) - oneList_RoI_Dec[[3]][1]
# #     # # END TEST
# #     
# #     # IF RANGE IS LESS THAN 7 FOR XY OR 20 FOR Z THEN EXTEND THE RANGE (
# #     
# #     para_LAS_XY_Range <- length(unique(LAS_Vox$X))
# #     para_LAS_Z_Range <- length(unique(LAS_Vox$Z))
# #     
# #     # RangeX_inGrids <- (para_LAS_XY_Range-oneList_RoI_Dec[[1]][2]) - oneList_RoI_Dec[[1]][1]
# #     # if(RangeX_inGrids < para_IoU_XYRes){
# #     #   Need_X_Increased <- para_IoU_XYRes - (maxX_GT_Prior-minX_GT_Prior)
# #     #   }
# #     
# #     # DEALING WITH EDGE CASES WHEN EXPANDING THE SMALL TREES
# #     for(RS in 2:4){
# #       # GET PRESENT RANGE OF GT_PSID
# #       if(RS == 2){GT_PSID_Range <- (maxX_GT_Prior-minX_GT_Prior)} #X
# #         if(RS == 3){GT_PSID_Range<-(maxY_GT_Prior-minY_GT_Prior)} #Y
# #       if(RS == 4){GT_PSID_Range<-(maxZ_GT_Prior-minZ_GT_Prior)}   #Z
# #       if(RS <= 3){ # XY
# #         LAS_Range <- para_LAS_XY_Range
# #         IoU_Range <- para_IoU_XYRes
# #         Decimal_Vox_Shift <-Decimal_Vox_XY 
# #       }else{ # Z
# #         LAS_Range <-para_LAS_Z_Range
# #         IoU_Range <-para_IoU_ZRes
# #         Decimal_Vox_Shift <-Decimal_Vox_Z 
# #       }
# #       
# #       GT_PSID_Range <- (LAS_Range-oneList_RoI_Dec[[RS]][2]) - oneList_RoI_Dec[[RS]][1]
# #       if(GT_PSID_Range <= IoU_Range){ # IF RANGE NEEDS EXPANDING
# #         Need_Shift <- IoU_Range - GT_PSID_Range
# #         
# #         # ADUST BOTTOM SIDE
# #         Bot_Shift <- floor(Need_Shift/2)
# #         if (oneList_RoI_Dec[[RS]][1] > Bot_Shift){
# #           oneList_RoI_Dec[[RS]][1] <- oneList_RoI_Dec[[RS]][1] - Bot_Shift
# #           Need_Shift <- Need_Shift- Bot_Shift
# #         }else{
# #           Need_Shift <-  Need_Shift - oneList_RoI_Dec[[RS]][1]
# #           oneList_RoI_Dec[[RS]][1] <- 0 
# #         }
# #         
# #         # ADUST BOTTOM SIDE
# #         Top_Shift <-ceiling(Need_Shift/2)
# #         if (oneList_RoI_Dec[[RS]][2] > Top_Shift){
# #           oneList_RoI_Dec[[RS]][2] <- oneList_RoI_Dec[[RS]][2] - Top_Shift
# #           Need_Shift <- Need_Shift- Top_Shift
# #         }else{
# #           Need_Shift <-  Need_Shift - oneList_RoI_Dec[[RS]][2]
# #           oneList_RoI_Dec[[RS]][2] <- 0 
# #         }
# #         # IF THERE IS STILL SOME SHIFTING NECESSARY
# #         if(Need_Shift != 0){
# #           Index_Change <- which.max(c(oneList_RoI_Dec[[RS]][1], oneList_RoI_Dec[[RS]][2]))
# #           oneList_RoI_Dec[[RS]][Index_Change] <- oneList_RoI_Dec[[RS]][Index_Change] - Need_Shift
# #         }
# #       }
# #       
# #     } # RS LOOP
# #     
# #     List_RoI_Dec <- list.append(List_RoI_Dec, oneList_RoI_Dec)  
# #     browser()
# #     
# #     # # TEST
# #     # XRange_inGrids <- (para_LAS_XY_Range-oneList_RoI_Dec[[1]][2]) - oneList_RoI_Dec[[1]][1]
# #     # YRange_inGrids <- (para_LAS_XY_Range-oneList_RoI_Dec[[2]][2]) - oneList_RoI_Dec[[2]][1]
# #     # ZRange_inGrids <- (para_LAS_Z_Range-oneList_RoI_Dec[[3]][2]) - oneList_RoI_Dec[[3]][1]
# #     # 
# #     # print(paste("oneList_RoI_Orig_X: ", XRange_inGrids_Orig, "Y: ", YRange_inGrids_Orig, "Z: ", ZRange_inGrids_Orig))
# #     # print(oneList_RoI_Orig)
# #     # 
# #     # print(paste("oneList_RoI_X............: ", XRange_inGrids, "Y: ", YRange_inGrids, "Z: ", ZRange_inGrids))
# #     # print(oneList_RoI_Dec)
# #     #if(XRange_inGrids_Orig < 7 | YRange_inGrids_Orig < para_IoU_XYRes | ZRange_inGrids_Orig < para_IoU_ZRes){browser()}
# #     # END TEST
# #   }   
# #   return(List_RoI_Dec)
# # }
# # 
# 
# # INTERSECTION_TRIANGLE_FUN <-
# #   function (triangles, coord, points) 
# #   {
# #     points = as.matrix(points, nc = 3)
# #     # browser()
# #     inside <- integer(dim(points)[1])
# #     browser()
# #     # teVFdir = as.numeric(2 * runif(3) - 1)
# #     retour <- .C("pointinashape", as.integer(triangles), dim(triangles)[1], 
# #                  as.numeric(coord), dim(coord)[1], as.numeric(points), 
# #                  dim(points)[1], as.integer(inside), tempdir)
# #     return(retour[[7]])
# #   }
# 
# #### OLD  CODE THAT MAY BE USED LATER
# 
# 
# # Empty_Voxels$Count <- as.array(torch_flatten(oneB_oneRoI_Vox_Den, start_dim = 1L))
# # Empty_Voxels$Count2 <- as.vector(as.array(oneB_oneRoI_Vox_Den))
# # Y:\CNN\THINNED_SAMPLES_TORCH_V901\TRAIN_DATA\Flight_1\CSV\CSV_MP1\VOX_DF
# # Vox_Den <- read.csv(paste(DIR_LIST[bd], "/CSV/CSV_MP1/VOX_DF/F", as.array(FlightID_LIST)[bd], "_P", as.array(PlotID_LIST)[bd],"_LAS_Vox_N.csv", sep=""))
# # F1_P100_LAS_Vox_N
# 
# # browser()
# # Test <- read.csv(paste(Comp, "CNN/Test_LAS_Conversion.csv", sep=""))
# # Test_T <- torch_tensor(array(Test$Count, dim = c(2, 2, 3)))
# # Test_A <- as.array(Test_T)
# # Test_V <-as.vector(Test_A)
# # Test <- sapply(Test, as.numeric)
# 
# # LAS_Test <- LAS(Test)
# # 
# # Final_Array <- c()
# # for(zz in 1:40){
# #   for(yy in 1:16){
# #     Final_Array <- c(Final_Array, as.array(torch_flatten(oneB_oneRoI_Vox_Den[,yy,zz], start_dim = 1L)))
# #     # print("Test>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
# #     # print(Test)
# #     Test1 <- as.array(torch_flatten(oneB_oneRoI_Vox_Den[,yy,zz], start_dim = 1L))
# #     Test2 <- Vox_Den$Count_Norm[(((zz-1)*256)+((1*yy)*16-15)):(((zz-1)*256)+((1*yy)*16))]
# #     # print("Test2>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
# #     # print(Test2)
# #     # print("Diff>>>>>>>>>>>>>>>>>>>>>>>>>>>>>")
# #     # print(round(Test-Test2, 2))
# #     if(max(round(Test1-Test2, 2))> 0){browser()}
# #     print(paste("zz", zz, "yy", yy, " first:", (((zz-1)*256)+((1*yy)*16-15))))
# #   }
# # }
# # browser()
# # Empty_Voxels$Count <-Final_Array
# # Empty_Voxels$TID <- as.integer(0)
# # Test <- LAS(Empty_Voxels)
# # Test2 <- lasfilter(Test,Count > 0)
# #
# 
# 
# # TESTING XYZWHR_TO_VERT_GPU_FUN
# # TEST_LAS_DF <- data.frame(X =c(0,2,0,2,0,2,0,2), Y = c(0, 0, 2, 2, 0,0,2,2), Z = c(0,0,0,0,2,2,2,2))
# # TEST_LAS <-LAS(TEST_LAS_DF)
# # TEST_LAS@data$TID <- 2
# # TEST_LAS@data$TID[1:4] <- 1
# # Test_XYZWLHR <- BestPr_oneP_PRIOR_XYZWLHR_ExtP_Coord
# # Test_XYZWLHR[,1:2] <- 1
# # Test_XYZWLHR[,3] <- 0
# # Test_XYZWLHR[,4:5] <- 1
# # Test_XYZWLHR[,6:7] <- 0.5
# # Test_XYZWLHR[,8] <- 1
# # Test_XYZWLHR[,9] <- 85
# # Test_XYZWLHR[,10:11] <- 1
# # Test_XYZWLHR[,12:13] <- 1
# # Test_XYZWLHR[,14] <- 1.5
# # Test_XYZWLHR[,15] <- 85
# # Test_XYZWLHR[,16] <- 2
# # 
# # Test_Vert <- as.data.frame(XYZWHR_TO_VERT_GPU_FUN(Test_XYZWLHR, Base_WL = 0.2))
# # Test_Vert2 <- as.data.frame(XYZWHR_TO_VERT_GPU_FUN(Test_XYZWLHR, Base_WL = 0.2))
# # 
# # #plot(TEST_LAS, size=10)
# # colours <- c( "pink","blue")
# # PLOT_Ver1_Vert2_VOX_FUN(TEST_LAS , list(Test_Vert,Test_Vert2) , Title_Plot, colours) # oneB_Vox_Den_ExtR
# # 
# # 
# # Test_Length <- rbind(c(0.9,1,0), c(1.1,1,0))
# # Test_Length2 <- rbind(c(0.75,1,1), c(1.25,1,1))
# # Test_Length3 <- rbind(c(0.5,1,1.5), c(1.5,1,1.5))
# # lines3d(x = Test_Length[,1] , y = Test_Length[,2], z = Test_Length[,3], fill=FALSE, col="light blue", lwd=5)
# # lines3d(x = Test_Length2[,1] , y = Test_Length2[,2], z = Test_Length2[,3], fill=FALSE, col="light green", lwd=5)
# # lines3d(x = Test_Length3[,1] , y = Test_Length3[,2], z = Test_Length3[,3], fill=FALSE, col="orange", lwd=5)
# # 
# 
# 
# # plot(Test2)
