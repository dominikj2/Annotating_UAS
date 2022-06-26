numextract <- function(string){
  str_extract(string, "\\-*\\d+\\.*\\d*")
}
numextract_all <- function(string){
  unlist(regmatches(string,gregexpr("[[:digit:]]+\\.*[[:digit:]]*",string)))
}

GAP_DENSITY_FUNCTION_NEW2 = function(Z_Values,
                                     Para_BW = 0.4,
                                     Para_Threshold_Percent = 0.2,
                                     Plot = "No",
                                     Plot_Heading = 1) #also include argument about average height for flight...
{

  pdens <- density(Z_Values, bw=Para_BW)

  #browser()
  # GET Peak_Dip_Summary
  Peak_Dip_Summary <- Peak_Dip_FUN(pdens) # Peak_Dip_Summary$Peak_Dip_Summary

  # GET ALL SECTIONS THAT ARE WITHIN 20% ABOVE THE MINIMUM DIP
  Den_Threshold <- Peak_Dip_Summary$minDip_DF$Density + Peak_Dip_Summary$Range_Den_minDip_maxPeak * Para_Threshold_Percent
  #browser()
  # IF THERE ARE NO MINIMUM DIPS
  if(length(Den_Threshold) == 0){
    Start_Largest_Gap <- max(Peak_Dip_Summary$Peak_Dip_Summary$Z)
    End_Largest_Gap <- max(Peak_Dip_Summary$Peak_Dip_Summary$Z)

  }else{

    # Near_min_density <- which(pdens$y >= Peak_Dip_Summary$minDip_DF$Density & pdens$y < Den_Threshold)
    Near_min_density <- which(pdens$y < Den_Threshold)
    # REMOVE STRING OF LOW DENSITIES NEAR BOTTOM HEIGHT AND TOP CANOPY HEIGHT

    Index_PeakDip_inMinDen <- Peak_Dip_Summary$Peak_Dip_Summary$Index[which(Peak_Dip_Summary$Peak_Dip_Summary$Index %in% Near_min_density)]


    # IDENTIFY THE SECTION THAT HAS THE LARGEST STRETCH AND USE THOSE BOUNDS TO DETERMINE
    # UNDER TOP AND CANOPY BASE AS FIRST PASS

    # VEG_PROGILE_GAPS _WITH_LOW_LIDAR_DENSITY FUNCTION
    Near_min_Density_Function = function(Near_min_density)
    {
      # CALCULATING WHERE THE GAP IS

      Zero_Density <- rle(diff(Near_min_density))
      myZero_Density <- which(Zero_Density$values == TRUE & Zero_Density$lengths > 0)
      Zero_Density.lengths.cumsum <- cumsum(Zero_Density$lengths)
      ends <- Zero_Density.lengths.cumsum[myZero_Density]
      newindex <- ifelse(myZero_Density>1, myZero_Density-1, 0)
      starts <- Zero_Density.lengths.cumsum[newindex] + 1
      starts_2 <- starts
      #browser()
      if(length(which(newindex == 0))>0){starts_2 <-  c(1,starts)}

      starts <- starts_2
      Start_Height <- pdens$x[Near_min_density[starts]]
      End_Height <- pdens$x[Near_min_density[ends]]

      # REMOVING THE GAP BELOW THE GROUND
      if(End_Height[1] < 0){
        Start_Height <- Start_Height[-1]
        End_Height <- End_Height[-1]
      }

      return(list(Start_Height=Start_Height,
                  End_Height=End_Height))
    }
    # END Near_min_Density_Function

    Near_Zero_Density_List <-Near_min_Density_Function(Near_min_density)

    # GET OUTPUT FROM ONE OF TWO METHODS ABOVE
    Start_Height <- Near_Zero_Density_List$Start_Height
    End_Height <- Near_Zero_Density_List$End_Height
    #new_Max_Value <- Near_Zero_Density_List$new_Max_Value

    # plot(pdens$x, pdens$y, type="l", ylim=c(0, 0.6),
    #      main= paste( "F:", PointSourceID),
    #      ylab = paste("Start:", round(Start_Height, 2)),
    #      xlab = paste("End:", round(End_Height, 2)))
    #
    # abline(v = Start_Height, col="blue", lwd=4)
    # abline(v = End_Height, col="dark green", lwd=4)
    # abline(h = Den_Threshold, col= "red")

    # GET THE LARGEST GAP WITH DENSITY CLOSE TO ZERO AND THAT DETERMINES THE UNDER AND CANOPY BASE
    Gaps <- End_Height -Start_Height
    Largest_Gap <- max(Gaps) # Second_Largest_Gap <- sort(Gaps,partial=length(Gaps)-1)[length(Gaps)-1]
    Start_Largest_Gap <- Start_Height[which(Gaps == Largest_Gap)][1]
    End_Largest_Gap <- End_Height[which(Gaps == Largest_Gap)]

    # GET ALL PEAKS AND DIPS WITHIN FIRST PASS OF START AND END
    PeakDip_Within_minDen <- Peak_Dip_Summary$Peak_Dip_Summary[which(Peak_Dip_Summary$Peak_Dip_Summary$Index %in% Index_PeakDip_inMinDen),]

    # MAKE THE START OF GAP AT THE "FIRST MIN" WITHIN THAT GAP
    # WHERE "FIRST MIN" IS THE FIRST MIN THAT IS LESS THAN THE 50 PERCENTILE OF ALL MINS (AVOIDS USING MINS THAT ARE NOT SIGNIFICANT MINS)
    # DOM DOM! SOME MINS STAY MIN FOR LONGER WHEN THERE ARE NO POINTS OVER A HEIGHT... MAYBE THIS NEEDS EXPLOITING!
    #browser()

    Density_Dips_Start <- PeakDip_Within_minDen$Density[ which(PeakDip_Within_minDen$Z > Start_Largest_Gap &
                                                                 PeakDip_Within_minDen$Peak_Dip == "Dip")]

    Constraint <- which(PeakDip_Within_minDen$Z > Start_Largest_Gap &
            PeakDip_Within_minDen$Peak_Dip == "Dip" &
            PeakDip_Within_minDen$Density <= median(Density_Dips_Start))

    if(length(Constraint) > 0){
      Start_Largest_Gap <- min(PeakDip_Within_minDen$Z[Constraint])
    } else{
      Index_Dip_Below_Start <- which(PeakDip_Within_minDen$Peak_Dip == "Dip")
      End_Largest_Gap <- PeakDip_Within_minDen$Z[min(Index_Dip_Below_Start)]
      Index_Dip_Below_End <- which(PeakDip_Within_minDen$Z < End_Largest_Gap)
      Start_Largest_Gap <- PeakDip_Within_minDen$Z[max(Index_Dip_Below_End)]
    }



    # MAKE THE END OF GAP AT THE LAST MIN WITHIN THAT GAP

    Density_Dips_Ends <- PeakDip_Within_minDen$Density[ which(PeakDip_Within_minDen$Z < End_Largest_Gap &
                                                                PeakDip_Within_minDen$Peak_Dip == "Dip")]

    Constraint <- which(PeakDip_Within_minDen$Z < End_Largest_Gap &
            PeakDip_Within_minDen$Peak_Dip == "Dip" &
            PeakDip_Within_minDen$Density <= median(Density_Dips_Ends))

    if(length(Constraint) > 0){
      # ONLY WORK WITH DENSITIES THAT ARE BELOW THE MEDIAN OF ALL DIPS
      End_Largest_Gap <- max(PeakDip_Within_minDen$Z[Constraint])
    }else{
      End_Largest_Gap <- Start_Largest_Gap
    }

  }

  if(Plot == "Yes"){

    plot(pdens$x, pdens$y, type="l", ylim=c(0, 0.6),
         main= paste( "F:", Plot_Heading),
         ylab = "Density", #paste("Start:", round(Start_Largest_Gap, 2)),
         xlab = "Height (m)", #paste("End:", round(End_Largest_Gap, 2)),
         cex.lab = 1.5,
         cex.axis = 2)

    abline(v = Start_Largest_Gap, col="blue", lwd = 3)
    abline(v = End_Largest_Gap, col="dark green", lwd = 3)


    #plot(pdens)
    abline(h = Den_Threshold, col= "red", lwd = 2)

    legend("topright", legend = c("Threshold Height", "Bottom Minima", "Top Minima"),
           lty= 1, col=c("red", "blue", "dark green"), lwd=3,
           cex= 1.7)
  }

  return(list(Start_Largest_Gap=Start_Largest_Gap,
              End_Largest_Gap=End_Largest_Gap,
              Peak_Dip_Summary = Peak_Dip_Summary))
}

############################################################################################################################################################
# FINDING PEAKS WHEN THERE ARE NO ZEROS
Peak_Dip_FUN = function(pdens)
{
  TurnPnts <- turnpoints(pdens$y)
  Index <- c(1, TurnPnts$tppos, TurnPnts$n)
  # DATAFRAME OF TURNING POINTS IN KERNAL DENSITY
  Z <- c(min(pdens$x), pdens$x [TurnPnts$tppos], max(pdens$x)) # Adding one extra turning point at start and end
  Density <- c(0, pdens$y[TurnPnts$tppos], 0)    # Adding one extra turning point at start and end
  Peak_Dip_Summary <- t(data.frame(rbind(Z, Density, Index)))
  colnames(Peak_Dip_Summary) <- c("Z", "Density", "Index")
  Peak_Dip_Summary<- data.frame( Peak_Dip_Summary, Peak_Dip = rep("Dip", nrow(Peak_Dip_Summary)), stringsAsFactors = FALSE)
  Peak_Dip_Summary$Peak_Dip [c(1, nrow(Peak_Dip_Summary))] <- c("Start_End", "Start_End")

  ##############################################################################
  # PEAKS AND DIPS
  ##############################################################################

  Y_Peak <- TurnPnts$points[TurnPnts$peaks]
  X_Peak <- pdens$x[TurnPnts$pos[TurnPnts$peaks]]

  Peak_Dip_Summary$Peak_Dip[match(X_Peak,Peak_Dip_Summary$Z)] <- "Peak"

  Dip_DF <- Peak_Dip_Summary[which(Peak_Dip_Summary$Peak_Dip == "Dip"),]
  minDip_DF <- Dip_DF[which.min(Dip_DF$Density),]

  Peak_DF <- Peak_Dip_Summary[which(Peak_Dip_Summary$Peak_Dip == "Peak"),]
  maxPeak_DF <- Peak_DF[which.max(Peak_DF$Density),]

  Range_Den_minDip_maxPeak <- maxPeak_DF$Density - minDip_DF$Density

  return(list(Peak_Dip_Summary=Peak_Dip_Summary,
              minDip_DF = minDip_DF,
              maxPeak_DF = maxPeak_DF,
              Range_Den_minDip_maxPeak = Range_Den_minDip_maxPeak))
}

BBOX_PNTS_FUN <- function(p) {
  # Analyze the convex hull edges
  # browser()
  a <- chull(p)                                   # Indexes of extremal points
  a <- c(a, a[1])                                 # Close the loop
  e <- p[a[-1],] - p[a[-length(a)], ]             # Edge directions
  norms <- sqrt(rowSums(e^2))                     # Edge lengths
  v <- e / norms                                  # Unit edge directions
  w <- cbind(-v[,2], v[,1])                       # Normal directions to the edges
  # browser()
  # Find the MBR
  vertices <- p[a, ]                              # Convex hull vertices
  x <- apply(vertices %*% t(v), 2, range)         # Extremes along edges
  y <- apply(vertices %*% t(w), 2, range)         # Extremes normal to edges
  areas <- (y[1,]-y[2,])*(x[1,]-x[2,])            # Areas
  k <- which.min(areas)                           # Index of the best edge (smallest area)
  # Form a rectangle from the extremes of the best edge
  cbind(x[c(1,2,2,1,1),k], y[c(1,1,2,2,1),k]) %*% rbind(v[k,], w[k,])
}

#############################################################################################################################################
BEARING_FUN <- function(XY_Vertices, P1){  #, P2 = c(1,3)
  #browser()
  dL = XY_Vertices[P1[2],1] - XY_Vertices[P1[1],1]
  X = cos(XY_Vertices[P1[2],2]*pi/180)*sin(dL*pi/180) # =COS(39.099912*pi/180)
  Y = (cos(XY_Vertices[P1[1],2]*pi/180)*sin(XY_Vertices[P1[2],2]*pi/180)) -
    (sin(XY_Vertices[P1[1],2]*pi/180)*cos(XY_Vertices[P1[2],2]*pi/180)*cos(dL*pi/180))
  bearing_rad <- atan2(X,Y)
  bearing_deg <- (bearing_rad *180)/pi
  bearing_deg <- round(bearing_deg)   # DOM DOM DOM !!! SEE angle
  # browser()
  if(bearing_deg < 0){bearing_deg <- bearing_deg + 180 }
  return(bearing_deg)
}

XYZWLHR_FUN  <- function(onePSID_BBox, Prefix, Height_Strata, Sample_Att){

  # REMOVE THE LAST POINT WHICH IS A REPLICA OF FIRST
  onePSID_BBox <- onePSID_BBox[1:4,]

  # ORDER POINTS SO MOST X VALUE IS FIRST
  Ord_First <- which.min(onePSID_BBox$X)
  if(Ord_First != 1){
    onePSID_BBox <- onePSID_BBox[c(Ord_First:nrow(onePSID_BBox), 1:(Ord_First-1)),]
  }
  onePSID_BBox$Order <- 1:nrow(onePSID_BBox)

  Index_Col_XY <- which(colnames(onePSID_BBox) %in% c("X", "Y"))
  
  Lengths <- raster::pointDistance(as.matrix(onePSID_BBox[1:4,Index_Col_XY]), lonlat =FALSE)
  Length_both <- c(Lengths[1,2], Lengths[2,3])

  Length_Y <- Length_both[which.max(Length_both)]
  Width_X <- Length_both[which.min(Length_both)]

  #=c(0, Length_Y*2), ylim=c(0, Length_Y*2)

  onePSID_BBox_N <- onePSID_BBox
  X_Scale <- min(onePSID_BBox_N$X)
  Y_Scale <- min(onePSID_BBox_N$Y)
  onePSID_BBox_N$X <- onePSID_BBox_N$X - X_Scale
  onePSID_BBox_N$Y <- onePSID_BBox_N$Y - Y_Scale
  # browser()

  Bearing_1 <-BEARING_FUN(onePSID_BBox_N[,Index_Col_XY], 1:2) ### DOM DOM DOM 27/10/
  Bearing_2 <-BEARING_FUN(onePSID_BBox_N[,Index_Col_XY], 2:3)
  Bearing_both <- c(Bearing_1, Bearing_2)
  # Bearing_both <- geosphere::bearing(onePSID_BBox_N[,Index_Col_XY]) # Box_R <- Box_R[1:2]

  Index_Longest <- which.max(Length_both)
  if(Index_Longest == 1){
    Rotate_Deg <- Bearing_both[1]
  }else{
    Rotate_Deg <- Bearing_both[2]
  }
  if(Rotate_Deg < 0) {Rotate_Deg <- Rotate_Deg + 180}

  Centre_XY <-  c(sum(onePSID_BBox$X[1:4])/4, sum(onePSID_BBox$Y[1:4])/4)
  Cent_X <- Centre_XY[1]
  Cent_Y <- Centre_XY[2]

  # Rotate_Deg <- rad2deg(atan((onePSID_BBox$X[1] - onePSID_BBox$X[2]) / (onePSID_BBox$Y[1] - onePSID_BBox$Y[2])))
  #
  # onePSID_BBox_N <- onePSID_BBox
  # onePSID_BBox_N$X <- onePSID_BBox_N$X - min(onePSID_BBox_N$X)
  # onePSID_BBox_N$Y <- onePSID_BBox_N$Y - min(onePSID_BBox_N$Y)
  #
  # Box_R_1 <- bearing(onePSID_BBox_N[1,4:5], onePSID_BBox_N[2,4:5])
  # Box_R_2 <- bearing(onePSID_BBox_N[2,4:5], onePSID_BBox_N[3,4:5])
  # Box_R_2 <- bearing(onePSID_BBox_N[3,4:5], onePSID_BBox_N[4,4:5])
  #
  # Width_X <- raster::pointDistance(as.matrix(oneLevel_Vertices[2,2:3]), oneLevel_Vertices[3,2:3], lonlat =FALSE)
  # Box_R <- bearing(oneLevel_Vertices[1,2:3], oneLevel_Vertices[2,2:3])
  #
  # if(Rotate_Deg < 0){
  #   Rotate_Deg <- Rotate_Deg+180
  # }
  # Centre_XY <-  c(sum(onePSID_BBox$X[1:4])/4, sum(onePSID_BBox$Y[1:4])/4)
  # coords.Rot <- Rotation(data.frame(X = c(onePSID_BBox$X-Centre_XY[1]) , Y = c(onePSID_BBox$Y-Centre_XY[2])), Rotate_Deg*pi/180)
  # coords.Rot[,1] <- coords.Rot[,1]+Centre_XY[1]
  # coords.Rot[,2] <- coords.Rot[,2]+Centre_XY[2]
  #
  # Width_X <- max(coords.Rot[,1])-min(coords.Rot[,1])
  # Length_Y <- max(coords.Rot[,2])-min(coords.Rot[,2])
  #
  # Cent_X <- (max(coords.Rot[,1]) +min(coords.Rot[,1]))/2
  # Cent_Y <- (max(coords.Rot[,2])+min(coords.Rot[,2]))/2

  Height <- onePSID_BBox$Z[1]
  TID  <- onePSID_BBox$TID [1]
  TreeHeight <- max(onePSID_BBox$Z)

  Height_Strata <- Height_Strata
  if(Sample_Att == "Yes"){
    Sample  <- onePSID_BBox$Sample  [1]
    BBox <- data.frame(TID = TID, Sample = Sample, Height_Strata = Height_Strata,
                       X_Box= Cent_X, Y_Box = Cent_Y,
                       L_Box= Length_Y, W_Box =Width_X, Z_Box =Height,
                       R_Box = Rotate_Deg,
                       Z_Tree = TreeHeight)

  }else{
    BBox <- data.frame(TID = TID, Height_Strata = Height_Strata,
                       X_Box = Cent_X, Y_Box = Cent_Y,
                       L_Box = Length_Y, W_Box = Width_X, Z_Box = Height,
                       R_Box = Rotate_Deg,
                       Z_Tree = TreeHeight)
  }


  # "X_Base",  Cent_X
  # "Y_Base", Cent_Y
  # "Z_Base",
  #  "X_BotBox",
  # "Y_BotBox",
  # "L_BotBox",
  # "W_BotBox",
  # "Z_BotBox",
  # "Rot_BotBox",
  #  "X_TopBox",
  # "Y_TopBox",
  # "L_TopBox",
  # "W_TopBox",
  # "Z_TopBox",
  # "Rot_TopBox",
  # "Z_Tree"

  # browser()
  # REMOVE BOTTOM TREE HEIGHT AS NOT NECESSARY
  if(Prefix == "Bot"){
    BBox <- BBox[,-which(colnames(BBox) == "Z_Tree")]
  }

  if(Sample_Att == "Yes"){
    colnames(BBox)[4:ncol(BBox)] <- paste(substr(colnames(BBox)[4:ncol(BBox)],1,2), Prefix,
                                          substr(colnames(BBox)[4:ncol(BBox)],3, nchar(colnames(BBox)[4:ncol(BBox)])), sep="")
  }else{
    colnames(BBox)[3:ncol(BBox)] <- paste(substr(colnames(BBox)[3:ncol(BBox)],1,2), Prefix,
                                          substr(colnames(BBox)[3:ncol(BBox)],3, nchar(colnames(BBox)[3:ncol(BBox)])), sep="")
  }

  # # PLOT THE VISUALISATION OF THE RESULTS
  # plot(onePSID_BBox_N$X, onePSID_BBox_N$Y, xlim=c(0, Length_Y*2), ylim=c(0, Length_Y*2))
  # text(onePSID_BBox_N$X, onePSID_BBox_N$Y,  onePSID_BBox$Order, pos =2)
  # par(new=TRUE)
  # plot(onePSID_BBox_N$X[1:2], onePSID_BBox_N$Y[1:2], col = "red",
  #      xlim=c(0, Length_Y*2), ylim=c(0, Length_Y*2))
  # Cent_X_Plot <- Cent_X - X_Scale
  # Cent_Y_Plot <- Cent_Y - Y_Scale
  # par(new=TRUE)
  # plot(Cent_X_Plot, Cent_Y_Plot, xlim=c(0, Length_Y*2), ylim=c(0, Length_Y*2), col="green" )
  # text(Width_X,Length_Y, paste("L:", round(BBox[, c(6)],1), "  W:",round(BBox[, c(7)],1), "  R:",round(BBox[, c(9)],1)))
  # browser()
  return(BBox)
}

BBox_Sqr_FUN <- function(Cent_X, Cent_Y, Cent_Z, Sqr_Offset = Para_Base_WL/2){
  X1 <- Cent_X -Sqr_Offset
  X2 <- Cent_X +Sqr_Offset
  Y1 <- Cent_Y -Sqr_Offset
  Y2 <- Cent_Y +Sqr_Offset

  X3 <- Cent_X + Sqr_Offset
  X4 <- Cent_X - Sqr_Offset
  Y3 <- Cent_Y + Sqr_Offset
  Y4 <- Cent_Y - Sqr_Offset

  XYZ <- data.frame(X = c(X1,  X3, X2, X4, X1),
                    Y = c(Y1,  Y3, Y2, Y4, Y1),
                    Z = rep(Cent_Z, 5))
  return(XYZ)
}

# FUNCTIONS FOR TRIANGULATED SHAPES
INTERSECT_TRI_FUN <-function (triangles, coord, points)
  {
    points = as.matrix(points, nc = 3)
    inside <- integer(dim(points)[1])
    teVFdir = as.numeric(2 * runif(3) - 1)
    #browser()
    retour <- .C("pointinashape", as.integer(triangles), dim(triangles)[1],
                 as.numeric(coord), dim(coord)[1], as.numeric(points),
                 dim(points)[1], as.integer(inside), tempdir)
    return(retour[[7]])
}

XYZWHR_To_VERT_FUN  <- function(XYZWHRT_Data = oneTriShp, Base_WL = 0.5, Normalised = "Yes", Para_Cnt = 10, Format = "Old"){
  
  # Format = "New" ..... X_Base         Y_Base         Z_Base       X_TopBox       Y_TopBox       Z_TopBox       L_TopBox       W_TopBox        H_TopBox        R_TopBox
  # Format = "OLD" ..... X_Base         Y_Base         Z_Base       X_BotBox       Y_BotBox       L_BotBox       W_BotBox       Z_BotBox        R_BotBox       X_TopBox       Y_TopBox       L_TopBox       W_TopBox       Z_TopBox
  
  if(Para_Cnt == 16){
    Prefix <- c("", "Bot", "Top")
    BB_loop <- 3
  }else{
    Prefix <- c("", "Top")
    BB_loop <- 2
  }

  for(BB in 1:2) { # BB_loop

    if(BB == 1){
      Cent_X <- XYZWHRT_Data[,which(colnames(XYZWHRT_Data) == "X_Base")]
      Cent_Y <- XYZWHRT_Data[,which(colnames(XYZWHRT_Data) == "Y_Base")]
      Length_Y <- Base_WL
      Width_X <- Base_WL
      Rotate_Deg <- 0
      Height <-XYZWHRT_Data[,which(colnames(XYZWHRT_Data) == "Z_Base")]
    }else{
      #browser()
      Cent_X <- XYZWHRT_Data[,which(colnames(XYZWHRT_Data) == paste( "X_", Prefix[BB],"Box", sep=""))]
      Cent_Y <- XYZWHRT_Data[,which(colnames(XYZWHRT_Data) == paste( "Y_", Prefix[BB],"Box", sep=""))]
      Length_Y <- XYZWHRT_Data[,which(colnames(XYZWHRT_Data) == paste("L_", Prefix[BB],"Box", sep=""))]
      Width_X <-XYZWHRT_Data[,which(colnames(XYZWHRT_Data) == paste( "W_", Prefix[BB],"Box", sep=""))]
      Rotate_Deg <-XYZWHRT_Data[,which(colnames(XYZWHRT_Data) == paste( "R_", Prefix[BB], "Box",sep=""))]
      
      if(Format == "New"){
        MidHeight <-XYZWHRT_Data[,which(colnames(XYZWHRT_Data) == paste( "Z_", Prefix[BB],"Box", sep=""))]
        H_TopBox <- XYZWHRT_Data[,which(colnames(XYZWHRT_Data) == paste( "H_", Prefix[BB],"Box", sep=""))] 
        Height <- MidHeight - 0.5*H_TopBox
      }else{ # OLD FORMAT
        Height <-XYZWHRT_Data[,which(colnames(XYZWHRT_Data) == paste( "Z_", Prefix[BB],"Box", sep=""))]
        }
      

      if(BB==2){ #BB_loop
        if(Format == "New"){
          MidHeight <-XYZWHRT_Data[,which(colnames(XYZWHRT_Data) == paste( "Z_", Prefix[BB],"Box", sep=""))]
          H_TopBox <- XYZWHRT_Data[,which(colnames(XYZWHRT_Data) == paste( "H_", Prefix[BB],"Box", sep=""))] 
          Tree_Height <- MidHeight + 0.5*H_TopBox
        }else{
          Tree_Height <- XYZWHRT_Data[,which(colnames(XYZWHRT_Data) == "Z_TopTree")] 
        }
      }
      if(Normalised == "Yes"){Rotate_Deg <- Rotate_Deg*180}
    }

    Rotate_Deg <- deg2rad(Rotate_Deg) # Rotate_Deg$deg2rad #

    # translate point to origin
    tempX <- c(Cent_X-(0.5*Width_X), Cent_X-(0.5*Width_X), Cent_X+(0.5*Width_X),  Cent_X+(0.5*Width_X))
    tempY <- c(Cent_Y-(0.5*Length_Y), Cent_Y+ (0.5*Length_Y), Cent_Y+(0.5*Length_Y), Cent_Y- (0.5*Length_Y))
    # browser()
    tempX = tempX - Cent_X #c(tempX,Cent_X) #- Cent_X;
    tempY = tempY - Cent_Y #c(tempY,Cent_Y) #- Cent_Y;

    # now apply rotation
    rotatedX = tempX*cos(Rotate_Deg) - tempY*sin(Rotate_Deg);
    rotatedY = tempX*sin(Rotate_Deg) + tempY*cos(Rotate_Deg);

    # translate back
    XNew <- rotatedX + Cent_X
    YNew <- rotatedY + Cent_Y

    XYZ <- data.frame(X= XNew, Y = YNew, Z = rep(Height, 4))

    # # ORDER SO SMALLEST X IS ALWAYS FIRST AND THEN MOVE AROUND CONSISTENTLY FROM THERE
    # browser()
    # XYZ <- XYZ[order(XYZ$X, XYZ$Y),]

    if(BB == 1){
      Vertices <- XYZ
    }else{
      # browser()
      Vertices <- rbind(Vertices, XYZ)
    }
  }
  XYZ$Z <- Tree_Height

  Vertices <- rbind(Vertices, XYZ)
  Vertices <- as.matrix(Vertices)
  Vertices <- na.omit(Vertices)

  # # PLOTTING TO SEE RESULTS
  # browser()
  # Vertices_Plot <- data.frame(Vertices)
  # Shift_X <- min(Vertices_Plot$X)
  # Shift_Y <-   min(Vertices_Plot$Y)
  # Vertices_Plot$X <- Vertices_Plot$X - Shift_X
  # Vertices_Plot$Y <- Vertices_Plot$Y - Shift_Y
  # plot(Vertices_Plot$X[1:4], Vertices_Plot$Y[1:4], col="red", xlim = c(range(Vertices_Plot$X)), ylim=(range(Vertices_Plot$Y)))
  # par(new=TRUE)
  # plot(Vertices_Plot$X[5:8], Vertices_Plot$Y[5:8], col="blue", xlim = c(range(Vertices_Plot$X)), ylim=(range(Vertices_Plot$Y)))
  # XYZWHRT_Data_N <- XYZWHRT_Data  .. , ylim = c(range(Vertices_Plot)), xlim = c(range(Vertices_Plot))
  return(Vertices)
}

VERTICIES_ORDER_FUN <- function(Vertices_Mx, Box_Levels = 4){

  # MAKE SURE BOX HAS VERTICES GOING IN CORRECT ORDER
  all_Box <- Vertices_Mx[1:4,]
  dist_oneBox <- rdist(all_Box[,1:2], all_Box[,1:2]) # rdist(row, col)
  Order_dist_oneBox_Largest <- apply(dist_oneBox, 1, which.max)
  if(!all(Order_dist_oneBox_Largest == c(3,4,1,2))){browser()} # DOM DOM DOM YOUR FIRST BOX IS NOT IN CORRECT ORDER

  CentX <- mean(all_Box[,1])
  CentY <- mean(all_Box[,2])
  Previous_Box <- all_Box

  for(BL in 2:Box_Levels){
    Index <- seq((((BL-1)*4)+1), ((BL-1)*4) + 4, 1)

    one_Box <- Vertices_Mx[Index,]
    one_Box <- cbind(BBOX_PNTS_FUN(Vertices_Mx[Index,1:2])[1:4,],Vertices_Mx[Index,3])
    dist_oneBox <- rdist(one_Box[,1:2], one_Box[,1:2]) # rdist(row, col)
    Order_dist_oneBox_Largest <- apply(dist_oneBox, 1, which.max)
    if(!all(Order_dist_oneBox_Largest == c(3,4,1,2))){browser()} # DOM DOM DOM... BOX IS NOT IN CORRECT ORDER

    # BETWEEN BOXES ORDER
    Dif_CentX <- mean(one_Box[,1]) - CentX
    Dif_CentY <- mean(one_Box[,2]) - CentY
    subBox_Adj <- one_Box

    subBox_Adj[,1] <- subBox_Adj[,1] - Dif_CentX
    subBox_Adj[,2] <- subBox_Adj[,2] - Dif_CentY

    dist <- rdist(Previous_Box[,1:2], subBox_Adj[,1:2]) # rdist(row, col)
    Order_dist_Smallest <- apply(dist, 1, which.min)
    dist_Smallest <- apply(dist, 1, min)

    one_Box <- one_Box[Order_dist_Smallest,]

    all_Box <- rbind(all_Box,one_Box)}
    Previous_Box <- one_Box
    CentX <- mean(Previous_Box[,1])
    CentY <- mean(Previous_Box[,2])

    return(all_Box)
    # browser()
}

PLOT_COL_FUN <- function(LAS_PLOT, Title, size = 2, TID_Number = "Yes", ShowGridID="Yes", Check_Plot = "Yes"){
  
  if(Check_Plot == "Yes"){
    nn <- length(unique(LAS_PLOT@data$TID))
    if(nn < 2000){
      palette <- sample(distinctColorPalette(nn)) 
    }else{
      palette <- sample(distinctColorPalette(1999))
      palette <- rep(palette, ceiling(nn/1999))
      palette <- 1:nn
    }
    
    # GENERATE ATTRIBUTE FOR COLOURING EACH POINT
    LAS_PLOT@data$Color <- as.character("")
    SHIFT_X <- min(LAS_PLOT$X)
    SHIFT_Y <- min(LAS_PLOT$Y)
    Color_ID <- data.frame(Unique_PSID = unique(LAS_PLOT@data$TID),
                           ID_PSID = palette)
    index_Color_PSID_1 <- which(LAS_PLOT@data$TID %in% Color_ID$Unique_PSID)
    index_Color_PSID_2 <- match(LAS_PLOT@data$TID[index_Color_PSID_1],
                                Color_ID$Unique_PSID)
    LAS_PLOT@data$Color[index_Color_PSID_1] <- as.character(Color_ID$ID_PSID[index_Color_PSID_2])
    #browser()
    #LAS_PLOT1 <- filter_poi(LAS_PLOT, PointSourceID > 0)
    plot(LAS_PLOT, color="Color", size = size) 
    bg3d("white")
    if(TID_Number == "Yes"){
      
      
      if(ShowGridID=="Yes"){
        XYZ <- as.data.frame(LAS_PLOT@data %>%
                               dplyr::group_by(TID) %>%
                               dplyr::summarise(Zmax = max(Z), 
                                                Zmin = min(Z),
                                                PID  = PID [which(Z == min(Z))[1]],
                                                Xminz = X[which(Z == min(Z))[1]], 
                                                Yminz = Y[which(Z == min(Z))[1]], 
                                                X = X[which(Z == max(Z))[1]], 
                                                Y = Y[which(Z == max(Z))[1]], 
                                                Color = Color[which(Z == max(Z))[1]], 
                                                .groups = 'drop'))
        # browser()
        text3d(x =(XYZ$X-SHIFT_X), y= (XYZ$Y-SHIFT_Y), z= XYZ$Zmax+1, XYZ$TID, col=XYZ$Color, pos=2)
        XYZ <- XYZ[which(XYZ$TID > 1),]
        if(nrow(XYZ)>0){
          text3d(x =(XYZ$X-SHIFT_X), y= (XYZ$Y-SHIFT_Y), z= XYZ$Zmax+1, paste( XYZ$PID ,  "_", XYZ$TID, sep="") , col="black", pos=3)
          text3d(x =(XYZ$Xminz-SHIFT_X), y= (XYZ$Yminz-SHIFT_Y), z= XYZ$Zmin, paste( XYZ$PID ,  "_", XYZ$TID, sep="") , col=XYZ$Color, pos=1)
        }
        
      }else{
        XYZ <- as.data.frame(LAS_PLOT@data %>%
                               dplyr::group_by(TID) %>%
                               dplyr::summarise(Zmax = max(Z), 
                                                X = X[which(Z == max(Z))[1]], 
                                                Y = Y[which(Z == max(Z))[1]], 
                                                Color = Color[which(Z == max(Z))[1]], 
                                                .groups = 'drop'))
        text3d(x =(XYZ$X-SHIFT_X), y= (XYZ$Y-SHIFT_Y), z= XYZ$Zmax+1, XYZ$TID, col=XYZ$Color, pos=2)
        text3d(x =(XYZ$X-SHIFT_X), y= (XYZ$Y-SHIFT_Y), z= XYZ$Zmax+1, XYZ$TID, col="black", pos=3)
        
      }
      
      
    }
    
    text3d(x =0, y= 0, z= max(LAS_PLOT$Z)+0.2*max(LAS_PLOT$Z), Title, col="black")
  } # ELSE DO NOT PLOT THIS ANYWHERE WHERE IT IS
  return(Color_ID)  
  }

EMPTY_VOX_FUN <- function(Vox_TID_Count, Para_Vox_Res, Para_MaxZ_Shrink) {

  X_Seq <- seq(min(Vox_TID_Count$X), max(Vox_TID_Count$X), Para_Vox_Res)
  Y_Seq <- seq(min(Vox_TID_Count$Y), max(Vox_TID_Count$Y), Para_Vox_Res)
  XY_Seq <- as.data.frame(tidyr::crossing(X_Seq, Y_Seq))
  XYZ_Seq <- XY_Seq[rep(seq_len(nrow(XY_Seq)), each = ((round(Para_MaxZ_Shrink)*(1/Para_Vox_Res))+1)), ] ## XYZ_Seq <- XY_Seq[rep(seq_len(nrow(XY_Seq)), each = Para_MaxZ_Shrink+1), ]
  XYZ_Seq$Z_Seq <- as.double(rep(seq(0,round(Para_MaxZ_Shrink), Para_Vox_Res), nrow(XY_Seq))) ## XYZ_Seq$Z_Seq <- as.double(rep(0:Para_MaxZ_Shrink, nrow(XY_Seq)))

  # SORT BY Z THEN Y AND FINALLY X (SO MOVES ACROSS X FIRST, THEN Y AND THEN Z)
  XYZ_Seq <- XYZ_Seq[order( XYZ_Seq[,3], XYZ_Seq[,2], XYZ_Seq[,1]),]
  colnames(XYZ_Seq) <-  c("X", "Y", "Z")

  # FIND THE EMPY VOXELS THAT NEED TO BE ADDED TO LAS
  Empty_Voxels <- anti_join(XYZ_Seq,Vox_TID_Count[,1:3])
  Empty_Voxels$Count <- 0
  Empty_Voxels$TID <- as.integer(0)
  return(Empty_Voxels)
 }

SCALE_LAS_FUN <- function(las, Scale_Factor, Offset_Factor_X, Offset_Factor_Y, Offset_Factor_Z) {

  las@header@PHB[["X scale factor"]] <- Scale_Factor
  las@header@PHB[["Y scale factor"]] <- Scale_Factor
  las@header@PHB[["Z scale factor"]] <- Scale_Factor
  las@header@PHB[["X offset"]] <- Offset_Factor_X
  las@header@PHB[["Y offset"]] <- Offset_Factor_Y
  las@header@PHB[["Z offset"]] <- Offset_Factor_Z

  xmin <- las@header@PHB[["Min X"]]
  ymin <- las@header@PHB[["Min Y"]]
  zmin <- las@header@PHB[["Min Z"]]
  xmax <- las@header@PHB[["Max X"]]
  ymax <- las@header@PHB[["Max Y"]]
  zmax <- las@header@PHB[["Max Z"]]

  projection(las) <- CRS()
  las@header@VLR <- list()

  return(las)
}

XYZWLHR_To_XYZWLHR_N_FUN <- function(XYZWLHR = XYZWLHR_oneF_oneMP_oneP_allPrior, Shift, Para_Cnt = Para_Cnt){

  XYZWLHR$X_Base <- (XYZWLHR$X_Base- Shift$Shift_Vox_X)/Shift$Shift_Vox_N_X
  XYZWLHR$Y_Base  <- (XYZWLHR$Y_Base- Shift$Shift_Vox_Y)/Shift$Shift_Vox_N_Y

  # if(Para_Cnt == 16){
  #   XYZWLHR$X_BotBox <- (XYZWLHR$X_BotBox- Shift$Shift_Vox_X)/Shift$Shift_Vox_N_X
  #   XYZWLHR$Y_BotBox <- (XYZWLHR$Y_BotBox- Shift$Shift_Vox_Y)/Shift$Shift_Vox_N_Y
  #
  #   XYZWLHR$L_BotBox <- (XYZWLHR$L_BotBox)/Shift$Shift_Vox_N_Y
  #   XYZWLHR$W_BotBox <- (XYZWLHR$W_BotBox)/Shift$Shift_Vox_N_X
  #   XYZWLHR$R_BotBox <- (XYZWLHR$R_BotBox)/180
  #   XYZWLHR$Z_BotBox  <- (XYZWLHR$Z_BotBox - Shift$Shift_Vox_Z)/Shift$Shift_Vox_N_Z
  # }


  XYZWLHR$X_TopBox <- (XYZWLHR$X_TopBox- Shift$Shift_Vox_X)/Shift$Shift_Vox_N_X
  XYZWLHR$Y_TopBox <- (XYZWLHR$Y_TopBox- Shift$Shift_Vox_Y)/Shift$Shift_Vox_N_Y

  XYZWLHR$L_TopBox <- (XYZWLHR$L_TopBox)/Shift$Shift_Vox_N_Y
  XYZWLHR$W_TopBox <- (XYZWLHR$W_TopBox)/Shift$Shift_Vox_N_X
  XYZWLHR$R_TopBox <- (XYZWLHR$R_TopBox)/180

  XYZWLHR$Z_Base <- (XYZWLHR$Z_Base- Shift$Shift_Vox_Z)/Shift$Shift_Vox_N_Z

  XYZWLHR$Z_TopBox <- (XYZWLHR$Z_TopBox- Shift$Shift_Vox_Z)/Shift$Shift_Vox_N_Z
  XYZWLHR$Z_TopTree <- (XYZWLHR$Z_TopTree- Shift$Shift_Vox_Z)/Shift$Shift_Vox_N_Z
  #browser()
  return(XYZWLHR)
}


######################################################################################################################################################
######################################################################################################################################################
# MERGE_TID_FUN
######################################################################################################################################################
######################################################################################################################################################

MERGE_TID_FUN <- function(LAS_oneP, Merge_Trees_From, REMOVE_Plot, oneP_PlotID, MP, FID, Check_Plot, THIN_OR_ALL_LAS){
  # browser()
  if(THIN_OR_ALL_LAS == "Thin"){
    Dir_Prefix <- paste("/LAS_P", "/F", FID, "_MP", MP,"_P" , sep="")
    Dir_Suffix <- ".laz"
  }else{
    Dir_Prefix <- paste("/LAS_P_ALL", "/F",FID,"_P", sep="")
    Dir_Suffix <- "_allT_AttTIDallLAS.laz"
  }
    
  ########################################
  # LOOP THROUGH TREES THAT ARE REASSIGNED
  ######################################## 
  for(MM in 1:length(Merge_Trees_From)){ 
    print("IN REMOVE_Plot ... Merge_Trees_From...MM")
    oneMerge_Trees_From <- Merge_Trees_From[MM]
    oneMerge_Trees_To <-Merge_Trees_To[MM]
    From_Merge_ID <- as.numeric(numextract_all(oneMerge_Trees_From))
    To_Merge_ID <- as.numeric(numextract_all(oneMerge_Trees_To))
    
    # OPEN PLOT THAT TREE GETS MOVED TO AND ADD THE TREE BEFORE OVERWRITING THE FILE WITH NEW TREE
    if(To_Merge_ID[1] != oneP_PlotID){
      LAS_oneP_To <- readLAS(paste(FOLDER_LAS, Dir_Prefix, To_Merge_ID[1], Dir_Suffix, sep=""), select = "xyzp0")
      
      LAS_oneP_To@data$Grid_ID <- as.numeric(To_Merge_ID[1])
    } else{
      LAS_oneP_To <- LAS_oneP
    }
    Title <- paste("MOVING_TID ...: ", From_Merge_ID[1], "_" , From_Merge_ID[2], " To ", To_Merge_ID[1],"_" ,To_Merge_ID[2])

    if(From_Merge_ID[1] != oneP_PlotID){
      # THIS SHOULD NOT HAPPEN. OTHERWISE ITS AN ERROR!!!
      print("THIS SHOULD NOT HAPPEN: From_Merge_ID[1] NEEDS TO BE SUBJECT PLOT !!!")
      Error 
    }
    
    # EXTRACT THE ONE TREE THAT YOU WANT TO MOVE AND GIVE NEW TID VALUE
    LAS_oneP_oneT <- filter_poi(LAS_oneP, TID == as.numeric(From_Merge_ID[2]))
    LAS_oneP_oneT@data$Grid_ID <- as.numeric(To_Merge_ID[1])
    LAS_oneP_oneT@data$TID <- as.numeric(To_Merge_ID[2])
    #browser()
    
    if(From_Merge_ID[1] != To_Merge_ID[1]){ # IF CHANGING TID WITHIN DIFFERENT
      # browser()
      # UPDATE PLOT IF REMOVED FROM
      LAS_oneP <- filter_poi(LAS_oneP, TID != as.numeric(From_Merge_ID[2]))
      one_DIR <- paste(FOLDER_LAS, Dir_Prefix ,From_Merge_ID[1],Dir_Suffix, sep="")
      
      
      print(paste("UPDATING: ", one_DIR))
      writeLAS(LAS_oneP, one_DIR)
      LAS_oneP_To <- rbind(LAS_oneP_To, LAS_oneP_oneT)
      one_DIR <- paste(FOLDER_LAS, Dir_Prefix ,To_Merge_ID[1], Dir_Suffix, sep="")
      print(paste("UPDATING: ", one_DIR))
      writeLAS(LAS_oneP_To, one_DIR)
    }else{
      LAS_oneP_To_Removed_TID <-filter_poi(LAS_oneP_To, TID != as.numeric(From_Merge_ID[2]))
      LAS_oneP <- rbind(LAS_oneP_To_Removed_TID, LAS_oneP_oneT)
      one_DIR <-paste(FOLDER_LAS, Dir_Prefix, To_Merge_ID[1], Dir_Suffix, sep="")
      print(paste("UPDATING: ", one_DIR))
      writeLAS(LAS_oneP, one_DIR)
    }
    #PLOT_COL_FUN(LAS_oneP, Title="MERGE TID", size = 10, TID_Number = "Yes", ShowGridID="Yes", Check_Plot= Check_Plot)
    
    # print(Title)
    # PLOT_COL_FUN(LAS_oneP, Title, size = 10, TID_Number = "Yes", ShowGridID="Yes", Check_Plot= Check_Plot)
    
    DF_MERGE_TID[(nrow(DF_MERGE_TID)+1),] <- c(From_Merge_ID[1], From_Merge_ID[2], To_Merge_ID[1],  To_Merge_ID[2]  )  # Merge_From_PlotID, Merge_From_TID, Merge_To_PlotID, Merge_To_TID
    
  } # END MM LOOP
  
  
  ### DOM DOM DOM THE OUTPUT SHOULD BE THE UPDATE LAS_oneP
  return(list(DF_MERGE_TID, LAS_oneP))
}

  ######################################################################################################################################################
  ######################################################################################################################################################
  # SPLIT_TREES_FUN
  ######################################################################################################################################################
  ######################################################################################################################################################
  
SPLIT_TREES_FUN <- function(Method = "Vox" , LAS_oneP, Split_Trees_From, Split_Trees_To, Split_Trees_Retain, MP, FID, oneP_PlotID, FOLDER_LAS, Check_Plot, THIN_OR_ALL_LAS, Para_Vx_R_Res){  

  if(THIN_OR_ALL_LAS == "Thin"){
    Dir_Prefix <- paste("/LAS_P", "/F", FID, "_MP", MP,"_P" , sep="")
    Dir_Suffix <- ".laz"
  }else{
    Dir_Prefix <- paste("/LAS_P_ALL", "/F",FID,"_P", sep="")
    Dir_Suffix <- "_allT_AttTIDallLAS.laz"
  }
  
  Dir_LAS_oneP <- paste(FOLDER_LAS, Dir_Prefix ,oneP_PlotID, Dir_Suffix, sep="")
  
  print("IN Split_Trees_From")
  
  # LOOP THROUGH EACH TID NEEDING SPLIT
  LAS_oneP_Updated <- LAS_oneP
  for(SS in 1:length(Split_Trees_From)){
    #print(paste("SS", SS) )
    LAS_oneP <- LAS_oneP_Updated
    # LOOP THROUGH EACH TID NEEDING TO BE SPLIT
    
    LIST_LAS_Neigh_REST <- c()
    LIST_DIR_LAS_Neigh_REST <- c()
    LIST_Split_Neigh_TID <- c()
    LIST_Split_Plot <- c()
    
    oneSplit_Trees_Retain <- Split_Trees_Retain[SS]
    oneSplit_ID_From <- numextract_all(Split_Trees_From[SS])
    oneSplit_PlotID_From <- oneSplit_ID_From[1]
    oneSplit_TID_From <- oneSplit_ID_From[2]
    
    # SEPERATE TID FOR SPLITTING (REMOVE FROM LAS_oneP)
    LAS_Split_oneTID_From <- filter_poi(LAS_oneP, TID == oneSplit_TID_From)
    LAS_oneP_Updated <- filter_poi(LAS_oneP_Updated, TID != oneSplit_TID_From)
    
    Split_Trees_To_UNLIST <- unlist(Split_Trees_To[[SS]])
    Check_Subject_Plot_in_List <- 1
    # LOOP THROUGH NEAREST TID THAT WILL RECEIVE SPLIT TID
    all_PlotID_To <- c()
  
    for(SSS in 1:length(Split_Trees_To_UNLIST)){
      #print(paste("SSS", SSS) )
      oneSplit_Tree_To <- Split_Trees_To_UNLIST[SSS]
      oneN_oneSplit_To <- numextract_all(oneSplit_Tree_To)
      oneN_oneSplit_PlotID_To <- oneN_oneSplit_To[1]
      all_PlotID_To <- c(all_PlotID_To, oneN_oneSplit_PlotID_To)
      oneN_oneSplit_TID_To <- oneN_oneSplit_To[2]
      
      ###############################
      # GENERATE LISTS FOR PROCESSING
      ###############################
      
      # IF THE SPLIT TID IS ADDED TO DIFFERENT PLOT
      
      if(oneN_oneSplit_PlotID_To != oneSplit_PlotID_From ){
        
        # IF PLOT HAS ALREADY BEEN ACCESSED THEN EXTRACT FROM LIST OTHERWISE OPEN IT:
        if(oneN_oneSplit_PlotID_To %in% LIST_Split_Plot){
          # GET NEIGHBOUR PLOT THAT HAS HAD ANOTHER TID REMOVED FROM IT ALREADY
          Index_LIST <- which(LIST_Split_Plot %in% oneN_oneSplit_PlotID_To)
          LAS_Split_Neigh <- LIST_LAS_Neigh_REST[[Index_LIST]]
          
          # REMOVE TID FROM PLOT THAT ALREADY HAD TID REMOVED (UPDATE LAS AND ALSO UPDATE TIDs REMOVED FROM THAT LAS)
          LAS_Split_TID_Neigh_To <- filter_poi(LAS_Split_Neigh, TID == oneN_oneSplit_TID_To)
          LAS_Split_TID_Neigh_To_REST <- filter_poi(LAS_Split_Neigh, TID != oneN_oneSplit_TID_To) 
          LIST_LAS_Neigh_REST[[Index_LIST]] <- LAS_Split_TID_Neigh_To_REST
          LIST_Split_Neigh_TID[[Index_LIST]] <- c(LIST_Split_Neigh_TID[Index_LIST], oneN_oneSplit_TID_To)
          
        }else{
          # GET NEW NEIGH PLOT AND REMOVE TID THAT SPLIT ATTACHES TO
          Dir_LAS_Neigh <- paste(FOLDER_LAS,Dir_Prefix, oneN_oneSplit_PlotID_To, Dir_Suffix, sep="")

          LAS_Split_Neigh <- readLAS(Dir_LAS_Neigh, select = "xyzp0")
          LAS_Split_Neigh@data$Grid_ID <- as.numeric(oneN_oneSplit_PlotID_To)
          
          # REMOVE TID 
          LAS_Split_TID_Neigh_To <- filter_poi(LAS_Split_Neigh, TID == oneN_oneSplit_TID_To)
          LAS_Split_TID_Neigh_To_REST <- filter_poi(LAS_Split_Neigh, TID != oneN_oneSplit_TID_To) 
          
          # KEEP TRACK OF WHAT HAS BEEN REMOVED
          LIST_LAS_Neigh_REST <- c(LIST_LAS_Neigh_REST, list(LAS_Split_TID_Neigh_To_REST))
          LIST_DIR_LAS_Neigh_REST <- c(LIST_DIR_LAS_Neigh_REST, Dir_LAS_Neigh)
          LIST_Split_Plot <- c(LIST_Split_Plot, oneN_oneSplit_PlotID_To)
          LIST_Split_Neigh_TID[[length(LIST_Split_Neigh_TID)+1]] <- oneN_oneSplit_TID_To #LIST_Split_Neigh_TID <- c(LIST_Split_Neigh_TID, oneN_oneSplit_TID_To)
        }
        
      }else{
        Check_Subject_Plot_in_List <- 2
        # IF TID FOR ATTACHING IS IN THE SUBJECTS (SPLIT TID) PLOT  .... (REMOVE IT FROM UPDATED PLOT AS IT WILL BE PUT BACK IN LATER ON )
        LAS_Split_TID_Neigh_To <- filter_poi(LAS_oneP, TID == oneN_oneSplit_TID_To)
        LAS_oneP_Updated <- filter_poi(LAS_oneP_Updated, TID != oneN_oneSplit_TID_To)   
        
        # IF PLOT HAS ALREADY BEEN ACCESSED THEN EXTRACT FROM LIST OTHERWISE CREATE NEW LIST ITEM
        # KEEP TRACK OF WHAT HAS BEEN REMOVED:
        if(oneP_PlotID %in% LIST_Split_Plot){
          Index_LIST <- which(LIST_Split_Plot %in% oneP_PlotID)
          LAS_Split_Neigh <- LIST_LAS_Neigh_REST[[Index_LIST]]
          
          LIST_LAS_Neigh_REST[[Index_LIST]] <- LAS_oneP_Updated
          LIST_Split_Neigh_TID[[Index_LIST]] <- c(LIST_Split_Neigh_TID[Index_LIST], oneN_oneSplit_TID_To)
          
        }else{
          LIST_LAS_Neigh_REST <- c(LIST_LAS_Neigh_REST, list(LAS_oneP_Updated))
          LIST_DIR_LAS_Neigh_REST <- c(LIST_DIR_LAS_Neigh_REST, Dir_LAS_oneP)
          LIST_Split_Plot <- c(LIST_Split_Plot, oneP_PlotID)
          LIST_Split_Neigh_TID[[length(LIST_Split_Neigh_TID)+1]] <- oneN_oneSplit_TID_To #c(LIST_Split_Neigh_TID, oneN_oneSplit_TID_To)
        }
      }
      
      # END GENERATE LISTS FOR PROCESSING
      ##############################################
      
      
      # GENERATE A LAYER WITH ALL THE TID THAT WILL BE USED TO DISTRIBUTING POINTS.
      LAS_Split_oneTID_From <- rbind(LAS_Split_oneTID_From, LAS_Split_TID_Neigh_To)
      
      # UPDATE DATAFRAME FOR TRACKING CHANGES
      DF_SPLIT_TID[(nrow(DF_SPLIT_TID)+1),] <- c(oneSplit_PlotID_From, oneSplit_TID_From, oneN_oneSplit_PlotID_To, oneN_oneSplit_TID_To, oneSplit_Trees_Retain)
    } # LOOP THROUGH TIDS 

    
    ###############################################################################################################################################################
    # COMPUTING THE REPARTITIONING OF SPLIT LAS POINTS TO NEIGHBOURING TID
    ######################################################################
    # print("Repartitioning")
    # browser()
    # GET LOWEST POINT OF EACH TID INVOLVED IN SPLIT 
    # (ALL LAS BELOW THE MIN OF THE SPLIT TID WILL BE USED TO GENERATE RASTER AND EACH PIXEL OF THIS RASTER WILL BE USED IN RDIST CALCULATION)
    XYZ_Split_TID <- as.data.frame( LAS_Split_oneTID_From@data %>%
                                      dplyr::group_by(TID, Grid_ID) %>%
                                      dplyr::summarise(Zmin = min(Z),
                                                       X_Zmin = X[which(Z == Zmin)],
                                                       Y_Zmin = Y[which(Z == Zmin)],
                                                       .groups = 'drop'))
    
    minZ_Split_TID <- max(XYZ_Split_TID$Zmin) # XYZ_Split_TID$Zmin[which(XYZ_Split_TID$TID == oneSplit_TID_From)]
    
    # RASTERISE THE TID THAT WILL BE USED FOR DISTRIBUTING SPLIT TID
    R_TID <-  grid_metrics(LAS_Split_oneTID_From, ~as.numeric(names(table(TID)[which.max(table(TID))])), res = 0.5)
    R_minZ <-  grid_metrics(LAS_Split_oneTID_From, min(Z), res = 0.5) # MAKE IT THE MOST COMMON TID VALUE
    R_Binary <- R_TID/R_TID
    R_GRID_ID <-  grid_metrics(LAS_Split_oneTID_From, ~as.numeric(names(table(Grid_ID)[which.max(table(Grid_ID))])), res = 0.5)
    
    R_Stack <- raster::stack(R_TID, R_minZ, R_Binary, R_GRID_ID)
    
    # GENERATE DATAFRAME OF THE RASTER VALUES
    DF_Values <- as.data.frame(values(R_Stack))
    colnames(DF_Values) <- c("TID", "minZ", "New_TID", "Grid_ID")
    DF_XY <-as.data.frame(coordinates(R_Stack))
    DF_XY_Values <- na.omit(cbind(DF_XY, DF_Values))
    
    # browser()
    # PREPARE THE PIXEL XY COORDINATES THAT WILL BE USED IN DISTANCE CALCULATION
    if(oneSplit_Trees_Retain == "Y"){
      DF_XY_below_min <- DF_XY_Values[which(DF_XY_Values$minZ <= minZ_Split_TID),]
      DF_XY_Rest <- DF_XY_Values[-which(DF_XY_Values$minZ <= minZ_Split_TID),]
      
    }else{
      #browser()
      minZ_Split_TID_NotSplit <- max(XYZ_Split_TID$Zmin[which(XYZ_Split_TID$TID!= oneSplit_TID_From)])
      DF_XY_below_min <- DF_XY_Values[which(DF_XY_Values$minZ <= minZ_Split_TID_NotSplit & DF_XY_Values$TID != oneSplit_TID_From),]
      DF_XY_Rest <- DF_XY_Values[-which(DF_XY_Values$minZ <= minZ_Split_TID & DF_XY_Values$TID != oneSplit_TID_From),]
      
      # DF_XY_below_min <- DF_XY_Values[which(DF_XY_Values$minZ < XYZ_Split_TID$Zmin[which(XYZ_Split_TID$TID == oneSplit_TID_From)]),]
      # DF_XY_Rest <- DF_XY_Values[-which(DF_XY_Values$minZ < XYZ_Split_TID$Zmin[which(XYZ_Split_TID$TID == oneSplit_TID_From)]),]
    }
    
    # MAKING SURE TID VALUES ARE CORRECT FOR THE MIN POINTS 
    Dist_belowminTID <- rdist(DF_XY_below_min[,1:2], XYZ_Split_TID[,4:5])
    Dist_XYZ_Split_TID <- apply(Dist_belowminTID, 1, which.min)
    DF_XY_below_min$TID <- XYZ_Split_TID$TID[Dist_XYZ_Split_TID]
    
    # PERFORM DISTANCE CALCULATION AND CHANGE TID VALUE ACCORDINGLY
    Dist <- rdist(DF_XY_below_min[,1:2], DF_XY_Rest[,1:2])
    Dist_min_to_Rest <- apply(Dist, 2, which.min)
    New_TID <- DF_XY_below_min$TID[Dist_min_to_Rest]
    DF_XY_Rest$New_TID <- DF_XY_Rest$TID
    DF_XY_Rest$New_TID[which(DF_XY_Rest$TID == oneSplit_TID_From)] <- New_TID[which(DF_XY_Rest$TID == oneSplit_TID_From)]
    DF_XY_below_min$New_TID <- DF_XY_below_min$TID
    all_DF_XY_newTID <- rbind(DF_XY_Rest, DF_XY_below_min)
    
    # GENERATE RASTER AND PERFORM SPATIAL MERGE OF NEW TID VALUES
    xy_newTID_SP <- SpatialPointsDataFrame(coords = all_DF_XY_newTID[,1:2], data = all_DF_XY_newTID[,c(3:6)])
    #
    Index_1 <- which(xy_newTID_SP$New_TID %in%  XYZ_Split_TID$TID )
    Index_2 <- match(xy_newTID_SP$New_TID[Index_1],  XYZ_Split_TID$TID )
    xy_newTID_SP$Grid_ID[Index_1] <- as.integer(XYZ_Split_TID$Grid_ID[Index_2])
    
    # # UPDATE THE GRID IDS.
    R_newTID_Update <- rasterize(xy_newTID_SP, R_Binary, field="New_TID")
    R_new_GRID_ID_Update <- rasterize(xy_newTID_SP, R_Binary, field="Grid_ID")
    #
    oneTID_LAS_Split_TID <- filter_poi(LAS_Split_oneTID_From, TID ==  oneSplit_TID_From)
    LAS_Split_TID_RemoveOne <- filter_poi(LAS_Split_oneTID_From, TID !=  oneSplit_TID_From) 
    #
    oneTID_LAS_Split_TID<- merge_spatial(oneTID_LAS_Split_TID, R_newTID_Update, attribute = "TID")
    oneTID_LAS_Split_TID<- merge_spatial(oneTID_LAS_Split_TID, R_new_GRID_ID_Update, attribute = "Grid_ID")
    #
    LAS_Split_newTID<- rbind(LAS_Split_TID_RemoveOne, oneTID_LAS_Split_TID)
    #
    PLOT_COL_FUN(LAS_Split_newTID, Title="NEW TID", size = 2, TID_Number = "Yes", ShowGridID="Yes", Check_Plot= Check_Plot)
    #
    Shift_X <- min(LAS_Split_newTID@data$X)
    Shift_Y <-min(LAS_Split_newTID@data$Y)
    
    if(Check_Plot == "Yes"){
      text3d(DF_XY_below_min[,1]-Shift_X, DF_XY_below_min[,2]-Shift_Y, minZ_Split_TID, DF_XY_below_min[,3]  )
    }
    
    # browser()
    # END STANDARD REPARTITIONING USING SPLIT
    
    ###############################################################################################################################################################
    # PERFORM VOXELISATION PROCEDURE FOR PARTITIONING
    ################################
    # print("VOXELISING")
    # browser()
    # START THE VOXELISATION, CLUSTERING AND REPARTITIONING
    if(Method == "Vox"){
      LAS_Split_newTID <- VOX_CLUSTERING_FUN(LAS_Split_newTID, Para_Vx_R_Res, Para_EDT_V = Para_Vx_R_Res*4, Para_d_clust)
      # if(length(which(colnames(LAS_Split_newTID@data) == "PID") == 0)){
      #   LAS_Split_newTID@data$PID <- oneP_PlotID #### DOM DOM DOM !!! THIS WAS ADDED IN DURING THE FIXING PROCEDURE ... IT MAY CREATE ISSUES IN FUTURE 
      # }
      
      }
    
    
    # END REPARTITIONING WITH SPLIT
    #######################################################################
    ###############################################################################################################################################################
    
    ###############################################################################################################################################################
    
    #####################################
    # UPDATE THE LAS FILE AND THE OUTPUT
    ##################################### 

    #Flag <- 33; print(paste("Flag: ", Flag))
    # 
    # browser()
    # UPDATE SUBJECT PLOTS IF ITS NOT IN THE SPLIT PLOTS TO (i.e. plot in "Split_Trees_From" is not in "Split_Trees_To")
    if(oneP_PlotID %in% unique(as.numeric(all_PlotID_To)) | oneSplit_Trees_Retain == "Y"){
      #browser()
      LAS_Split_subPlot_NewTID <- filter_poi(LAS_Split_newTID, Grid_ID == oneSplit_PlotID_From)
      LAS_oneP_Updated <- rbind(LAS_oneP_Updated, LAS_Split_subPlot_NewTID)
      #

      print(paste("UPDATING: ", Dir_LAS_oneP))
      writeLAS(LAS_oneP_Updated, Dir_LAS_oneP)
    }
    
    if(oneSplit_Trees_Retain == "N"){
      # #browser()
      # LAS_Split_subPlot_NewTID <- filter_poi(LAS_Split_newTID, Grid_ID == oneSplit_PlotID_From)
      # LAS_oneP_Updated <- rbind(LAS_oneP_Updated, LAS_Split_subPlot_NewTID)
      # #
      print(paste("UPDATING: ", Dir_LAS_oneP))
      writeLAS(LAS_oneP_Updated, Dir_LAS_oneP)
    }
   
    # UPDATE NEIGHBOURING PLOTS
    if(length(LIST_LAS_Neigh_REST)>0){
      # FIX GRID ID
      for(LL in 1:length(LIST_LAS_Neigh_REST)){
        #browser()
        if(LIST_Split_Plot[[LL]] != oneP_PlotID){
          # LAS_Split_newTID@data$Grid_ID[which(LAS_Split_newTID@data$TID %in% LIST_Split_Neigh_TID[[LL]])] <- LIST_Split_Plot[[LL]]
          LAS_Split_Plot_NewTID_Add <- filter_poi(LAS_Split_newTID, Grid_ID == LIST_Split_Plot[[LL]])
          oneLAS <- LIST_LAS_Neigh_REST[[LL]]
          oneLAS <- rbind(oneLAS, LAS_Split_Plot_NewTID_Add)
          one_DIR <- LIST_DIR_LAS_Neigh_REST[[LL]]
          print(paste("UPDATING: ", one_DIR))
          # browser()
          writeLAS(oneLAS, one_DIR)
          # PLOT_COL_FUN(oneLAS, Title="AFTER Split Plot", size = 10, TID_Number = "Yes", ShowGridID="Yes", Check_Plot= Check_Plot)
        }


      } # END LL LOOP
    } # IF STATEMENT FOR LL LOOP
  } # LOOP SS  ....   SPLIT TID
  
  print("END SPLIT")
  return(list(DF_SPLIT_TID, LAS_oneP_Updated)) # , max_TID_Value
}

FILL_ZEROS_FUN <- function(LAS_oneP, Fill_Zeros_Below, MP, FID, oneP_PlotID, FOLDER_LAS, Check_Plot, THIN_OR_ALL_LAS,
                           Para_C_Base_Thresh, Para_C_Base_BW){  

  if(THIN_OR_ALL_LAS == "Thin"){
    Dir_Prefix <- paste("/LAS_P", "/F", FID, "_MP", MP,"_P" , sep="")
    Dir_Suffix <- ".laz"
  }else{
    Dir_Prefix <- paste("/LAS_P_ALL", "/F",FID,"_P", sep="")
    Dir_Suffix <- "_allT_AttTIDallLAS.laz"
  }
  
  LAS_Zeros <- filter_poi(LAS_oneP, TID == 0)
  LAS_oneP_notZeros <- filter_poi(LAS_oneP, TID != 0)
  print("IN FILL_ZEROS")
  
  FILL_ZERO_TID <- as.numeric(numextract_all(Fill_Zeros_Below)[c(FALSE,TRUE)])
  # browser()
  LAS_FILL_ZERO_oneTID<- filter_poi(LAS_oneP, TID %in% FILL_ZERO_TID)
  #R_TID <-  grid_metrics(LAS_FILL_ZERO_oneTID, ~as.numeric(names(table(TID)[which.max(table(TID))])), res = 2)
  R_TID <-  grid_metrics(LAS_FILL_ZERO_oneTID, ~as.numeric(TID[which.min(Z)]), res = 2)
  R_TID[is.na(R_TID)] <- 0
  
  LAS_FILL_ZERO_oneTID<- merge_spatial(LAS_Zeros, R_TID, attribute = "TID")
  LAS_FILL_ZERO_oneTID@data$TID[is.na(LAS_FILL_ZERO_oneTID@data$TID)] <- 0
  # LAS_FILL_ZERO_oneTID@data$TID[which(LAS_FILL_ZERO_oneTID$Z < 4)] <- 0

  # browser()
  LAS_oneP_Temp <- rbind(LAS_oneP_notZeros, LAS_FILL_ZERO_oneTID)
  LAS_oneP_Temp_allT <- filter_poi(LAS_oneP_Temp, (TID %in% FILL_ZERO_TID))
  LAS_oneP_Updated <- filter_poi(LAS_oneP_Temp, !(TID %in% FILL_ZERO_TID))
   
  for(TZ in 1:length(FILL_ZERO_TID)){
    # CORRECTING THE TID IN UNDERSTOREY
    LAS_oneP_oneT_FillZero <-  filter_poi(LAS_oneP_Temp, TID %in% FILL_ZERO_TID[TZ])
    Profile_oneT <- GAP_DENSITY_FUNCTION_NEW2(LAS_oneP_oneT_FillZero$Z,
                                              Para_BW = Para_C_Base_BW ,
                                              Para_Threshold_Percent = Para_C_Base_Thresh ,
                                              Plot = "No",
                                              Plot_Heading = paste("FillZero P:", oneP_PlotID,  sep=""))
    
    LAS_oneP_oneT_Stem <- filter_poi(LAS_oneP_oneT_FillZero, Z >= Profile_oneT$Start_Largest_Gap &  Z <= Profile_oneT$End_Largest_Gap)
    
    Order_Z <- order(LAS_oneP_oneT_Stem$Z)
    X_mean <- mean(LAS_oneP_oneT_Stem$X[Order_Z[which(Order_Z < 25)]])
    Y_mean <- mean(LAS_oneP_oneT_Stem$Y[Order_Z[which(Order_Z < 25)]])

    Height <- Profile_oneT$Start_Largest_Gap + (0/5* (Profile_oneT$End_Largest_Gap - Profile_oneT$Start_Largest_Gap))
    Index_Above <- which(LAS_oneP_oneT_FillZero$Z >=   Height)
    Index_Below <- which(LAS_oneP_oneT_FillZero$Z <  Height &
                           LAS_oneP_oneT_FillZero$Y > (Y_mean - 1) &
                           LAS_oneP_oneT_FillZero$Y < (Y_mean + 1) &
                           LAS_oneP_oneT_FillZero$X > (X_mean - 1) &
                           LAS_oneP_oneT_FillZero$X < (X_mean + 1))

    print(paste("Index_Below:", length(Index_Below), "Start:", Profile_oneT$Start_Largest_Gap, "End:", Profile_oneT$End_Largest_Gap))
    
    if(length(Index_Below) > 0){
      LAS_oneP_oneT_FillZero@data$TID <- 0
      LAS_oneP_oneT_FillZero@data$TID[c(Index_Below, Index_Above)] <- FILL_ZERO_TID[TZ] 
      }

    LAS_oneP_Updated <- rbind(LAS_oneP_Updated, LAS_oneP_oneT_FillZero)
  }
  
  


  

  
  one_DIR <-   paste(FOLDER_LAS, Dir_Prefix, oneP_PlotID, Dir_Suffix, sep="")
  print(paste("UPDATING: ", one_DIR))
  writeLAS(LAS_oneP_Updated, one_DIR)
  
  PLOT_COL_FUN(LAS_oneP_Updated, Title=paste("FILL_ZERO_F", oneP_PlotID), size = 2, TID_Number = "Yes", ShowGridID="Yes", Check_Plot= Check_Plot)
  
  # KEEP TRACK OF CHANGES
  for(DF in 1:length(FILL_ZERO_TID)){
    DF_BASE_FILL_ZERO[(nrow(DF_BASE_FILL_ZERO)+1),] <- c(oneP_PlotID, FILL_ZERO_TID[DF])
  }
  
  return(list(DF_BASE_FILL_ZERO, LAS_oneP_Updated))
}
    
BASE_SPLIT_FUN <- function(Method = "Vox", LAS_oneP, Base_Split, Base_Count, Max_TID_ID, MP, FID, oneP_PlotID, FOLDER_LAS, Check_Plot, 
                           THIN_OR_ALL_LAS, Para_Vx_R_Res, Para_C_Base_BW, Para_C_Base_Thresh){   

  if(THIN_OR_ALL_LAS == "Thin"){
    Dir_Prefix <- paste("/LAS_P", "/F", FID, "_MP", MP,"_P" , sep="")
    Dir_Suffix <- ".laz"
  }else{
    Dir_Prefix <- paste("/LAS_P_ALL", "/F",FID,"_P", sep="")
    Dir_Suffix <- "_allT_AttTIDallLAS.laz"
  }
  
  for(BB in 1:length(Base_Split)){
    Base_Split_TID <- as.numeric(numextract_all(Base_Split[BB])[c(FALSE,TRUE)])
    LAS_Base_TID <- filter_poi(LAS_oneP, TID == Base_Split_TID)
    LAS_oneP_Updated <- filter_poi(LAS_oneP, TID != Base_Split_TID)
    # FIND HEIGHT WHEN THERE ARE TWO STEMS

   
    # Profile_oneT <- GAP_DENSITY_FUNCTION_NEW2(LAS_Base_TID$Z,
    #                                           Para_BW = Para_C_Base_BW ,
    #                                           Para_Threshold_Percent = Para_C_Base_Thresh ,
    #                                           Plot = "Yes",
    #                                           Plot_Heading = paste("FillZero P:", oneP_PlotID,  sep=""))
    #
    # LAS_Base_TID_minZ <- filter_poi(LAS_Base_TID, Z >= Profile_oneT$Start_Largest_Gap &  Z <= Profile_oneT$End_Largest_Gap)
#
    minZ_BS <- min(LAS_Base_TID$Z)
    LAS_Base_TID_minZ <- filter_poi(LAS_Base_TID, Z < (minZ_BS+7) & Z > (minZ_BS+3) )

    
    XYZ_coords <-LAS_Base_TID_minZ@data[,1:3] 
    Cluster <- kmeans(XYZ_coords[,1:2], Base_Count[BB])
    Cluster <- Cluster$cluster
    XYZ_coords <- cbind(XYZ_coords, Cluster)
    colnames(XYZ_coords)[1:3] <- c("x", "y", "z")
    
    XY_Base <- as.data.frame( XYZ_coords %>%
                                dplyr::group_by(Cluster) %>%
                                dplyr::summarise(Z = min(z),
                                                 X= x[which(z  == Z)][1],
                                                 Y= y[which(z  == Z)][1],
                                                 .groups = 'drop'))
    # browser()
    XY_Base <- XY_Base[,c(3,4,2)]
    # R_minZ <-  grid_metrics(LAS_Base_TID_minZ, ~ min(Z), res = 0.5)
    # browser()
    # Clumps <- clump(R_minZ, directions=4)
    # 
    # # GENERATE DATAFRAME OF THE RASTER VALUES
    # R_Stack <- raster::stack(R_minZ, Clumps)
    # DF_Values <- as.data.frame(values(R_Stack))
    # colnames(DF_Values) <- c( "minZ", "Clumps")
    # DF_XY <-as.data.frame(coordinates(R_Stack))
    # DF_XY_Values <- na.omit(cbind(DF_XY, DF_Values))
    # 
    # XY_Base <- as.data.frame( DF_XY_Values %>%
    #                             dplyr::group_by(Clumps) %>%
    #                             dplyr::summarise(Z = min(minZ),
    #                                              X= x[which(minZ  == Z)],
    #                                              Y= y[which(minZ  == Z)],
    #                                              .groups = 'drop'))
    # XY_Base <- XY_Base[order(XY_Base$Z)[1:Base_Count[BB]],]
    # XY_Base$Z <- mean(XY_Base$Z)
    # XY_Base <- XY_Base[,c(3,4,2)]
    
    #XYZ_LAS <- data.frame(X =LAS_Base_TID$X, Y =LAS_Base_TID$Y, Z=LAS_Base_TID$Z)
    XYZ_LAS <- data.frame(X =LAS_Base_TID$X, Y =LAS_Base_TID$Y)
    
    # SPLIT BASED ON DISTANCE AND UPDATE TID
    Dist_XYZ <- rdist(XY_Base[,1:2], XYZ_LAS)
    New_TID <- apply(Dist_XYZ, 2, which.min)
    New_TID[which(New_TID == 1)] <- Base_Split_TID
    # browser()
    #unique_new_TID <-unique(sort(New_TID))
    for(MX in 1:(Base_Count[BB]-1)){
      Max_TID_ID <- Max_TID_ID + MX
      New_TID[which(New_TID == (MX+1))] <- Max_TID_ID # unique_new_TID[(MX+1)]
      # print("SOMETHING WRONG WITH BASE SPLIT PROCEDURE")
      # browser()
    }

    LAS_Base_TID@data$TID <- New_TID
    
    # START THE VOXELISATION, CLUSTERING AND REPARTITIONING
    if(Method == "Vox"){
      LAS_Base_TID <- VOX_CLUSTERING_FUN(LAS_Base_TID, Para_Vx_R_Res, Para_EDT_V = Para_Vx_R_Res*4, Para_d_clust)
    } 
    
    # UPDATE LAS
    LAS_oneP_Updated <- rbind(LAS_oneP_Updated, LAS_Base_TID)
    LAS_oneP <- LAS_oneP_Updated
    
    one_DIR <-   paste(FOLDER_LAS, Dir_Prefix ,oneP_PlotID, Dir_Suffix, sep="")
    print(paste("UPDATING: ", one_DIR))
    writeLAS(LAS_oneP_Updated, one_DIR)
    
    PLOT_COL_FUN(LAS_Base_TID, Title=paste("SPLIT_BASE_F", Base_Split_TID), size = 2, TID_Number = "Yes", ShowGridID="Yes", Check_Plot= Check_Plot)
    
    # UPDATE  Max_TID_ID
    DIR_Max_TID_ID <- paste(FOLDER_O, "/F",FID, "_Max_TID_ID.csv", sep="")
    write.csv(Max_TID_ID, DIR_Max_TID_ID, row.names = FALSE)
    
    DF_BASE_SPLIT_TID[(nrow(DF_BASE_SPLIT_TID)+1),] <- c(oneP_PlotID, Base_Split_TID, Max_TID_ID, Base_Count[BB])
    

    
  }
  return(list(DF_BASE_SPLIT_TID, LAS_oneP_Updated, Max_TID_ID))
}

DIAM_SPLIT_FUN <- function(LAS_oneP, Diam_Split, MP, FID, oneP_PlotID, FOLDER_LAS, Check_Plot, THIN_OR_ALL_LAS){   
  
  if(THIN_OR_ALL_LAS == "Thin"){
    Dir_Prefix <- paste("/LAS_P", "/F", FID, "_MP", MP,"_P" , sep="")
    Dir_Suffix <- ".laz"
  }else{
    Dir_Prefix <- paste("/LAS_P_ALL", "/F",FID,"_P", sep="")
    Dir_Suffix <- "_allT_AttTIDallLAS.laz"
  }
  
  for(DD in 1:length(Diam_Split)){
    Diam_Split_TID <- as.numeric(numextract_all(Diam_Split[DD])[c(FALSE,TRUE)])
    LAS_Base_TID <- filter_poi(LAS_oneP, TID == Diam_Split_TID)
    LAS_oneP_Updated <- filter_poi(LAS_oneP, TID != Diam_Split_TID)
    
    # FIND HEIGHT WHEN THERE ARE TWO STEMS
    
    R_minZ <-  grid_metrics(LAS_Base_TID, ~ min(Z), res = 0.5)
    R_Binary <- R_minZ/R_minZ
    DF_Values <- values(R_minZ)
    DF_XY <-as.data.frame(coordinates(R_minZ))
    DF_XY_Values <- na.omit(cbind(DF_XY, DF_Values))
    DF_XY_Values$TID <- Diam_Split_TID
    DF_XY_Base <- DF_XY_Values[which.min(DF_XY_Values$DF_Values),]
    Dist_XY <- rdist(DF_XY_Base[,1:2], DF_XY_Values[,1:2])
    maxDist_FromBase <- max(Dist_XY)
    DF_XY_Values$TID[which(Dist_XY >  0.5*maxDist_FromBase)] <- 0
    
    # GENERATE RASTER AND PERFORM SPATIAL MERGE OF NEW TID VALUES
    xy_newTID_SP <- SpatialPointsDataFrame(coords = DF_XY_Values[,1:2], data = DF_XY_Values[,3:4])
    
    R_newTID_Update <- rasterize(xy_newTID_SP, R_Binary, field="TID")
    LAS_Base_TID<- merge_spatial(LAS_Base_TID, R_newTID_Update, attribute = "TID")
    LAS_oneP_Updated<- rbind(LAS_oneP_Updated, LAS_Base_TID)
    
    
    one_DIR <-   paste(FOLDER_LAS, Dir_Prefix ,oneP_PlotID, Dir_Suffix, sep="")
    print(paste("UPDATING: ", one_DIR))
    writeLAS(LAS_oneP_Updated, one_DIR)
    LAS_oneP <- LAS_oneP_Updated
    PLOT_COL_FUN(LAS_oneP_Updated, Title=paste("DIAM_SPLIT_F", Diam_Split_TID), size = 2, TID_Number = "Yes", ShowGridID="Yes", Check_Plot= Check_Plot)
    DF_DIAM_SPLIT_TID[(nrow(DF_DIAM_SPLIT_TID)+1),] <- c(oneP_PlotID, Diam_Split_TID)
  }

  return(list(DF_DIAM_SPLIT_TID, LAS_oneP_Updated))
}

###########################################################################################################################################################
###########################################################################################################################################################
###########################################################################################################################################################

LAS_ALL_UPDATE_FUN <- function(FOLDER_UPDATE_QA_INPUT, oneP_PlotID, Plot_Files){
  # Index_Files <- which(Plot_IDS %in% oneP_PlotID)
  # Plot_Files <- QA_Files[Index_Files]
  
  ###################
  # CHANGE AND RE-RUNQ
  ###################
  REMOVE_Plot <- "N"           # Y or N
  #
  Merge_Trees_From <- c( ""  ) #ALL "" c("")
  Merge_Trees_To <-   c("" )  #ALL  "" c("") 214_536
  #
  Split_Trees_From <- c("" )  #""  DOM DOM DOM ALWAYS NEEDS TO BE SUBJECT PLOT
  Split_Trees_Retain <- c("") #
  Split_Trees_To <- c( list(c( "" )))  #"" "65_136", "65_125"  , "65_170"  , "65_177"
  #
  Diam_Split <- c("")
  #
  Fill_Zeros_Below <- c( "") #ALL WHEN MERGE ALL Merge_Trees_To[1]
  #
  Base_Split <- c("")
  Base_Count <- c("")
  #
  Remove_Tree <- c("")
  #
  
  Files_Track <- c("_Track_Changes_MERGE.csv", "_Track_Changes_DIAM_SPLIT.csv" , "_Track_Changes_SPLIT.csv","_Track_Changes_BASE_SPLIT.csv",
                   "_Track_Changes_FILL_ZERO.csv", "_Track_Changes_REMOVE_TID.csv")

  for(FFF in 1:length(Files_Track)){
    File_OneTrack <- Plot_Files[grep(pattern = Files_Track[FFF], Plot_Files)]
    if(length(File_OneTrack) > 0){ # if FILE EXISTS
      #browser()
      DATA_OneTrack <- read.csv(paste(FOLDER_UPDATE_QA_INPUT, "/", File_OneTrack, sep=""))
      
      if(nrow(DATA_OneTrack) > 0){ # if FILE HAS ROWS
        if(Files_Track[FFF] == "_Track_Changes_MERGE.csv"){
          Merge_Trees_From <- paste(DATA_OneTrack[,1], "_", DATA_OneTrack[,2], sep="")
          Merge_Trees_To <- paste(DATA_OneTrack[,3], "_", DATA_OneTrack[,4], sep="")
        }
        
        if(Files_Track[FFF] == "_Track_Changes_DIAM_SPLIT.csv"){
          Diam_Split <- paste(DATA_OneTrack[,1], "_", DATA_OneTrack[,2], sep="")
        }
        
        if(Files_Track[FFF] == "_Track_Changes_SPLIT.csv"){  # # half way through flight 13 SPLIT stored the Retain value (Y or N) .. before that just assume it is Y
          
          TID_Unique <- unique(DATA_OneTrack$Split_TID)
          Split_Trees_From <- paste(DATA_OneTrack[1,1], "_", TID_Unique, sep="")

          Split_Trees_To <- c()
          Split_Trees_Retain <- c()
          for(TT in 1:length(TID_Unique)){
            oneTID_DATA_OneTrack <- DATA_OneTrack[which(DATA_OneTrack$Split_TID == TID_Unique[TT]),]
            Split_Trees_To <- c(Split_Trees_To, list(paste(oneTID_DATA_OneTrack[,3], "_", oneTID_DATA_OneTrack[,4], sep="")))
            
            if(ncol(DATA_OneTrack) == 5){
              Split_Trees_Retain <- c(Split_Trees_Retain, DATA_OneTrack[which(DATA_OneTrack$Split_TID %in% TID_Unique[TT])[1],5]  )
            }else{
              Split_Trees_Retain <- c(Split_Trees_Retain, "Y") # rep("Y", length(Split_Trees_From)))
            }
          }
        } 
        
        if(Files_Track[FFF] == "_Track_Changes_BASE_SPLIT.csv"){
          Base_Split <-  paste(DATA_OneTrack[,1], "_", DATA_OneTrack[,2], sep="")
          if(ncol(DATA_OneTrack) == 4){  # # half way through flight 15 BASE_SPLIT stored the Base count value .. before that just assume it is 2 
            Base_Count <- DATA_OneTrack[,4]
            
          }else{
            Base_Count <- rep(2, nrow(DATA_OneTrack))
          }
          
        }
        
        if(Files_Track[FFF] == "_Track_Changes_FILL_ZERO.csv"){
          Fill_Zeros_Below <- paste(DATA_OneTrack[,1], "_", DATA_OneTrack[,2], sep="")
        }
        
        if(Files_Track[FFF] == "_Track_Changes_REMOVE_TID.csv"){
          # FIXING ERROR WITH REMOVING GROUND
          Index_Ground <- which(DATA_OneTrack[,2] == 1)
          if(length(Index_Ground) > 0){
            Remove_Tree <- paste(DATA_OneTrack[-Index_Ground,1], "_", DATA_OneTrack[-Index_Ground,2], sep="")
          }else{
            Remove_Tree <- paste(DATA_OneTrack[,1], "_", DATA_OneTrack[,2], sep="")
          }
          
          
          
          
        }
        
      } # if _Track_Changes_ FILE EXISTS 
    } # if _Track_Changes_ FILE HAS ROWS
  } # LOOP FFF
  return(list(Merge_Trees_From, Merge_Trees_To, Diam_Split, Split_Trees_From, Split_Trees_Retain, Split_Trees_To, Base_Split, Base_Count, Fill_Zeros_Below, Remove_Tree))
} 


VISUALISE_PLOT_START_FUN <- function(THIN_FOLDER, ALL_FOLDER, oneP_PlotID, oneP_Neigh, Poly_Neigh, FID){

  #####################################################################################################################################################################
  FILE_LAS_THIN <- paste("/F", FID,"_MP1_P", oneP_PlotID, ".laz", sep="")
  # FILE_THIN_ALL <- paste("F", FID, "_MP1", "_P", oneN_PlotID, ".laz", sep="") #paste("/F", FID,"_P", oneP_PlotID, "_allT_AttTIDallLAS.laz", sep="")
  LAS_oneP <- readLAS(paste(THIN_FOLDER, FILE_LAS_THIN, sep=""),  select = "xyzp0")
  LAS_oneP@data$Grid_ID <- oneP_PlotID
  UNIQUE_TID <- unique(LAS_oneP@data$TID)[which(unique(LAS_oneP@data$TID) > 1)]
  
  oneP_SHifT_X <-  min(LAS_oneP$X)
  oneP_SHifT_Y <-  min(LAS_oneP$Y) 
  
  # Title <- paste("Orig P:", oneP_PlotID, sep="")
  # PLOT_COL_FUN(LAS_oneP, Title, size = 4, TID_Number = "Yes" , ShowGridID="Yes", Check_Plot= "Yes")
  # oneP_Dev <- rgl.dev.list()[length( rgl.dev.list())]
  
  # Title <- paste("OrigAllLAS P:", oneP_PlotID, sep="")
  # PLOT_COL_FUN(LAS_oneP_AllLAS, Title, size = 5, TID_Number = "Yes" , ShowGridID="Yes", Check_Plot= Check_Plot)
  # oneP_All_Dev <- rgl.dev.list()[length( rgl.dev.list())]

  
  LAS_allN <- LAS_oneP
  Max_Z_LAS_allN <- max(LAS_allN$Z)
  LIST_XY_oneN <- c()
 
  for(GG in 1:length(oneP_Neigh)){
    oneN_PlotID <- oneP_Neigh[GG]
    Poly_oneN <- Poly_Neigh[which(Poly_Neigh$layer == oneN_PlotID), ]
    XY_oneN <- data.frame(st_coordinates(Poly_oneN))
    LIST_XY_oneN <- c(LIST_XY_oneN, list(XY_oneN ))
    
    # #if(Check_Plot == "Yes"){
    # set3d(oneP_Dev, silent = FALSE)
    # polygon3d(XY_oneN[,1]-oneP_SHifT_X, XY_oneN[,2]-oneP_SHifT_Y, rep(max(LAS_oneP$Z+1), nrow(XY_oneN)), fill = F,  color="red", lwd=2)
    #}
    
    # THIN_FILE <- paste("/F", FID, "_MP", MP,"_P", oneP_PlotID, ".laz", sep="")
    FILE_THIN_ALL <- paste("F", FID, "_MP1", "_P", oneN_PlotID, ".laz", sep="") #paste("/F", FID,"_P", oneP_PlotID, "_allT_AttTIDallLAS.laz", sep="")
    Dir_oneLAS <- paste(THIN_FOLDER, "/", FILE_THIN_ALL, sep="")
    File_oneLAS <-list.files(paste(THIN_FOLDER,  sep=""), pattern = FILE_THIN_ALL)
    
    if(length(File_oneLAS) == 1){
      LAS_oneN <- readLAS(Dir_oneLAS, select = "xyzp0")
      LAS_oneN@data$Grid_ID <- oneN_PlotID
      # LAS_oneN@data$TID <- LAS_oneN@data$TID # REMOVE 8/11/21
      LAS_oneN <- filter_poi(LAS_oneN, TID > 1)
      LAS_allN <- rbind(LAS_allN, LAS_oneN)
    }          
  } # LOOP GRIDS
  #browser()
  Title <- paste("P:", oneP_PlotID, sep="")
  
  allN_SHifT_X <-  min(LAS_allN$X) 
  allN_SHifT_Y <-  min(LAS_allN$Y)
  
  PLOT_COL_FUN(LAS_allN, Title, size = 4, TID_Number = "Yes", ShowGridID="Yes", Check_Plot= "Yes")
  # browser()
#  if(Check_Plot == "Yes"){
  allN_Dev <- rgl.dev.list()[length( rgl.dev.list())]
  set3d(allN_Dev, silent = FALSE)
  polygon3d(XY_oneP[,1]-allN_SHifT_X, XY_oneP[,2]-allN_SHifT_Y, rep(max(LAS_oneP$Z+1), nrow(XY_oneP)), fill = F,  color="blue", lwd=2)
  
  for(LL in 1:length(LIST_XY_oneN)){
    XY_oneN <- LIST_XY_oneN[[LL]]
    polygon3d(XY_oneN[,1]-allN_SHifT_X, XY_oneN[,2]-allN_SHifT_Y, rep(max(LAS_oneP$Z+1), nrow(XY_oneN)), fill = F,  color="red", lwd=2)
  }
  Centroid_Neighs <- Centroid_allP[which(Centroid_allP$Grid_ID %in% oneP_Neigh),]
  set3d(allN_Dev, silent = FALSE)
  text3d(Centroid_Neighs$X- allN_SHifT_X, Centroid_Neighs$Y- allN_SHifT_Y, rep(Max_Z_LAS_allN,nrow(Centroid_Neighs)) , Centroid_Neighs$Grid_ID, col="blue", size=20 ) 
  
  text3d(rep((-length(UNIQUE_TID)-1), length.out = length(UNIQUE_TID)), 
         seq((-length(UNIQUE_TID)-1), length.out = length(UNIQUE_TID)),    
         rep(4,length(UNIQUE_TID)) , UNIQUE_TID, col="orange") 
#  }
  
  # OPEN SUBJECT PLOT (ALL TREES)
  FILE_LAS_ALL <- paste("/F", FID,"_P", oneP_PlotID, "_allT_AttTIDallLAS.laz", sep="")
  LAS_oneP <- readLAS(paste(ALL_FOLDER, FILE_LAS_ALL, sep=""),  select = "xyzp0")
  LAS_oneP@data$Grid_ID <- oneP_PlotID
  
  UNIQUE_TID <- sort(unique(LAS_oneP@data$TID)[which(unique(LAS_oneP@data$TID) > 1)])
  TREE_CODE <- paste(oneP_PlotID, "_", UNIQUE_TID, sep="")
  oneP_SHifT_X <-  min(LAS_oneP$X)
  oneP_SHifT_Y <-  min(LAS_oneP$Y) 
  
  extent(LAS_oneP)
  Ext <- extent(LAS_oneP)
  LAS_allN_Buff <-filter_poi(LAS_allN, X > Ext[1]-2 & X < Ext[1] & Y > Ext[3]-2 & Y < Ext[4] + 2  | 
                               X > Ext[2] & X < Ext[2]+2 & Y > Ext[3]-2 & Y < Ext[4] + 2 | 
                               Y > Ext[3]-2 & Y < Ext[3]& X > Ext[1]-2 & X < Ext[2]+2  | 
                               Y > Ext[4] & Y < Ext[4]+2 & X > Ext[1]-2 & X < Ext[2]+2) 
  LAS_allN_Buff <- remove_lasattribute(LAS_allN_Buff, "MovPos1")
  LAS_allN_Buff <-remove_lasattribute(LAS_allN_Buff, "Grid_MP1")
  LAS_allN_Buff <-remove_lasattribute(LAS_allN_Buff, "MP_ID")

  LAS_oneP_Buff <- rbind(LAS_oneP, LAS_allN_Buff)
  
  Title <- paste("Orig P:", oneP_PlotID, sep="")
  PLOT_COL_FUN(LAS_oneP_Buff, Title, size = 4, TID_Number = "Yes" , ShowGridID="Yes", Check_Plot= "Yes")

  Title <- paste("Orig P:", oneP_PlotID, sep="")
  PLOT_COL_FUN(LAS_oneP, Title, size = 4, TID_Number = "Yes" , ShowGridID="Yes", Check_Plot= "Yes")
  
return(TREE_CODE)  
}

PLOT_FLIGHT_FUN <- function(FOLDER_O, FID, Poly_allP, Neighbour_mtx){
  LAS_ALS_FLIGHT <- readLAS(paste(FOLDER_O, "/LAS", "/All_ALS_F",FID,".laz", sep=""), select = "xyzp0")
  max_ALS_Flight <- max(LAS_ALS_FLIGHT$Z)

  SHifT_X <- min(LAS_ALS_FLIGHT$X)
  SHifT_Y <-   min(LAS_ALS_FLIGHT$Y)
  Title <- paste("FLIGHT:", FID, sep="")
  PLOT_COL_FUN(LAS_ALS_FLIGHT, Title, size=1, TID_Number = "No", ShowGridID="No", Check_Plot=  "Yes" )
  Flight_Dev <- rgl.dev.list()[length( rgl.dev.list())]

  XY_allP <- data.frame(st_coordinates(Poly_allP))
  colnames(XY_allP) <- c("X","Y","Flight","Plot")
  polygon3d(XY_allP[,1]- SHifT_X, XY_allP[,2]- SHifT_Y, rep(max_ALS_Flight, nrow(XY_allP)), fill = F, plot = TRUE, lwd=0.5)
  
  text3d(Centroid_allP$X- SHifT_X, Centroid_allP$Y- SHifT_Y, rep(max_ALS_Flight+5,nrow(Centroid_allP)) , Centroid_allP$Grid_ID, col="blue", size=10 ) 
  
  Points_SCETrees <- st_read( 
    dsn= paste(FOLDER_O, "/SHP", sep="") , 
    layer=paste("F" ,  FID, "_AttTID_SCETrees_Pnts", sep=""))  %>% st_set_crs(Proj_Sys)
  XY_allSCETrees <- st_coordinates(Points_SCETrees)
  points3d(XY_allSCETrees[,1]- SHifT_X, XY_allSCETrees[,2]- SHifT_Y, rep(max_ALS_Flight+3, nrow(XY_allSCETrees)), fill = T, plot = TRUE, size=7, col="red")
  
  Points_allTID <- st_read( 
    dsn= paste(FOLDER_O, "/SHP", sep="") , 
    layer=paste("F" ,  FID, "_AttTID_Pnts", sep=""))  %>% st_set_crs(Proj_Sys)
  XY_allTID <- st_coordinates(Points_allTID)
  points3d(XY_allTID[,1]- SHifT_X, XY_allTID[,2]- SHifT_Y, rep(max_ALS_Flight+3, nrow(XY_allTID)), fill = T, plot = TRUE, size=5, col="black")
  return(list(Flight_Dev, SHifT_X, SHifT_Y))
}

###########################################################################################################################################################
###########################################################################################################################################################
# VOXEL_CLUSTERING_SLICING_FUNCTIONS ALL BELOW
###########################################################################################################################################################
###########################################################################################################################################################


VOXEL_FUN <- function(LAS_XYZ_ID, Para_Vox_Res = 1)
{
  
  # DETERMINING WHICH VOXEL EACH TID GOES INTO   
  Vox_XYZ <- voxel_metrics(LAS_XYZ_ID, list(PID), res = Para_Vox_Res) # Assign XYZ of Voxel to each Voxel
  colnames(Vox_XYZ) <- c("X", "Y", "Z", "PID")
  Vox_XYZ_Collapse <-Vox_XYZ[,list(PID = paste(unique(as.numeric(PID)),collapse = ',')),by = c("X", "Y", "Z")] # Gives each voxel one row and Points clisted in PID
  # browser()
  Vox_XYZ_Collapse <- cbind(Vox_ID=seq(1, nrow(Vox_XYZ_Collapse), 1), Vox_XYZ_Collapse) # GIVES EACH VOXEL A UNIQUE IDENTIFYER
  
  # Create vector of points ordered in same order as Vox_ID 
  Vox_XYZ_Collapse_PointID <-paste(Vox_XYZ_Collapse$PID, ",")
  Vox_XYZ_Collapse_PointID <-unlist(strsplit(Vox_XYZ_Collapse_PointID, ","))
  Vox_XYZ_Collapse_PointID <-as.numeric(Vox_XYZ_Collapse_PointID)
  
  # CALCULATING HOW MANY POINTS IN EACH VOXEL
  Vox_XYZ_Point_Count <- voxel_metrics(LAS_XYZ_ID, length(Z), res = Para_Vox_Res) 
  colnames(Vox_XYZ_Point_Count)[ncol(Vox_XYZ_Point_Count)] <- "Point_Count_In_Vox" 
  # browser()
  Vox_XYZ_Point_Count <- cbind(Vox_XYZ_Point_Count, PID= Vox_XYZ_Collapse$PID, Vox_ID=Vox_XYZ_Collapse$Vox_ID)
  
  # CREATING VECTOR THE LENGTH OF POINTS WITH Vox_ID FOR EACH POINT
  Vox_XYZ_Expand <- unlist(mapply(rep, Vox_XYZ_Collapse$Vox_ID,  Vox_XYZ_Point_Count$Point_Count_In_Vox))
  if(class(Vox_XYZ_Expand)[1] == "matrix"){Vox_XYZ_Expand <-as.vector(Vox_XYZ_Expand)}
  
  #ASSIGNING EACH POINT TO VOXEL and PUT VOX ID INTO STEM LAS FILE
  VoxID_TID_df <- data.frame(PID=Vox_XYZ_Collapse_PointID, Vox_ID=Vox_XYZ_Expand)
  LAS_XYZ_ID@data$Vox_ID[match(VoxID_TID_df$PID,LAS_XYZ_ID@data$PID)] <- VoxID_TID_df$Vox_ID
  Output <- list(LAS_XYZ_ID, Vox_XYZ_Point_Count, VoxID_TID_df)
}

##########################################################################################################################################
##########################################################################################################################################
# SLICE_SURROUND_BBOX_FUN:
# GETS ALL THE TID THAT FALL WITHIN THE BBoX Z RANGE
# INCREMENTALLY MOVES UP THE SLICE AND PERFORMS WS ON ABOVE SLICE TO CHANGE ITS TID BASED ON PROXIMITY TO BELOW SLICE
# THE WHILE LOOP USES THE NEW ABOVE SLICE VOXEL TID  TO REPEATIDLY UNDERTAKING DISTANCE CALCULATION BETWEEN ORIGINAL AND NEW TID VALUES.
# THE LOOP STOPS WHEN NO FURTHER VOXELS HAVE BEEN CHANGED DUE TO CONSTRAINTS IN VOX_SLICE_DIST_FUN
SLICE_SURROUND_BBOX_FUN2 = function(LAS_surrOneCl_rmOneCl_allTID, LAS_Empty, Unique_TID,
                                   Para_EDT_V, Z_Inc_Slice, Para_Vx_R_Res, Para_Sl_Z, 
                                   LAS_or_Vox = "Vox"){
  
  LAS_bboxTID_allSl <- LAS_Empty
  Move_Above_Sl_NonGnd_All <- "No"
  for(mm in 1:length(Z_Inc_Slice)){
    
    # GET BELOW AND ABOVE SLICE
    LAS_Above_oneSl <- filter_poi(LAS_surrOneCl_rmOneCl_allTID, Z >= Z_Inc_Slice[mm] & Z < Z_Inc_Slice[mm]+(Para_EDT_V))

    LAS_Below_oneSl <- filter_poi(LAS_surrOneCl_rmOneCl_allTID, Z >= (Z_Inc_Slice[mm]-Para_EDT_V) & Z < Z_Inc_Slice[mm])
    # Mean_EachTID <- as.data.frame(LAS_Below_oneSl@data %>%
    #                                          dplyr::group_by(TreeID) %>%
    #                                          dplyr::summarise(Mean_X = mean(X),
    #                                                           Mean_Y = mean(Y),
    #                                                           .groups = 'drop'))
    # LAS_Below_oneSl@data <- LAS_Below_oneSl@data[1:nrow(Mean_EachTID),]
    # LAS_Below_oneSl@data[,1:3] <- Mean_EachTID
    
    if(nrow(LAS_Above_oneSl@data) > 0 & nrow(LAS_Below_oneSl@data) > 0){ 
      
      #####################################################
      # WS CLUSTER:  FOR MOVING ABOVE SLICE and BELOW SLICE 
      #####################################################
      
      WS_Move_Slice<- WS_MOVING_FUN(LAS_Above_oneSl, LAS_Below_oneSl,
                                    Para_Vx_R_Res = Para_Vx_R_Res,
                                    Slice_Thickness = Para_EDT_V,
                                    Z_Inc_Slice_mm = Z_Inc_Slice[mm],
                                    LAS_or_Vox = "Vox")
      LAS_Above_oneSl_New <- WS_Move_Slice$LAS_Above_oneSl_New
      #browser()
      if(nrow(LAS_Above_oneSl_New@data) > 0){ 
        
        # UPDATE WS_MOVING_FUN RESULTS  (ASSIGN TreeID AS WSID AND REMOVE WSID ATTRIBUTE)    
        LAS_Above_oneSl_New@data$TreeID <- as.integer(LAS_Above_oneSl_New@data$WSID) 
        Index_Remove <- which(colnames(LAS_Above_oneSl_New@data) == "WSID")
        LAS_Above_oneSl_New@data <- LAS_Above_oneSl_New@data[,-Index_Remove, with=FALSE]
        
        Index_Above_Sl_OrigTID <- which(unique(LAS_Above_oneSl_New@data$TreeID) %in% Unique_TID)
        if(length(Index_Above_Sl_OrigTID) > 0){
          
          # WHILE LOOP ONLY UPDATES VOXELS WITHIN Para_EDT_V AND REPEATS UNTIL ALL UPDATED WITHIN SLICE
          Check_Orig_Count <- 1
          Change_Orig_Count <- 0
          while(Check_Orig_Count != 0){
            
            # USE VOX PROXIMITY TO REPLACE ANY NonGnd TID with Neighbours
            LAS_Above_oneSl_New_Orig <-  filter_poi(LAS_Above_oneSl_New, TreeID %in% Unique_TID)
            LAS_Above_oneSl_New_Changed <-  filter_poi(LAS_Above_oneSl_New, !(TreeID %in% Unique_TID)) # CHANGED DUE TO "WS_MOVING_FUN"
            
            if(nrow(LAS_Above_oneSl_New_Changed@data) > 0){ 
              # GET CLOSE VOXEL SUMMARY BETWEEN NonGnd_TID and Gnd_TID
              Index_Col1 <- match(c("X", "Y", "Z", "TreeID", "VoxID"), colnames(LAS_Above_oneSl_New_Orig@data))
              XYZ_New_Orig <- LAS_Above_oneSl_New_Orig@data[,..Index_Col1]
              Index_Col2 <- match(c("X", "Y", "Z", "TreeID", "VoxID"), colnames(LAS_Above_oneSl_New_Changed@data))
              XYZ_New_Changed <- LAS_Above_oneSl_New_Changed@data[,..Index_Col2]
              
              ################################################################################################ 12
              # DIST CALC FOR ABOVE SLICE (BETWEEN VOXELS WITH ORIG TID AND THOSE THAT CHANGED  WITH "WS_MOVING_FUN")
              ################################################################################################ 12
              
              Dist_NewOrig_NewChange <- VOX_SLICE_DIST_FUN(XYZ_New_Orig,
                                                           XYZ_New_Changed, # THESE ARE LIKE THE SURROUNDING TID
                                                           Max_Dist = Para_EDT_V, 
                                                           Para_Vx_R_Res= Para_Vx_R_Res,
                                                           Slice_Thickness= Para_Sl_Z)
              
              Table_CloseVox <- Dist_NewOrig_NewChange$All_Closest_Vox_Surr_oneCl_NoDup
              
              # UPDATING TID IN ABOVE SLICE
              Index <- match(Table_CloseVox$VoxID_oneTID, LAS_Above_oneSl_New@data$VoxID )
              LAS_Above_oneSl_New@data$TreeID[Index] <- Table_CloseVox$TID_Surr
              
              # CHECKING IF WHILE LOOP SHOULD BE REPEATED
              Check_Orig_Count <- length(which(LAS_Above_oneSl_New@data$TreeID %in% Unique_TID))
              if(Change_Orig_Count == Check_Orig_Count){
                Check_Orig_Count <- 0 # BREAK OUT OF WHILE LOOP
              }
            }else{
              Check_Orig_Count <- 0 # BREAK OUT OF WHILE LOOP
            }
            Change_Orig_Count <- Check_Orig_Count
          } # WHILE LOOP
          
        } # IF ABOVE SLICE STILL HAS NonGnd TID
        
        if(nrow(LAS_Above_oneSl_New@data) > 0){ 
          # UPDATING LAS WITH ABOVE SLICE TID
          Index_1 <- which(LAS_surrOneCl_rmOneCl_allTID@data$VoxID %in%  LAS_Above_oneSl_New@data$VoxID )
          Index_2 <- match(LAS_surrOneCl_rmOneCl_allTID@data$VoxID[Index_1],  LAS_Above_oneSl_New@data$VoxID )
          LAS_surrOneCl_rmOneCl_allTID@data$TreeID[Index_1] <- as.integer(LAS_Above_oneSl_New@data$TreeID[Index_2])
          
          if(length(which(LAS_Above_oneSl_New@data$TreeID == Unique_TID)) > 0) {
            LAS_Above_oneSl_NonGnd <- filter_poi(LAS_Above_oneSl_New, TreeID %in% Unique_TID)
            if(nrow(LAS_bboxTID_allSl@data) == 0){
              LAS_bboxTID_allSl <- filter_poi(LAS_surrOneCl_rmOneCl_allTID, Z < Z_Inc_Slice[mm]+(Para_EDT_V) & TreeID == Unique_TID)
            }else{
              LAS_bboxTID_allSl@data <- rbind(LAS_bboxTID_allSl@data, LAS_Above_oneSl_NonGnd@data)
            }
            Move_Above_Sl_NonGnd_All <- "No"
          }else{
            Move_Above_Sl_NonGnd_All <- "Yes"
          }
          
          if(nrow(LAS_bboxTID_allSl@data) > 0 & Move_Above_Sl_NonGnd_All == "Yes"){ 
            
            # GET CLOSE VOXEL SUMMARY BETWEEN NonGnd_TID and Gnd_TID
            Index_Col1 <- match(c("X", "Y", "Z", "TreeID", "VoxID"), colnames(LAS_bboxTID_allSl@data))
            XYZ_Above_Sl_NonGnd_All <- LAS_bboxTID_allSl@data[,..Index_Col1]
            Index_Col2 <- match(c("X", "Y", "Z", "TreeID", "VoxID"), colnames(LAS_Above_oneSl_New@data))
            XYZ_Above_Sl_New <- LAS_Above_oneSl_New@data[,..Index_Col2]
            
            ################################################################################################
            # DIST CALC FOR ABOVE SLICE (BETWEEN VOXELS WITH ORIG TID AND THOSE THAT HAVE NOT BEEN CHANGED)
            ################################################################################################ 
            Dist_Above_Sl_NonGnd_All <- VOX_SLICE_DIST_FUN(XYZ_Above_Sl_NonGnd_All,
                                                           XYZ_Above_Sl_New,
                                                           Max_Dist = 40, # Para_EDT_H,
                                                           Para_Vx_R_Res= Para_Vx_R_Res,
                                                           Slice_Thickness= Para_Sl_Z)
            
            Dist_Above_Sl_NonGnd_All_Summary <- Dist_Above_Sl_NonGnd_All$Summary_closeSurr
            if(!is.null(Dist_Above_Sl_NonGnd_All_Summary)){
              if(nrow(Dist_Above_Sl_NonGnd_All_Summary) == 1){
                LAS_bboxTID_allSl@data$TreeID <- as.integer(Dist_Above_Sl_NonGnd_All_Summary$TID_Surr)
              }else{
                #browser()
                Dist_Above_Sl_NonGnd_All_VoxMatch <- Dist_Above_Sl_NonGnd_All$All_Closest_Vox_Surr_oneCl_NoDup
                
                Index_1 <- which(LAS_bboxTID_allSl@data$VoxID %in%  Dist_Above_Sl_NonGnd_All_VoxMatch$VoxID_oneCl)
                Index_2 <- match(LAS_bboxTID_allSl@data$VoxID[Index_1],  Dist_Above_Sl_NonGnd_All_VoxMatch$VoxID_oneCl)
                LAS_bboxTID_allSl@data$TreeID[Index_1] <- as.integer(Dist_Above_Sl_NonGnd_All_VoxMatch$TID_Surr[Index_2])
              }
              Index_1 <- which(LAS_surrOneCl_rmOneCl_allTID@data$VoxID %in%  LAS_bboxTID_allSl@data$VoxID )
              Index_2 <- match(LAS_surrOneCl_rmOneCl_allTID@data$VoxID[Index_1],  LAS_bboxTID_allSl@data$VoxID )
              LAS_surrOneCl_rmOneCl_allTID@data$TreeID[Index_1] <- as.integer(LAS_bboxTID_allSl@data$TreeID[Index_2])
            }
            LAS_bboxTID_allSl <- LAS_Empty
            Move_Above_Sl_NonGnd_All <- "No"
          }
        }
      }
    } # IF THERE ARE VOXELS ABOVE SLICE AND VOXELS BELOW SLICE
  } # END mm Slice LOOP
  return(list(LAS_surrOneCl_rmOneCl_allTID = LAS_surrOneCl_rmOneCl_allTID))
}

VOX_CLUSTERING_FUN <- function(LAS_allSt, Para_Vx_R_Res, Para_EDT_V = Para_Vx_R_Res*4, Para_d_clust){

  # ADD PID FOR EACH POINT TO KEEP TRACK OF VOXEL ASSIGNMENT
  LAS_allSt <- add_lasattribute(LAS_allSt, x=seq(1,length(LAS_allSt$X),1 ), name="PID", desc ="PID")
  LAS_allSt <- add_lasattribute(LAS_allSt, x=LAS_allSt@data$TID, name="TreeID", desc ="TreeID")
  
  ##################################################################################################################################################
  #########################################
  # VOXELISE THE NEW DISTRIBUTION OF POINTS
  #########################################
  unique_TID <- unique(LAS_allSt@data$TID)
  max_Vox_Cl <- 0
  max_VoxID <- 0
  # browser()
  for(UT in 1:length(unique_TID)){

    LAS_oneTID <-filter_poi(LAS_allSt, TID == unique_TID[UT])
    
    # GET VOX_ID FOR EACH LAS POINT
    Vx_oneTID_Output <- VOXEL_FUN(LAS_oneTID, Para_Vox_Res = Para_Vx_R_Res)
    LAS_oneTID <-Vx_oneTID_Output[[1]]
    
    # CLUSTER THE VOXELS
    LAS_oneTID@data$Vox_ID <- LAS_oneTID@data$Vox_ID + max_VoxID
    Vx_oneTID <- Vx_oneTID_Output[[2]]
    Vx_oneTID$Vox_ID <- Vx_oneTID$Vox_ID + max_VoxID
    max_VoxID <- max(Vx_oneTID$Vox_ID)

    if(nrow(Vx_oneTID) > 1){ 
      Index_Col1 <- match(c("X", "Y", "Z"), colnames(Vx_oneTID))
      Vx_oneTID_Cl <- VoxR::distance_clustering(data = Vx_oneTID[,..Index_Col1], 
                                                d_clust=Para_d_clust) # CHANGED 21/0/2022 THIS WAS CHANGED FOR QUALITY ASSESSMENT BUT NOT SURE IF USED ELSEWEHERE    # REPLACE  (VoxR 1.0 from 0.5.1) During lidR 3.0.5
      colnames(Vx_oneTID_Cl)[4] <- "VoxCl"
    }else{
      Index_Col1 <- match(c("X", "Y", "Z"), colnames(Vx_oneTID))
      Vx_oneTID_Cl <- cbind(Vx_oneTID[,..Index_Col1], VoxCl = 1)
    }

    # GENERATE LAS USING VOXELS WITH VOXEL CLUSTERS
    Vx_oneTID_Cl <- data.frame(TreeID = unique_TID[UT], ##Unique_TID[v],
                               Vx_oneTID_Cl,
                               Vox_PntCnt = Vx_oneTID$Point_Count_In_Vox,
                               Vox_ID = Vx_oneTID$Vox_ID)

    Index_Vox_Cl<- match(LAS_oneTID@data$Vox_ID, Vx_oneTID_Cl$Vox_ID)
    LAS_oneTID@data$VoxCl <- Vx_oneTID_Cl$VoxCl[Index_Vox_Cl]
    
    # LAS_TEST <- LAS_oneTID
    # LAS_TEST@data$TID <-  LAS_TEST@data$VoxCl
    # LAS_TEST@data$Grid_ID <-  0
    # PLOT_COL_FUN(LAS_TEST, Title=paste("VoxCl", unique_TID[UT]), size = 5, TID_Number = "Yes", ShowGridID="Yes", Check_Plot= Check_Plot)
    # browser()
    
    # INCREMENTALLY UPDATING Vx_oneTID_Cl$Vox_ID
    LAS_oneTID@data$VoxCl <- LAS_oneTID@data$VoxCl + max_Vox_Cl
    Vx_oneTID_Cl$VoxCl <- Vx_oneTID_Cl$VoxCl + max_Vox_Cl
    max_Vox_Cl <- max(LAS_oneTID@data$VoxCl)

    # CREATING LAS_Vx_oneTID 
    LAS_Vox_TID <- LAS(Vx_oneTID_Cl)
    if(UT == 1){
      LAS_Vox_ALL <- LAS_Vox_TID
      LAS_allTID <-LAS_oneTID
    }else{
      LAS_Vox_ALL <- rbind(LAS_Vox_TID, LAS_Vox_ALL)
      LAS_allTID <- rbind(LAS_allTID, LAS_oneTID)
    }
  }

  ####################################################################################################################################### 7
  ####################################################################################################################################### 7
  # LOOP VC: BOUNDING BOX OF VoxCl AND SURROUND STEMS (Obj.rec) TO CLEAN UP EACH St (BY MOVING CLUSTERS) BUT DOESN'T GENERATE ANY NEW St
  ####################################################################################################################################### 7
  ####################################################################################################################################### 7

  # GET LOWEST HEIGHT OF EACH TID AND EACH CLUSTER
  Summary_allCl <- as.data.frame(LAS_Vox_ALL@data %>%
                                  dplyr::group_by(TreeID, VoxCl) %>%
                                  dplyr::summarise(MinZ = min(Z),
                                                   MaxZ = max(Z),
                                                   Range = max(Z)-min(Z),
                                                   Cnt = length(Z),
                                                   .groups = 'drop'))
  Summary_allCl <- Summary_allCl[rev(order(Summary_allCl$Cnt)),]
  
  Summary_TID <- as.data.frame(LAS_Vox_ALL@data %>%
                                  dplyr::group_by(TreeID) %>%
                                  dplyr::summarise(MinZ = min(Z),
                                                   MaxZ = max(Z),
                                                   Range = max(Z)-min(Z),
                                                   Halfway_Z = MinZ  + ((MaxZ - MinZ)/2),
                                                   Cnt = length(Z),
                                                   .groups = 'drop'))
  
  # GET CLUSTER THAT ARE BELOW HALF WAY POINT OF EACH TID
  Summary_allCl$TID_Halfway_Z <- Summary_TID$Halfway_Z[match(Summary_allCl$TreeID, Summary_TID$TreeID)]
  Summary_BelowHalf_Cl <- Summary_allCl[which(Summary_allCl$MinZ < Summary_allCl$TID_Halfway_Z),]

  # START WITH TID CLUSTER WITH THE BIGGEST Z RANGE AND LOW TO GROUND (i.e. below Halfway_Z) FOR EACH UNIQUE TID
  TID_MaxVox_Cl <- as.data.frame(Summary_BelowHalf_Cl  %>%
                                  dplyr::group_by(TreeID) %>%
                                  dplyr::summarise(VoxCl_maxRange = VoxCl[which(Range == max(Range))[1]],
                                                   VoxCl_maxCnt = VoxCl[which(Cnt == max(Cnt))[1]], # OR IS RANGE MORE IMPORANT (THINK RANGE IS MORE IMPORTANT)
                                                   .groups = 'drop'))
  Index_Remove <- which( paste(Summary_allCl$TreeID, "_", Summary_allCl$VoxCl) %in% paste(TID_MaxVox_Cl$TreeID, "_", TID_MaxVox_Cl$VoxCl_maxRange))
  
  Rest_Cl <- Summary_allCl[-Index_Remove,]
  TID_Cl <- Summary_allCl[Index_Remove,]
  Unique_TID <- TID_MaxVox_Cl$TreeID 

  LAS_Vox_UpdateTID <- filter_poi(LAS_Vox_ALL, TreeID %in% TID_MaxVox_Cl$TreeID & VoxCl %in% TID_MaxVox_Cl$VoxCl_maxRange)

  # LAS_TEST <- LAS_Vox_UpdateTID
  # LAS_TEST@data$TID <-  LAS_TEST@data$TreeID
  # LAS_TEST@data$Grid_ID <-  0
  # PLOT_COL_FUN(LAS_TEST, Title=paste("FIRST TWO", unique_TID[UT]), size = 5, TID_Number = "Yes", ShowGridID="Yes", Check_Plot= Check_Plot)
  # browser()
  
  ###############################
  # LOOP EACH TID, CLUSTER VOXELS
  ############################### 7a
  print(paste("START VC: ", 1, " of ", nrow(Rest_Cl), sep=""))
  for(VC in 1:nrow(Rest_Cl)){
  
    # ONE St VOX CLUSTERS 
    LAS_oneCl_Vx <- filter_poi(LAS_Vox_ALL, TreeID == Rest_Cl$TreeID[VC] & VoxCl == Rest_Cl$VoxCl[VC])
    
    # IF VOX ID STILL EXISTS
    if(nrow(LAS_oneCl_Vx@data) > 0){

      # BOUNDARY OF ALL CLUSTERS (LAS_oneCl_Vx)
      Bound_oneTID_allCl <- as.data.frame(LAS_oneCl_Vx@data %>%
                                            dplyr::group_by(VoxCl) %>%
                                            dplyr::summarise(# TreeID = unique(TreeID), # 
                                                             Vox_Count = length(Z),
                                                             MinX = min(X)-2*Para_Vx_R_Res,
                                                             MaxX = max(X)+2*Para_Vx_R_Res,
                                                             MinY = min(Y)-2*Para_Vx_R_Res,
                                                             MaxY = max(Y)+2*Para_Vx_R_Res,
                                                             MinZ = min(Z)-2*Para_Vx_R_Res,
                                                             MaxZ = max(Z)+2*Para_Vx_R_Res,
                                                             Range = range(Z)[2] - range(Z)[1],
                                                             .groups = 'drop'))
 
      
      # GET NEIGHBOUR LAS SURROUNDING "Subject" TID IN BOX 
      LAS_surrOneTID_Vx <- filter_poi(LAS_Vox_UpdateTID, X >= Bound_oneTID_allCl$MinX - 2*Para_Vx_R_Res &
                                        X <= Bound_oneTID_allCl$MaxX + 2*Para_Vx_R_Res &
                                        Y >= Bound_oneTID_allCl$MinY - 2*Para_Vx_R_Res &
                                        Y <= Bound_oneTID_allCl$MaxY + 2*Para_Vx_R_Res &
                                        Z >= Bound_oneTID_allCl$MinZ - 2*Para_Vx_R_Res &
                                        Z <= Bound_oneTID_allCl$MaxZ + 2*Para_Vx_R_Res)

      if(nrow(LAS_surrOneTID_Vx@data) > 0){ 
        
        # GET CLOSE VOXEL SUMMARY BETWEEN oneCl and surrOneCl_rmOneCl
        Index_Col1 <- match(c("X", "Y", "Z", "TreeID", "Vox_ID"), colnames(LAS_oneCl_Vx@data))
        XYZ_oneCl <- as.data.frame(LAS_oneCl_Vx@data)[,Index_Col1]
        Index_Col2 <- match(c("X", "Y", "Z", "TreeID", "Vox_ID"), colnames(LAS_surrOneTID_Vx@data))
        XYZ_surrOneCl <- as.data.frame(LAS_surrOneTID_Vx@data)[,Index_Col2]
        
        # DISTANCE BETWEEN ALL VOXELS IN TWO LAS FILES (THE VOXELS ARE ONLY WITHIN BBox). MAX DISTANCE SPECIFIED
        Index_Col1 <- match(c("X", "Y", "Z"), colnames(XYZ_oneCl))
        Index_Col2 <- match(c("X", "Y", "Z"), colnames(XYZ_surrOneCl))
        Dist_oneCl_Surr <- rdist(XYZ_oneCl[,Index_Col1], 
                                 XYZ_surrOneCl[,Index_Col2]) # Row Col
        #
        Dist_oneCl_Surr <- round( Dist_oneCl_Surr, 3)
        Index_Dist_oneCl_Surr <- as.data.frame(which(Dist_oneCl_Surr <= Para_EDT_V, arr.ind = TRUE))  
        
        # UPDATE TABLE OF ALL CLOSE MATCHES
        if(nrow(Index_Dist_oneCl_Surr) > 0){
          
          Closest_Vox_Surr<- XYZ_surrOneCl[Index_Dist_oneCl_Surr$col,]
          colnames(Closest_Vox_Surr)[which(colnames(Closest_Vox_Surr) %in% c("TreeID", "Vox_ID"))] <- c("TID_Surr", "VoxID_Surr")
          
          Closest_Vox_St <- XYZ_oneCl[Index_Dist_oneCl_Surr$row,]
          colnames(Closest_Vox_St)[which(colnames(Closest_Vox_St) %in% c("TreeID", "Vox_ID"))] <- c("TID_oneCl", "VoxID_oneCl")
          Dist_St_Surr_Close <- Dist_oneCl_Surr[ cbind(Index_Dist_oneCl_Surr$row, Index_Dist_oneCl_Surr$col) ] 
          Closest_Vox_Surr$Dist <- Dist_St_Surr_Close
          Closest_Vox_Surr_oneCl <- data.frame(Closest_Vox_Surr, Closest_Vox_St)
          #browser()
          if(nrow(Closest_Vox_Surr_oneCl) >= 1){
            Summary_closeSurr <- as.data.frame(Closest_Vox_Surr_oneCl %>%
                                                 dplyr::group_by(TID_Surr) %>%
                                                 dplyr::summarise(minDist = min(Dist),
                                                                  cntVox = length(which(Dist <= Para_EDT_V + 0.01)),
                                                                  MinZ = min(Z),
                                                                  MaxZ = max(Z),
                                                                  .groups = 'drop'))
          }
          
          # WORK OUT HOW TO DETERMINE WHICH ONE TO USE
          if(nrow(Summary_closeSurr) == 1){
            # ONLY ONE SURROUND IS CLOSE SO GIVE THE CLUSTER THIS TID VALUE
            LAS_oneCl_Vx@data$TreeID  <- as.integer(Summary_closeSurr$TID_Surr[1])
            LAS_Vox_UpdateTID <- rbind(LAS_Vox_UpdateTID, LAS_oneCl_Vx)
          }else{
            ############################################################################################
            # IF THERE IS MORE THAN ONE CLOSEST SURROUNDING TID, UNDERTAKE SLICING PROCEDURE TO ALLOCATE
            ############################################################################################
            if(!is.null(Summary_closeSurr)){
              
              #################################
              # IF IT IS A LARGE CLUSTER, MERGE 
              #################################
              if(Rest_Cl$Cnt[VC] > 100){ # FOR SLICE SPLITTING THE CLUSTER NEEDS TO BE > 100 VOXELS IN SIZE
                
                # GET CLOSE VOXEL SUMMARY BETWEEN oneCl and surrOneCl_rmOneCl
                Index_Col1 <- match(c("X", "Y", "Z", "TreeID", "Vox_ID"), colnames(LAS_oneCl_Vx@data))
                XYZ_oneCl <- as.data.frame(LAS_oneCl_Vx@data)[,Index_Col1]
                colnames(XYZ_oneCl)[which(colnames(XYZ_oneCl) == "Vox_ID")] <- "VoxID"
                
                Index_Col2 <- match(c("X", "Y", "Z", "TreeID", "Vox_ID"), colnames(LAS_surrOneTID_Vx@data))
                XYZ_surrOneCl <- as.data.frame(LAS_surrOneTID_Vx@data)[,Index_Col2]
                colnames(XYZ_surrOneCl)[which(colnames(XYZ_surrOneCl) == "Vox_ID")] <- "VoxID"
                
                Dist_oneCl_Surr <- rdist(XYZ_oneCl[,1:3], XYZ_surrOneCl[,1:3])
                Index_New_TID <- apply(Dist_oneCl_Surr, 1, which.min)
                New_TID <- XYZ_surrOneCl$TreeID[Index_New_TID]
                LAS_oneCl_Vx@data$TreeID <- New_TID
                LAS_Vox_UpdateTID <- rbind(LAS_Vox_UpdateTID, LAS_oneCl_Vx)
                
                # LAS_TEST <- LAS_Vox_UpdateTID
                # LAS_TEST@data$TID <-  LAS_TEST@data$TreeID
                # LAS_TEST@data$Grid_ID <-  0
                # PLOT_COL_FUN(LAS_TEST, Title=paste("FIRST TWO", unique_TID[UT]), size = 5, TID_Number = "Yes", ShowGridID="Yes", Check_Plot= Check_Plot)
                # 
                
                # Dist_oneCl_surrOneCl <- VOX_SLICE_DIST_FUN(XYZ_oneCl,
                #                                            XYZ_surrOneCl,
                #                                            Max_Dist = Para_EDT_V,
                #                                            Para_Vx_R_Res= Para_Vx_R_Res,
                #                                            Slice_Thickness= Para_Sl_Z)
                # 
                # Table_CloseVox <- Dist_oneCl_surrOneCl$All_Closest_Vox_Surr_oneCl_NoDup
                # browser()
                # # UPDATING TID IN ABOVE SLICE
                # Index <- match(Table_CloseVox$VoxID_oneCl, LAS_oneCl_Vx@data$Vox_ID )
                # LAS_oneCl_Vx@data$TreeID[Index] <- Table_CloseVox$TID_Surr
                # 
                # LAS_Vox_UpdateTID <- rbind(LAS_Vox_UpdateTID, LAS_oneCl_Vx)
                
                # LAS_oneCl_Vx_Temp <- remove_lasattribute(LAS_oneCl_Vx_Temp,  name="Tree_ID")
                # Test <- rbind(LAS_Vox_UpdateTID, LAS_oneCl_Vx)
                # Test@data$TID <- Test@data$TreeID
                # Test@data$Grid_ID <- 0
                # PLOT_COL_FUN(Test, Title="FIXED TIDS USING VOX METHOD", size = 5, TID_Number = "Yes", ShowGridID="Yes", Check_Plot= Check_Plot)

                
                ### DOM DOM DOM HERE YOU NEED TO DEVICE A METHOD WHEREBY IF THE VOXEL IS LARGE AND IT IS DIRECTLY ABOVE A SHORT TID AND BESIDE A 
                # LARGE TID THEN IT SHOULD BE ATTACHED TO THE SHORT TID.
                
                # OR IT CALLS FOR A SLICING PROCEDURE UNDER CIRCUMSTANCES WHERE ITS A LARGE CLUSTER
                
                
                # 
                # ###############################################
                # # CALCULATE AMOUNT OF TREE IS UNDER THE CLUSTER
                # ###############################################
                # LAS_oneCl_Vx_Temp <- LAS_oneCl_Vx
                # LAS_oneCl_Vx_Temp@data$TreeID <- 9999
                # LAS_allTID_oneCl <- rbind(LAS_Vox_UpdateTID, LAS_oneCl_Vx_Temp)
                # R_TID_oneCl <-  grid_metrics(LAS_allTID_oneCl, ~TreeID[which.max(Z)], res = 0.5)
                # R_TID_Trees <-  grid_metrics(LAS_Vox_UpdateTID, ~TreeID[which.max(Z)], res = 0.5)
                # #
                # TID_TOPS_COVERED_Cls <- as.data.frame(table(values(R_TID_oneCl))[-which(names(table(values(R_TID_oneCl))) == 9999)])
                # TID_TOPS <-as.data.frame(table(values(R_TID_Trees)))
                # Merge_TOPS <- merge(TID_TOPS, TID_TOPS_COVERED_Cls, by = "Var1", all=TRUE)
                # Merge_TOPS[is.na(Merge_TOPS)] <- 0
                # Merge_TOPS$Portion_Covered <- 1 - Merge_TOPS$Freq.y/Merge_TOPS$Freq.x
                # 
                # # ############################################################################
                # # # IF MORE THAN 1/2 OF TREE IS UNDER THE CLUSTER THE PUT CLUSTER ON THAT TREE
                # # ############################################################################
                # Index_Covered_TID <- which(Merge_TOPS$Portion_Covered > 0.5)
                # if(length(Index_Covered_TID) == 1){ 
                #   Covered_TID <- Merge_TOPS$Var1[Index_Covered_TID]
                #   
                #   LAS_oneCl_Vx@data$TreeID  <- as.integer(Covered_TID)
                #   LAS_Vox_UpdateTID <- rbind(LAS_Vox_UpdateTID, LAS_oneCl_Vx)
                #   
                # }else{ # FOR SLICE SPLITTING THE CLUSTER CAN NOT BE COVERING AT LEAST 50% OF AN NEIGHBOURING STEM (USING RASTER FOR "BIRDS EYE VIEW)
                # 
                #   # 
                #   # # SLICE INCREMENTS
                #   # SeqMinMax <- c(((floor(min(LAS_oneCl_Vx$Z)/Para_Vx_R_Res)*Para_Vx_R_Res)-Para_Sl_Z),  # ADDED *2 ... 1/12/21
                #   #                ceiling(max(LAS_oneCl_Vx$Z)/Para_Vx_R_Res)*Para_Vx_R_Res + Para_Sl_Z)  # ADDED *2 ... 1/12/21
                #   # Z_Inc_Slice <- seq(min(SeqMinMax), max(SeqMinMax), Para_Sl_Z)
                #   # #
                #   # LAS_Empty <- filter_poi(LAS_oneCl_Vx, Z < -9999)
                #   # LAS_surrOneCl_rmOneCl_allTID <- filter_poi(LAS_Vox_UpdateTID, Z>= min(Z_Inc_Slice) & Z<max(Z_Inc_Slice))
                #   # 
                #   # LAS_SURR_OneCl_Slicing <- rbind(LAS_surrOneTID_Vx, LAS_oneCl_Vx)
                #   # Unique_TID <- unique(LAS_SURR_OneCl_Slicing@data$TreeID)
                #   # # browser()
                #   # # DISTANCE BETWEEN ALL VOXELS IN TWO LAS FILES (THE VOXELS ARE ONLY WITHIN BBox). MAX DISTANCE SPECIFIED
                #   # LAS_SURR_OneCl_Slicing_OUPTUT <- SLICE_SURROUND_BBOX_FUN2(LAS_SURR_OneCl_Slicing , LAS_Empty, Unique_TID,
                #   #                                   Para_EDT_V, Z_Inc_Slice, Para_Vx_R_Res, Para_Sl_Z, 
                #   #                                   LAS_or_Vox = "Vox")
                #   # LAS_SURR_OneCl_New <- LAS_SURR_OneCl_Slicing_OUPTUT$LAS_surrOneCl_rmOneCl_allTID
                #   # 
                #   # # browser()
                #   # 
                #   # Index_1 <- which(LAS_SURR_OneCl_New@data$VoxCl %in% LAS_oneCl_Vx@data$VoxCl)
                #   # LAS_oneCl_Vx@data <- LAS_SURR_OneCl_New@data[Index_1]
                #   # LAS_Vox_UpdateTID <- rbind(LAS_Vox_UpdateTID, LAS_oneCl_Vx)
                #   # 
                #   # # LAS_allTID_oneCl@data$Grid_ID <- 0
                #   # # PLOT_COL_FUN(LAS_allTID_oneCl, Title="FIXED TIDS USING VOX METHOD", size = 5, TID_Number = "Yes", ShowGridID="Yes", Check_Plot= Check_Plot)
                #   # 
                # 
                #  
                # }
                

              }else{
                Summary_closeSurr <- Summary_closeSurr[order(Summary_closeSurr$MinZ),]
                
                min_Dist <- Summary_closeSurr$TID_Surr[which.min(Summary_closeSurr$minDist)]
                max_cntVox <-Summary_closeSurr$TID_Surr[which.max(Summary_closeSurr$cntVox)]
                min_MinZ <-Summary_closeSurr$TID_Surr[which.min(Summary_closeSurr$MinZ)]

                minZ_Diff <- diff(Summary_closeSurr$MinZ)
                
                if(minZ_Diff > 3){
                  LAS_oneCl_Vx@data$TreeID  <- min_MinZ
                  LAS_Vox_UpdateTID <- rbind(LAS_Vox_UpdateTID, LAS_oneCl_Vx)
                }else{
                  LAS_oneCl_Vx@data$TreeID  <- max_cntVox
                  LAS_Vox_UpdateTID <- rbind(LAS_Vox_UpdateTID, LAS_oneCl_Vx)
                }
              }
 
            ############
            # IF NO SURR 
            ############
            }else{ # if(!is.null(Summary_closeSurr))
              # IF CLUSTER FAR FROM ANY OF THE TREES DON'T CHANGE THE TREE ID
              # LAS_oneCl_Vx@data$TreeID  <- as.integer(0)
              LAS_Vox_UpdateTID <- rbind(LAS_Vox_UpdateTID, LAS_oneCl_Vx)
            }
          }
        } else{
          LAS_oneCl_Vx@data$TreeID  <- as.integer(0)
          LAS_Vox_UpdateTID <- rbind(LAS_Vox_UpdateTID, LAS_oneCl_Vx)
        }

        ########################################
        # IF NO SURR: (NO LAS_surrOneCl_rmOneCl) 
        ########################################    
      }else{
        # IF CLUSTER FAR FROM ANY OF THE TREES DON'T CHANGE THE TREE ID  (i.e. nrow(LAS_surrOneTID_Vx@data) > 0)

        LAS_Vox_UpdateTID <- rbind(LAS_Vox_UpdateTID, LAS_oneCl_Vx)
      }
    } # IF VOX ID STILL EXISTS (SOME GET MOVED IN PREVIOUS ITERATIONS)
  } # LOOP VC THROUGH EACH CLUSTER THAT IS NOT THE MAIN ONE
  print(paste("VC: ", VC, " of ", nrow(Rest_Cl), "sep="))
  
  ######################################### 7
  # MAIN UPDATE (LAS_Vox_ALL and LAS_TID)
  ######################################### 7

  print("UPDATE THE LAS FILE USING THE VOXEL INFORMATION")

  # # UPDATING CHANGES FOR VC LOOP (LAS_Vox_ALL & LAS_TID)
  if(nrow(LAS_Vox_UpdateTID@data) > 0){
    # browser()
    Index_1 <- which(LAS_allTID@data$Vox_ID %in% c(LAS_Vox_UpdateTID@data$Vox_ID)) #LAS_Vox_UpdateTID
    Index_2 <- match(LAS_allTID@data$Vox_ID[Index_1], LAS_Vox_UpdateTID@data$Vox_ID)
    LAS_allTID@data$TreeID[Index_1] <- LAS_Vox_UpdateTID@data$TreeID[Index_2]
    LAS_allTID@data$TID <- LAS_allTID@data$TreeID
    
    # CHECK RESULTS
    PLOT_COL_FUN(LAS_allTID, Title="FIXED TIDS USING VOX METHOD", size = 5, TID_Number = "Yes", ShowGridID="Yes", Check_Plot= Check_Plot)
  }
  
  LAS_allTID <- remove_lasattribute(LAS_allTID,  name="PID")
  LAS_allTID <- remove_lasattribute(LAS_allTID, name="TreeID")
  LAS_allTID <- remove_lasattribute(LAS_allTID, name="VoxCl")
return(LAS_allTID)  
} # VOX CLUSTER FUNCTION

#################################################################################################################################################
#################################################################################################################################################
#################################################################################################################################################
# BELOW FUNCTION ARE USED IN THE TORCH_FINAL_TID_CLUSTER_MOVE_V2
#################################################################################################################################################
#################################################################################################################################################
#################################################################################################################################################

Vox_Metri_TID_PID = function(TID, PID, Z) # NOT USING THIS ANYMORE
{
  ret = list(
    TID = names(table(TID))[which.max(table(TID))],
    PID = names(table(PID))[which.max(table(PID))],
    Point_Count_In_Vox = length(Z)
  )
  return(ret)
}

VOXEL_FUN_2 <- function(LAS_XYZ_ID, Para_Vox_Res = 1)
{
  # DETERMINING WHICH VOXEL EACH TID GOES INTO
  Vox_XYZ <- voxel_metrics(LAS_XYZ_ID, list(PointID), res = Para_Vox_Res) # Assign XYZ of Voxel to each Voxel
  colnames(Vox_XYZ) <- c("X", "Y", "Z", "PointID")
  Vox_XYZ_Collapse <-Vox_XYZ[,list(PointID = paste(unique(as.numeric(PointID)),collapse = ',')),by = c("X", "Y", "Z")] # Gives each voxel one row and Points clisted in PointID
  # browser()
  Vox_XYZ_Collapse <- cbind(VoxID=seq(1, nrow(Vox_XYZ_Collapse), 1), Vox_XYZ_Collapse) # GIVES EACH VOXEL A UNIQUE IDENTIFYER
  #
  # Create vector of points ordered in same order as VoxID
  Vox_XYZ_Collapse_PointID <-paste(Vox_XYZ_Collapse$PointID, ",")
  Vox_XYZ_Collapse_PointID <-unlist(strsplit(Vox_XYZ_Collapse_PointID, ","))
  Vox_XYZ_Collapse_PointID <-as.numeric(Vox_XYZ_Collapse_PointID)
  #
  # CALCULATING HOW MANY POINTS IN EACH VOXEL
  Vox_XYZ_Point_Count <- voxel_metrics(LAS_XYZ_ID, length(Z), res = Para_Vox_Res)
  colnames(Vox_XYZ_Point_Count)[ncol(Vox_XYZ_Point_Count)] <- "Point_Count_In_Vox"
  # browser()
  Vox_XYZ_Point_Count <- cbind(Vox_XYZ_Point_Count, PointID= Vox_XYZ_Collapse$PointID, VoxID=Vox_XYZ_Collapse$VoxID)
  #
  # CREATING VECTOR THE LENGTH OF POINTS WITH VoxID FOR EACH POINT
  Vox_XYZ_Expand <- unlist(mapply(rep, Vox_XYZ_Collapse$VoxID,  Vox_XYZ_Point_Count$Point_Count_In_Vox))
  if(class(Vox_XYZ_Expand)[1] == "matrix"){Vox_XYZ_Expand <-as.vector(Vox_XYZ_Expand)}
  #browser()
  #ASSIGNING EACH POINT TO VOXEL and PUT VOX ID INTO STEM LAS FILE
  VoxID_TID_df <- data.frame(PointID=Vox_XYZ_Collapse_PointID, VoxID=Vox_XYZ_Expand)
  LAS_XYZ_ID@data$VoxID[match(VoxID_TID_df$PointID,LAS_XYZ_ID@data$PointID)] <- VoxID_TID_df$VoxID
  Output <- list(LAS_XYZ_ID, Vox_XYZ_Point_Count, VoxID_TID_df)
}

FUNCTION_RASTERISE_RM_OUTLIERS <- function(TID, data){ #, CLUSTER_SIZE , Constraint_1_2_3_4_5
  # browser()
  if(TID != 0){
    print(TID)
    LAS_VOX_oneT <- LAS(data)
    R_Cnt <-   grid_metrics(LAS_VOX_oneT, length(Z), res = Para_Raster_Res) # CHANGED FROM TID[which.max(Z)] to TID[which.max(Z)]
    Pnts_Cnt <- rasterToPoints(R_Cnt, spatial = TRUE)
    R_ZRange <-   grid_metrics(LAS_VOX_oneT, max(Z)-min(Z), res = Para_Raster_Res)
    Pnts_ZRange <- rasterToPoints(R_Cnt, spatial = TRUE)@data
    #
    Coord_R <- as.data.table(Pnts_Cnt@coords); Coord_R$Z <- 1
    
    if(nrow(Coord_R) > 1){
      Vox_oneT_RmOutlier_Cl <- VoxR::distance_clustering(data = Coord_R,  d_clust=(Para_Raster_Res*3)) 
      
      R_Cluster <- rasterize(Pnts_Cnt, R_Cnt, as.vector(Vox_oneT_RmOutlier_Cl[,4]))
      # plot(R_Cluster)
      
      # SEE WHICH CLUSTER (XY DISTANCING DIMENSION) HAS THE LARGEST Z RANGE AS THIS WILL BE THE TREE
      R_Stack <- stack(R_ZRange,
                       R_Cluster)
      names(R_Stack) <- c("ZRange", "Cluster")
      Stack_Values <- values(R_Stack)
      Stack_Values <-as.data.frame(Stack_Values[rowSums(is.na(Stack_Values)) != ncol(Stack_Values),])  
      TID_Cluster <- Stack_Values$Cluster[which.max(Stack_Values$ZRange)] 
      #
      R_Cluster[R_Cluster != TID_Cluster] <-  0
      R_Cluster[R_Cluster == TID_Cluster] <- 1
      LAS_VOX_oneT_ClusterOutlier <- merge_spatial(LAS_VOX_oneT, R_Cluster, attribute = "Cluster")
      
      # UPDATE THE RESULTS
      data$MakeZeroOutliers[which(LAS_VOX_oneT_ClusterOutlier@data$Cluster == 0)] <- 1
    }
    #         PLOT_test <- LAS_VOX_oneT_ClusterOutlier
    #         PLOT_test@data$TID <- PLOT_test@data$Cluster
    #         PLOT_COL_FUN(PLOT_test, Title="Test", size = 5, TID_Number = "No", ShowGridID="No", Check_Plot= "Yes")
    #         browser()
  }
 
  return(data)
} 

###################################################################################################################################################################
# FUNCTION FOR CONSTRAINT (6) IF THE SUBJECT CLUSTER IS BEYOND A CERTAIN SIZE THEN TEST WHETHER THEY OVERLAP USING RASTERISING PROCEDURE.
FUNCTION_RASTERISE <- function(TID, data, CLUSTER_ID){ #, CLUSTER_SIZE , Constraint_1_2_3_4_5

  # print(TID)
  Zero_Subject_Clusters <- unique(Close_Clusters_allSl$Subj_Cluster[which(Close_Clusters_allSl$Subj_TID == 0)])
  
  Index_Clusters <- which(CLUSTER_ID %in% Zero_Subject_Clusters) # & CLUSTER_SIZE > 500
  #browser()
  #if(TID == 298){browser()}
  if(length(Index_Clusters) > 0){
    flag <- 1
    # NEED TO WORK OUT WHAT PORTION OF EACH NEAR CLUSTER_ID HAS SAME PIXELS AS SUBJECT CLUSTER_ID
    # FOR THOSE THAT SHARE SAME PIXELS I NEED TO WORK OUT WHETHER THE BOTTOM OF ONE IS BELOW THE TOP OF THE OTHER (DID THIS BUT NOT USING)
    Clusters_For_Rasterising <- CLUSTER_ID[Index_Clusters]
    Close_Clusters_Rasterising <- Close_Clusters_allSl[which(Close_Clusters_allSl$Subj_Cluster  %in% Clusters_For_Rasterising),]
    unique_Near <- unique(Close_Clusters_Rasterising$Near_Cluster)
    
    # CREATE LAS WITH VOXELS FOR SUBJ AND NEAR CLUSTER_IDS
    SubjVox <- Vox_allT_allP_Cl[which(Vox_allT_allP_Cl$Cluster_ID %in% Clusters_For_Rasterising),]
    LAS_SubjVox <- LAS(SubjVox)
    NearVox <- Vox_allT_allP_Cl[which(Vox_allT_allP_Cl$Cluster_ID %in% unique_Near),]
    LAS_NearVox<- LAS(NearVox)
    #
    # GENERATE RASTERS
    R_Subj_TID <-   grid_metrics(LAS_SubjVox, TID[which.min(Z)], res = Para_Raster_Res) # CHANGED FROM TID[which.max(Z)] to TID[which.max(Z)]
    R_Subj_CLUSTER_ID <-   grid_metrics(LAS_SubjVox, Cluster_ID[which.min(Z)], res = Para_Raster_Res)
    R_Subj_MinZ <-   grid_metrics(LAS_SubjVox, min(Z), res = Para_Raster_Res)
    e_Subj <- extent(R_Subj_MinZ)
    #
    R_Near_TID <-   grid_metrics(LAS_NearVox, TID[which.max(Z)], res = Para_Raster_Res)
    R_Near_CLUSTER_ID <-   grid_metrics(LAS_NearVox, Cluster_ID[which.max(Z)], res = Para_Raster_Res)
    R_Near_MaxZ <-   grid_metrics(LAS_NearVox, max(Z), res = Para_Raster_Res)
    e_Near <- extent(R_Near_MaxZ)
    #browser()
    E_Overlap <- intersect(e_Near, e_Subj) 
    if(!is.null(E_Overlap)){ # IF THEY INTERSECT
      flag <- 2
      if((round(E_Overlap[1], 3) != round(E_Overlap[2], 3)) & (round(E_Overlap[3], 3) !=  round(E_Overlap[4], 3))){ # IF INTERSECT IS IN BOTH DIRECTION X and Y
        flag <- 3
        # STACK RASTERS (FIRST CROP TO SAME EXTENT)
        # Extent_Both <- c(max(c(e_Subj[1], e_Near[1])),min(c(e_Subj[2], e_Near[2])), max(c(e_Subj[3], e_Near[3])), min(c(e_Subj[4], e_Near[4])))
        R_Near_MaxZ_Crop <- crop(R_Near_MaxZ, E_Overlap)
        R_Near_CLUSTER_ID_Crop <- crop(R_Near_CLUSTER_ID, E_Overlap)
        R_Near_TID_Crop <- crop(R_Near_TID, E_Overlap)
        #
        #ee <- extent(R_Near_MaxZ_Crop)
        R_Subj_MinZ_Crop <- crop(R_Subj_MinZ, E_Overlap)
        R_Subj_CLUSTER_ID_Crop <- crop(R_Subj_CLUSTER_ID, E_Overlap)
        R_Subj_TID_Crop <- crop(R_Subj_TID, E_Overlap)
        #
        R_Stack <- stack(R_Subj_TID_Crop,
                         R_Subj_CLUSTER_ID_Crop,
                         R_Subj_MinZ_Crop, 
                         R_Near_TID_Crop,
                         R_Near_CLUSTER_ID_Crop,
                         R_Near_MaxZ_Crop)
        
        # GENERATE VOXEL WISE DATAFRAME OF STACKED RASTERS
        names(R_Stack) <- c("Subj_TID", "Subj_CLUSTER_ID", "Subj_MinZ", "Near_TID", "Near_CLUSTER_ID",  "Near_MaxZ")
        Stack_Values <- values(R_Stack)
        
        if(nrow(Stack_Values) > 1){ # iF THERE IS AT LEAST ONE PIXEL OVERLAPPING AFTER CROPPING
          flag <- 4
          Stack_Values <-as.data.frame(Stack_Values[rowSums(is.na(Stack_Values)) != ncol(Stack_Values),])
          
          # GENERATE TABLE OF NEAR_TID TOTALS AND SUBJ_CL TOTALS
          Near_TID_Cnt <- data.frame(table(Stack_Values$Near_TID))
          colnames(Near_TID_Cnt) <- c("Near_TID", "NearTID_Total_Cnt")
          Subj_Cluster_ID_Cnt <- data.frame(table(Stack_Values$Subj_CLUSTER_ID))
          colnames(Subj_Cluster_ID_Cnt) <- c("Subj_CLUSTER_ID", "SubjCl_Total_Cnt")
          
          
          Overlap_Near_Subj <- na.omit(Stack_Values) # THIS IDENTIFIES PIXELS THAT SHARE (OVERLAP) WITH SUBJ AND NEAR
          Overlap_Near_Subj <- Overlap_Near_Subj %>%
            group_by(Subj_TID, Subj_CLUSTER_ID , Near_TID, Near_CLUSTER_ID, ) %>%
            summarize(Subj_Near_Overlap_Cnt = length(Subj_CLUSTER_ID)
                      #, subj_MinZ_Mean = mean(Subj_MinZ),
                      #Near_MaxZ_Mean = mean(Near_MaxZ),
                      #Diff_SubjMinz_NearMaxZ = mean(Near_MaxZ-Subj_MinZ),# DOM DOM DOM .... I AM NOT USING  AT THIS STAGE
                      # Close_Overlap_Cnt = length(which((Near_MaxZ-Subj_MinZ) < 0.5))
            )
          #browser()
          # DOM DOM DOM !!! MAKE SURE BELOW IS CORRECT (HAVEN'T LOOK AT CLOSELY)
          Overlap_Near_Subj$SubjCl_Total_Cnt <- 0
          Index_1 <- which(Overlap_Near_Subj$Subj_CLUSTER_ID  %in%  Subj_Cluster_ID_Cnt$Subj_CLUSTER_ID )
          Index_2 <- match(Overlap_Near_Subj$Subj_CLUSTER_ID[Index_1],  Subj_Cluster_ID_Cnt$Subj_CLUSTER_ID)
          Overlap_Near_Subj$SubjCl_Total_Cnt[Index_1] <- as.integer(Subj_Cluster_ID_Cnt$SubjCl_Total_Cnt[Index_2])
          #    
          Overlap_Near_Subj$NearTID_Total_Cnt <- 0
          Index_1 <- which(Overlap_Near_Subj$Near_TID  %in%  Near_TID_Cnt$Near_TID )
          Index_2 <- match(Overlap_Near_Subj$Near_TID[Index_1],  Near_TID_Cnt$Near_TID)
          Overlap_Near_Subj$NearTID_Total_Cnt[Index_1] <- as.integer(Near_TID_Cnt$NearTID_Total_Cnt[Index_2])
          #
          Overlap_Near_Subj$Portion_Sub_Overlap <- Overlap_Near_Subj$Subj_Near_Overlap_Cnt/Overlap_Near_Subj$NearTID_Total_Cnt # SubjCl_Total_Cnt 
        }else{
          Overlap_Near_Subj <- list(rep(NA, length(CLUSTER_ID))) }  # WHEN THERE IS ONLY ONE PIXEL SHARED AND NOT WORTH PROCESSING
      }else{
        Overlap_Near_Subj <- list(rep(NA, length(CLUSTER_ID))) } # WHEN THERE IS ONLY ONE PIXEL SHARED AND NOT WORTH PROCESSING
    }else{
      Overlap_Near_Subj <- list(rep(NA, length(CLUSTER_ID))) }
    #if(TID == 0){browser()}
    return(list(Overlap_Near_Subj))    # return(Overlap_Near_Subj <- list(rep(NA, length(CLUSTER_ID))) #
  }else{
    return(list(rep(NA, length(CLUSTER_ID))))}
} # END FUNCTION FOR CONSTRAINT 6

#############################################################################################################################################
##############################################################################################################
# FUNCTION FOR FILLING IN NAS OF WS SLICE ANALYSIS ..... USED IN GET_TREE_ATTRIBUTES_CNN_FUN
##############################################################################################################
FUNCTION_FILL_WS_NA <- function(R_WS,  WS_Done, WS_Missing, WS_Done_SUMMARY, Cluster_ID, Missing_CellID, Raster_Res, Sl_ID){
  #print(paste("Cluster_ID", Cluster_ID))

  Index_Missing_CID <- which(WS_Missing$Cluster_ID %in% Cluster_ID)
  
  if(!all(is.na(WS_Done_SUMMARY))){ # IF THERE ARE DONE WATERSHEDS IN THE SLICE
    Index_Done_CID <- which(WS_Done_SUMMARY$Cluster_ID %in% Cluster_ID)
    
    if(length(Index_Done_CID) == 0){
      #print(1)
      #if(Sl_ID == 37){browser()}
      WS_Missing$WS[Index_Missing_CID] <- WS_Missing$Index_Miss_CID[Index_Missing_CID]
      WS_Missing <- WS_Missing[Index_Missing_CID,]
      WSID_New <- WS_Missing$WS
    }else{
      #############################################
      # DISTANCE CALCULATION BETWEEN MISSING AND DONE
      #############################################
      XY_Miss <- xyFromCell(R_WS, Missing_CellID, spatial=FALSE)
      #
      CellID_Done <- WS_Done$Cell_ID[which(WS_Done$Cluster_ID == Cluster_ID)]
      XY_Done <- xyFromCell(R_WS, CellID_Done, spatial=FALSE)
      #
      Dist_Missed_Done_Same_ClusterID <- rdist(XY_Miss, XY_Done) # ROW , COL
      #  
      Min_Miss = apply(Dist_Missed_Done_Same_ClusterID, 1, function(x) sort(x)[which(sort(x) > 0)[1]]) # FOR EACH MISSING CALC NEAREST
      indexMiss_Min = apply(Dist_Missed_Done_Same_ClusterID, 1, function(x) order(x)[which(sort(x) > 0)[1]]) # FOR EACH MISSING CALC INDEX OF NEAREST
      #
      Constraint_Dist <- sqrt((Raster_Res)^2 + (Raster_Res)^2) + 0.01
      IndexMiss_Close_Min <- which(Min_Miss <= Constraint_Dist)
      IndexMiss_Far_Min <- which(Min_Miss > Constraint_Dist)
      
      ##############################
      # # CID NOT IN WS_Done_SUMMARY
      ##############################
      if(length(Index_Done_CID) ==  0){
        #print(1)
        #if(Sl_ID == 32){browser()}
        WS_Missing$WS[Index_Missing_CID] <- WS_Missing$Index_Miss_CID[Index_Missing_CID]
        WS_Missing <- WS_Missing[Index_Missing_CID,]
        WSID_New <- WS_Missing$WS
      }
      # ##############################
      # # ONLY ONE WS WITH CID
      # ##############################
      # if(length(Index_Done_CID) == 1){
      #   #print(2)
      #   WSID_New <- WS_Done_SUMMARY$WS[which(WS_Done_SUMMARY$Cluster_ID%in% Cluster_ID)]
      #   WS_Missing$WS[Index_Missing_CID] <- WSID_New
      # }
      
      ##############################
      # MORE THAN ONE WS WITH CLUSTER_ID (ASSIGN TO NEAREST WITH DIST CALC)
      ##############################
      # if(Sl_ID ==35){browser()}
      if(length(Index_Done_CID) >= 1){
        # browser()
        # IF A CLUSTER_ID IS ASSOCIATED WITH MORE THAN ONE WS THEN YOU HAVE TO PUT IN THE NEAREST WS
        # CALCULATE DISTANCES BETWEEN WS CELL COORDS AND MISSING CELLS AND ASSIGN CELLS TO NEAREST WS.
        WS_Missing_Orig <- WS_Missing
        if(length(IndexMiss_Close_Min) > 0){
          #INDEX_Close <- indexMiss_Min[IndexMiss_Close_Min]
          #
          IndexDone_CellID <- match(CellID_Done[indexMiss_Min],WS_Done$Cell_ID)
          CellID_Done_Min <- WS_Done$Cell_ID[IndexDone_CellID]
          #        
          Index_Missing_Cell_ID <- match(Missing_CellID, WS_Missing$Cell_ID)   
          #
          WS_Missing$WS[Index_Missing_Cell_ID]  <- WS_Done$WS[IndexDone_CellID]  
          WS_Missing <- WS_Missing[Index_Missing_Cell_ID,]
          WS_Missing_Close <- WS_Missing
          if(length(IndexDone_CellID) != length(Index_Missing_Cell_ID)){browser()}
          #
          # if(length(match(Missing_CellID,WS_Missing$Cell_ID)) != length(INDEX_Close)){browser()}
        }
        #print(4)
        
        if(length(IndexMiss_Far_Min) > 0){
          WS_Missing <- WS_Missing_Orig
          #INDEX_Far <- indexMiss_Min[IndexMiss_Far_Min]
          # if(length(INDEX_Far) > 0){browser()}
          WS_Missing$WS[match(Missing_CellID,WS_Missing$Cell_ID)[IndexMiss_Far_Min] ]  <- max(WS_Done$WS) + 1 # WS_Done$WS[INDEX_Far]  
          WS_Missing <- WS_Missing[match(Missing_CellID,WS_Missing$Cell_ID)[IndexMiss_Far_Min],]
          WS_Missing_Far <- WS_Missing
        }
        #if(Sl_ID == 55){browser()}
        if(exists("WS_Missing_Far") & exists("WS_Missing_Close")){
          WS_Missing <- rbind(WS_Missing_Far, WS_Missing_Close) 
        }
        
        
        WSID_New <- WS_Missing$WS
        # ASSIGN MISS TO NEAREST WS .... IF MORE THAN A CERTAIN DISTANCE IT CAN BE ASSIGNED TO NEW WS.
        #print(5)
      }
    } # 13      1  2        931              4
    
  }else{
    ##########################################################################
    # THERE ARE NO WS IN SLICE SO GIVE THEM INDEX OF M
    ##########################################################################
    print(6)
    # browser()
    WS_Missing$WS[Index_Missing_CID] <- WS_Missing$Index_Miss_CID[Index_Missing_CID]
    WS_Missing <- WS_Missing[Index_Missing_CID,]
    WSID_New <- WS_Missing$WS
  }
  return(WS_Missing)
}
#############################################################################################################################################
# GET_TREE_ATTRIBUTES_CNN_FUN ... COMPUTE TID ATTRIBUTES FOR CNN 
##################################################################################

#### DOM DOM DOM !!! SEE GET_TREE_ATTRIBUTES_CNN_FUN FOR A VARIANT (in Y:\R_Code\CNN_R_Code\PreMASTER_CNN)

GET_TREE_ATTRIBUTES_CNN_FUN = function(X, Y, Z, Classify,  Vox_PntCnt,  Cluster_ID, Slice_Size_Zaxis, Unique_Poly_ID = 0, LAS_Type)
{
  #browser()
  # BINNING TREE
  if((length(seq(min(Z), max(Z)+Slice_Size_Zaxis, Slice_Size_Zaxis))) >1) { # THIS IF STATEMENT GETS AROUND TREE HAVING ONE BIN
    Slice_Height <- cut(Z, seq(min(Z), max(Z)+Slice_Size_Zaxis, Slice_Size_Zaxis),labels=seq(min(Z)+Slice_Size_Zaxis, max(Z)+Slice_Size_Zaxis, Slice_Size_Zaxis), include.lowest= TRUE) # labels=1:(length(seq(min(Z), max(Z),Slice_Size_Zaxis))-1))
  }else{
    Slice_Height <- rep(1, length(Z))
  }
  
  Slices_ID_DF <-na.omit(data.frame(X,Y,Z,Vox_PntCnt,  Cluster_ID ,Slice_Height, LAS_Type))
  Unique_BIN_ID <- sort(as.numeric(unique(Slices_ID_DF$Slice_Height)))
  
  # Splitting data.frame into lists for each bin
  splits <- split(Slices_ID_DF, Slices_ID_DF$Slice_Height)
  Count_Splits <- length(sapply(splits, NROW))
  if(length(splits) != length(Unique_BIN_ID)){ # WORK AROUND WHEN THERE IS AN EMPTY LIST
    splits <- splits[- which(as.vector(sapply(splits, NROW)) ==0)]
  }

  # Test2 <- as.numeric(unlist(lapply(splits, function(x) nrow(x))))
  # sum(Test2)
  # Test <- do.call(rbind,splits)
  # #browser()
  # PSID <- sort(Test$LAS_Type)
  # PSID2 <- LAS_One_TID@data$LAS_Type
  # setdiff(PSID,PSID2  )
  # setdiff(PSID2,PSID)

  ####################################################################################################
  ####################################################################################################
  ################################################
  # GET POLYGON OF EACH BIN AND AREA IT REPRESENTS
  ################################################
  
  Raster_Res <- 0.2
  
  Polygon_Slices <- lapply(splits, function(x) {
    
    Sl_Height <- as.numeric(as.character(x$Slice_Height[1]))  
    Sl_ID <- which(as.numeric( names(splits)) %in% Sl_Height)
    # LAS SLICE
    LAS_oneSt_oneSl <- LAS(x)
    # BEFORE_PSID <- nrow(LAS_oneSt_oneSl@data)
    #if(min(LAS_oneSt_oneSl$Z) > 55){browser()}
    # RASTERISING THE SLICE LAS COUNT, SLICE 
    if(nrow(LAS_oneSt_oneSl@data) > 1){
      
      # browser()
      # if(Sl_Height 25.281 25.501)
      
      # if(length(which(LAS_oneSt_oneSl@data$Cluster_ID == 5468)) > 0){browser()}
      
      R_oneSl_ZCount <-   grid_metrics(LAS_oneSt_oneSl, ~sum(Vox_PntCnt), res = Raster_Res) #
      R_oneSl_Cluster_ID <-   grid_metrics(LAS_oneSt_oneSl, ~as.numeric(names(sort(table(Cluster_ID),decreasing=TRUE)))[1], res = Raster_Res) # as.numeric(names(table(Cluster_ID)[which.max(table(Cluster_ID))]))
      # R_oneSl_PixelVal <- R_oneSl_Cluster_ID
      # values(R_oneSl_PixelVal) <- seq(1, ncell(R_oneSl_PixelVal),1)
      
      if(ncell(R_oneSl_ZCount) > 1){
        algo = lidR::watershed(R_oneSl_ZCount, tol = 1, ext=5) 
        LAS_oneSt_oneSl <- segment_trees(LAS_oneSt_oneSl, algorithm = algo, attribute = "WSID", uniqueness = "incremental")
        R_WS <- algo() 
        Unique_WS <- as.numeric(names(table(LAS_oneSt_oneSl@data$WSID)))
        # if(Sl_ID == 35){browser()}
        # SANITY CHECK
        print(paste("Unique_WS", length(Unique_WS)))
        if(length(which(Unique_WS == 0)) > 0){flag <- 1; browser()} ### DOM DOM DOM !!!  7/4/2022 I WONDER IF THE WS 0 NEED TO BE CHANGED INTO SOMETHING
          # plot(R_WS); plot(R_oneSl_ZCount)
        Unique_WS <- Unique_WS[which(Unique_WS > 0)]  
        
        if(length(Unique_WS) > 0){

          ####################
          # STACK THE RASTERS
          ####################
          # WORK AROUND FOR WHEN THE ROWS AND COLS ARE BUGGY (FLIPPED FOR UNKNOWN REASON)
          if((R_WS@ncols == R_oneSl_ZCount@ncols)&(R_WS@nrows == R_oneSl_ZCount@nrows)){
            R_Stack <- stack(R_oneSl_ZCount,  R_WS, R_oneSl_Cluster_ID) # R_oneSl_ClID,
          }else{
            R_WS_Nrows <- R_WS@nrows
            R_WS_Ncols <- R_WS@ncols
            R_WS@ncols <-  R_WS_Nrows
            R_WS@nrows <-  R_WS_Ncols
            R_Stack <- stack(R_oneSl_ZCount,  R_WS) # R_oneSl_ClID,
          }
          
          R_Stack_DF <- data.frame(Cell_ID = seq(1, ncell(R_oneSl_ZCount),1) , getValues(R_Stack))
          colnames(R_Stack_DF) <- c("Cell_ID",
                                    "ZCount",
                                    "WS",
                                    "Cluster_ID")
          R_Stack_DF <- data.frame(sapply(R_Stack_DF, function(x) as.numeric(as.character(x))))

          ######################################
          # IMPROVE R_WS BY FILLING IN NA VALUES 
            # (FILL NA WITH  WS USING CLUSTER_ID OF CELL.... FIND ALL WS THAT ARE REPRESENTED WITH THE SAME Cluster_ID.... 
              # USE DISTANCE CALCULATION THAT FINDS NEAREST WS WITH SAME CLUSTER ID)
              # IF NOT SAME CLUSTER ID GIVE A NEW WS_ID
          ######################################
          
          LAS_oneSt_oneSl <- merge_spatial(LAS_oneSt_oneSl, R_WS, attribute = "WSID")
          #LAS_oneSt_oneSl <- merge_spatial(LAS_oneSt_oneSl, R_oneSl_PixelVal, attribute = "PixVal")
          
          if(!all(is.na(R_Stack_DF$ZCount) ==  is.na(R_Stack_DF$WS))){  # if(!all(!is.na(LAS_oneSt_oneSl@data$WSID))){ # 
            # PixVal_Missing <- LAS_oneSt_oneSl@data$PixVal[which(is.na(LAS_oneSt_oneSl@data$WSID))]
            # IDENTIFY PIXELS THAT HAVE MISSING WS. 
            Index_WS_NAs_ForFill <- which(is.na(R_Stack_DF$ZCount) !=  is.na(R_Stack_DF$WS))
            WS_Missing <- R_Stack_DF[Index_WS_NAs_ForFill,]
            max_WSID <- max(R_Stack_DF$WS, na.rm = T)
            
            # GENERATE A UNIQUE WS FOR EACH MISSING PIXEL WITH A UNIQUE CLUSTER_ID ( IF DISTANCE OF PIXEL IS FAR FROM OTHER PIXELS THEN THIS UNIQUE WS WILL BE ASSIGNED LATER)
            Indexing_MISSING_CID <- data.frame(Unique_CID  = unique(WS_Missing$Cluster_ID), Index_Miss_CID = (max_WSID+1):(max_WSID+length(unique(WS_Missing$Cluster_ID))))
            WS_Missing$Index_Miss_CID <- Indexing_MISSING_CID$Index_Miss_CID[match(WS_Missing$Cluster_ID, Indexing_MISSING_CID$Unique_CID)]
            
            # SANITY CHECK.... 
            if(nrow(WS_Missing) == 0){browser()}
            
            # INDEX ALL PIXELS WITH A CLUSTER_ID THAT IS FOUND IN PIXELS THAT HAVE MISSING CLUSTERS
            Index_Cluster_ID <- which(R_Stack_DF$Cluster_ID %in% unique(WS_Missing$Cluster_ID))
            WS_Done <- R_Stack_DF[setdiff(Index_Cluster_ID, Index_WS_NAs_ForFill),]   # & !Index_WS_NAs_ForFill
            if(nrow(WS_Done) > 0){
              WS_Done_SUMMARY <- as.data.frame(WS_Done %>%
                                                 dplyr::group_by(WS, Cluster_ID) %>%
                                                 dplyr::summarise(Sum_ZCount = sum(ZCount),
                                                                  Count_Unique_WS = length(unique(WS)),
                                                                  .groups = 'drop'))
            }else{
              #print("HAS NO WS_Done")
              WS_Done_SUMMARY <- NA
            }
            #if(Sl_ID == 27){browser()}
            # RUN FUNCTION FOR FILLING IN WS WITH NA
            WS_Missing_SUMMARY <- WS_Missing %>%
                group_by(Cluster_ID) %>%
                nest() %>%
                rowwise() %>%
                mutate(Missing_CellID  = list(data$Cell_ID)) %>%
                mutate(WS_Missing_Filled  = list(FUNCTION_FILL_WS_NA(R_WS, WS_Done, WS_Missing, WS_Done_SUMMARY, Cluster_ID, Missing_CellID, Raster_Res, Sl_ID))) %>%
                mutate(WSID_New  = list(WS_Missing_Filled$WS))

            # if(Sl_ID == 37){browser()}
            WS_Missing_Filled <- na.omit(reduce(WS_Missing_SUMMARY$WS_Missing_Filled, full_join))

            R_WS@data@values[WS_Missing_Filled$Cell_ID] <- WS_Missing_Filled$WS
           
            # UPDATE DATA
            LAS_oneSt_oneSl <- merge_spatial(LAS_oneSt_oneSl, R_WS, attribute = "WSID")
            } # FILL NA PROCEDURE
  ###########################################################

          # HAVE NA WSID THAT NEED TO SORT OUT !!!
          WS_Grid_Count_Add <- c()
          if(!all(!is.na(LAS_oneSt_oneSl@data$WSID))){
            print("LAS WSID HAS NAs")
            # LAS_NAs <- filter_poi(LAS_oneSt_oneSl, is.na(WSID))
            # NAs  <- nrow(LAS_NAs@data)
            max_WSID <- max(LAS_oneSt_oneSl@data$WSID, na.rm= TRUE)

            Index_NAS <- which(is.na(LAS_oneSt_oneSl@data$WSID))
            NEW_WSID <- seq(max_WSID+1,max_WSID+length(Index_NAS),1)
            LAS_oneSt_oneSl@data$WSID[Index_NAS] <- NEW_WSID
            flag <- 333
            WS_Grid_Count_Add <- rep(1, length(Index_NAS)) 
          }
          
          # UPDATE UNIQUE WSID
          Unique_WS_Updated <- as.numeric(names(table(LAS_oneSt_oneSl@data$WSID)))
          # else{
          #   NAs  <- 0
          # }

          #browser()
          ######################################
          # WATERSHED POINT COUNT AND GRID COUNT
          ######################################
          WS_Pnt_Count <- as.vector(table(LAS_oneSt_oneSl@data$WSID))
          WS_Grid_Count <-c(as.vector(table(getValues(R_WS))), WS_Grid_Count_Add)

         
          # # IF SOME WS GRIDS DON'T HAVE LAS POINTS (SLIGHT OFFSET OR ON EDGE DOESN'T REGISTER)
          # 
          # if(length(WS_Pnt_Count) != length(WS_Grid_Count)){
          # 
          #   #if(min(LAS_oneSt_oneSl$Z) > 55){flag <- 2; browser()}
          #   Remove_Length <- length(WS_Grid_Count) - length(WS_Pnt_Count)
          #   Remove_Value <- sort(WS_Grid_Count)[Remove_Length]
          #   Remove_Index <- which(WS_Grid_Count %in% Remove_Value)[Remove_Length]
          #   WS_Grid_Count <- WS_Grid_Count[-Remove_Index]
          # 
          # }
          
          #########################################
          # LOCATE MAX DENSITY CELL_ID FOR EACH WS    # THIS BECOMES XY CENTROID
          #########################################
          # Cell_ID_maxDen <- as.data.frame(R_Stack_DF %>%
          #                                   dplyr::select(Cell_ID, ZCount, WS) %>%
          #                                   dplyr::group_by(WS) %>%
          #                                   dplyr::summarise(Cell_ID_maxDen = Cell_ID[which.max(ZCount)][1], .groups = 'drop'))
          # 
          # Cell_ID_maxDen <- na.omit(Cell_ID_maxDen)
          # Cell_ID_maxDen <- Cell_ID_maxDen[which(Cell_ID_maxDen$WS %in% Unique_WS),]
          # XY_WS_MaxDen <- xyFromCell(R_WS, Cell_ID_maxDen$Cell_ID_maxDen, spatial=FALSE)  # THIS BECOMES XY CENTROID
          # Pnts_WS_MaxDen_oneSl<- SpatialPoints(XY_WS_MaxDen)
          Cell_ID_maxDen <- as.data.frame(LAS_oneSt_oneSl@data) %>%
                                            dplyr::select(X, Y, Z, WSID) %>%
                                            dplyr::group_by(WSID) %>%
                                            dplyr::summarise(X = mean(X),
                                                             Y = mean(Y),
                                                             Z = round(mean(Z), 2), .groups = 'drop')
          Cell_ID_maxDen <- na.omit(Cell_ID_maxDen)
          Cell_ID_maxDen <- as.data.frame(Cell_ID_maxDen[which(Cell_ID_maxDen$WSID %in% Unique_WS_Updated),])
          Pnts_WS_MaxDen_oneSl <- SpatialPoints(Cell_ID_maxDen[,2:3])
          #Pnts_WS_MaxDen_oneSl <- SpatialPointsDataFrame(Pnts_WS_MaxDen_oneSl, )
          
          if(nrow(Cell_ID_maxDen) > 0){
        
            ########################
            # GET POLYGON OF EACH WS (MIN/MAX Length, AREA)
            ########################
            Poly_oneSl_allPoly <- SpatialPolygons(list()) # , proj4string = CRS(Proj_Sys)
            PolyMaxLength_oneSt_oneSl <- c()
            #PolyMinLength_oneSt_oneSl <- c()
            oneSl_maxCnt_CID <- c()
            LIST_oneSl_NotmaxCnt_CID <- c()
            

            
            print(paste("length(Unique_WS_Updated)", length(Unique_WS_Updated)))
            # if(Sl_ID == 62){browser()}
            for(UWS in 1:length(Unique_WS_Updated)){
              
              Unique_Poly_ID <- Unique_Poly_ID + 1
              LAS_oneSt_oneSl_onePoly <- filter_poi(LAS_oneSt_oneSl, WSID == Unique_WS_Updated[UWS])
              #
              # WHOLE SLICE WS
              Slice_CHull = CONVHULL_FUN(LAS_oneSt_oneSl_onePoly@data$X,
                                         LAS_oneSt_oneSl_onePoly@data$Y,
                                         Unique_Poly_ID)
              #
              Poly_SP_oneSl_onePoly <- SpatialPolygons(Slice_CHull$Poly) # , proj4string = CRS(Proj_Sys)
              Poly_SP_oneSl_onePoly <- gBuffer( Poly_SP_oneSl_onePoly, width=0.2, byid=TRUE ) # DOM DOM DOM !!! 7/4/2022 CONSIDER MAKING A SMALL BUFFER AROUND EACH 
              #
              CoordPoly_oneSl_onePoly  <- Poly_SP_oneSl_onePoly@polygons[[1]]@Polygons[[1]]@coords
              PolyMaxLength_oneSt_oneSl = c(PolyMaxLength_oneSt_oneSl, max(dist(CoordPoly_oneSl_onePoly)))# PolyMaxLength_oneSt_oneSl = c(PolyMaxLength_oneSt_oneSl, max(dist(cbind(unlist(Slice_CHull$X_Hull),unlist(Slice_CHull$Y_Hull)))))
              #
              Poly_oneSl_allPoly <- SpatialPolygons(c(slot(Poly_oneSl_allPoly,
                                                           "polygons"), Poly_SP_oneSl_onePoly@polygons)) #  Slice_CHull$Poly
              #
              
              Cluster_ID_Cnt <- as.numeric(names(which.max(table(LAS_oneSt_oneSl_onePoly@data$Cluster_ID))))
              oneSl_maxCnt_CID <- c(oneSl_maxCnt_CID, Cluster_ID_Cnt)  
              
              #########################################################################################################
              # TRACK THE CLUSTERS THAT ARE WITHIN WS BUT NOT THE CLUSTER WITH MAX NUMBER OF POINTS ASSOCIATED WITH WS.
                # # THIS TABLE NEEDS TO BE USED TO REMOVE SOME CLUSTER_IDS.
              #########################################################################################################
              Unique_oneSl_CID <- unique(LAS_oneSt_oneSl_onePoly@data$Cluster_ID)
              Index_NotMaxCnt <- which(Unique_oneSl_CID != Cluster_ID_Cnt)
              if(length(Index_NotMaxCnt)> 0){
                # NOTE THE FIRST TWO VALUE ARE SLICE NUMBER (Sl_ID) AND WS_ID
                LIST_oneSl_NotmaxCnt_CID <- append(LIST_oneSl_NotmaxCnt_CID, list(c(Sl_ID, Unique_WS_Updated[UWS], Unique_oneSl_CID[Index_NotMaxCnt])))
              }else{
                LIST_oneSl_NotmaxCnt_CID <- append(LIST_oneSl_NotmaxCnt_CID, list(c(Sl_ID, Unique_WS_Updated[UWS])))
                }
            } # END UWS LOOP
            
            # 
            # # THIS IS TO KEEP TRACK OF THE MISSING CLUSTERS FOR EACH SLICE
            # Unique_Cluster_ID_R <- sort(na.omit(unique(values(R_oneSl_Cluster_ID))))
            # Unique_Cluster_ID_LAS <- sort(as.numeric(names(table(LAS_oneSt_oneSl@data$Cluster_ID))))
            # Missing_ClusterID_inR <- setdiff(Unique_Cluster_ID_LAS, Unique_Cluster_ID_R)
            # 
            # if(Sl_ID == 62){browser()}
            # ### DOM DOM DOM !!! HERE YOU NEED TO GENERATE A TABLE OF ALL THE OTHER CLUSTER_IDS ASSOCIATED WITH EACH MAIN CLUSTER ID.

            Poly_oneSl_allPolyArea <- unlist(lapply(Poly_oneSl_allPoly@polygons, function(x) slot(x, "area")))

            # MIN LengthETER IS CALCULATED USING MAX DIAMETERS AND AREA IN AN ELIPSOID CALCULATION
            # PolyMinLength_oneSt_oneSl =Poly_oneSl_allPolyArea/as_units((pi * PolyMaxLength_oneSt_oneSl/2)*2, "m^2") ### DOM DOM DOM !!!  7/4/2022 IS THIS CORRECT
            PolyMinLength_oneSt_oneSl = Poly_oneSl_allPolyArea/PolyMaxLength_oneSt_oneSl
            
            Poly_oneSl_allPolyArea_Total <- sum(Poly_oneSl_allPolyArea)
            flag <- 5
            
          }else{ # IF THERE ARE NO Cell_ID_maxDen
            flag <- 13
          }
        }else{ # IF THERE ARE NO Unique WS IN SLICE  ..... (length(Unique_WS_Updated) > 0)
          flag <- 14
          print("NO Unique WS IN SLICENO Unique WS IN SLICE")
          Poly_oneSl_allPoly <- SpatialPolygons(list())
        }
      }else{ # IF THERE IS ONE OR LESS THAN ONE CELLS IN R ZCont ......  (ncell(R_oneSl_ZCount) > 1)
        flag <- 15
        print("ONE OR LESS THAN ONE CELLS IN R ZCont")
        Poly_oneSl_allPoly <- SpatialPolygons(list())

      }
    } else{  # IF THERE IS ONE OR LESS POINTS IN SLICE ......  (nrow(LAS_oneSt_oneSl@data)> 1)
      flag <- 16
      print("ONE OR LESS POINTS IN SLICE")
      Poly_oneSl_allPoly <- SpatialPolygons(list())
      
      # THIS NEEDS TO GO INTO A FUNCTION FOR CREATING OUTPUT FOR SLICES WITH VERY MINIMAL DATA.
          # WORK AROUND IS TO REMOVE THIS DAT IN TORCH_1c_TID_GT_CHARACTERISATION
      # N_Pnts <- nrow(LAS_oneSt_oneSl@data)
      # NEW_WSID <- seq(1,N_Pnts,1)
      # LAS_oneSt_oneSl@data$WSID[Index_NAS] <- NEW_WSID
      # 
      # Cell_ID_maxDen <- as.data.frame(LAS_oneSt_oneSl@data) %>%
      #   dplyr::select(X, Y, Z, WSID) %>%
      #   dplyr::group_by(WSID) %>%
      #   dplyr::summarise(X = mean(X),
      #                    Y = mean(Y),
      #                    Z = round(mean(Z), 2), .groups = 'drop')
      # Cell_ID_maxDen <- na.omit(Cell_ID_maxDen)
      # Cell_ID_maxDen <- as.data.frame(Cell_ID_maxDen[which(Cell_ID_maxDen$WSID %in% Unique_WS_Updated),])
      # Pnts_WS_MaxDen_oneSl <- SpatialPoints(Cell_ID_maxDen[,2:3])
    }
   
    if(length(Poly_oneSl_allPoly) > 0){
      print(paste("DONE Sl_ID", Sl_ID))
      #if(Sl_ID == 35){browser()}
      Poly_DF <- data.frame(Sl_ID = Sl_ID,
                            X_Cent = Cell_ID_maxDen[,2],
                            Y_Cent = Cell_ID_maxDen[,3],
                            Sl_Height= Sl_Height,   
                            Cluster_ID = oneSl_maxCnt_CID,
                            PolyArea = Poly_oneSl_allPolyArea,
                            allPolyArea_Total = Poly_oneSl_allPolyArea_Total,
                            PolyMaxLength = PolyMaxLength_oneSt_oneSl,
                            PolyMinLength = PolyMinLength_oneSt_oneSl,
                            WS_Grid_Count = WS_Grid_Count)
      Poly_oneSl_allPoly_SPDF <- SpatialPolygonsDataFrame(Poly_oneSl_allPoly, Poly_DF, match.ID = F)

      
      # GENERATE SPATIAL POINT DATA.FRAME FOR CENTROID CALCULATION
      #browser()
      Pnts_DF <- data.frame(Sl_ID = Sl_ID,
                            Cell_ID_maxDen[,c(1,4)], 
                            Sl_Height= Sl_Height,
                            oneSl_maxCnt_CID = oneSl_maxCnt_CID) # ,Cell_ID_maxDen = Cell_ID_maxDen$Cell_ID_maxDen
      Pnts_WS_MaxDen_oneSl <- SpatialPointsDataFrame(Pnts_WS_MaxDen_oneSl, Pnts_DF)
      # AFTER_PSID <- nrow(LAS_oneSt_oneSl@data)
      # if(BEFORE_PSID != AFTER_PSID){browser()}
      # print(paste(".............................................................................................................",AFTER_PSID))
      # print(paste(".............................................................................................................",BEFORE_PSID))
      #browser()
      list(Poly_oneSl_allPoly = Poly_oneSl_allPoly_SPDF,
           Pnts_WS_MaxDen_oneSl = Pnts_WS_MaxDen_oneSl,
           LIST_oneSl_NotmaxCnt_CID = LIST_oneSl_NotmaxCnt_CID,
           LAS_OUPUT = LAS_oneSt_oneSl) # , NAs = NAs)
    } 
  }) # END POLYGON FUNCTION

  ####################################################################################################
  ####################################################################################################
  ########################
  # EACH WS POLYGON OUTPUT
  ########################
  
  Poly_allSl_List <- sapply(Polygon_Slices,function(x) x[1])
  Pnts_allSl_List  <- sapply(Polygon_Slices,function(x) x[2])
  LIST_oneSl_NotmaxCnt_CID  <- sapply(Polygon_Slices,function(x) x[3])
  LAS_OUPUT  <- sapply(Polygon_Slices,function(x) x[4])
  # browser()
  # NAs  <- sapply(Polygon_Slices,function(x) x[4])

  return(list(Poly_allSl_List = Poly_allSl_List,
              Pnts_allSl_List = Pnts_allSl_List,
              LIST_oneSl_NotmaxCnt_CID = LIST_oneSl_NotmaxCnt_CID,
              LAS_OUPUT = LAS_OUPUT #, NAs = NAs
              ))
  
} # END GET_TREE_ATTRIBUTES_CNN_FUN

# Poly_oneSl_allPolyArea = Poly_oneSl_allPolyArea,
# Poly_oneSl_allPolyArea_Total = Poly_oneSl_allPolyArea_Total,
# PolyMaxLength_oneSt_oneSl = PolyMaxLength_oneSt_oneSl,
# PolyMinLength_oneSt_oneSl = PolyMinLength_oneSt_oneSl,
# XY_WS_MaxDen = XY_WS_MaxDen,                           # LOCATE MAX DENSITY CELL_ID FOR EACH WS .... THIS BECOMES XY CENTROID
# WS_Pnt_Count = WS_Pnt_Count,
# WS_Grid_Count = WS_Grid_Count

# Centroids_allSl_allPoly <- sapply(Polygon_Slices,function(x) x[6])
# AreaPoly_allSl_allPoly <- sapply(Polygon_Slices,function(x) x[2])
# AreaSl_allSl_allPoly <- sapply(Polygon_Slices,function(x) x[3])
# PolyMaxLength_allSl_allPoly <- sapply(Polygon_Slices,function(x) x[4])
# PolyMinLength_allSl_allPoly <- sapply(Polygon_Slices,function(x) x[5])
# Cluster_ID_allSl_allPoly <- sapply(Polygon_Slices,function(x) x[7])

# AreaPoly_allSl_allPoly = list(AreaPoly_allSl_allPoly),
# AreaSl_allSl_allPoly = list(AreaSl_allSl_allPoly),
# PolyMaxLength_allSl_allPoly = list(PolyMaxLength_allSl_allPoly),
# PolyMinLength_allSl_allPoly = list(PolyMinLength_allSl_allPoly),
# Cluster_ID_allSl_allPoly = list(Cluster_ID_allSl_allPoly)))
# 
# for(SA in 1:length(Poly_eachWS)){
#   oneSl_Poly_eachWS <- unlist(Poly_eachWS[SA], use.names = TRUE)
#   oneSl_Centroids_allSl_allPoly <- Centroids_allSl_allPoly [SA]
#   oneSl_AreaPoly_allSl_allPoly <- AreaPoly_allSl_allPoly [SA]
#   oneSl_AreaSl_allSl_allPoly <- AreaSl_allSl_allPoly [SA]
#   oneSl_PolyMaxLength_allSl_allPoly <- PolyMaxLength_allSl_allPoly[SA] 
#   oneSl_PolyMinLength_allSl_allPoly <- PolyMinLength_allSl_allPoly [SA]
#   oneSl_maxCnt_CID_allSl_allPoly <- Cluster_ID_allSl_allPoly [SA]
# }
# 


# 
# 
# # DOM DOM DOM !!! 8/04/22 SANITY CHECKING
# # Poly_eachWS <- unlist(sapply(Polygon_Slices,function(x) x[1]), use.names = TRUE)
# # Centroids_allSl_allPoly <- unlist(sapply(Polygon_Slices,function(x) x[6]), use.names = FALSE)
# # 
# # for(aaa in 10:length(Poly_eachWS)){
# #   plot(Poly_eachWS[aaa,],  pch=1, add=TRUE)
# #   plot(Pnts_SlCl_Centroids[aaa,], col=c("red"))
# #   
# #   
# #   browser()
# # }
# # 
# # browser()
# 
# # ID SLICES THAT WERE NOT COMPUTED DUE TO RASTER COUNT IN WS CALCULATION 
# Centroids_allSl_allPoly_X <- Centroids_allSl_allPoly[which(Centroids_allSl_allPoly < 5000000)]
# Zero_Centroid<- which(Centroids_allSl_allPoly_X == 0)
# 
# flag <- 6
#  #if(min(LAS_oneSt_oneSl$Z) > 55){browser()}  ### DOM DOM DOM !!! TRY AND MAKE SENSE OF THIS "TROUBLE SHOOT"
# 
# ### ALSO MAKE SURE THAT THE POLYGONS AND THE CENTROIDS CORRESPOND. MAYBE GENERATE A UNIQUE IDENTIFIER TO LINK THEM UP....
# 
# #browser()
# if(length(Zero_Centroid) >0){
#   Centroids_allSl_allPoly_X <- Centroids_allSl_allPoly_X[-Zero_Centroid]
#   Centroids_allSl_allPoly_Y <- Centroids_allSl_allPoly[which(Centroids_allSl_allPoly > 5000000)]
#   Centroids_allSl_allPoly <-Centroids_allSl_allPoly [which(Centroids_allSl_allPoly == 0)]
#   
#   AreaPoly_allSl_allPoly <- unlist(sapply(Polygon_Slices,function(x) x[2])   , use.names = TRUE)
#   AreaPoly_allSl_allPoly_Height <-str_extract(names(AreaPoly_allSl_allPoly), "\\-*\\d+\\.*\\d*")[-Zero_Centroid]
#   AreaPoly_allSl_allPoly <- as.vector(AreaPoly_allSl_allPoly)[-Zero_Centroid]
#   
#   PolyMaxLength_allSl_allPoly <- unlist(sapply(Polygon_Slices,function(x) x[4]), use.names = FALSE)[-Zero_Centroid]
#   PolyMinLength_allSl_allPoly <- unlist(sapply(Polygon_Slices,function(x) x[5]), use.names = FALSE)[-Zero_Centroid]
#   Cluster_ID_allSl_allPoly <- unlist(sapply(Polygon_Slices,function(x) x[7]), use.names = FALSE) [-Zero_Centroid]
#   # WSPntCount_allSl_allPoly <- unlist(sapply(Polygon_Slices,function(x) x[8]), use.names = FALSE) [-Zero_Centroid]
#   # WSGridCount_allSl_allPoly <- unlist(sapply(Polygon_Slices,function(x) x[9]), use.names = FALSE) [-Zero_Centroid]
#   Poly_eachWS <- unlist(sapply(Polygon_Slices,function(x) x[1]), use.names = TRUE)[-Zero_Centroid]
#   
# }else { # ALL WS RASTERS HAVE BEEN COMPUTED SO NO SLICES NEED REMOVAL
#   Centroids_allSl_allPoly_X <- Centroids_allSl_allPoly_X
#   Centroids_allSl_allPoly_Y <- Centroids_allSl_allPoly[which(Centroids_allSl_allPoly > 5000000)]
#   Centroids_allSl_allPoly <-Centroids_allSl_allPoly
#   
#   AreaPoly_allSl_allPoly <- unlist(sapply(Polygon_Slices,function(x) x[2])   , use.names = TRUE)
#   AreaPoly_allSl_allPoly_Height <-str_extract(names(AreaPoly_allSl_allPoly), "\\-*\\d+\\.*\\d*")
#   AreaPoly_allSl_allPoly <- as.vector(AreaPoly_allSl_allPoly)
#   
#   PolyMaxLength_allSl_allPoly <- unlist(sapply(Polygon_Slices,function(x) x[4]), use.names = FALSE)
#   PolyMinLength_allSl_allPoly <- unlist(sapply(Polygon_Slices,function(x) x[5]), use.names = FALSE)
#   Cluster_ID_allSl_allPoly <- unlist(sapply(Polygon_Slices,function(x) x[7]), use.names = FALSE) 
#   # WSPntCount_allSl_allPoly <- unlist(sapply(Polygon_Slices,function(x) x[7]), use.names = FALSE)
#   # WSGridCount_allSl_allPoly <- unlist(sapply(Polygon_Slices,function(x) x[8]), use.names = FALSE)
#   Poly_eachWS <- unlist(sapply(Polygon_Slices,function(x) x[1]), use.names = TRUE)
#   
#   AreaSl_allSl_allPoly <- unlist(sapply(Polygon_Slices,function(x) x[3]) , use.names = TRUE)
#   AreaSl_allSl_allPoly_Height <-str_extract(names(AreaSl_allSl_allPoly), "\\-*\\d+\\.*\\d*")
#   AreaSl_allSl_allPoly <- as.vector(AreaSl_allSl_allPoly)
# }
# # browser()
# Output_eachWS  <- data.frame( TreeID = rep(Classify, length(AreaPoly_allSl_allPoly_Height)),
#                               PolyHeight = as.numeric(as.character(AreaPoly_allSl_allPoly_Height)),
#                               PolyArea = AreaPoly_allSl_allPoly,
#                               PolyVolume = AreaPoly_allSl_allPoly * Slice_Size_Zaxis,
#                               PolyMaxLength = PolyMaxLength_allSl_allPoly,
#                               PolyMinLength = PolyMinLength_allSl_allPoly,
#                               LengthMean = (PolyMaxLength_allSl_allPoly - PolyMinLength_allSl_allPoly)/2,
#                               # WS_PntCount = WSPntCount_allSl_allPoly,
#                               # WSGrid_Count = WSGridCount_allSl_allPoly,
#                               # WSGrid_Volume = WSGridCount_allSl_allPoly * (Raster_Res*Raster_Res*Slice_Size_Zaxis),
#                               Cluster_ID = Cluster_ID_allSl_allPoly,
#                               X_Cent = Centroids_allSl_allPoly_X,
#                               Y_Cent = Centroids_allSl_allPoly_Y)
# 
# Points_Centroids_XYZ <- Output_eachWS[, match(c("X_Cent", "Y_Cent", "PolyHeight", "TreeID", "Cluster_ID", "LengthMean"), colnames(Output_eachWS))]
# coordinates(Points_Centroids_XYZ) = ~X_Cent + Y_Cent
# #crs(Points_Centroids_XYZ) <- crs(Proj_Sys)
# 
# ####################################################################################################
# ####################################################################################################
# 
# flag <- 7
#  #if(min(LAS_oneSt_oneSl$Z) > 55){browser()}  ### DOM DOM DOM !!! WORK OUT WHAT BELOW DOES
# 
# for(a in 1:length(Poly_eachWS)){
#   if(a == 1){
#     Poly_eachWS_All <- Poly_eachWS[[a]]
#     PolyID_Max <- max(as.numeric(sapply(slot(Poly_eachWS_All, "polygons"), function(x) slot(x, "ID"))))
#   }else{
#     Poly_oneWS <-Poly_eachWS[[a]]
#     new_IDs = as.numeric(sapply(slot(Poly_oneWS, "polygons"), function(x) slot(x, "ID"))) + PolyID_Max
#     if(length(new_IDs) > 0){
#       for (i in 1:length(slot(Poly_oneWS, "polygons"))){
#         slot(slot(Poly_oneWS, "polygons")[[i]], "ID") = as.character(new_IDs[i])
#       }
#       Poly_eachWS_All <- rbind(Poly_eachWS_All, Poly_oneWS)
#       PolyID_Max <- max(as.numeric(sapply(slot(Poly_eachWS_All, "polygons"), function(x) slot(x, "ID"))))
#     }else{
#       # WORK AROUND FOR WHEN A Poly_eachWS IS EMPTY AND YOU NEED TO REMOVE A ROW FROM THE OUTPUT DF TO MATCH IT
#       Remove_Output_Row <- Output_eachWS[which(Output_eachWS$PolyHeight == unique(Output_eachWS$PolyHeight)[a]),]
#       Remove_Output_Row_Area <- Remove_Output_Row$PolyArea[which(Remove_Output_Row$PolyArea == min(Remove_Output_Row$PolyArea))]
#       Index_Remove_Output_Row <- which(Output_eachWS$PolyHeight == unique(Output_eachWS$PolyHeight)[a] &
#                                          Output_eachWS$PolyArea == Remove_Output_Row_Area)
#       if(length(Index_Remove_Output_Row) > 0){
#         #oneRemovedOutput_eachWS <- Output_eachWS[Index_Remove_Output_Row,]
#         Output_eachWS <- Output_eachWS[-Index_Remove_Output_Row,]
#         
#         
#       }
#     }
#   }
# }
# 
# rname_eachWS <- row.names(Output_eachWS)
# rname_Poly <- sapply(slot(Poly_eachWS_All, "polygons"), function(x) slot(x, "ID"))
# 
# setdiff_RowNames_Output_eachWS_missing <- setdiff(rname_eachWS, rname_Poly)
# if(length(setdiff_RowNames_Output_eachWS_missing) > 0 ){
#   Index_Remove1 <- which( rname_eachWS %in% setdiff_RowNames_Output_eachWS_missing)
#   Output_eachWS <- Output_eachWS[-Index_Remove1,]
# }
# 
# setdiff_RowNames_Poly_eachWS_All_missing <- setdiff(rname_Poly, rname_eachWS)
# if(length(setdiff_RowNames_Poly_eachWS_All_missing) > 0 ){
#   Index_Remove2 <- which( rname_Poly %in% setdiff_RowNames_Poly_eachWS_All_missing)
#   Poly_eachWS_All <- Poly_eachWS_All[-Index_Remove2,]
# }
# 
# Poly_eachWS_All_SPDF <- SpatialPolygonsDataFrame(Poly_eachWS_All, Output_eachWS)
# 
# # OUTPUT
# return(list(Points_Centroids_XYZ = list(Points_Centroids_XYZ),
#             Poly_eachWS_All_SPDF = list(Poly_eachWS_All_SPDF))) 
#############################################################################################################################################
# USED IN Nested_Dist_PntPCA_SlCl_XYZ TO FIND WHICH CLUSTERS ARE FAR FROM PCA AND NEED REMOVAL
##############################################################################################
FUNCTION_CLUSTER_FAR <- function(Cluster_ID, Cluster_FarPCA_F, Nested_Dist_SlCl_XYZ_F){
  if(Cluster_FarPCA_F == "Yes"){
    CLFAR_BtwCl_CloseCl <- unlist(Nested_Dist_SlCl_XYZ_F$BtwCl_CloseCl[which(Nested_Dist_SlCl_XYZ_F$Cluster_ID == Cluster_ID)])  # ALL CLOSE CLUSTERS TO SUBJECT CLUSTER
    if(length(CLFAR_BtwCl_CloseCl) > 0){
      # SEE IF ALL THE NEAREST NEIGHBOURS ARE ALSO FAR FROM PCA... IF SO REMOVE (i.e. IF ONE HAS "No" THEN DON'T REMOVE)
      Clusters_close_to_subject <- unique(unlist(Nested_Dist_SlCl_XYZ_F$BtwCl_CloseCl[which(Nested_Dist_SlCl_XYZ_F$Cluster_ID %in% CLFAR_BtwCl_CloseCl)]))
      if(length(Clusters_close_to_subject) == 0){ #### DOM DOM DOM !!! DO SOMETHING IF length(Clusters_close_to_subject) == 0
        Removed_Clusters <- list(Cluster_ID)
      }else{
        YES_NO <- Cluster_FarPCA_F[which(Cluster_ID %in% Clusters_close_to_subject)]
        if(all(YES_NO == "Yes")){
          Removed_Clusters <- list(Cluster_ID)
        }else{
          Removed_Clusters <-list()
        }
      }
    }else{
      # NOT CLOSE TO ANY OTHER CLUSTER SO DEFINITELY REMOVE
      Removed_Clusters <- list(Cluster_ID)
    }
  }else{
    Removed_Clusters <- list()
  }
  return(Removed_Clusters) 
}

##############################################
# REMOVED FROM ... GET_TREE_ATTRIBUTES_CNN_FUN
##############################################

# WS_Missing <- xyFromCell(R_WS, R_Stack_DF$Cell_ID[Index_WS_NAs_ForFill])
# 
# WS_Missing
# 
# WS_Done <- xyFromCell(R_WS, R_Stack_DF$Cell_ID[!Index_WS_NAs_ForFill])
# Dist_WS <- fields.rdist.near(WS_Missing, WS_Done, delta = Raster_Res*2)
# browser()  # DOM DOM DOM !!!  7/4/2022 HAVEN'T COMPLETED THIS PROCEDURE.....

# THINGS TO UPDATE
# WSID  in LAS_oneSt_oneSl <- merge_spatial(LAS_oneSt_oneSl, WS_Updated , attribute = "WSID")


# LAS_oneSt_oneSl@data$Cluster_ID

# # Dist_WS$ind[which(Dist_WS$ra)]
# 
# 
# 
# 

# DOM DOM DOM !!! MAYBE THE WS NA VALUES FOR ALL LAS FILES CAN BE ASSIGNED TO NEAREST WS CENTROID IF WITHIN A CERTAIN DISTANCE LATER ON.
# OTHERWISE MAKE THOSE LAS POINT TID=ZERO


#################################################################################################################################
# STEM_PCA_FUN
#################################################################################################################################

STEM_PCA_N_Points_FUN = function(One_Br, Para_Slice_Height, Up_Down = "UP", Number_Points, TID_maxZ){ #, Type_Tree
  
  PCA_Output <- data.frame(TreeID=numeric(),
                           X=numeric(),
                           Y=numeric(),
                           Z=numeric())
  
  STEM_PCA <- data.frame(TreeID=numeric(),
                         X=numeric(),
                         Y=numeric(),
                         Z=numeric())
  
  # for(S in 1:length(Para_Slice_Height)){
  
  if(Up_Down == "UP"){
    MinZ_PCA <- min(One_Br$Z)
    MaxZ_PCA <- max(One_Br$Z) + Para_Slice_Height#[S]
    TreeID <- One_Br[1,1]
  }
  if(Up_Down == "DOWN"){
    MinZ_PCA <- min(One_Br$Z) - Para_Slice_Height#[S]
    MaxZ_PCA <- max(One_Br$Z)
    TreeID <- One_Br[1,1]
  }
  
  mean_xyz <- apply(One_Br[,match(c("X", "Y", "Z") ,colnames(One_Br))], 2, mean) #
  xyz_pca   <- princomp(One_Br[,match(c("X", "Y", "Z") ,colnames(One_Br))])
  dirVector <- xyz_pca$loadings[, 1]   # PC1
  #Comp_1 <- xyz_pca$score[,1]
  
  # Calculating coordinates of N Points along a line
  Z_Values_Calc <- seq(MinZ_PCA, TID_maxZ, length.out= Number_Points)
  allPnts <- c() 
  #browser()
  Points <- (1:Number_Points)
  for(aa in Points){ 
    t_ends <- c((Z_Values_Calc[Points[aa]]-mean_xyz[3])/dirVector[3], (Z_Values_Calc[Points[aa] + 1]-mean_xyz[3])/dirVector[3])
    endpts <- rbind(mean_xyz + t_ends[[1]]*dirVector, mean_xyz + t_ends[[2]]*dirVector)
    colnames(endpts) <- c("X", "Y", "Z")
    if(aa == 1){allPnts <- endpts
    }else{allPnts <- rbind(allPnts, endpts)}
  }
  allPnts <- na.omit(allPnts)
  allPnts <- as.data.frame(allPnts[!duplicated(allPnts),])
  
  STEM_PCA_Extended <- allPnts
  STEM_PCA_Extended <- data.frame(TreeID = rep(TreeID,Number_Points), STEM_PCA_Extended)
  #browser()
  STEM_PCA_TwoThridsTID <- allPnts[which(allPnts$Z < MaxZ_PCA),]
  STEM_PCA_TwoThridsTID <- data.frame(TreeID = rep(TreeID,nrow(STEM_PCA_TwoThridsTID)), STEM_PCA_TwoThridsTID)
  
  # OUTPUTTING PCA DIRECTIONAL VECTOR AND MEAN XYZ
  Output_MeanXYZ_dirVector <- rbind(mean_xyz, dirVector)
  Output_MeanXYZ_dirVector <- cbind(TreeID = rep(TreeID,2),Output_MeanXYZ_dirVector)
  PCA_Output <- rbind(PCA_Output, Output_MeanXYZ_dirVector)
  
  # # FINAL OUTPUT
  # 
  # STEM_PCA <- rbind(STEM_PCA, STEM_PCA_Extended)
  #}
  return(list(STEM_PCA_TwoThridsTID=STEM_PCA_TwoThridsTID, STEM_PCA_Extended=STEM_PCA_Extended, PCA_Output=PCA_Output))
} # END STEM_PCA_FUN_Simple


