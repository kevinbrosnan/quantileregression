########## Combining the data sets using R ##########

# Loading required packages
  
  library(foreign)

# Loading the data
  
  setwd("Data Files")
  options(warn=-1)
  T1 <- as.data.frame(read.spss("T1FINAL.sav"))
  T2 <- as.data.frame(read.spss("T2excTPO.sav"))
  T3 <- as.data.frame(read.spss("T3FINAL.sav"))

# One dataset from the three data sources
  
  options(warn=0)

  # T1, T2, T3: Adding a variable inicating the Trimester in which the data was recorded
  
  T1["Trimester"] <- rep("T1",length(T1$MRN))
  T2["Trimester"] <- rep("T2",length(T2$MRN))
  T3["Trimester"] <- rep("T3",length(T3$MRN))
  
  # T2: Removing Gestation variable in T2 data
  
  T2 <- T2[ , -which(names(T2) %in% c("Gestation"))]
  
  # T2: Rename Gest variable to Gestation
  
  names(T2)[names(T2) == "Gest"] <- "Gestation"
  
  # T2: Move Gestation variable to correct column in data frame
  
  T2 <- T2[,c(1:3,5:9,4,10)]

  # T1, T2, T3: Combine the three datasets
  
  thyroid <- rbind(T1,T2,T3)

  # Thyroid: Redefining Levels for Smoking variable; No, 1-5, 6-10, >10
  
  old_names <- unique(thyroid$Smoking)
  new_names <- c("No","1-5","1-5","1-5","6-10","6-10","6-10","1-5","1-5","6-10",
                 "1-5","1-5","No","1-5",">10",NA,"1-5","1-5","1-5",">10",
                 "1-5","1-5","1-5",">10","6-10","1-5","1-5","6-10","6-10","6-10",
                 ">10",">10","1-5","6-10")
  
  thyroid["SmokingNew"] <- rep("",length(thyroid$Smoking))
  for(i in 1:length(thyroid$Smoking)){
    for(j in 1:length(old_names)){
      if(thyroid$Smoking[i]==old_names[j]){
        thyroid$SmokingNew[i] <- new_names[j]
        break;
      }
    }
  }

  thyroid["Smoking"] <- thyroid["SmokingNew"]
  thyroid <- thyroid[, -which(names(thyroid) %in% c("SmokingNew"))]

  # Removing patients with NA or exceptional values in certain fields
  
  for(i in 1:length(thyroid$Gestation)){
    if(is.na(thyroid$Gestation[i])){
      thyroid <- thyroid[-i,]
    }
  }

  for(i in 1:length(thyroid$T4)){
    if(is.na(thyroid$T4[i])){
      thyroid <- thyroid[-i,]
    }else if(thyroid$T4[i]>20){
      thyroid <- thyroid[-i,]
    }
  }

  for(i in 1:length(thyroid$T3)){
    if(is.na(thyroid$T3[i])){
      thyroid <- thyroid[-i,]
    }else if(thyroid$T3[i]>8){
      thyroid <- thyroid[-i,]
    }
  }
  
  thyroid <- thyroid[-which(thyroid$TPO==max(thyroid$TPO, na.rm=TRUE)),]
  thyroid <- thyroid[-which(thyroid$TSH==max(thyroid$TSH, na.rm=TRUE)),]

# Saving data to a .csv file for future use

  write.csv(thyroid, file="Thyroid Data.csv",row.names=FALSE)
  