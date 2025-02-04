#Loading libraries----
library(ARTool)
library(readr)
library(ggplot2)
library(Rmisc)
library("gridExtra") 
library(multcompView)
library(dplyr)
library(mvnormtest)
library(rcompanion)
library(pgirmess)
library(FSA)
library(car)
library(factoextra)
library(corrplot)
library(vegan)
library(DescTools)
library(corrplot)
library(dunn.test)
library(stringr)
library(stringi)
library(tidyverse)
library(purrr)
library(patchwork)
library(reshape)
library(grid)
library(ggrepel)
library(ggpubr)
library(ggtext)
library(grid)
library(gridExtra)
library(gtable)
library(RColorBrewer)
library(scales)
library(readxl)
library(openxlsx)



#Data Processor Function before doing analysis
process_dataset_for_data_distribution <- function(data, keep_LOQ_values=FALSE) {
  # Check if the dataset is Heavy Metals (Ag is in the 16th column)
  if (grepl("Fe|Ag", colnames(data)[16])) {
    
    # Processing for Heavy Metals dataset
    data <- data %>%
      relocate(1:15, colnames(data)[16:25]) %>%
      mutate(across(4, ~ paste0(., ".")))  # Add "." to column 4
    
    # FUNCTIONS TO HELP MODIFY ID NUMBERS FOR FURTHER ANALYSIS (HM)
    ID_num_modifier <- function(x){#padding 0 to first two digits
      if (str_detect(substring(x[4],1,2),"\\.") == TRUE){ 
        sk=paste('0',substring(x[4],1,))
        sk<- gsub(" ", "",  sk)
        x[4] <- gsub(substring(x[4],1,), sk,  x[4])
      }
      return(x)
    }
    
    # Apply the 'ID_num_modifier' function and remove "." from the 'Area' column for data version 1 with numerical values as concentrations
    data1 <- as.data.frame(t(apply(data, MARGIN = 1, ID_num_modifier)))
    data$ID_num <- gsub("\\.", "", data$ID_num)
    
    # Apply the 'ID_num_modifier' function and remove "." from the 'Area' column for data version 2 with classification groups (categorical values) representing concentration values
    data2 <- as.data.frame(t(apply(data, MARGIN = 1, ID_num_modifier)))
    data2$ID_num <- gsub("\\.", "", data2$ID_num)
    
    if(keep_LOQ_values == FALSE){
      cat("The LOQ values were replaced with 0.\n")
      # Replace "BLOQ" values with 0 in the relevant columns (16:25)
      data1[, 16:25] <- lapply(data1[, 16:25], gsub, pattern = ".*BLOQ.*", replacement = 0)
      data1[,c(16:25)] <- lapply(data1[,c(16:25)], gsub, pattern = ".*BB.*", replacement = 0)
      data1[,c(16:25)] <- lapply(data1[,c(16:25)], gsub, pattern = "^0$", replacement = 0)
      data1[,c(16:25)]  <- lapply(data1[,c(16:25)] , gsub, pattern = "N/A", replacement = 0)
      
      data2[,c(16:25)] <- lapply(data2[,c(16:25)], gsub, pattern = ".*BLOQ.*", replacement = 'BLOQ')
      data2[,c(16:25)] <- lapply(data2[,c(16:25)], gsub, pattern = ".*BB.*", replacement = 'BLOQ')
      data2[,c(16:25)] <- lapply(data2[,c(16:25)], gsub, pattern = "^0$", replacement = 'Not_detected')
      data2[,c(16:25)]  <- lapply(data2[,c(16:25)] , gsub, pattern = "N/A", replacement = 'Not_detected')
      data2[,c(16:25)] <- lapply(data2[,c(16:25)], gsub, pattern = "[0-9.]+", replacement = "Detected")
      return(list(dataset_with_numerical_values=data1, dataset_with_categorical_values=data2))
      
    }else{
      # Removing "BLOQ" and "BB" values in the relevant columns (16:25)
      data1[, 16:25] <- lapply(data1[, 16:25], gsub, pattern = " BLOQ", replacement = "")
      data1[,c(16:25)] <- lapply(data1[,c(16:25)], gsub, pattern = " BB", replacement = "")
      data1[,c(16:25)] <- lapply(data1[,c(16:25)], gsub, pattern = "^0$", replacement = 0)
      data1[,c(16:25)]  <- lapply(data1[,c(16:25)] , gsub, pattern = "N/A", replacement = 0)
      
      data2[,c(16:25)] <- lapply(data2[,c(16:25)], gsub, pattern = ".*BLOQ.*", replacement = 'BLOQ')
      data2[,c(16:25)] <- lapply(data2[,c(16:25)], gsub, pattern = ".*BB.*", replacement = 'BLOQ')
      data2[,c(16:25)] <- lapply(data2[,c(16:25)], gsub, pattern = "^0$", replacement = 'Not_detected')
      data2[,c(16:25)]  <- lapply(data2[,c(16:25)] , gsub, pattern = "N/A", replacement = 'Not_detected')
      data2[,c(16:25)] <- lapply(data2[,c(16:25)], gsub, pattern = "[0-9.]+", replacement = "Detected")
      
      process_with_user_input <- function() {
        # Ask the user if they want to keep LOQ values
        keep_loq <- readline(prompt = "Do you want to keep the original LOQ values? (yes/no): ")
        
        if (tolower(keep_loq) == "yes") {
          cat("You chose to keep LOQ values.\n")
          return("Keep LOQ") # Return a clear message indicating the choice
        } else if (tolower(keep_loq) == "no") {
          multiplier <- NA
          
          while (is.na(multiplier)) {
            user_input <- readline(prompt = "What number do you want to multiply the LOQ values by? ")
            multiplier <- suppressWarnings(as.numeric(user_input))
            
            if (is.na(multiplier)) {
              cat("Invalid input. Please enter a numeric value.\n")
            }
          }
          
          cat("You chose a multiplier of ", multiplier, ".\n")
          return(multiplier) # Return the chosen multiplier
        } else {
          cat("Invalid input. Please answer with 'yes' or 'no'.\n")
          return(NULL) # Return NULL for invalid input
        }
      }
      
      # To get user input for 
      user_choice <- process_with_user_input()
      
      # Modify "BLOQ" and "BB" values with userinput in the relevant columns (16:25)
      process_data <- function(data, multiplier) {
        # Validate user input
        if (is.na(multiplier)) {
          stop("Invalid multiplier. Please enter a numeric value.")
        }
        
        # Process the data
        data[, 16:25] <- lapply(data[, 16:25], function(column) {
          # Replace " BLOQ" and " BB" with empty strings
          cleaned_column <- gsub(" BLOQ", "", column)
          cleaned_column <- gsub(" BB", "", cleaned_column)
          
          # Convert to numeric
          numeric_column <- as.numeric(cleaned_column)
          
          # Multiply by the user input
          if (is.numeric(multiplier)){
            result_column <- numeric_column * multiplier
            return(result_column)
          }else{
            return (numeric_column)
          }
          
        })
        
        # Return the updated dataset
        return(list(dataset_with_numerical_values=data1, dataset_with_categorical_values=data2))
      }
      data <-process_data(data, user_choice)
      return(list(dataset_with_numerical_values=data1, dataset_with_categorical_values=data2))
    }
    
  } else {
    # Processing for Organic Compounds dataset
    data <- data %>%
      relocate(Area, .after = ID_num) %>%
      mutate(across(4, ~ paste0(., ".")))  # Add "." to column 4
    
    # Convert column 6 to lowercase
    data[, 6] <- tolower(data[, 6])
    
    
    # FUNCTIONS TO HELP MODIFY ID NUMBERS FOR FURTHER ANALYSIS (HM)
    ID_num_modifier <- function(x){#padding 0 to first two digits
      if (str_detect(substring(x[4],1,2),"\\.") == TRUE){ 
        sk=paste('0',substring(x[4],1,))
        sk<- gsub(" ", "",  sk)
        x[4] <- gsub(substring(x[4],1,), sk,  x[4])
      }
      return(x)
    }
    
    # Apply the 'ID_num_modifier' function and remove "." from the 'Area' column for data version 1 with numerical values as concentrations
    data1 <- as.data.frame(t(apply(data, MARGIN = 1, ID_num_modifier)))
    data1$ID_num <- gsub("\\.", "", data1$ID_num)
    
    # Apply the 'ID_num_modifier' function and remove "." from the 'Area' column for data version 2 with classification groups representing concentration values
    data2 <- as.data.frame(t(apply(data, MARGIN = 1, ID_num_modifier)))
    data2$ID_num <- gsub("\\.", "", data2$ID_num)
    
    if(keep_LOQ_values == FALSE){
      cat("The LOQ values were replaced with 0.\n")
      # Replace "BLOQ", "N/A", and "0" values with 0 in the relevant columns (16:19)
      data1[, 16:19] <- lapply(data1[, 16:19], gsub, pattern = ".*BLOQ.*", replacement = 0)
      data1[, 16:19] <- lapply(data1[, 16:19], gsub, pattern = "N/A", replacement = 0)
      data1[, 16:19] <- lapply(data1[, 16:19], gsub, pattern = "^0$", replacement = 0)
      
      data2[,c(16:19)] <- lapply(data2[,c(16:19)], gsub, pattern = ".*BLOQ.*", replacement = 'BLOQ')
      data2[,c(16:19)] <- lapply(data2[,c(16:19)], gsub, pattern = "^0$", replacement = 'Not_detected')
      data2[,c(16:19)]  <- lapply(data2[,c(16:19)] , gsub, pattern = "N/A", replacement = 'Not_detected')
      data2[,c(16:19)] <- lapply(data2[,c(16:19)], gsub, pattern = "[0-9.]+", replacement = "Detected")
      
      return(list(dataset_with_numerical_values=data1, dataset_with_categorical_values=data2))
      
    }else{
      # Removing "BLOQ" and  values in the relevant columns (16:19)
      data1[, 16:19] <- lapply(data1[, 16:19], gsub, pattern = " BLOQ", replacement = "")
      data1[, 16:19] <- lapply(data1[, 16:19], gsub, pattern = "N/A", replacement = 0)
      data1[, 16:19] <- lapply(data1[, 16:19], gsub, pattern = "^0$", replacement = 0)
      
      data2[,c(16:19)] <- lapply(data2[,c(16:19)], gsub, pattern = ".*BLOQ.*", replacement = 'BLOQ')
      data2[,c(16:19)] <- lapply(data2[,c(16:19)], gsub, pattern = "^0$", replacement = 'Not_detected')
      data2[,c(16:19)]  <- lapply(data2[,c(16:19)] , gsub, pattern = "N/A", replacement = 'Not_detected')
      data2[,c(16:19)] <- lapply(data2[,c(16:19)], gsub, pattern = "[0-9.]+", replacement = "Detected")
      
      process_with_user_input <- function() {
        # Ask the user if they want to keep LOQ values
        keep_loq <- readline(prompt = "Do you want to keep LOQ values? (yes/no): ")
        
        if (tolower(keep_loq) == "yes") {
          cat("You chose to keep LOQ values.\n")
          return("Keep LOQ") # Return a clear message indicating the choice
        } else if (tolower(keep_loq) == "no") {
          multiplier <- NA
          
          while (is.na(multiplier)) {
            user_input <- readline(prompt = "What number do you want to multiply the LOQ values by? ")
            multiplier <- suppressWarnings(as.numeric(user_input))
            
            if (is.na(multiplier)) {
              cat("Invalid input. Please enter a numeric value.\n")
            }
          }
          
          cat("You chose a multiplier of ", multiplier, ".\n")
          return(multiplier) # Return the chosen multiplier
        } else {
          cat("Invalid input. Please answer with 'yes' or 'no'.\n")
          return(NULL) # Return NULL for invalid input
        }
      }
      
      # To get user input for 
      user_choice <- process_with_user_input()
      
      # Modify "BLOQ" and "BB" values with userinput in the relevant columns (16:25)
      process_data <- function(data, multiplier) {
        # Validate user input
        if (is.na(multiplier)) {
          stop("Invalid multiplier. Please enter a numeric value.")
        }
        
        # Process the data
        data[, 16:19] <- lapply(data[, 16:19], function(column) {
          # Replace " BLOQ" and " BB" with empty strings
          cleaned_column <- gsub(" BLOQ", "", column)
          #cleaned_column <- gsub(" BB", "", cleaned_column)
          
          # Convert to numeric
          numeric_column <- as.numeric(cleaned_column)
          
          # Multiply by the user input
          if (is.numeric(multiplier)){
            result_column <- numeric_column * multiplier
            return(result_column)
          }else{
            return (numeric_column)
          }
          
        })
        
        # Return the updated dataset
        return(list(dataset_with_numerical_values=data1, dataset_with_categorical_values=data2))
      }
      data <-process_data(data, user_choice)
      return(list(dataset_with_numerical_values=data1, dataset_with_categorical_values=data2))
    }
  }
}


# Data Processing for Heavy Metals dataset
heavymetals_data <- read.csv("Final_Results_From_Task 1/Final_HMresults_mgkg.csv", header = TRUE)
datasets_for_heavy_metals_data_distribution <- process_dataset_for_data_distribution(heavymetals_data, keep_LOQ_values = FALSE) 


# Data Processing for Organic Compounds dataset
organiccompounds_data <- read.csv("Final_Results_From_Task 1/Final_OCresults_mgkg.csv", header = TRUE)
datasets_for_organic_compounds_data_distribution <- process_dataset_for_data_distribution(organiccompounds_data, keep_LOQ_values = FALSE)


data_distribution <- function(data_list){
  #y=hmfull
  list0 <- list()
  list0names <- c()
  list1 <- list()
  list1names <- c()
  list2 <- list()
  list2names <- c()
  list3 <- list()
  list3names <- c()
  if (grepl("Fe|Ag", colnames(data)[16])) {
  #if(str_detect(colnames(y[17]),'Ag')==TRUE){
    range <- colnames(y[16:25])#Fe:Pb HM
    # icons_hm <- apply(HMiconz, 1, markdownfunchm)
    # names(icons_hm) <- names(urlz)
    # icons_markdown <- icons_hm
    column_range <- 16:25
    fdf<-data_list$dataset_with_numerical_values
  }else{
    range <- colnames(y[16:19])#Adipic_acid:Ibuprofen SM
    # icons_sm <- apply(SMiconz, 1, markdownfuncsm)
    # names(icons_sm) <- names(urls)
    # icons_markdown <- icons_sm
    column_range <- 16:19
    fdf<-data_list$dataset_with_numerical_values
  }
  h<-1
  Subset_of_dataset_with_categorical_values <-data_list$dataset_with_categorical_values[,c(column_range,1,3,6)]
  #ndf1 <-x[,c(numrang,1,3,6)]
  Subset_of_dataset_with_numerical_values <-data_list$dataset_with_numerical_values[,c(column_range,1,3,6)]
  years <- levels(factor(data_list$dataset_with_numerical_values[,3]))
  # final2 <- data.frame(matrix(ncol=5, nrow = 0), check.names = FALSE)
  # finalcomb <- data.frame(matrix())
  
  #making empty dataframe for summarized dataset for visualization
  summarized_full <- data.frame(matrix(ncol=8, nrow = 0), check.names = FALSE)
  colnames(summarized_full)<-c('year', 'pollutant', 'Tissue', 'Total_N', 'Not_Detected', 'BLOQ', 'Detected', 'outliers')
  Final_dataset_for_outlier_detection_and_removal <- data.frame(matrix(ncol=5, nrow = 0), check.names = FALSE)
  colnames(Final_dataset_for_outlier_detection_and_removal)<-c('year','Tissue', 'pollutant', 'concentrations', 'outlier')
  fulldf2 <- data.frame(matrix(ncol=ncol(Subset_of_dataset_with_categorical_values), nrow = 0), check.names = FALSE)
  colnames(fulldf2)<-colnames(Subset_of_dataset_with_categorical_values)
  #while(h != length(yr)+1){
  for (h in 1:length(years)){
    pollutant <-years[h] 
    ndf <-Subset_of_dataset_with_categorical_values
    ndf2 <-Subset_of_dataset_with_numerical_values
    dataframe_categorical_values <- Subset_of_dataset_with_categorical_values %>% pivot_longer(all_of(range), names_to = "pollutant", values_to = "status")
    dataframe_numerical_values <- Subset_of_dataset_with_numerical_values %>% pivot_longer(all_of(range), names_to = "pollutant", values_to = "concentrations")%>%subset(conc !=0)
    column_names <- colnames(data_list$dataset_with_categorical_values)[column_range]
    final_dataset <- data.frame(matrix(ncol=8, nrow = 0), check.names = FALSE)
    colnames(final_dataset)<-c('year', 'pollutant', 'Tissue', 'Total_N', 'Not_Detected', 'BLOQ', 'Detected', 'outliers')
    dataframe_categorical_values <-as.data.frame(filter(dataframe_categorical_values, Year==years[h]))
    dataframe_numerical_values <-as.data.frame(filter(dataframe_numerical_values, Year==years[h]))
    #print(nzY)
    #for tissue----
    for(j in 1:length(column_names)){
      finalcomb <- data.frame(matrix(ncol=5, nrow = 0), check.names = FALSE)
      colnames(finalcomb)<-c('year','Tissue', 'pollutant', 'conc', 'outlier')
      fin <- data.frame(matrix(ncol=8, nrow = 0), check.names = FALSE)
      colnames(fin)<-c('year', 'pollutant', 'Tissue', 'Total_N', 'Not_Detected', 'BLOQ', 'Detected', 'outliers')
      nzM<-as.data.frame(filter(nzY, pollutant==ml[j]))
      nzM2<-as.data.frame(filter(nzY2, pollutant==ml[j]))
      #st <- c('Not Detected', 'BLOQ', 'Detected')
      c1 <- 'Not_detected'
      c2 <- 'BLOQ'
      c3 <- 'Detected'
      #tis <- levels(factor(y[,'Tissue']))
      tis <- c('liver', 'stomach', 'muscle', 'inksac')
      #print(tis)
      for(k in 1:length(tis)){
        nzT<-as.data.frame(filter(nzM, Tissue==tis[k]))
        nzT2<-as.data.frame(filter(nzM2, Tissue==tis[k])%>%mutate(outlier=NA))
        #print(ml)
        # for(s in 1:length(st)){
        #   nzST<-as.data.frame(filter(nzM, status==st[s]))
        # }
        #print(nzM)
        fin[1,1] <- yr[h]
        fin[1,2] <- ml[j]
        fin[1,3] <- tis[k]
        fin[1,4] <- nrow(nzT)
        fin[1,5] <- sum(str_count(nzT$status,c1))
        fin[1,6] <- sum(str_count(nzT$status,c2))
        fin[1,7] <- sum(str_count(nzT$status,c3))
        if(nrow(nzT2)>1){
          outliercheck <- function(x, v){
            if(str_detect(colnames(v[17]),'Ag')==TRUE){
              numrang <- 17:26
              z<-v
            }else{
              numrang <- 17:30
              z<-v
            }
            #print(numrang)
            list0 <- list()
            list0names <- c()
            count_otl<- c()
            for (ch in 1:nrow(x)){
              if((x[ch,'conc'])>(3*mean(x[-ch,'conc']))){
                count_otl <- append(count_otl, 1)
                x[ch,'outlier'] <- 'yes'
                }else{
                  count_otl <- append(count_otl, 0)
                  x[ch,'outlier'] <- 'no'
                }
            }
            #print(x)
              finalctol <- sum(count_otl)
              list0<-append(list(finalctol),list0, 0)
              name0 <- paste("ctol", sep = "")
              list0names <- append(list0names,name0) 
              list0<-append(list(x),list0, 0)
              name0 <- paste("dataset", sep = "")
              list0names <- append(list0names,name0)
              list0<-append(list(z),list0, 0)
              name0 <- paste("fullnewdataset", sep = "")
              list0names <- append(list0names,name0)
              return(datanew=list0)
            }
            fin[1,8] <- outliercheck(x=nzT2, v=fdf)[[1]]
            df<-outliercheck(x=nzT2, v=fdf)[[2]]
            finalcomb <- rbind(finalcomb, df)
            fulldf<-outliercheck(x=nzT2, v=fdf)[[3]]
            fulldf2 <- rbind(fulldf2, fulldf)
            print(finalcomb)
            #print(fdf)
          }else{
            #print(k)
            #print(ndf4)
            #print(fdf)
            fin[1,8] <- 0
            nzT2[ 1,'Year'] <- paste(yr[h])
            nzT2[ 1,'pollutant'] <- paste(ml[j])
            nzT2[ 1,'Tissue'] <- paste(tis[k])
            if(nrow(nzT2)==1){
              nzT2[ 1,'conc'] <- paste(nzT2[,'conc'])
            }else{
              nzT2[ 1,'conc'] <- 0 
            }
            nzT2[ 1,'outlier'] <- 'no'
            #print(z)
            finalcomb <- rbind(finalcomb, nzT2)
          }
          final <- rbind(final, fin)
        }
        finalcomb2 <- rbind(finalcomb2, finalcomb)
    }
      finalful <- rbind(finalful, final)
  }
  # print(fulldf2)
   print(finalful)
  for (te in 1:nrow(finalcomb2)){
     if(finalcomb2[te,'outlier']=='yes'){
       contaminant <- unique(finalcomb2[te,'pollutant'])
    for(ne in 1:length(numrang)){
    if(colnames(fdf)[ne+16]!= contaminant){
      next
    }else{
      for (se in 1:nrow(fdf)){
        if(finalcomb2[te,'ID']==fdf[se,'ID'] & finalcomb2[te,'Year']==fdf[se,'Year'] & finalcomb2[te,'Tissue']==fdf[se,'Tissue']){
          print(fdf[se,ne+16])
          fdf[se,ne+16]<-0
          #fdf[se,ne+16]<-fdf[se,ne+16]
        }else{
          next
        }
      }
    }
  }
     }else{
       next
     }
  }
  return(list (datadis2=finalful, datastats=finalcomb2, fulldf=fdf))
}
  datadis2 <- datadis2(y=smdis, x=sm1)
  write.xlsx(datadis2$fulldf, file = "fdfsm1.xlsx",colNames = TRUE, sheetName = "B1", append = FALSE)
  