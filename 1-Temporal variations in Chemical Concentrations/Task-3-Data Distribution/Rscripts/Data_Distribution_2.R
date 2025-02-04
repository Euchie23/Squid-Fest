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
    
    # Apply the 'ID_num_modifier' function and remove "." from the 'Area' column for data version 1 with numerical values as concentrationentrations
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
  
  #Preparing empty lists to store results
  list0 <- list()
  list0names <- c()
  list1 <- list()
  list1names <- c()
  list2 <- list()
  list2names <- c()
  list3 <- list()
  list3names <- c()
  
  #Identifying the input dataset into the function to know whether it is the heavy metal or small molecule dataset.
  if (grepl("Fe|Ag", colnames(data)[16])) {
    #range <- colnames(y[16:25])#Fe:Pb HM
    column_range <- 16:25
    input_dataset<-data_list$dataset_with_numerical_values
    h<-1
    Subset_of_dataset_with_categorical_values <-data_list$dataset_with_categorical_values[,c(column_range,1,3,6)]
    Subset_of_dataset_with_numerical_values <-data_list$dataset_with_numerical_values[,c(column_range,1,3,6)]
    years <- levels(factor(data_list$dataset_with_numerical_values[,3]))
    
    #making empty dataframes to store data
    summarized_categorical_values <- data.frame(matrix(ncol=8, nrow = 0), check.names = FALSE)
    colnames(summarized_categorical_values)<-c('year', 'pollutant', 'Tissue', 'Total_N', 'BLOD','BB+BLOQ', 'Detected', 'Outliers')
    outlier_detection_using_numerical_values <- data.frame(matrix(ncol=5, nrow = 0), check.names = FALSE)
    colnames(outlier_detection_using_numerical_values)<-c('year','Tissue', 'pollutant', 'concentration', 'outlier')
    # fulldf2 <- data.frame(matrix(ncol=ncol(Subset_of_dataset_with_categorical_values), nrow = 0), check.names = FALSE)
    # colnames(fulldf2)<-colnames(Subset_of_dataset_with_categorical_values)
    #while(h != length(yr)+1){
    
    # Used to subset datasets per year
    for (year in 1:length(years)){ #h
      #pollutant <-years[h] 
      long_dataset_with_categorical_values <- Subset_of_dataset_with_categorical_values %>% pivot_longer(all_of(range), names_to = "pollutant", values_to = "status")
      long_dataset_with_numerical_values <- Subset_of_dataset_with_numerical_values %>% pivot_longer(all_of(range), names_to = "pollutant", values_to = "concentration")%>%subset(concentration !=0)
     column_names <- colnames(Subset_of_dataset_with_categorical_values)[column_range]
       outlier_count_accumulated <- data.frame(matrix(ncol=8, nrow = 0), check.names = FALSE)
        colnames(outlier_count_accumulated)<-c('year', 'pollutant', 'Tissue', 'Total_N', 'BLOD', 'BB+BLOQ', 'Detected', 'Outliers')
        long_dataset_with_categorical_values_per_year <-as.data.frame(filter(long_dataset_with_categorical_values, Year==years[year]))
        long_dataset_with_numerical_values_per_year<-as.data.frame(filter(long_dataset_with_numerical_values, Year==years[year]))
 
        
      for(pollutant in 1:length(column_names)){ #j
        outlier_detection_using_numerical_values <- data.frame(matrix(ncol=5, nrow = 0), check.names = FALSE)
        colnames(outlier_detection_using_numerical_values) <-c('year','Tissue', 'pollutant', 'concentration', 'outlier')
         outlier_count <- data.frame(matrix(ncol=8, nrow = 0), check.names = FALSE)
          colnames(categorical_values_partial1)<-c('year', 'pollutant', 'Tissue', 'Total_N', 'BLOD','BB+BLOQ', 'Detected', 'Outliers')
          category1 <-'BLOD'
          category2 <-"BB+BLOQ"
          category3 <-'Detected'
          dataset_for_categorical_values_per_pollutant <-as.data.frame(filter(long_dataset_with_categorical_values_per_year, pollutant==column_names[pollutant]))
          dataset_for_numerical_values_per_pollutant <-as.data.frame(filter(long_dataset_with_numerical_values_per_year, pollutant==column_names[pollutant]))
        tissues <- c('liver', 'stomach', 'muscle', 'inksac')
       
        # Used to subset datasets per tissue
        for(tissue in 1:length(tissues)){ #K
          dataset_for_categorical_values_per_tissue<-as.data.frame(filter(dataset_for_categorical_values_per_pollutant, Tissue==tissues[tissue]))
          dataset_for_numerical_values_per_tissue<-as.data.frame(filter(dataset_for_numerical_values_per_pollutant, Tissue==tissues[tissue])%>%mutate(outlier=NA))
          #print(dataset_for_numerical_values_per_tissue)
          #print(column_names)
          # for(s in 1:length(st)){
          #   nzST<-as.data.frame(filter(nzM, status==st[s]))
          # }
          #print(nzM)
           outlier_count[1,1] <- years[year]
           outlier_count[1,2] <- column_names[pollutant]
           outlier_count[1,3] <- tissues[tissue]
           outlier_count[1,4] <- nrow(dataset_for_categorical_values_per_tissue)
           outlier_count[1,5] <- sum(str_count(dataset_for_categorical_values_per_tissue$status,category1))
           outlier_count[1,6] <- sum(str_count(dataset_for_categorical_values_per_tissue$status,fixed(category2)))
           outlier_count[1,7] <- sum(str_count(dataset_for_categorical_values_per_tissue$status,category3))
          if(nrow(dataset_for_numerical_values_per_tissue)>1){
            outliercheck <- function(x, v){
              if (grepl("Fe|Ag", colnames(data)[16])) {
                column_range <- 16:25
                z<-v
              }else{
                column_range <- 16:19
                z<-v
              }
              #print(column_range)
              list0 <- list()
              list0names <- c()
              count_outliers<- c()
              for (row in 1:nrow(x)){
                if((x[row,'concentration'])>(3*mean(x[-row,'concentration']))){
                  count_outliers <- append(count_outliers, 1)
                  x[row,'outlier'] <- 'yes'
                }else{
                  count_outliers <- append(count_outliers, 0)
                  x[row,'outlier'] <- 'no'
                }
              }
              #print(x)
              final_count_outliers <- sum(count_outliers)
              list0<-append(list(final_count_outliers),list0, 0)
              name0 <- paste("outlier_count", sep = "")
              list0names <- append(list0names,name0) 
              list0<-append(list(x),list0, 0)
              name0 <- paste("outlier_detection_dataset1", sep = "")
              list0names <- append(list0names,name0)
              list0<-append(list(z),list0, 0)
              name0 <- paste("partial_input_dataset", sep = "")
              list0names <- append(list0names,name0)
              return(dataset_list=list0)
            }
           outlier_count[1,8] <- outliercheck(x=dataset_for_numerical_values_per_tissue, v=input_dataset)[[1]]
            outlier_detection_dataset1 <- outliercheck(x=dataset_for_numerical_values_per_tissue, v=input_dataset)[[2]]
            outlier_detection_using_numerical_values <- rbind(outlier_detection_using_numerical_values, outlier_detection_dataset1)
            partial_input_dataset <- outliercheck(x=dataset_for_numerical_values_per_tissue, v=input_dataset)[[3]]
            full_input_dataset <- rbind(full_input_dataset, partial_input_dataset)
          }else{
           outlier_count[1,8] <- 0
            dataset_for_numerical_values_per_tissue[ 1,'Year'] <- paste(years[year])
            dataset_for_numerical_values_per_tissue[ 1,'pollutant'] <- paste(column_names[pollutant])
            dataset_for_numerical_values_per_tissue[ 1,'Tissue'] <- paste(tissues[tissue])
            if(nrow(dataset_for_numerical_values_per_tissue)==1){
              dataset_for_numerical_values_per_tissue[ 1,'concentration'] <- paste(dataset_for_numerical_values_per_tissue[,'concentration'])
            }else{
              dataset_for_numerical_values_per_tissue[ 1,'concentration'] <- 0 
            }
            dataset_for_numerical_values_per_tissue[ 1,'outlier'] <- 'no'
            #print(z)
            outlier_detection_using_numerical_values <- rbind(outlier_detection_using_numerical_values, dataset_for_numerical_values_per_tissue)
          }
         outlier_count_accumulated <- rbind(outlier_count_accumulated,outlier_count)
        }
        outlier_detection_using_numerical_values <- rbind(outlier_detection_using_numerical_values, outlier_detection_using_numerical_values)
      }
      summarized_categorical_values <- rbind(summarized_categorical_values,outlier_count_accumulated)
      finy <- data.frame(matrix(ncol=8, nrow = 0), check.names = FALSE)
      colnames(finy)<-c('year', 'pollutant', 'Tissue', 'Total_N', 'BLOD','BB+BLOQ', 'Detected', 'Outliers')
      category1 <-'BLOD'
      category2 <-"BB+BLOQ"
      category3 <-'Detected'
      finy[1,1] <- years[year]
      finy[1,2] <- "all"
      finy[1,3] <- paste('Total')
      finy[1,4] <- nrow(long_dataset_with_categorical_values_per_year)
      finy[1,5] <- sum(str_count(long_dataset_with_categorical_values_per_year$status,category1))
      finy[1,6] <- sum(str_count(long_dataset_with_categorical_values_per_year$status,fixed(category2)))
      finy[1,7] <- sum(str_count(long_dataset_with_categorical_values_per_year$status,category3))
      finy[1,8] <- sum(as.numeric(summarized_categorical_values$Outliers))
      summarized_categorical_values <- rbind(summarized_categorical_values, finy)
    }
    # print(fulldf2)
    # print(summarized_categorical_values)
    for (te in 1:nrow(outlier_detection_using_numerical_values)){
      if(outlier_detection_using_numerical_values[te,'outlier']=='yes'){
        contaminant <- unique(outlier_detection_using_numerical_values[te,'pollutant'])
        for(ne in 1:length(column_range)){
          if(colnames(input_dataset)[ne+16]!= contaminant){
            next
          }else{
            for (se in 1:nrow(input_dataset)){
              if(outlier_detection_using_numerical_values[te,'ID']==input_dataset[se,'ID'] & outlier_detection_using_numerical_values[te,'Year']==input_dataset[se,'Year'] & outlier_detection_using_numerical_values[te,'Tissue']==input_dataset[se,'Tissue']){
                #print(input_dataset[se,ne+16])
                #input_dataset[se,ne+16]<-0
                input_dataset[se,ne+16]<-input_dataset[se,ne+16]
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
    summarized_categorical_values1 <- summarized_categorical_values
    for(pi in 1:nrow(summarized_categorical_values1)) {
      #here----
      summarized_categorical_values1[pi,'Detected'] <- summarized_categorical_values1[pi,'Detected']-summarized_categorical_values1[pi,'Outliers']
    }
    statlevs<- c('BLOD','BB+BLOQ','Outliers','Detected')
    dfn<- summarized_categorical_values1 %>% pivot_longer(all_of(statlevs), names_to = "status", values_to = "values")

    nzdPT <- dfn %>%group_by(status,Tissue) %>%mutate(Percentage = (values/Total_N)* 100)
    nzdPT[is.na(nzdPT)] = 0
  #print(nzdPT)
    #nzdPT <- dfn %>%group_by(status,year, Tissue) %>%summarise(Percentage = n() / nrow(dfn) * 100)
    print(nzdPT)
    #nzdP1 <- long_dataset_with_numerical_values_per_year %>%group_by(status, Year) %>%summarise(Percentage = n() / nrow(long_dataset_with_numerical_values_per_year) * 100) %>%mutate(Tissue='Total')
    #nzdPT <- dfn %>%group_by(status, Tissue) %>%mutate(Percentage = (values/Total_N)* 100)
    #nzdP0 <-dfn %>%group_by(status, Year) %>%summarise(N = n())
    statlevs<- c('BLOD','BB+BLOQ','Outliers','Detected')
    Colors <-setNames( c('#F8766D','#00A9FF','yellow','#00BA38'),statlevs)
    tissue_levels <- c("liver","stomach","muscle","inksac","Total")
    metlevs <- c("Ag","Cd","Co","Cu","Fe","Hg","Ni","Pb","Tl","Zn", "Total")
    plt <-ggplot(nzdPT, aes(x = factor(Tissue, levels = tissue_levels), y = Percentage, fill= factor(status, levels = statlevs))) + geom_bar(stat="summary", width=0.97) + scale_fill_manual(values=Colors)+
                   coord_cartesian(expand = FALSE)+
                   labs(title = paste('Data Distribution for Heavy Metals',sep =" "),
                        y = "Data Status Proportion per Tissue", x = "Tissues", fill= ' Data Status')+
                   #scale_fill_manual(values=Colors)+
                   scale_y_continuous(labels = function(x) paste0(x, "%"))+
                   facet_wrap(vars(year), scales ="free_x", ncol=3, drop = FALSE)+ theme(strip.text = element_text(size = 15, colour = 'black'), axis.text.x = element_text(size = 10, colour = 'black'))
                 print(plt)
                 list0<-append(list(plt),list0, 0)
                 name0 <- paste("Data Distribution Barplots", sep = "")
                 list0names <- append(list0names,name0)
    # print(summarized_categorical_values)
    # print(summarized_categorical_values1)
  }else{
    range <- colnames(Subset_of_dataset_with_categorical_values[17:30])#Adipic_acid:Tolycaine SM
    column_range <- 17:30
    input_dataset<-x
  h<-1
  Subset_of_dataset_with_categorical_values <-Subset_of_dataset_with_categorical_values[,c(column_range,1,3,6)]
  #Subset_of_dataset_with_numerical_values <-x[,c(column_range,1,3,6)]
  Subset_of_dataset_with_numerical_values <-x[,c(column_range,1,3,6)]
  years <- levels(factor(Subset_of_dataset_with_categorical_values[,3]))
  # final2 <- data.frame(matrix(ncol=5, nrow = 0), check.names = FALSE)
  # outlier_detection_using_numerical_values <- data.frame(matrix())
  summarized_categorical_values <- data.frame(matrix(ncol=8, nrow = 0), check.names = FALSE)
  colnames(summarized_categorical_values)<-c('year', 'pollutant', 'Tissue', 'Total_N', 'BLOD', 'BLOQ', 'Detected', 'Outliers')
  outlier_detection_using_numerical_values <- data.frame(matrix(ncol=5, nrow = 0), check.names = FALSE)
  colnames(outlier_detection_using_numerical_values)<-c('year','Tissue', 'pollutant', 'concentration', 'outlier')
  fulldf2 <- data.frame(matrix(ncol=ncol(Subset_of_dataset_with_categorical_values), nrow = 0), check.names = FALSE)
  colnames(fulldf2)<-colnames(Subset_of_dataset_with_categorical_values)
  #while(h != length(years)+1){
  for (year in 1:length(years)){
    #year <-years[h] 
    # ndf <-Subset_of_dataset_with_categorical_values
    # ndf2 <-Subset_of_dataset_with_numerical_values
    long_dataset_with_categorical_values<- Subset_of_dataset_with_categorical_values %>% pivot_longer(all_of(range), names_to = "pollutant", values_to = "status")
    long_dataset_with_numerical_values<- Subset_of_dataset_with_numerical_values %>% pivot_longer(all_of(range), names_to = "pollutant", values_to = "concentration")%>%subset(concentration !=0)
  column_names <- colnames(Subset_of_dataset_with_categorical_value)[column_range]
   outlier_count_accumulated <- data.frame(matrix(ncol=8, nrow = 0), check.names = FALSE)
    colnames(outlier_count_accumulated)<-c('year', 'pollutant', 'Tissue', 'Total_N', 'BLOD','BLOQ', 'Detected', 'Outliers')
    long_dataset_with_categorical_values_per_year<-as.data.frame(filter(long_dataset_with_categorical_values, Year==years[year]))
    long_dataset_with_numerical_values_per_year<-as.data.frame(filter(long_dataset_with_numerical_values, Year==years[year]))
   outlier_count <- data.frame(matrix(ncol=8, nrow = 0), check.names = FALSE)
    colnames(categorical_values_partial1)<-c('year', 'pollutant', 'Tissue', 'Total_N', 'BLOD','BLOQ', 'Detected', 'Outliers')
    category1 <-'BLOD'
    category2 <-"BLOQ"
    category3 <-'Detected'
   outlier_count[1,1] <- years[year]
   outlier_count[1,2] <- "Total"
   outlier_count[1,3] <- 'all'
   outlier_count[1,4] <- nrow(long_dataset_with_categorical_values_per_year)
   outlier_count[1,5] <- sum(str_count(long_dataset_with_categorical_values_per_year$status,category1))
   outlier_count[1,6] <- sum(str_count(long_dataset_with_categorical_values_per_year$status,category2))
   outlier_count[1,7] <- sum(str_count(long_dataset_with_categorical_values_per_year$status,category3))
   outlier_count[1,8] <- 0
    summarized_categorical_values <-outlier_count
  
    #print(long_dataset_with_categorical_values_per_year)
    #for tissue----
    for(pollutant in 1:length(column_names)){
      outlier_detection_using_numerical_values <- data.frame(matrix(ncol=5, nrow = 0), check.names = FALSE)
      colnames(outlier_detection_using_numerical_values)<-c('year','Tissue', 'pollutant', 'concentration', 'Outlier')
      dataset_for_categorical_values_per_pollutant<-as.data.frame(filter(long_dataset_with_categorical_values_per_year, pollutant==column_names[pollutant]))
      dataset_for_numerical_values_per_pollutant<-as.data.frame(filter(long_dataset_with_numerical_values_per_year, pollutant==column_names[pollutant]))
       outlier_count <- data.frame(matrix(ncol=8, nrow = 0), check.names = FALSE)
        colnames(categorical_values_partial1)<-c('year', 'pollutant', 'Tissue', 'Total_N', 'BLOD', 'BLOQ', 'Detected', 'Outliers') 
        category1 <- 'BLOD'
        category2 <- 'BLOQ'
        category3 <- 'Detected'
      tissues <- c('liver', 'stomach', 'muscle', 'inksac')
      #print(tissues)
      for(tissue in 1:length(tissues)){
        dataset_for_categorical_values_per_tissue<-as.data.frame(filter(dataset_for_categorical_values_per_pollutant, Tissue==tissues[tissue]))
        dataset_for_numerical_values_per_tissue<-as.data.frame(filter(dataset_for_numerical_values_per_pollutant, Tissue==tissues[tissue])%>%mutate(outlier=NA))
   
         outlier_count[1,1] <- years[year]
         outlier_count[1,2] <- column_names[pollutant]
         outlier_count[1,3] <- tissues[tissue]
         outlier_count[1,4] <- nrow(dataset_for_categorical_values_per_tissue)
         outlier_count[1,5] <- sum(str_count(dataset_for_categorical_values_per_tissue$status,category1))
         outlier_count[1,6] <- sum(str_count(dataset_for_categorical_values_per_tissue$status,category2))
         outlier_count[1,7] <- sum(str_count(dataset_for_categorical_values_per_tissue$status,category3))
        if(nrow(dataset_for_numerical_values_per_tissue)>1){
          outliercheck <- function(x, v){
            if (grepl("Fe|Ag", colnames(data)[16])) {
              column_range <- 16:25
              z<-v
            }else{
              column_range <- 16:19
              z<-v
            }
            #print(column_range)
            list0 <- list()
            list0names <- c()
            count_outliers<- c()
            for (row in 1:nrow(x)){
              if((x[row,'concentration'])>(3*mean(x[-row,'concentration']))){
                count_outliers <- append(count_outliers, 1)
                x[row,'outlier'] <- 'yes'
              }else{
                count_outliers <- append(count_outliers, 0)
                x[row,'outlier'] <- 'no'
              }
            }
            #print(x)
            final_count_outliers <- sum(count_outliers)
            list0<-append(list(final_count_outliers),list0, 0)
            name0 <- paste("outlier_count", sep = "")
            list0names <- append(list0names,name0) 
            list0<-append(list(x),list0, 0)
            name0 <- paste("outlier_detection_dataset1", sep = "")
            list0names <- append(list0names,name0)
            list0<-append(list(z),list0, 0)
            name0 <- paste("partial_input_dataset", sep = "")
            list0names <- append(list0names,name0)
            return(dataset_list=list0)
          }
          outlier_count[1,8] <- outliercheck(x=dataset_for_numerical_values_per_tissue, v=input_dataset)[[1]]
          outlier_detection_dataset1 <- outliercheck(x=dataset_for_numerical_values_per_tissue, v=input_dataset)[[2]]
          outlier_detection_using_numerical_values <- rbind(outlier_detection_using_numerical_values, outlier_detection_dataset1)
          partial_input_dataset <- outliercheck(x=dataset_for_numerical_values_per_tissue, v=input_dataset)[[3]]
          full_input_dataset <- rbind(full_input_dataset, partial_input_dataset)
          #print(outlier_detection_using_numerical_values_partial)
          #print(input_dataset)
        }else{
          #print(k)
          #print(ndf4)
          #print(input_dataset)
         outlier_count[1,8] <- 0
          dataset_for_numerical_values_per_tissue[ 1,'Year'] <- paste(years[year])
          dataset_for_numerical_values_per_tissue[ 1,'pollutant'] <- paste(column_names[pollutant])
          dataset_for_numerical_values_per_tissue[ 1,'Tissue'] <- paste(tissues[tissue])
          if(nrow(dataset_for_numerical_values_per_tissue)==1){
            dataset_for_numerical_values_per_tissue[ 1,'concentration'] <- paste(dataset_for_numerical_values_per_tissue[,'concentration'])
          }else{
            dataset_for_numerical_values_per_tissue[ 1,'concentration'] <- 0 
          }
          dataset_for_numerical_values_per_tissue[ 1,'outlier'] <- 'no'
          #print(z)
          outlier_detection_using_numerical_values <- rbind(outlier_detection_using_numerical_values, dataset_for_numerical_values_per_tissue)
        }
       outlier_count_accumulated <- rbind(outlier_count_accumulated,outlier_count)
      }
      outlier_detection_using_numerical_values <- rbind(outlier_detection_using_numerical_values, outlier_detection_using_numerical_values)
    }
    summarized_categorical_values <- rbind(summarized_categorical_values,outlier_count_accumulated)
    finy <- data.frame(matrix(ncol=8, nrow = 0), check.names = FALSE)
    colnames(finy)<-c('year', 'pollutant', 'Tissue', 'Total_N', 'BLOD','BLOQ', 'Detected', 'Outliers')
    category1 <-'BLOD'
    category2 <-"BLOQ"
    category3 <-'Detected'
    finy[1,1] <- years[year]
    finy[1,2] <- "all pollutants"
    finy[1,3] <- "all tissues"
    finy[1,4] <- nrow(long_dataset_with_categorical_values_per_year)
    finy[1,5] <- sum(str_count(long_dataset_with_categorical_values_per_year$status,category1))
    finy[1,6] <- sum(str_count(long_dataset_with_categorical_values_per_year$status,fixed(category2)))
    finy[1,7] <- sum(str_count(long_dataset_with_categorical_values_per_year$status,category3))
    finy[1,8] <- sum(as.numeric(summarized_categorical_values$Outliers))
    summarized_categorical_values <- rbind(summarized_categorical_values, finy)
    }
  # print(fulldf2)
  # print(summarized_categorical_values)
  for (te in 1:nrow(outlier_detection_using_numerical_values)){
    if(outlier_detection_using_numerical_values[te,'outlier']=='yes'){
      contaminant <- unique(outlier_detection_using_numerical_values[te,'pollutant'])
      for(ne in 1:length(column_range)){
        if(colnames(input_dataset)[ne+16]!= contaminant){
          next
        }else{
          for (se in 1:nrow(input_dataset)){
            if(outlier_detection_using_numerical_values[te,'ID']==input_dataset[se,'ID'] & outlier_detection_using_numerical_values[te,'Year']==input_dataset[se,'Year'] & outlier_detection_using_numerical_values[te,'Tissue']==input_dataset[se,'Tissue']){
              input_dataset[se,ne+16]<-input_dataset[se,ne+16]
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
  summarized_categorical_values1 <- summarized_categorical_values
  for(pi in 1:nrow(summarized_categorical_values1)) {
  summarized_categorical_values1[pi,'Detected'] <- summarized_categorical_values1[pi,'Detected']-summarized_categorical_values1[pi,'Outliers']
  }
  statlevs<- c('BLOD','BLOQ','Outliers','Detected')
  dfn<- summarized_categorical_values1 %>% pivot_longer(all_of(statlevs), names_to = "status", values_to = "values")
  
  nzdPT <- dfn %>%group_by(status,Tissue) %>%mutate(Percentage = (values/Total_N)* 100)
  nzdPT[is.na(nzdPT)] = 0
  print(nzdPT)
  #nzdPT <- dfn %>%group_by(status,year, Tissue) %>%summarise(Percentage = n() / nrow(dfn) * 100)
  print(nzdPT)
  #nzdP1 <- long_dataset_with_numerical_values_per_year %>%group_by(status, Year) %>%summarise(Percentage = n() / nrow(long_dataset_with_numerical_values_per_year) * 100) %>%mutate(Tissue='Total')
  #nzdPT <- dfn %>%group_by(status, Tissue) %>%mutate(Percentage = (values/Total_N)* 100)
  #nzdP0 <-dfn %>%group_by(status, Year) %>%summarise(N = n())
  statlevs<- c('BLOD','BLOQ','Outliers','Detected')
  Colors <-setNames( c('#F8766D','#00A9FF','yellow','#00BA38'),statlevs)
  tissue_levels <- c("liver","stomach","muscle","inksac")
  metlevs <- c("Adipic_acid","Aminobenzoic_acid","Caprolactam","Chlorpyrifos","Diaminohexane","Estradiol","Ethylene_glycol","Ibuprofen","Metolachlor","Nortestosterone","Sulpiride","Terephthalic_acid","Toluidine","Tolycaine", "Total")
  plt <-ggplot(nzdPT, aes(x = factor(Tissue, levels = tissue_levels), y = Percentage, fill= factor(status, levels = statlevs))) + geom_bar(stat="summary", width=0.97) + scale_fill_manual(values=Colors)+
    coord_cartesian(expand = FALSE)+
    labs(title = paste('Data Distribution for Small Molecules',sep =" "),
         y = "Data Status Proportion per Tissue", x = "Tissues", fill= ' Data Status')+
    #scale_fill_manual(values=Colors)+
    scale_y_continuous(labels = function(x) paste0(x, "%"))+
    facet_wrap(vars(year), scales ="free_x", ncol=3, drop = FALSE)+ theme(strip.text = element_text(size = 15, colour = 'black'), axis.text.x = element_text(size = 10, colour = 'black'))
  print(plt)
  list0<-append(list(plt),list0, 0)
  name0 <- paste("Data Distribution Barplots", sep = "")
  list0names <- append(list0names,name0)
  }
  print(summarized_categorical_values)
  print(summarized_categorical_values1)
  
  #detminout<- as.data.frame(t(apply(data.frame(summarized_categorical_values), MARGIN = 1, outlierminus)))
  #print(detminout)
  
  return(list (summarized_categorical_values=summarized_categorical_values, outlier_detection_using_numerical_values=outlier_detection_using_numerical_values, fulldf=input_dataset))
}
datadis3 <- datadis3(y=hmdis, x=hmfull)
