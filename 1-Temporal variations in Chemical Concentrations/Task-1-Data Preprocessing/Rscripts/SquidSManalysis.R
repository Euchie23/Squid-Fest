#LOADING LIBRARIES----
library(readxl)
library(dplyr)
library(magrittr)
library(stringr)
library(stringi)
library(gdata)



#LOADING DATASETS----

#below code goes through the excel file and makes each sheet into one CSV file and saves all the CSV files in one folder with todays date and time as the name. each CSV file represents a organic compound only need to use once. UNMUTE IF YOU WANT TO START FROM THE EXCEL FILES 
# require(gdata)
# excelFile <- file.choose() # Ask for Excel file
# names = sheetNames(excelFile)
# noOfSheets = length(names)
# out <- paste(dirname(excelFile), as.POSIXct(Sys.time()), sep="/") # same directory as source file + timestamp to avoid override
# dir.create(out)
# for (i in 1:noOfSheets) {
#   currentSheet <- read.xls(excelFile, sheet=i)
#   write.csv(currentSheet, file=paste(out, paste(names[i], "csv", sep="."), sep="/"), row.names = FALSE)
# }
#-------------------------------------------RUN FIRST AND ONLY ONCE TO CHOOSE ORGANIC COMPOUND (OC) EXCEL FILE, THEN GO BACK IN SAME FOLDER TO SEE ANOTHER FOLDER WITH CSV FILES (IGNORE WARNING MESSAGE) ALSO YOU CAN CHANGE THE NAME TO MAKE IT MORE RELATABLE IN MY EXAMPLE I RENAMED THE FILES WITH THE SAME NAME AS THE EXCEL FILES --------------------------------------------#

#adding csv files to list
 smnames <- c('Adipic_acid','Caprolactam','Chlorpyrifos','Ibuprofen')

 
 
#taking the CSV files from the folder and making them into a list (each file/organic compounds has its own dataset)
files <- list.files(path = "Organic Compounds/2020_OC/")
Smcsv <- list()
for (i in 1:length(files)) {
  Smcsv[[i]] <- read.csv(paste0("Organic Compounds/2020_OC/",files[i]), header = T, sep = ",", dec = ".")
}

names(Smcsv) <- smnames

#SM dry weight#
#PLEASE REMEMBER TO CHANGE DATASET TO CORRECT YEAR
sm_dw1 <- read.csv("Organic Compounds/Dry weight data /2020dryweight_SM.csv", header= TRUE)

if(colnames(sm_dw1)[ncol(sm_dw1)]!= "Ink.sac..mg."){
  colnames(sm_dw1) <- c("ID","s","l","m") 
}else{
  colnames(sm_dw1) <- c("ID","s","l","m","i")
}

#distance to Argentina and Falkland Islands.distance to land (dtl)
dtl<- read.csv("Squid Catch Data/Distance_to_land.csv", header= TRUE)

#SQUID CATCH DATA...CHANGE BASED ON THE YEAR YOU ARE PROCESSING, Basic information = bi
bi<- read.csv("Squid Catch Data/2020_catch_data.csv", header= TRUE)

#STEP 1: CLEANING AND MODIFYING DATASETS----
#1 String manipulation and data cleaning
manip0<- function(x){
  colnames(x) = gsub("\\..", "_", colnames(x)) 
  x$Sample_Name <- gsub("_D10000", "", x$Sample_Name)
  x$Sample_Name <- gsub("*NAT*", "", x$Sample_Name)
  x$Sample_Name <- gsub("\\**", "", x$Sample_Name)
  x$Tissue <- str_extract(x$Sample_Name, "^[^_]*")#Takes only the first characters before the underscore
  x$Tissue <- gsub("_", "", x$Tissue)
  x$Tissue <- gsub("\\d+$", "", x$Tissue)
  fid <- str_extract(x[,1], "(?<=_)[^_]+[^_]") 
  print(fid)
  x$Sample_Name <- gsub("-", "_", x$Sample_Name)
  x$Sample_Name <- paste(fid,x$Tissue, sep = "_")
  x$Sample_Name = tolower(x$Sample_Name)
  x$Sample_Name <- gsub("-", "_", x$Sample_Name)
  x$Sample_Name <- gsub(" ", "", x$Sample_Name)
  return(x)
}
Sm <- lapply(Smcsv, manip0)
# #Check point 1 (Sm list (any dataset)) ----
#------------------------------------------------------------------------------------------------



#2 FUNCTIONS TO HELP MODIFY SAMPLE IDS
dcc <- function(x){#padding 0 to first two digits
  if (str_detect(substring(x[1],1,2), '_') == TRUE){ 
    sk=print(paste('0',substring(x[1],1,)))
    sk<- gsub(" ", "",  sk)
    x[1] <- gsub(substring(x[1],1,), sk,  x[1])
  }
  x[1] =  gsub("-", "_",  x[1])
  return(x)
}

dc<-function(x){ #padding 0 to second two digits
  if (str_detect(substring(x[1],4,5), '_') == TRUE){ 
    sr=print(paste('0',substring(x[1],4,)))
    sr<- gsub(" ", "",  sr)
    x[1] <- gsub(substring(x[1],4,), sr,  x[1]) 
  }
  return(x)
}

#2 code to activate function for sample name mmodification 
Sm1 <- list()
rw <- data.frame(matrix(NA, ncol=ncol(Sm[[1]])))
rw1 <- data.frame(matrix(NA, ncol=ncol(Sm[[1]])))
for (l in 1:length(Sm)){
  rw <- apply(Sm[[l]], MARGIN = 1, dcc)
  rw1 <- apply(t(rw), MARGIN = 1, dc)
  Sm1<-append(list(data.frame(t(rw1))),Sm1, 0)
}
names(Sm1)<-names(Sm)

#3 ID number and Area number 
manip<- function(x){
   x$ID_num <- str_extract(x$Sample_Name, "(?<=_)..")
   x$ID_num <- gsub("-", "", x$ID_num)
   x$Area= str_extract(x$Sample_Name, "(?<=)..")
   x$dry_weight=NA
   x$conc_mg_kg=NA
   x$ID_num <- gsub("_", "", x$ID_num)
   x[,'Cal_Conc.'] <- gsub("N/A", 0,  x$Cal_Conc.)# replace N/A with 0 because no peak was detected meaning no concentration of the small molecule was detected.
   x$Cal_Conc. <- gsub("BLOQ", 9999999,  x$Cal_Conc.) #replace BLOQ with 999999
x$Cal_Conc. <- gsub("< 0", 9999999,  x$Cal_Conc.) #replace < 0 with 999999
x$Cal_Conc. <- gsub(" ","",  x$Cal_Conc.)
 x$Cal_Conc. = as.numeric( x$Cal_Conc.)
  x<-select(x, -contains(c('Expected_RT','Height','Retention_Time','Retention_Time_Delta_(min)','Accuracy','Ion_Ratio', 'Component_Name','Actual_Concentration','Used','X','Dilu_Factor','Back_al_Conc.')))
  return(x)
}
Sm2<-lapply(Sm1, manip)

#check point 2 (Sm2 list (any dataset)) ----
#------------------------------------------------------------------------------------------
  
  
#STEP 2: ADDING SAMPLE SEPARATION DATA (dry weight) TO CONCENTRATION DATASET AND CALCULATIING CONCETRATION MG/KG----

#4 formatting sample names of dry weight dataset
dcc1 <- function(x){#padding 0 to first two digits
  x[1] =  gsub("-", "_",  x[1])
  print(paste(x[1]))
  if (str_detect(substring(x[1],1,2), '_') == TRUE){
    sk=print(paste('0',substring(x[1],1,)))
    sk<- gsub(" ", "",  sk)
    x[1] <- gsub(substring(x[1],1,), sk,  x[1])
  }
  return(x)
}


dc1<-function(x){ #padding 0 to second two digits
  if (str_detect(substring(x[1],5), "^$") == TRUE){
    sr=print(paste('_0',substring(x[1],4,)))
    sr<- gsub(" ", "",  sr)
    x[1] <- gsub(substring(x[1],3,), sr,  x[1])
  }
  return(x)
}

rt <- apply(sm_dw1, MARGIN = 1, dc1) #pad 0 to site in dry weight sample names
sm_dw0 <- apply(t(rt), MARGIN = 1, dcc1) #pad 0 to ID num in dry weight sample names
sm_dw1 <- data.frame(t(sm_dw0))


#combining formatted sample names in dry weight dataset to the concentration dataset for mg/kg calculation
findw<- function(y){ #x= dryweight dataset, y=mass_spec_res 
  i<-2
  x=sm_dw1
  for(h in 1:nrow(y)){
    while((str_extract(y[h,1], "(?<=.....)[^_]") == colnames(x)[i]) ==FALSE) {
      if(i==ncol(x)){
        i<-2
      }
      if((str_extract(y[h,1], "(?<=.....)[^_]") == colnames(x)[i]) ==FALSE) {
        print(paste("This is h",h, "column", i))
        i<-i+1
        print(paste("This is h",h, "column", i))
      }
    }
    if((str_extract(y[h,1], "(?<=.....)[^_]") == colnames(x)[i]) ==TRUE) {
      for(j in 1:nrow(x)){
        if ((str_extract(y[h,1], "(?<=)[^_]+[^_]...")== x[j,1]) ==TRUE) { 
          y[,'dry_weight'][h] <- x[j,i]
          print(paste("h", h, "matches with", "j",j,"column",i))
          if((y$Cal_Conc.[h] == "9999999")!=TRUE){
            y[,'conc_mg_kg'][h] <- round(as.numeric(y$Cal_Conc.[h])/as.numeric(y$dry_weight[h])*1000/1000,digits = 3)#calculating concentration mg/kg if the row in Cal_conc. is not equal to 9999999
          }else{
            y[,'conc_mg_kg'][h] <- paste(as.numeric(y[,'LOQ'][h]),"BLOQ") #if the row in Col.conc is equal to 999999 then paste LOQ it to BLOQ
          }
        }else{
          j<-j+1
        }
      }
      
      if (h == nrow(y)){
        break
      }
    }
  }
  return(y)
}


Sm3 <- list() 
rt <- data.frame(matrix(NA, ncol=ncol(Sm2[[1]])))
rt1 <- data.frame(matrix(NA, ncol=ncol(Sm2[[1]])))
for (l in 1:length(Sm2)){
  rt1 <- findw(Sm2[[l]])#add dry weight to dataframes
  Sm3<-append(list(rt1),Sm3, 0)
}
names(Sm3)<-names(Sm2)
#check point 3 (Sm3 list (any dataset)) ----


#----------------------------------------------------------------------------------
#STEP 3: ADDING CONCENTRATIONS MG/KG FROM EACH DATASET IN A NEW DATASET TO RESEMBLE HEAVY METALS DATASET----
sm.2 <- data.frame(matrix(NA, nrow=nrow(Sm2[[1]]), ncol=length(Sm2)+5))
sm.2[,1]<-Sm3[[1]][,1]
sm.2[,2]<-Sm3[[1]][,2]
sm.2[,3]<-Sm3[[1]][,9]
sm.2[,4]<-Sm3[[1]][,8]
sm.2[,5]<-Sm3[[1]][,7]
colnames(sm.2)[1]<-colnames(Sm2[[1]])[1]
colnames(sm.2)[2]<-colnames(Sm2[[1]])[2]
colnames(sm.2)[3]<-colnames(Sm2[[1]])[9]
colnames(sm.2)[4]<-colnames(Sm2[[1]])[8]
colnames(sm.2)[5]<-colnames(Sm2[[1]])[7]
colnames(sm.2)[1:length(names(Sm3))+5]<-names(Sm3)


k <- 6
h <- 1
l <- 1
x <- sm.2

while (l!= length(Sm3)+1){
    if (names(Sm3)[[l]]== colnames(x)[k]){
        print(paste('L is ', l))
            for (i in 1:nrow(sm.2)){
              print(paste('L is ', l,'in the loop'))
                  if((Sm3[[1]][i,1] == x[h,1]) ==TRUE) {
                    x[h,k] <- Sm3[[l]][i,'conc_mg_kg']
                    h <- h+1
                     }
              if(h == nrow(sm.2)+1){
                h <- 1
                break
              }
              }
      l <- l+1
    }else{
      k <- k+1
    }
}

if (26 %in% x$Area) {
  x$Year<-2019
} else if(60 %in% x$Area){
  x$Year<-2020
}else{
  x$Year<-2021
}
x <- x %>% relocate(Year, .after = dry_weight)
#check point 4----

# FUNCTION TO MODIFY SAMPLE IDS FOR DTL AND BI DATASET 
dck0<-function(x){ #padding 0 TO AREA COLUMN
  if (str_detect(substring(x[1],2), "^$") == TRUE){
    sr=print(paste('0',substring(x[1],1)))
    sr<- gsub(" ", "",  sr)
    print(paste(sr))
    x[1] <- gsub(substring(x[1],1), sr,  x[1])
    print(paste(x[1]))
  }
  return(x)
}

#ID NUMBER
dck<-function(x){ #padding 0 to ID_NUMBER COLUMN
  if (str_detect(substring(x[2],2), "^$") == TRUE){
    sr=print(paste('0',substring(x[2],1)))
    sr<- gsub(" ", "",  sr)
    print(paste(sr))
    x[2] <- gsub(substring(x[2],1), sr,  x[2])
    print(paste(x[2]))
  }
  return(x)
}


#formatting samples names to match concentration dataset
dtl$Area. <- as.character(dtl$Area.) 
dtl1<-data.frame(t(apply(dtl,1, dck0)))
dtl2<-data.frame(t(apply(dtl1,1, dck)))
#ADDING distance to Argentina and Falkland Islands
x$dta_km <- NA
x$dtfl_km <- NA
h <- 1 
while(h!= nrow(x)+1){
  for (i in 1:nrow(dtl2)){
    if ((x$Area[h] == dtl2$Area.[i])==TRUE){
      x$dta_km[h] <- dtl2[i,4]
      x$dtfl_km[h] <- dtl2[i,5]
      print(paste('This is  x', h,'and dtl2', i))
      h <- h+1
      if (h == nrow(x)+1){
        break
      }
    }
  }
}


#FOR SETTING UP CATCH DATA F0R SQUIDS MAKE SURE "ID" AND "AREA' COLUMNS ARE CHARACTERS
bi$ID <- as.character(bi$ID)
bi$Area <- as.character(bi$Area)

#formatting samples names to match concentration dataset
bi1<-data.frame(t(apply(bi,1, dck0)))
binfo<-data.frame(t(apply(bi1,1, dck)))

#ADDING CATCH DATA TO DATASET
x$Gender <- NA
x$Longitude <- NA
x$Latitude <- NA
x$Month_of_Capture <- NA
x$Mantle_length_mm <- NA
x$Wet_Weight_g <- NA
x$Maturity_level <- NA
h <- 1 
while(h!= nrow(x)+1){
  
  for (i in 1:nrow(binfo)){
    if ((x$Area[h] == binfo$Area[i] && x$ID_num[h] == binfo$ID[i])==TRUE){
      x$Longitude[h] <- binfo$Longitude[i]
      x$Latitude[h] <- binfo$Latitude[i]
      x$Month_of_Capture[h] <- binfo$Month_of_Capture[i]
      x$Mantle_length_mm[h] <- binfo$Mantle_length_mm[i]
      x$Wet_Weight_g[h] <- binfo$Wet_Weight_g[i]
      # Check if Gender column exists.. Only females were caught for 2019 which is why no gender was included for 2019,
      if ("Gender" %in% colnames(binfo)) { #0 = Females, 1 = Males
        x$Gender[h] <- binfo$Gender[i]
      } else{ # this means that if binfo doesn't have gender then it must be from 2019 catch data
        x$Gender[h] <- 0
      }
      x$Maturity_level[h] <- binfo$Maturity_level[i]
      print(paste('This is x', h,'and bi1', i))#tells you which row in the datasets match.
      h <- h+1
      if (h == nrow(x)+1){
        break
      }
    }
  }
}


#REARRANGING COLUMNS FOR FINAL DATASET
x=x[,c(1:6, 11:19, 7:10)]
colnames(x)[c(1,3)] <- c("ID", "DW")

   # #EXPORTING EXTRACTION RESULTS FILE INTO CSV/EXCEL
   # write.csv(x, "Results/Final_SMresults_mgkg.csv", row.names = FALSE)
   
   #APPENDING FUTURE BATCHES TO EXTRACTION RESULTS CSV
   Final_res_SM = "Results/Final_SMresults_mgkg.csv"
   write.table(x, file = Final_res_SM, sep = ",",#change the rows index
               append = TRUE, quote = FALSE,
               col.names = FALSE, row.names = FALSE)

