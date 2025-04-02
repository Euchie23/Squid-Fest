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

#HM datasets----

# FUNCTIONS TO HELP MODIFY AREA NUMBERS FOR FURTHER ANALYSIS (HM)
dcc <- function(x){#padding 0 to first two digits
  if (str_detect(substring(x[4],1,2),"\\.") == TRUE){ 
    sk=print(paste('0',substring(x[4],1,)))
    sk<- gsub(" ", "",  sk)
    x[4] <- gsub(substring(x[4],1,), sk,  x[4])
  }
  return(x)
}
#Final resuitls for Heavy Metals
hm1<- read.csv("~/Documents/For_Thesis/Chemical_analysis_of_Illex_argentinus/THESIS_DATA_AND_ANALYSES/data_and results from_running_stats _analysis/Chem_lab_excel_files /R_Exported_excel:CSV_files/final results HM & SM/HM_results_forTSUNGHAN.csv", header= TRUE)
hm1[,4] <- paste0(hm1[,4], ".")
hmfull <- as.data.frame(t(apply(hm1, MARGIN = 1, dcc)))
hmfull$Area <- gsub("\\.", "",hmfull$Area)
hmfull[,c(17:26)] <- lapply(hmfull[,c(17:26)], gsub, pattern = ".*BLOQ.*", replacement = 0)
hmfull[,c(17:26)] <- lapply(hmfull[,c(17:26)], gsub, pattern = ".*BB.*", replacement = 0)
hmfull[,c(17:26)] <- lapply(hmfull[,c(17:26)], gsub, pattern = "^0$", replacement = 0)
hmfull[,c(17:26)]  <- lapply(hmfull[,c(17:26)] , gsub, pattern = "N/A", replacement = 0)
hmfull[,c(7,10:12,14:26)] =as.numeric(unlist(hmfull[,c(7,10:12,14:26)]))
hmfull[,10]<- cut(hmfull[,10], breaks = 3, labels =c(2,3,4))
#hmfull[,7]<- cut(hmfull[,7], breaks = 2, labels =c(0,1))
hmfull[, 12] <- as.numeric(gsub(2811.1, 281.1, hmfull[, 12]))
hmfull[,13]<- as.character(hmfull[,13])
hmfull[,3]<- as.character(hmfull[,3])
# hmfull[,15]<- cut(hmfull[,15], breaks = 4, labels =c(200, 400, 600, 800))
# hmfull[,16]<- cut(hmfull[,16], breaks = 5, labels =c(100, 130, 140, 150, 190))
# Assuming 'y' is your data frame, and you want to reorder columns 17 to 25 alphabetically:

# Get the column names for columns 17 to 25
cols_to_reorder <- names(hmfull)[17:26]

# Sort the column names alphabetically
sorted_cols <- sort(cols_to_reorder)

# Reorder the columns based on the sorted names
hmfull <- hmfull[, c(1:16, match(sorted_cols, names(hmfull)))]



#SM datasets----

# FUNCTIONS TO HELP MODIFY AREA NUMBERS FOR FURTHER ANALYSIS (SM)
dkc <- function(x){#padding 0 to first two digits
  if (str_detect(substring(x[4],1,2),"\\.") == TRUE){ 
    sk=print(paste('0',substring(x[4],1,)))
    sk<- gsub(" ", "",  sk)
    x[4] <- gsub(substring(x[4],1,), sk,  x[4])
  }
  return(x)
}

sm2<- read.csv("~/Documents/For_Thesis/Chemical_analysis_of_Illex_argentinus/THESIS_DATA_AND_ANALYSES/data_and results from_running_stats _analysis/Chem_lab_excel_files /R_Exported_excel:CSV_files/final results HM & SM/SM_results_forTSUNGHAN3.csv", header= TRUE)
sm2[,4] <- paste0(sm2[,4], ".")
colnames(sm2)[4]<- "Area"
sm1 <- as.data.frame(t(apply(sm2, MARGIN = 1, dkc)))
sm1$Area <- gsub("\\.", "",sm1$Area)
sm <- sm1 %>% relocate(Area, .after = ID_num)
sm[,c(17:30)] <- lapply(sm[,c(17:30)], gsub, pattern = ".*BLOQ.*", replacement = 0)
sm[,c(17:30)] <- lapply(sm[,c(17:30)], gsub, pattern = "N/A", replacement = 0)
sm[,c(17:30)] <- lapply(sm[,c(17:30)], gsub, pattern = "^0$", replacement = 0)
sm[,c(10:12,15:16,17:30)] =as.numeric(unlist(sm[,c(10:12,15:16,17:30)]))
sm[, 12] <- as.numeric(gsub(2811.1, 281.1, sm[, 12]))
sm[,10]<- cut(sm[,10], breaks = 3, labels =c(2,3,4))
sm[,6]<- tolower(sm[,6])
sm[,7]<- as.character(sm[,7])
sm[,3]<- as.character(sm[,3])
sm[,13]<- as.character(sm[,13])
#sm[,11]<- cut(sm[,11], breaks = 4, labels =c(150, 250, 350, 450))
#sm[,15]<- cut(sm[,15], breaks = 4, labels =c(200, 400, 600, 800))
#sm[,16]<- cut(sm[,16], breaks = 5, labels =c(100, 130, 140, 150, 190))
sm1 <- sm 

#working to automatically create plots using tidyverse, ggplot and for loop----
#generating the markdown for reading images:
#SMicons for Small Molecules
SMiconz <- data.frame(SM=c("Adipic_acid","Aminobenzoic_acid","Caprolactam","Chlorpyrifos","Diaminohexane","Estradiol","Ethylene_glycol","Ibuprofen","Metolachlor","Nortestosterone","Sulpiride","Terephthalic_acid","Toluidine","Tolycaine"), icons=c("https://img.icons8.com/?size=38&id=ZbNOoXleVpQN&format=png","https://img.icons8.com/?size=38&id=yeuBdleHsCXN&format=png","https://img.icons8.com/?size=38&id=ZbNOoXleVpQN&format=png","https://img.icons8.com/?size=38&id=15168&format=png","https://img.icons8.com/?size=38&id=ZbNOoXleVpQN&format=png","https://img.icons8.com/?size=38&id=108787&format=png","https://img.icons8.com/?size=38&id=ZbNOoXleVpQN&format=png","https://img.icons8.com/?size=38&id=108787&format=png","https://img.icons8.com/?size=38&id=15168&format=png","https://img.icons8.com/?size=38&id=108787&format=png","https://img.icons8.com/?size=38&id=108787&format=png","https://img.icons8.com/?size=38&id=ZbNOoXleVpQN&format=png","https://img.icons8.com/?size=38&id=15168&format=png","https://img.icons8.com/?size=38&id=108787&format=png"))
urls <-SMiconz$icons
names(urls) <- SMiconz$SM

#HMicons for Heavy Metals:
HMiconz <- data.frame(SM=c("Ag","Cd","Co","Cu","Fe","Hg","Ni","Pb","Tl","Zn"), icons=c("https://img.icons8.com/?size=160&id=8FW995comxyx&format=png","https://img.icons8.com/?size=160&id=8FW995comxyx&format=png","https://img.icons8.com/?size=160&id=8FW995comxyx&format=png","https://img.icons8.com/?size=160&id=8FW995comxyx&format=png","https://img.icons8.com/?size=160&id=8FW995comxyx&format=png","https://img.icons8.com/?size=160&id=8FW995comxyx&format=png","https://img.icons8.com/?size=160&id=8FW995comxyx&format=png","https://img.icons8.com/?size=38&id=ZbNOoXleVpQN&format=png","https://img.icons8.com/?size=38&id=ZbNOoXleVpQN&format=png","https://img.icons8.com/?size=160&id=8FW995comxyx&format=png"))
urlz <-HMiconz$icons
names(urlz) <- HMiconz$SM

#prepping markdown for ggplot. This is used to turn the url text into an image.
theme_icons <- function(base_size = 10,
                        title_size = 20,
                        ...){
  # CUSTOM THEME:
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      
      # axis
      axis.text = element_text( size = 10 ),
      axis.text.x = element_text( size = 10),
      axis.title = element_text( size = 16, face = "bold" ),
      
      # title
      plot.title = element_text(size = title_size),
      plot.title.position = "plot",
      
      #panel
      panel.background = element_rect(fill="white"),
      panel.ontop = FALSE,
      #legend
      legend.text = element_text(size = 15),
      legend.title = element_text(face = "bold", size = 16),
      
      #strip
      strip.text = element_text(size = 10),
      strip.background =element_rect(fill="lightgray"),
      strip.text.x = element_markdown(size = 20),
      ... 
    )
}
markdownfuncsm <- function(x){
  #step 1 saving the columns as vectors
  y <- paste0(x[1],"<img src=", "SMicons//", x[1], ".png", " width='17'/>")
  return(y)
}

# path<-"Users/mrnobody/Documents/For_Thesis/Chemical_analysis_of_Illex_argentinus/THESIS_DATA_AND_ANALYSES/data_and results from_running_stats _analysis/HMicons"
#For Heavy Metals:
markdownfunchm <- function(x){
  if(file.exists(paste0("HMicons//",x[1],".png"))&file.exists(paste0("HMicons//",x[1],"1",".png"))&file.exists(paste0("HMicons//",x[1],"2",".png"))==TRUE){
    y <- paste0(" ","<img src=", "HMicons//", x[1], ".png", " width='17'/>"," ", " ","<img src=","HMicons//", x[1],"1",".png"," width='17'/>"," ", " ","<img src=","HMicons//", x[1],"2",".png"," width='17'/>")
  }else if(file.exists(paste0("HMicons//",x[1],".png"))&file.exists(paste0("HMicons//",x[1],"1",".png"))==TRUE){
    y <- paste0(" ", " ","<img src=", "HMicons//", x[1], ".png", " width='17'/>"," ", " ","<img src=", "HMicons//", x[1],"1", ".png", " width='17'/>")
  }else{
    y <- paste0(" ","<img src=", "HMicons//", x[1], ".png", " width='17'/>")
  }
  return(y)
}

forcof <- function (x, per_of_outl_for_yaxis=.65){
  #x=dfnz
  siz <- levels(factor(x$size)) #remeber to change based on HM orSM
  tis <- levels(factor(x$Tissue))
  yr <- levels(factor(x$Year))
  s<-1
  if(unique(x$Tissue)=='inksac'){
    num <- 3
  }else{
    num <- 4
  }
  ctrfull1 <- data.frame(matrix(ncol=9, nrow = 0))
  while(s != num) {
    ctr0 <- data.frame(matrix(ncol=9, nrow = 0))
    ctr1 <- data.frame(matrix(ncol=9, nrow = 0))
    colnames(ctr1) <- c('Tissue','Year', 'SM','conc','distance','rho', 'pval' ,'yul','size')
    colnames(ctr0) <- c('Tissue','Year','SM', 'conc', 'distance','rho', 'pval', 'yul', 'size')
    ctrfull <- data.frame(matrix(ncol=9, nrow = 0))
    colnames(ctrfull) <- colnames(ctr1)
    colnames(ctrfull1) <- colnames(ctr1)
    outl  <-which(x$conc > quantile(x$conc,.99))
    outls <- x$conc[c(outl)]
    if(length(outls)!=0){
      metmol2 <- x[-c(outl),]
      #maxconc <- max(metmol2$conc)
      minoutl <- min(outls)
      #print(M[h])
      yscale <-minoutl*per_of_outl_for_yaxis 
      #print(yscale)
    }else{
      yscale <-0
      #maxconc <- max(as.numeric(year$conc))
    }
    
    year <- filter(as.data.frame(x), Year == yr[s])
    if(nrow(year)!=0){
      for (i in 1:length(siz)){
        sizedf <- filter(year, size == siz[i])
        # print(yr[s])
        #print(sizedf)
        #   filtbytis <- filter(filtbymet, Tissue == tis[i])
        #   filtbyyear <- filter(filtbytis, Year == yr[s])
        #conc_max <- max(filtbytis$conc) #taking the maximum concentration to make into an X-coord
        if(nrow(sizedf)<2|all(sizedf[-1,'conc'] == sizedf[1,'conc']|max(sizedf$conc)== 0)==TRUE){
          #print(unique(year$SM))
          #print(sizedf)
          ctr0[1,1] <- unique(year$Tissue)
          ctr0[1,2] <- yr[s]
          ctr0[1,3] <- unique(year$SM)
          ctr0[1,4] <- 0# X-coord for r coefficient and p-value
          ctr0[1,5] <- 0
          ctr0[1,6] <- 0
          ctr0[1,7] <- 0
          ctr0[1,8] <- max(sizedf$conc)
          ctr0[1,9] <- siz[i]
          ctrfull <- rbind(ctrfull, ctr0)
          #next
        }else if(max(sizedf$conc)!= 0 & str_detect(siz[i], "large")== TRUE){
          ctr <- cor.test(as.numeric(as.factor(sizedf$distance)), as.numeric(sizedf$conc), method="spearman", exact = FALSE)
          ctr0[1,1] <- unique(sizedf$Tissue)
          ctr0[1,2] <- yr[s]
          ctr0[1,3] <- unique(sizedf$SM)
          # if(unique(year$Year)== '2020'){
          #   ctr0[1,4] <- yscale+6
          # }else if (unique(year$Year)== '2021'){
          #   ctr0[1,4] <- yscale+6
          # }else{
          ctr0[1,4] <- signif((yscale*.95), 3)
          #}
          ctr0[1,5] <- max(as.numeric(as.factor(year$distance)))*.5
          ctr0[1,6] <- ctr$estimate
          ctr0[1,7] <- ctr$p.value
          ctr0[1,8] <- yscale
          ctr0[1,9] <- unique(sizedf$size)
          ctrfull <- rbind(ctrfull, ctr0)
          #print(ctrfull)
        }else if(max(sizedf$conc)!= 0 & str_detect(siz[i], "medium")== TRUE){
          ctr <- cor.test(as.numeric(as.factor(sizedf$distance)), as.numeric(sizedf$conc), method="spearman", exact = FALSE)
          ctr0[1,1] <- unique(sizedf$Tissue)
          ctr0[1,2] <- yr[s]
          ctr0[1,3] <- unique(sizedf$SM)
          # if(unique(year$Year)== '2020'){
          #   ctr0[1,4] <- yscale+4
          # }else if (unique(year$Year)== '2021'){
          #   ctr0[1,4] <- yscale+4
          # }else{
          ctr0[1,4] <- signif((yscale*.85), 3)
          #}
          ctr0[1,5] <- max(as.numeric(as.factor(year$distance)))*.5
          ctr0[1,6] <- ctr$estimate
          ctr0[1,7] <- ctr$p.value
          ctr0[1,8] <- yscale
          ctr0[1,9] <- unique(sizedf$size)
          ctrfull <- rbind(ctrfull, ctr0)
        }else if(max(sizedf$conc)!= 0 & str_detect(siz[i], "small")== TRUE){
          ctr <- cor.test(as.numeric(as.factor(sizedf$distance)), as.numeric(sizedf$conc), method="spearman", exact = FALSE)
          ctr0[1,1] <- unique(sizedf$Tissue)
          ctr0[1,2] <- yr[s]
          ctr0[1,3] <- unique(sizedf$SM)
          # if(unique(year$Year)== '2020'){
          #   ctr0[1,4] <- yscale
          # }else if (unique(year$Year)== '2021'){
          #   ctr0[1,4] <- yscale
          # }else{
          ctr0[1,4] <- signif((yscale*.75), 3)
          #}
          ctr0[1,5] <- max(as.numeric(as.factor(year$distance)))*.5
          ctr0[1,6] <- ctr$estimate
          ctr0[1,7] <- ctr$p.value
          ctr0[1,8] <- yscale
          ctr0[1,9] <- unique(sizedf$size)
          ctrfull <- rbind(ctrfull, ctr0)
        }
      }
    }
    # }else{
    #   ctr0[1,1] <- unique(x$Tissue)
    #   ctr0[1,2] <- yr[1]
    #   ctr0[1,3] <- unique(x$SM)
    #   ctr0[1,4] <- 0 # X-coord for r coefficient and p-value
    #   ctr0[1,5] <- 0
    #   ctr0[1,6] <- 0
    #   ctr0[1,7] <- 0
    #   ctr0[1,8] <- 0
    #   ctr0[1,9] <- siz[1]
    #   ctrfull <- rbind(ctrfull, ctr0)
    # }
    # }else{
    #   ctr0[1,1] <- unique(x$Tissue)
    #   ctr0[1,2] <- yr[1]
    #   ctr0[1,3] <- unique(x$SM)
    #   ctr0[1,4] <- 0 # X-coord for r coefficient and p-value
    #   ctr0[1,5] <- 0
    #   ctr0[1,6] <- 0
    #   ctr0[1,7] <- 0
    #   ctr0[1,8] <- min(year$conc)*per_of_outl_for_yaxis 
    #   ctr0[1,9] <- siz[1]
    #   ctrfull <- rbind(ctrfull, ctr0)  
    # }
    s<-s+1
    ctrfull1 <- rbind(ctrfull1, ctrfull)
    ctrfull1$Tissue <- as.character(ctrfull1$Tissue)
    ctrfull1$Year <- as.character(ctrfull1$Year)
    ctrfull1$SM <- as.character(ctrfull1$SM)
  }
  return (ctrfull1)
}
finalcof <- function(x){
  x$rho <- signif(x$rho, 4)
  x$pval <- signif(x$pval)
  x$rho<-ifelse(x$pval>0.05|x$pval==0, x$rho==NA, x$rho)
  x$pval<-ifelse(x$pval>0.05|x$pval==0, x$pval==NA,x$pval)
  x$rho <- paste0("r=",x$rho)
  x$pval <- paste0("p-val=",x$pval)
  x$pval[duplicated(x$pval)] <- NA
  x$rho[duplicated(x$rho)] <- NA
  x$rho <- gsub(".*NA.*", NA,x$rho)
  x$pval <- gsub(".*NA.*", NA,x$pval)
  x$rho[is.na(x$rho)] <- 0
  x$pval[is.na(x$pval)] <- 0
  x$pval[is.nan(x$pval)] <- 0
  y <-as.data.frame(subset(x, pval!= 0))
  return(y)
}
cluml <- function(y, remove.zeroes=FALSE){
  # Rearranging columns in alphabetical order
 
  #y=hmfull
  list0 <- list()
  list1 <- list()
  list0names <- c()
  list1names <- c()
  if (grepl("Fe|Ag", colnames(y)[17])) {
  #if(str_detect(colnames(y[17]),'Ag')==TRUE){
    num <-round(1/3* nrow(y))
    y <- y[order(y$Mantle_Length_mm),]
    y$size=NA
    y$size[1:num]= paste('small','(',min(y$Mantle_Length_mm[1:num]),'mm-',max(y$Mantle_Length_mm[1:num]),'mm',')', sep = "")
    y$size[c(num+1):c(num*2)]=paste('medium','(',min(y$Mantle_Length_mm[c(num+1):c(num*2)]),'mm-',max(y$Mantle_Length_mm[c(num+1):c(num*2)]),'mm',')', sep="")
    y$size[c(num*2+1):c(num*3+1)]=paste('large','(',min(y$Mantle_Length_mm[c(num*2+1):c(num*3+1)]),'mm-',max(y$Mantle_Length_mm[c(num*2+1):c(num*3+1)]),'mm',')',sep = "")
    y %>% relocate(size, .after = dtfl_km)
    #print(y[c(280:600),])
    range <- colnames(y[17:26])#Fe:Pb HM
    icons_hm <- apply(HMiconz, 1, markdownfunchm)
    names(icons_hm) <- names(urlz)
    icons_markdown <- icons_hm
    numrang <- 17:26
    ndf0 <-y[,c(numrang,3, 6, 10, 15, 16, 27)]# streamlining dataframe for efficiency change back to 26 from 30 for heavy metals
  }else{
    num <-round(1/3* nrow(y))
    y <- y[order(y$Mantle_Length_mm),]
    y$size=NA
    y$size[1:c(num+1)]= paste('small','(',min(y$Mantle_Length_mm[1:c(num+1)]),'mm-',max(y$Mantle_Length_mm[1:c(num+1)]),'mm',')', sep = "")
    y$size[c(num+2): c(num*2+5)]=paste('medium','(',min(y$Mantle_Length_mm[c(num+2):c(num*2+5)]),'mm-',max(y$Mantle_Length_mm[c(num+2):c(num*2+5)]),'mm',')', sep = "")
    y$size[c(num*2+6):c(num*3)]=paste('large','(',min(y$Mantle_Length_mm[c(num*2+6):c(num*3)]),'mm-',max(y$Mantle_Length_mm[c(num*2+6):c(num*3)]),'mm',')', sep="")
    y %>% relocate(size, .after = dtfl_km)
    print(y)
    range <- colnames(y[17:30])#Adipic_acid:Tolycaine SM
    icons_sm <- apply(SMiconz, 1, markdownfuncsm)
    names(icons_sm) <- names(urls)
    icons_markdown <- icons_sm
    numrang <- 17:30
    ndf0 <-y[,c(numrang,3, 6, 10, 15, 16, 31)]# streamlining dataframe for efficiency change back to 26 from 30 for heavy metals
  }
    for(m in 1:length(numrang)){
      nzPcomb <- data.frame(matrix(ncol=9, nrow = 0))
      pvalcomb <- data.frame(matrix(ncol=9, nrow = 0))
      hm <-colnames(ndf0)[m] 
      print(hm)
      ndf <-ndf0
      siz <- levels(factor(ndf$size))
      yr <- levels(factor(ndf$Year))
      tis <- levels(factor(ndf$Tissue))
      # if(hm == 'Hg'){
      #   break
      #   break
      # }
      if(remove.zeroes==FALSE){
        dfnz<- ndf %>% pivot_longer(paste(hm), names_to = "SM", values_to = "conc")%>%pivot_longer (Month_of_Capture, names_to = "vars", values_to = "distance")
      }else{
        dfnz<- ndf %>% pivot_longer(paste(hm), names_to = "SM", values_to = "conc")%>%pivot_longer (Month_of_Capture, names_to = "vars", values_to = "distance")%>%subset(conc !=0)
         #print(dfnz[,c(9:21)])
        # print(nrow(dfnz))
      }
      if(nrow(dfnz)!= 0){
        for(i in 1:length(tis)){
          nzT<-dfnz %>% group_by(Year) %>% subset(Tissue == tis[i])
          # print(hm)
          # print(nzT[,c(9:21)])
          if((nrow(nzT)!=0)==TRUE){
            #print(nzT[,c(9:17)])
            nzP <- forcof(nzT)
            # print(tis[i])
            # print(nzP)
            nzPcomb <- rbind(nzPcomb, nzP)
          }
        }
        
        pvals <- finalcof(nzPcomb)
        #print(nzPcomb)
        #print(pvals)
        pvalcomb <- rbind(pvalcomb, pvals)
        print(pvalcomb)
        #Below is for setting y scale----
        nzV <-nzPcomb[,-c(4:7)]
        #print(nzV)
        nzV2 <- nzV %>% dplyr::distinct(Tissue, Year, yul, size, SM)
        #print(hm)
        #print(nzV2)
        nzYcoord <- dfnz%>% left_join(nzV2, by=c('Year'='Year','Tissue'='Tissue','size'='size'))
        #print(nzYcoord)
        repfunc <-function(x){
          if(is.na(x[18])==TRUE){
            x[18]<-x[15]
          }
          return(x)
        }
        nzY1 <-data.frame(t(apply(nzYcoord, 1, repfunc)))
        colnames(nzY1) <- colnames(nzYcoord)
        nzY1$yul <- as.numeric(nzY1$yul)
        nzY1.1<-nzY1 %>% group_by(Tissue) %>% filter(yul==max(yul))
        nzY2<-nzY1.1 %>% dplyr::distinct(Tissue, yul)
        #print(nzY2)
        df_scales <- data.frame(
          Tissue = c("inksac", "liver", "muscle", "stomach"),
          ymin = c(0, 0, 0, 0),
          ymax = c(NA),
          n = c(5, 5, 5, 5))
        
        df_scales %<>% inner_join(nzY2, by= "Tissue") %>%
          mutate(ymax = coalesce(yul)) %>%select(Tissue, ymin, ymax, n)
        
        df_scales <- split(df_scales, df_scales$Tissue)
        scales <- lapply(df_scales, function(x) {
          scale_y_continuous(limits = c(x$ymin, x$ymax), n.breaks = x$n)
        })
        #print(df_scales)
        #For graphs----
        Colors <-setNames( c('#F8766D', '#7CAE00', '#00A9FF'),siz)
        print("This is ICON ")
        print(icons_markdown[m])
        #print(dfnz[,c(12:22)])
          plt <-dfnz %>% ggplot(aes(distance, conc, colour = size, group=size)) +
            scale_colour_manual(values = Colors)+
            geom_smooth(method=lm, se=FALSE)+
            labs(title = paste(icons_markdown[m], '<B>',hm,'::','</B>','MOC',"Vs Conc mg/kg using size",sep =" "),
                 y = "Concentration mg/kg", x = "Month_of_Capture")+
            theme(plot.title = ggtext::element_markdown())+
            facet_grid(Tissue ~ Year, scales = "free", drop = FALSE)+
            ggh4x::facetted_pos_scales(y = scales)+ 
            geom_point(aes(shape = size, color = size), size = 2)+
            {if(nrow(pvalcomb)!=0) geom_text(pvalcomb, mapping=aes(label=paste(rho, pval, sep= ",")),hjust =-0.7, size = 3.5, fontface="italic", position = position_dodge(width = .1), check_overlap = FALSE)}
          list0<-append(list(plt),list0, 0)
          name0 <- paste(hm,"plots", "MOC", sep = "")
          list0names <- append(list0names,name0)
      }
    }
    names(list0)<-list0names
    names(list1)<-list1names
  return(list (metmol=list0))
}
clumlm<- cluml (hmfull, remove.zeroes = FALSE)

for(i in 1:length(clumlm$metmol)){
  # if(names(clumlm$metmol)[i]== "HgplotsMOC"){
  #   next
  # }else{
  png(paste0(names(clumlm$metmol)[i], ".png"), width = 1400, height = 800)
  plot(clumlm$metmol[[i]],title = names(clumlm$metmol)[i])
  dev.off()
  #}
}

# if(names(clumlm$metmol)[i]== "HMplotsMOC"|names(clumlm$metmol)[i]== "Ethylene_glycolplotsMOC"){