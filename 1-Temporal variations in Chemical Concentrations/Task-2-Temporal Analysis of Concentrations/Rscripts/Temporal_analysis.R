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
library(openxlsx)
library(readxl)
library(stringr)
library(stringi)
library(tidyverse)
library(purrr)
library(patchwork)
library(reshape)
library(grid)
library(ggrepel)
library(ggpubr)


#Loading libraries----
#library(ARTool) # used for ART tool in the analysis
#library(readr) 
library(ggplot2) # For creating visulaizations
library(Rmisc) # For miscellaneous statistical fucntions e.g summarySE()
library("gridExtra") # For arranging multiple plots e.g grid.arrange()
#library(multcompView) # For post-hoc comparisons specifically for non-parametric tests e.g Dunns test
library(dplyr) # For data manipulation e.g mutate(), filter(), and group_by()
#library(mvnormtest) # For multivariate normality tests.
library(rcompanion) #For creating summary statistics in non parametric tests.
library(pgirmess) #For non-parametric statistics like Kruskall Wallis test
#library(FSA) # For fisheries stock analysis
#library(car) # For advanced regression models
#library(factoextra) # For extracting and visualizing results from PCA.
#library(corrplot) # For correlation plots
#library(vegan) # For community ecology analysis 
library(DescTools) # For descriptive statistics
#library(corrplot) # for correlation plots
library(dunn.test) # For Dunn's test (non-parametric post-hoc test).
library(openxlsx) # For writing and reading Excel files.
library(readxl) #For reading Excel files.
library(stringr)
#library(stringi) #For string manipulation.
library(tidyverse) # For a collection of R packages used for data manipulation and visualization.
library(purrr) # For functional programming tools.
library(patchwork) # For combining multiple ggplot2 plots.
#library(reshape) # For data reshaping.
#library(grid) # For graphical functions.
library(ggrepel) # For text labels in ggplot.
#library(ggpubr) # For ggplot2-based publication-ready plots.


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
hm1<- read.csv("Final_Results_From_Task 1/Final_HMresults_mgkg.csv", header= TRUE)
hm1.1 = sort(colnames(hm1[,c(16:25)]))
hm1.2 <-hm1 %>% relocate(1:15, hm1.1)
hm1.2[,4] <- paste0(hm1.2[,4], ".")
hmfull <- as.data.frame(t(apply(hm1.2, MARGIN = 1, dcc)))
hmfull$Area <- gsub("\\.", "",hmfull$Area)
hmfull[,c(16:25)] <- lapply(hmfull[,c(16:25)], gsub, pattern = ".*BLOQ.*", replacement = 0)
hmfull[,c(16:25)] <- lapply(hmfull[,c(16:25)], gsub, pattern = ".*BB.*", replacement = 0)
hmfull[,c(16:25)] <- lapply(hmfull[,c(16:25)], gsub, pattern = "^0$", replacement = 0)
hmfull[,c(16:25)]  <- lapply(hmfull[,c(16:25)] , gsub, pattern = "N/A", replacement = 0)
hmfull[,c(8,9,12,13:14, 16:25)] =as.numeric(unlist(hmfull[,c(8,9,12,13:14, 16:25)]))
#hmfull[,10]<- cut(hmfull[,10], breaks = 3, labels =c(2,3,4))
#hmfull[,7]<- cut(hmfull[,7], breaks = 2, labels =c(0,1))
# hmfull[, 12] <- as.numeric(gsub(2811.1, 281.1, hmfull[, 12]))
# hmfull[,13]<- as.character(hmfull[,13])
hmfull[,3]<- as.character(hmfull[,3])

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

sm2<- read.csv("Final_Results_From_Task 1/Final_SMresults_mgkg.csv", header= TRUE)
sm2.1 <- sm2 %>% relocate(Area, .after = ID_num)
sm2.1[,4] <- paste0(sm2.1[,4], ".")
#colnames(sm2)[4]<- "Area"
sm <- as.data.frame(t(apply(sm2.1, MARGIN = 1, dkc)))
sm$ID_num <- gsub("\\.", "",sm$ID_num)
sm[,c(16:19)] <- lapply(sm[,c(16:19)], gsub, pattern = ".*BLOQ.*", replacement = 0)
sm[,c(16:19)] <- lapply(sm[,c(16:19)], gsub, pattern = "N/A", replacement = 0)
sm[,c(16:19)] <- lapply(sm[,c(16:19)], gsub, pattern = "^0$", replacement = 0)
sm[,c(7,8,13:14, 16:19)] =as.numeric(unlist(sm[,c(7,8,13:14, 16:19)]))
# sm[, 12] <- as.numeric(gsub(2811.1, 281.1, sm[, 12]))
# sm[,10]<- cut(sm[,10], breaks = 3, labels =c(2,3,4))
sm[,6]<- tolower(sm[,6])
# sm[,7]<- as.character(sm[,7])
# sm[,3]<- as.character(sm[,3])
# sm[,13]<- as.character(sm[,13])
# sm[,11]<- cut(sm[,11], breaks = 4, labels =c(150, 250, 350, 450))
# sm[,15]<- cut(sm[,15], breaks = 4, labels =c(200, 400, 600, 800))
# sm[,16]<- cut(sm[,16], breaks = 5, labels =c(100, 130, 140, 150, 190))
smfull <- sm 


#functions to run temporal variation
resprotvnzfl<- function(y, remove.zeroes=FALSE){ #y= raw data after manipulation e.g res2019 or res2020 etc nad z = list summarized HM or SM dataframes e.g sum19, sum20 etc
  list0 <- list()
  list1 <- list()
  list2 <- list()
  list3 <- list()
  pvalstv <- list()
  list0names <- c()
  list1names <- c()
  list2names <- c()
  list3names <- c()
  pvalsnames <- c()
  
  if(str_detect(colnames(y)[16],'Ag')==TRUE){
    range <- colnames(y)[16:25] #colnames with heavymetals  #Adipic_acid:Tolycaine SM
    numrang <- 16:25 # range of heavy metals
    filename <- "HMicons//"
    #recommended levels for heavy metals in mg/kg:
    rl <- data.frame(metmol=c("Ag","Cd","Co","Cu","Fe","Hg","Ni","Pb","Tl","Zn"), 
                     rl=c(0.01,2,0.0016,30,100,0.1,0.05,8,0.45,30), rlu=c(rep(NA, 10)), 
                     levs=c('Grasso et al. 2021: 0.01mg/kg','FAO/WHO: <0.05-2mg/kg','EFSA: 0.0016mg/kg','ANVISA: 30mg/kg', 'FAO/WHO: 100mg/kg','Brodziak-Dopierała et al. 2023: 0.1mg/kg','FAO/WHO: <0.05-2mg/kg','FAO/WHO: <0.5-8mg/kg','Makridis and Amberger, 1996; LaCoste et al. 2001:\n <0.45-2.28mg/kg (permissible range for animal feed)','FAO/WHO: <30-100mg/kg'),rfd=c(0.005, 0.01, 0.03, 0.04, 0.7, 0.1, 0.003, 0.04,0.00001, 0.3))
  }else{
    range <- colnames(y)[16:19] #Fe:Pb HM
    numrang <- 16:19
    filename <- "SMicons//"
    rl <- data.frame(metmol=c("Adipic_acid","Caprolactam","Chlorpyrifos","Ibuprofen"), rl=c(470, 50,0.01,40), rlu=c(NA, NA,NA,NA), levs=c('EPA: 470mg/kg/day','EPA: 50mg/kg/day', 'FAO/WHO: 0.01mg/kg/day','The Mayo Clinic: 40mg/kg/day'))
    # rl <- data.frame(metmol=c("Adipic_acid","Aminobenzoic_acid","Caprolactam","Chlorpyrifos","Diaminohexane","Estradiol","Ethylene_glycol","Ibuprofen","Metolachlor","Nortestosterone","Sulpiride","Terephthalic_acid","Toluidine","Tolycaine" ), rl=c(470, 200, 50,0.01,380,0.42,1330,40,3.5,0.4,2,280,231,4.5), rlu=c(NA, NA, NA, NA,1127,NA,NA,NA,NA,NA,10,NA,NA,NA), levs=c('EPA: 470mg/kg/day','SCCS: 200mg/kg/day','EPA: 50mg/kg/day', 'FAO/WHO: 0.01mg/kg/day','OECD SIDS: 380mg/kg/day','Li et al. 2022: 0.42mg/kg/day', 'Robinson and McCoy 1989: 1330mg/kg/day','The Mayo Clinic: 40mg/kg/day', 'WHO: 3.5mg/kg/day', 'Tamaki et al. 2003: 0.4mg/kg/day','Roessner et al. 2013: 2mg/kg/day', 'OECD SIDS: 280mg/kg/day', 'EPA: 231mg/kg/day', 'University of Iowa Health Care: \n 4.5mg/kg/day'))
  }
  for(h in 1:length(numrang)){ 
    if(remove.zeroes==FALSE){
      dfnz<- y[,c(numrang,3,5,6)]  %>%
        pivot_longer(all_of(range), names_to = "sm", values_to = "conc") %>% group_by( Tissue, sm)%>%mutate_at(vars(sm, Year, Tissue), factor)
    }else{
      dfnz<- y[,c(numrang,3,5,6)]  %>%
        pivot_longer(all_of(range), names_to = "sm", values_to = "conc") %>% group_by( Tissue, sm)%>%mutate_at(vars(sm, Year, Tissue), factor)%>%subset(conc!=0)
    }
    tis <- c('liver', 'muscle', 'stomach', 'inksac')
    yr <- levels(factor(y[,3]))
    Yr <- c('2019', '2020', '2021')
    hm <-colnames(y)[h+15]
    print(hm)
    rml<-rl %>% subset(metmol == hm)
    fmb <- data.frame(matrix(ncol=2, nrow = 0))
    colnames(fmb) <- c('Year', 'results')
    
    # print(paste('min conc', 'is', min(dfnz$conc)))
    #Just for information----
    for(a in 1:length(Yr)) {
      nzYa<-as.data.frame(dfnz %>% mutate_at(vars(conc), as.numeric) %>% subset(Year == Yr[a]))
        nzMa<-as.data.frame(nzYa %>% mutate_at(vars(conc), as.numeric) %>% subset(sm == hm))
        print(nzMa)
        #nzM$conc[is.na(nzM$conc)] <- 0
      print(paste('min conc', 'is', min(dfnz$conc)))
      print(paste('max conc', 'is', max(dfnz$conc)))
      fmz <- data.frame(matrix(ncol=2, nrow = 0))
      colnames(fmz) <- c('Year', 'results')
      if(nrow(nzYa)>3 & all(nzYa[-1,'conc'] == nzYa[1,'conc'])==FALSE){
        swm <-(shapiro.test(nzYa$conc)$p.value)
        fmz[1,1] <- yr[a]
        if(swm<0.05){
          fmz[1,2] <- 'fail' 
        }else{
          fmz[1,2] <- 'pass' 
        }
        fmb <- rbind(fmb, fmz)
      }else{
        fmz[1,1] <- yr[a]
        fmz[1,2] <- '<3 Obs' 
        fmb <- rbind(fmb, fmz)
      }
      #print(fmb)
      if(nrow(fmb)!=0 & 'fail' %in% fmb[,'results']==TRUE){
        kwfmb<- kruskal.test(conc ~ Year, data = dfnz)
        #print(paste('The pvalue is',kwfmb[["p.value"]],'with chisq=',signif(kwfmb$statistic[[1]],3)))
        if(kwfmb[["p.value"]]<0.05){
          dtb <- dunnTest(conc ~ Year, data=dfnz, method="bh")
          #print(dt)
          dtbres <- dtb$res 
          #print(dtbres)
        }else{
          print('sorry') 
        }
      }else if (nrow(fmb)!=0 & 'pass' %in% fmb[,'results']==TRUE & 'fail' %in% fmb[,'results']==FALSE) {
        aov.fmb <- aov(conc ~ Year, data = dfnz)
        #print(paste('The pvalue is',signif((anova(aov.fmb)[1,'Pr(>F)']),3),'with f=',signif(anova(aov.fmb)[1,'F value'],3)))
      }else{
        #print(paste('No observations'))
      }
    }
    #nzY<-dfnz %>% group_by(sm) %>% subset(Year == yr[s])
    #ar <- levels(factor(as.character(nzY$Area)))z
    nzM<-as.data.frame(dfnz %>% mutate_at(vars(conc), as.numeric) %>% subset(sm == hm))
    #nzM$conc[is.na(nzM$conc)] <- 0
    print(nzM)
    if((nrow(nzM)==0|max(nzM$conc)==0)==TRUE){
      sumnz <- nzM %>%
        group_by(Tissue, Year) %>%
        summarise(N = n(),
                  mean_= 0,
                  medi = 0,
                  mx  = 0,
                  mn = 0,
                  sd = 0,
                  se =0)
      #next
    }else{
      sumnz <- nzM %>%
        group_by(Tissue, Year) %>%
        summarise(N = n(),
                  mean_= mean(conc, na.rm = T),
                  medi = median(conc, na.rm = T),
                  mx  = max(conc, na.rm = T),
                  mn = min(conc, na.rm = T),
                  sd = sd(conc),
                  se =sd(conc)/sqrt(n()))
      
      #colnames(sumnz)[3]<- paste(hm)
      #print(sumnz)
      # sumnz<- summarySE(nzM, measure= "conc", groupvars =c("Tissue","Year"),.drop = FALSE)
      compar <- data.frame(matrix(ncol = 4, nrow = 0))
      colnames(compar) <- c("Comparison" ,"tstat","P.unadj","P.adj")
      cldtab <- data.frame(matrix(ncol = 4, nrow = 0))
      temp <-setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("Tissue", "pvalue",'teststat','degfree'))
      tempno <-setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("Tissue", "pvalue",'teststat','degfree'))
      tempy <-setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("Tissue", "pvalue",'teststat','degfree'))
      pvals <-setNames(data.frame(matrix(ncol = 4, nrow = 0)), c("Tissue", "pvalue",'teststat','degfree'))
      missing1 <- data.frame(matrix(ncol=9, nrow = 0))
      colnames(missing1) <- colnames(sumnz)
    }
    
    for(i in 1:length(tis)) {
      if((tis[i]%in%nzM$Tissue)==FALSE){
        #i<-i+1
        missing <- data.frame(matrix(ncol=9, nrow = 0))
        colnames(missing) <- colnames(sumnz)
        cldtnan <- data.frame(matrix(ncol = 4, nrow = 5))
        cnames <-c("Tissue","Year","Letter","MonoLetter")
        colnames(cldtnan) <-cnames
        cldtnan[c(1:3),1]<- tis[i]#change with year
        cldtnan[c(1:3),2]<- yr #lev19#change with year
        cldtnan[c(1:3),c(3:4)]<- paste(" ")
        cldtab <- rbind(cldtab,cldtnan)
        missing[1,1] <- tis[i]
        missing[1,2] <- paste('all')
        missing[1,3] <- 0
        missing[1,4] <- 0
        missing[1,5] <- 0
        missing[1,6] <- 0
        missing[1,7] <- 0
        missing[1,8] <- 0
        missing[1,9] <- 0
        missing1 <- rbind(missing1, missing)
      }else{
        tiss <- as.data.frame(filter(nzM, Tissue == tis[i]))
        #print(tiss)
        inter <- interaction(tiss[,3], tiss[,1])
        fm2 <- data.frame(matrix(ncol=4, nrow = 0))
        colnames(fm2) <- c('Year', 'tissue' , 'metmol', 'results')
        if(nrow(tiss)==0){
          fm[,1] <-unique(tiss$Year) #yr[k]
          fm[,2] <- unique(tiss$Tissue)#tis[i]
          fm[,3] <- hm
          fm[,4] <- '<3 Obs' 
          fm2 <- rbind(fm2, fm)
        }else{
          for(k in 1:length(yr)) {
            nzY<-as.data.frame(filter(tiss, Year==yr[k]))
            missing <- data.frame(matrix(ncol=9, nrow = 0))
            colnames(missing) <- colnames(sumnz)
            if(nrow(nzY)==0){
              missing[1,1] <- tis[i]
              missing[1,2] <- yr[k]
              missing[1,3] <- 0
              missing[1,4] <- 0
              missing[1,5] <- 0
              missing[1,6] <- 0
              missing[1,7] <- 0
              missing[1,8] <- 0
              missing[1,9] <- 0
              missing1 <- rbind(missing1, missing)
            }
            #print(nzY)
            fm <- data.frame(matrix(ncol=4, nrow = 0))
            colnames(fm) <- c('Year', 'tissue' , 'metmol', 'results')
            # print(nzM)
            #tis <- levels(factor(nzM[,'Tissue']))
            #interact <- c()
            nzY0 <- data.frame(matrix(ncol=8, nrow = 0))
            if(nrow(nzY)>3 & all(nzY[-1,'conc'] == nzY[1,'conc'])==FALSE){
              swm <-(shapiro.test(nzY$conc)$p.value)
              fm[1,1] <- yr[k]
              fm[1,2] <- tis[i]
              fm[1,3] <- hm
              if(swm<0.05){
                fm[1,4] <- 'fail' 
              }else{
                fm[1,4] <- 'pass' 
              }
              fm2 <- rbind(fm2, fm)
            }else{
              fm[1,1] <- yr[k]
              fm[1,2] <- tis[i]
              fm[1,3] <- hm
              fm[1,4] <- '<3 Obs' 
              fm2 <- rbind(fm2, fm)
            }
          }
        }
        #KW----
        if((nrow(fm2)!=0 & 'fail' %in% fm2[,'results']==TRUE & nrow(tiss)==1|length(levels(factor((tiss$Year))))==1)==FALSE){
          kw<- kruskal.test(conc ~ inter, data = tiss)
          #print(kw)
          temp[1,1] <- tis[i]
          temp[1,2] <- kw[["p.value"]]
          temp[1,3] <- paste('chisq=', signif(kw$statistic[[1]],3), sep = "")
          temp[1,4] <- kw$parameter[[1]]
          pvals <- rbind(pvals,temp)
          if (is.nan(kw[["p.value"]])){
            if((tis[i]=='inksac')==TRUE){
              cldtnan <- data.frame(matrix(ncol = 4, nrow = 4))
              cnames <-c("Tissue","Year","Letter","MonoLetter")
              colnames(cldtnan) <-cnames
              yrI <- levels(factor(tiss[,1]))
              cldtnan[c(1:2),1]<- tis[i]#change with year
              cldtnan[c(1:2),2]<- yrI #lev19#change with year
              cldtnan[c(1:2),c(3:4)]<- paste(" ")
            }else{
              cldtnan <- data.frame(matrix(ncol = 4, nrow = 5))
              cnames <-c("Tissue","Year","Letter","MonoLetter")
              colnames(cldtnan) <-cnames
              cldtnan[c(1:3),1]<- tis[i]#change with year
              cldtnan[c(1:3),2]<- yr #lev19#change with year
              cldtnan[c(1:3),c(3:4)]<- paste(" ")
            }
            cldtab <- rbind(cldtab,cldtnan)
            comparnan <- data.frame(matrix(ncol = 4, nrow = 30))
            comnames <-c("Comparison" ,"tstat","P.unadj","P.adj")
            colnames(comparnan) <-comnames
            comparnan[c(1:30),1]<- tis[i]#change with year
            comparnan[c(1:30),c(2:4)]<- 0
            compar <- rbind(compar,comparnan)
            next
            }else { 
              if (length(levels(factor((tiss$Year)))) < 3){
              dtt<-dunn.test (tiss$conc, inter, method=p.adjustment.methods, kw=TRUE)
              #print(dtt)
              dtresI <- data.frame(matrix(ncol = 4, nrow=1))
              comnames <-c("Comparison" ,"tstat","P.unadj","P.adj")
              colnames(dtresI) <-comnames
              dtresI[,1] <- dtt$comparisons
              dtresI[,2] <- dtt$Z
              dtresI[,3] <- dtt$P
              dtresI[,4] <- dtt$P.adjusted
              #print(dtresI)
              compar <- rbind(compar,dtresI)
              cldtt <- cldList(P.adj ~ Comparison, data = dtresI,threshold = 0.05, remove.zero = FALSE)
              #print(cldtt)
              #print(cldtt)
              cldtt <-cldtt %>% separate(Group, c("Tissue", "Year"))
              if(all(cldtt[-1,3]== cldtt[1,3])){
                cldtt[,c(3:4)] <- paste(" ")
                cldtab <- rbind(cldtab,cldtt)
                #print(cldtt)
              }else{
                cldtab <- rbind(cldtab,cldtt)
              }
            }else{
              #dtt----
              colnames(compar) <- c("Comparison" ,"tstat","P.unadj","P.adj")
              dt <- dunnTest(conc ~ inter,data=tiss, method="bh")
              #print(dt)
              dtres <- dt$res
              colnames(dtres)[2]<-"tstat"
              #print(dtres)
              compar <- rbind(compar,dtres)
              #print(compar)
              cldt <- cldList(P.adj ~ Comparison, data = dtres,threshold = 0.05, remove.zero = FALSE)
              #print(cldt)
              #cldt <-cldt %>% separate(Group, c("Tissue", "Area"))
              cldt <-cldt %>% separate(Group, c("Tissue", "Year"))
              if(all(cldt[-1,3]== cldt[1,3])){
                cldt[,c(3:4)] <- paste(" ")
                cldtab <- rbind(cldtab,cldt)
                #print(cldtt)
              }else{
                cldtab <- rbind(cldtab,cldt)
              }
              
            }
            }
          #aov----
        }else if ((nrow(fm2)!=0 & 'pass' %in% fm2[,'results']==TRUE & 'fail' %in% fm2[,'results']==FALSE & nrow(tiss)==1|length(levels(factor((tiss$Year))))==1)==FALSE){
          print(paste(hm, tis[i], 'running Tukey'))
          aov.model <- aov(conc ~ inter, data = tiss)
          anova_pval <- signif((anova(aov.model)[1,'Pr(>F)']),3)
          temp[1,1] <- tis[i]
          temp[1,2] <- anova_pval
          temp[1,3] <- paste('f=', signif(anova(aov.model)[1,'F value'],3), sep = "")
          temp[1,3] <- anova(aov.model)[1,'Df']
          pvals <- rbind(pvals,temp)
          #print(pvals)
          if (is.nan(anova_pval)){
            if((tis[i]=='inksac')==TRUE){
              cldtnan <- data.frame(matrix(ncol = 4, nrow = 4))
              cnames <-c("Tissue","Year","Letter","MonoLetter")
              colnames(cldtnan) <-cnames
              yrI <- levels(factor(tiss[,1]))
              cldtnan[c(1:2),1]<- tis[i]#change with year
              cldtnan[c(1:2),2]<- yrI #lev19#change with year
              cldtnan[c(1:2),c(3:4)]<- paste(" ")
            }else{
              cldtnan <- data.frame(matrix(ncol = 4, nrow = 5))
              cnames <-c("Tissue","Year","Letter","MonoLetter")
              colnames(cldtnan) <-cnames
              cldtnan[c(1:3),1]<- tis[i]#change with year
              cldtnan[c(1:3),2]<- yr #lev19#change with year
              cldtnan[c(1:3),c(3:4)]<- paste(" ")
            }
            cldtab <- rbind(cldtab,cldtnan)
            comparnan <- data.frame(matrix(ncol = 4, nrow = 30))
            comnames <-c("Comparison" ,"tstat","P.unadj","P.adj")
            colnames(comparnan) <-comnames
            comparnan[c(1:30),1]<- tis[i]#change with year
            comparnan[c(1:30),c(2:4)]<- paste(" ")
            compar <- rbind(compar,comparnan)
            next
        }else { 
          if (length(levels(factor((tiss$Year)))) < 3){
            dtt<-TukeyHSD(aov.model)
            #print(dtt)
            dtresI <- data.frame(matrix(ncol = 4, nrow=1))
            comnames <-c("Comparison" ,"tstat","P.unadj","P.adj")
            colnames(dtresI) <-comnames
            dtresI[,1] <- dtt$Year
            dtresI[,2] <- dtt$Z
            dtresI[,3] <- dtt$P.unadj
            dtresI[,4] <- dtt$P.adj
            compar <- rbind(compar,dtresI)
            cldtt <- cldList(P.adj ~ Comparison, data = dtresI,threshold = 0.05, remove.zero = FALSE)
            cldtt <-cldtt %>% separate(Group, c("Tissue", "Year"))
            if(all(cldtt[-1,3]== cldtt[1,3])){
              cldtt[,c(3:4)] <- paste(" ")
              cldtab <- rbind(cldtab,cldtt)
              #print(cldtt)
            }else{
              cldtab <- rbind(cldtab,cldtt)
            }
            
          }else{
            # colnames(compar) <- c("Comparison" ,"tstat","P.unadj","P.adj")
            # dt <- dunnTest(conc ~ inter,data=tiss,method="bonferroni")
            # dtres <- dt$res
            # compar <- rbind(compar,dtres)
            # cldt <- cldList(P.adj ~ Comparison, data = dtres,threshold = 0.05, remove.zero = FALSE)
            # #cldt <-cldt %>% separate(Group, c("Tissue", "Area"))
            # cldt <-cldt %>% separate(Group, c("Tissue", "Year"))
            # cldtab <- rbind(cldtab,cldt)
          }
        }
        }else if ((nrow(tiss)==1|length(levels(factor((tiss$Year))))==1)==TRUE){
          tempno[1,1] <- tis[i]
          tempno[1,2] <- NA
          tempno[1,3] <- NA
          tempno[1,4] <- NA
          pvals <- rbind(pvals,tempno)
          if((tis[i]=='inksac')==TRUE){
            cldtnan <- data.frame(matrix(ncol = 4, nrow = 4))
            cnames <-c("Tissue","Year","Letter","MonoLetter")
            colnames(cldtnan) <-cnames
            yrI <- levels(factor(tiss[,1]))
            cldtnan[c(1:2),1]<- tis[i]#change with year
            cldtnan[c(1:2),2]<- yrI #lev19#change with year
            cldtnan[c(1:2),c(3:4)]<- paste(" ")
          }else{
            cldtnan <- data.frame(matrix(ncol = 4, nrow = 5))
            cnames <-c("Tissue","Year","Letter","MonoLetter")
            colnames(cldtnan) <-cnames
            cldtnan[c(1:3),1]<- tis[i]#change with year
            cldtnan[c(1:3),2]<- yr #lev19#change with year
            cldtnan[c(1:3),c(3:4)]<- paste(" ")
          }
          cldtab <- rbind(cldtab,cldtnan)
          comparnan <- data.frame(matrix(ncol = 4, nrow = 30))
          comnames <-c("Comparison" ,"tstat","P.unadj","P.adj")
          colnames(comparnan) <-comnames
          comparnan[c(1:30),1]<- tis[i]#change with year
          comparnan[c(1:30),c(2:4)]<- paste(" ")
          compar <- rbind(compar,comparnan)
          next
        }else{
          tempno[1,1] <- tis[i]
          tempno[1,2] <- NA
          tempno[1,3] <- NA
          tempno[1,4] <- NA
          pvals <- rbind(pvals,tempno)
          #print(pvals)
          if((tis[i]=='inksac')==TRUE){
            cldtnan <- data.frame(matrix(ncol = 4, nrow = 4))
            cnames <-c("Tissue","Year","Letter","MonoLetter")
            colnames(cldtnan) <-cnames
            yrI <- levels(factor(tiss[,1]))
            cldtnan[c(1:2),1]<- tis[i]#change with year
            cldtnan[c(1:2),2]<- yrI #lev19#change with year
            cldtnan[c(1:2),c(3:4)]<- paste(" ")
          }else{
            cldtnan <- data.frame(matrix(ncol = 4, nrow = 5))
            cnames <-c("Tissue","Year","Letter","MonoLetter")
            colnames(cldtnan) <-cnames
            cldtnan[c(1:3),1]<- tis[i]#change with year
            cldtnan[c(1:3),2]<- yr #lev19#change with year
            cldtnan[c(1:3),c(3:4)]<- paste(" ")
          }
          cldtab <- rbind(cldtab,cldtnan)
          comparnan <- data.frame(matrix(ncol = 4, nrow = 30))
          comnames <-c("Comparison" ,"tstat","P.unadj","P.adj")
          colnames(comparnan) <-comnames
          comparnan[c(1:30),1]<- tis[i]#change with year
          comparnan[c(1:30),c(2:4)]<- paste(" ")
          compar <- rbind(compar,comparnan)
          next 
        }

      }
    }
    sumnz <- rbind(sumnz, missing1)
    #For icons----
    if(file.exists(paste0(filename,hm,".png"))==TRUE){
      icon <- file.exists(paste0(filename,hm,".png"))
      icons = png::readPNG(paste0(filename,hm,".png")) %>%
        rasterGrob(x= 0.95, y=1.03,interpolate = TRUE, height = 0.11, width = 0.06)}else{
          icon <- FALSE
        }
    #_________________________________0
    if(file.exists(paste0(filename,hm,"1",".png"))==TRUE){
      icon1 <- file.exists(paste0(filename,hm,"1",".png"))
      iconz = png::readPNG(paste0(filename,hm,"1",".png")) %>%
        rasterGrob(x= 0.88, y=1.03,interpolate = TRUE, height = 0.11, width = 0.06)}else{
          icon1 <- FALSE
        } 
    #__________________________________________1
    if(file.exists(paste0(filename,hm,"2",".png"))==TRUE){
      icon2 <- file.exists(paste0(filename,hm,"2",".png"))
      iconsz = png::readPNG(paste0(filename,hm,"2",".png")) %>%
        rasterGrob(x= 0.80, y=1.03,interpolate = TRUE, height = 0.11, width = 0.06)}else{
          icon2 <- FALSE
        } 
    print(hm)
    pvalfl <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("Tissue", "pvalue"))
    pvalfl <- rbind(pvalfl,pvals)
    pvalstv<-append(list(pvalfl),pvalstv, 0)
    pvaln <-paste(hm,"tmpvar", sep = "")
    pvalsnames <- append(pvalsnames,pvaln)
    cldtabt <- data.frame(matrix(ncol = 4, nrow = 0))
    cldtabt <- rbind(cldtabt,cldtab)
    list1<-append(list(data.frame(cldtabt)),list1, 0)
    name1 <-paste(hm,'restv', sep = "")
    list1names <-append(list1names, name1)
    #______________________________________________________________________
    list0<-append( list(data.frame(compar)),list0, 0)
    name0<- paste(hm,'restv', sep = "")
    list0names <- append(list0names,name0)
    #______________________________________________________________________
    #res1 <- res%>% left_join(cldtab, by=c('Area'='Area','Tissue'='Tissue'))
    #lev19 <- c('43', '04', '01', '88', '87')
    res1 <- as.data.frame(sumnz%>% left_join(cldtabt, by=c('Year'='Year','Tissue'='Tissue')))
    res1[,2] <- factor(res1[,2], levels=yr)#change with year
    res1[,c(10:11)][is.na(res1[,c(10:11)])] <-paste(" ")
    colnames(res1)[4] <-hm
    list2<-append(list(data.frame(res1)),list2, 0)
    name2 <- paste(hm,"restv", sep = "")
    #names(list2)[[h]]<-name2
    list2names <- append(list2names,name2)
    #print(res1)
    #____________________________________________________________________
    if (all(res1[-1, 10] == res1[1, 10])==FALSE){
      #print(hm)
      tiz <- c('liver', 'stomach', 'muscle', 'inksac')
      Colors <-setNames( c('red', 'green','blue'),yr)
      plot <- print(ggplot(res1, aes(factor(x=Tissue, levels = tiz), y=!! rlang::sym(paste0(hm)), fill=factor(Year, levels=c('2019','2020','2021')))) +
                      geom_bar(stat='identity', color="black", position=position_dodge()) +scale_fill_manual(values=Colors) +
                      geom_errorbar(aes(ymin=!! rlang::sym(paste0(hm)),ymax=!! rlang::sym(paste0(hm))+se),width=.1,position=position_dodge(.8))+
                      labs(title = paste0(hm))+
                      {if(icon==TRUE)annotation_custom(icons)}+
                      {if(icon1==TRUE)annotation_custom(iconz)}+
                      {if(icon2==TRUE)annotation_custom(iconsz)}+
                      coord_cartesian(clip = 'off')+
                      theme(plot.title = element_text(hjust = 0.5))+
                      labs(y = "Concentration mg/kg", x = "Tissue")+
                      labs(fill = "Years") + geom_text(aes(label=N), position = position_dodge(1), size = 3,vjust=1.5, color='#993300')+
                      geom_text(aes(label=Letter), position = position_dodge(1), size = 5,vjust=-0.2, hjust=-0.005, colour = "black")+ {if((rml[,2])>max(res1[,paste(hm)]))annotate('text', x=max(as.numeric(as.factor(unique(res1$Tissue))))*.78, y=max(res1[,paste(hm)]*1.40)*.87, label= rml[,4],fontface='bold', size=2.2, color="red") else geom_hline(yintercept=rml[,2], linetype="dashed", color = "red")})
                      # {if((is.na(rml[,2])==FALSE))geom_hline(yintercept=rml[,2], linetype="dashed", color = "red")}+
                      # {if((is.na(rml[,3])==FALSE))geom_hline(yintercept=rml[,3], linetype="dashed", color = "red")})
      list3<-append(list(plot),list3, 0)
      name3 <-paste(hm,"plottv", sep = "")
      #names(list3)[[h]]<-name3
      list3names <- append(list3names,name3)
    } else {
      #print(hm)
      tiz <- c('liver', 'stomach', 'muscle', 'inksac')
      Colors <-setNames( c('red', 'green','blue'),yr)
      plotnl <- print(ggplot(res1, aes(factor(x=Tissue, levels = tiz), y=!! rlang::sym(paste0(hm)), fill=factor(Year, levels=c('2019', '2020', '2021')))) +
                        geom_bar(stat='identity', color="black", position=position_dodge()) +scale_fill_manual(values=Colors) +
                        geom_errorbar(aes(ymin=!! rlang::sym(paste0(hm)), ymax=!! rlang::sym(paste0(hm))+se), width=.1, position=position_dodge(.8))+
                        labs(title = paste0(hm))+
                        {if(icon==TRUE)annotation_custom(icons)}+
                        {if(icon1==TRUE)annotation_custom(iconz)}+
                        {if(icon2==TRUE)annotation_custom(iconsz)}+
                        coord_cartesian(clip = 'off')+
                        theme(plot.title = element_text(hjust = 0.5))+
                        labs(y = "Concentration mg/kg", x = "Tissue")+
                        labs(fill = "Years")+geom_text(aes(label=N), position = position_dodge(1),size = 3,vjust=2, color='#993300')+
                        {if((rml[,2])>max(res1[,paste(hm)]))annotate('text', x=max(as.numeric(as.factor(unique(res1$Tissue))))*.78, y=max(res1[,paste(hm)]*1.40)*.87, label= rml[,4],fontface='bold', size=2.2, color="red") else geom_hline(yintercept=rml[,2], linetype="dashed", color = "red")})
                        # {if((is.na(rml[,2])==FALSE))geom_hline(yintercept=rml[,2], linetype="dashed", color = "red")}+{if((is.na(rml[,3])==FALSE))geom_hline(yintercept=rml[,3], linetype="dashed", color = "red")})
      # + geom_text(aes(label=Letter), position = position_dodge(1), size = 3,vjust=-0.2, hjust=-0.005, colour = "black")
      list3<-append(list(plotnl),list3, 0)
      name3 <-paste(hm,"plottv", sep = "")
      list3names <- append(list3names,name3)
      
    }
    names(list0)<-list0names
    names(list1)<-list1names
    names(list2)<-list2names
    names(list3)<-list3names
    names(pvalstv)<-pvalsnames
  }
  
  return(list (comparisons=list0, cldt=list1, summaryHM=list2, plots=list3, pvals=pvalstv))
}
tv <- resprotvnzfl(smfull, remove.zeroes = TRUE)    
do.call("grid.arrange", c(tv$plots[c(1,2,3,4,5,7,10)], ncol=3)) #HM
do.call("grid.arrange", c(tv$plots, ncol=5)) #HM
do.call("grid.arrange", c(tv$plots, ncol=4))  #SM



do.call("grid.arrange", c(tv$plots[c(1,2,3,8)], ncol=3)) #SM
#saving data from dataframes in list----
sink("tv.txt")
print(tv[c(1,2,3,5)])
sink()
