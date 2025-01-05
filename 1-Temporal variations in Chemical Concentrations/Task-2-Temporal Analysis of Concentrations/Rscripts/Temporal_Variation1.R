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

#FOR HEAVY METAL (HM) DATASET----

# FUNCTIONS TO HELP MODIFY AREA NUMBERS IN HEAVY METALS DATASET FOR FURTHER ANALYSIS
dcc <- function(x){#padding 0 to first two digits
  if (str_detect(substring(x[4],1,2),"\\.") == TRUE){ 
    sk=print(paste('0',substring(x[4],1,)))
    sk<- gsub(" ", "",  sk)
    x[4] <- gsub(substring(x[4],1,), sk,  x[4])
  }
  return(x)
}
#UPLOADING AND PREPARING HEAVY METALS DATASET FOR FURTHER ANALYSIS
hm1<- read.csv("Final_Results_From_Task 1/Final_HMresults_mgkg.csv", header= TRUE)
hm1.1 = sort(colnames(hm1[,c(16:25)]))# put column names in alphabetical order
hm1.2 <-hm1 %>% relocate(1:15, hm1.1)# add back to dataset
hm1.2[,4] <- paste0(hm1.2[,4], ".") # "." Needed to help modify area numbers using dcc function
hmfull <- as.data.frame(t(apply(hm1.2, MARGIN = 1, dcc))) #activating dcc function to modify area numbers
hmfull$ID_num <- gsub("\\.", "",hmfull$ID_num) # removing "."
hmfull[,c(16:25)] <- lapply(hmfull[,c(16:25)], gsub, pattern = ".*BLOQ.*", replacement = 0)
hmfull[,c(16:25)] <- lapply(hmfull[,c(16:25)], gsub, pattern = ".*BB.*", replacement = 0)
hmfull[,c(16:25)] <- lapply(hmfull[,c(16:25)], gsub, pattern = "^0$", replacement = 0)
hmfull[,c(16:25)]  <- lapply(hmfull[,c(16:25)] , gsub, pattern = "N/A", replacement = 0)
hmfull[,c(8,9,12,13:14, 16:25)] =as.numeric(unlist(hmfull[,c(8,9,12,13:14, 16:25)]))
#hmfull[,10]<- cut(hmfull[,10], breaks = 3, labels =c(2,3,4))
#hmfull[,7]<- cut(hmfull[,7], breaks = 2, labels =c(0,1))
#hmfull[, 12] <- as.numeric(gsub(2811.1, 281.1, hmfull[, 12]))
#hmfull[,13]<- as.character(hmfull[,13])
hmfull[,3]<- as.character(hmfull[,3])

#FOR ORGANIC COMPOUNDS (OC) DATASET----

# FUNCTIONS TO HELP MODIFY AREA NUMBERS IN ORGANIC COMPOUNDS DATASET FURTHER ANALYSIS
dkc <- function(x){#padding 0 to first two digits
  if (str_detect(substring(x[4],1,2),"\\.") == TRUE){ 
    sk=print(paste('0',substring(x[4],1,)))
    sk<- gsub(" ", "",  sk)
    x[4] <- gsub(substring(x[4],1,), sk,  x[4])
  }
  return(x)
}

sm2<- read.csv("Final_Results_From_Task 1/Final_SMresults_mgkg.csv", header= TRUE)
sm2[,4] <- paste0(sm2[,4], ".")
sm1.1 <- sm2 %>% relocate(Area, .after = ID_num)
#colnames(sm2)[4]<- "Area"
sm1 <- as.data.frame(t(apply(sm1.1, MARGIN = 1, dkc)))
sm1$ID_num <- gsub("\\.", "",sm1$ID_num)
sm[,c(16:19)] <- lapply(sm[,c(16:19)], gsub, pattern = ".*BLOQ.*", replacement = 0)
sm[,c(16:19)] <- lapply(sm[,c(16:19)], gsub, pattern = "N/A", replacement = 0)
sm[,c(16:19)] <- lapply(sm[,c(16:19)], gsub, pattern = "^0$", replacement = 0)
sm[,c(10:12,15:16,17:30)] =as.numeric(unlist(sm[,c(10:12,15:16,17:30)]))
#sm[, 12] <- as.numeric(gsub(2811.1, 281.1, sm[, 12]))
#sm[,10]<- cut(sm[,10], breaks = 3, labels =c(2,3,4))
sm[,6]<- tolower(sm[,6])
# sm[,7]<- as.character(sm[,7])
# sm[,3]<- as.character(sm[,3])
# sm[,13]<- as.character(sm[,13])
# sm[,11]<- cut(sm[,11], breaks = 4, labels =c(150, 250, 350, 450))
# sm[,15]<- cut(sm[,15], breaks = 4, labels =c(200, 400, 600, 800))
# sm[,16]<- cut(sm[,16], breaks = 5, labels =c(100, 130, 140, 150, 190))
smfull <- sm


#FUNCTION TO ANALYSE TEMPORAL VARIATION FOR BOTH HM AND OC DATASETS----
temp_var<- function(y, remove.zeroes=FALSE){ #Remove zeroes options to check analysis for only detected values since those that are zero are considered undetected.
  list0 <- list() # Preparing empty lists to store graphs later.
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
    range <- colnames(y)[16:25] #colnames with heavymetals  
    numrang <- 16:25 # range of heavy metals
    filename <- "HMicons//" # see file......
    #recommended levels for heavy metals in mg/kg:
    rl <- data.frame(metcom=c("Fe","Co" ,"Ni", "Cu", "Zn", "Ag", "Cd", "Hg", "Tl","Pb"), rl=c(100,0.0016, 8, 30,30,0.01,2,0.1,0.45,0.05), rlu=c(rep(NA, 10)), levs=c('FAO/WHO: 100mg/kg', 'EFSA: 0.0016mg/kg', 'FAO/WHO: <0.5-8mg/kg','ANVISA: 30mg/kg', 'FAO/WHO: <30-100mg/kg','Grasso et al. 2021: 0.01mg/kg','FAO/WHO: <0.05-2mg/kg','Brodziak-Dopierała et al. 2023: 0.1mg/kg','Makridis and Amberger, 1996; LaCoste et al. 2001: \n <0.45-2.28mg/kg (permissible range for animal feed)', 'FAO/WHO: <0.05-2mg/kg'))
  }else{
    range <- colnames(y)[16:19] 
    numrang <- 16:19
    filename <- "SMicons//"
    rl <- data.frame(metcom=c("Adipic_acid","Caprolactam","Chlorpyrifos","Ibuprofen"), rl=c(470, 50,0.01,40), rlu=c(NA, NA,NA,NA), levs=c('EPA: 470mg/kg/day','EPA: 50mg/kg/day', 'FAO/WHO: 0.01mg/kg/day','The Mayo Clinic: 40mg/kg/day'))
  }
  
  for(h in 1:length(numrang)){ 
    if(remove.zeroes==FALSE){
      dfnz<- y[,c(numrang,3,5,6)]  %>%
        pivot_longer(all_of(range), names_to = "mc", values_to = "conc") %>% group_by( Tissue, mc)%>%mutate_at(vars(mc, Year, Tissue), factor)
    }else{
      dfnz<- y[,c(numrang,3,5,6)]  %>%
        pivot_longer(all_of(range), names_to = "mc", values_to = "conc") %>% group_by( Tissue, mc)%>%mutate_at(vars(mc, Year, Tissue), factor)%>%subset(conc!=0)
    }
    tis <- levels(factor(y[,6]))
    yr <- levels(factor(y[,3]))
    mcom <-colnames(y)[h+15]
    rml<-rl %>% subset(metcom == mcom)
    fm2 <- data.frame(matrix(ncol=3, nrow = 0))
    colnames(fm2) <- c('Year', 'metcom', 'results')
    #nzY<-dfnz %>% group_by(sm) %>% subset(Year == yr[s])
    #ar <- levels(factor(as.character(nzY$Area)))
    nzM<-data.frame(dfnz %>% mutate_at(vars(conc), as.numeric) %>% subset(mc == mcom))
    if((nrow(nzM)==0|max(nzM$conc)==0)==TRUE){
      fm <- data.frame(matrix(ncol=3, nrow = 0))
      colnames(fm) <- c('Year', 'metcom', 'results')
      fm[1,1] <- yr[i]
      fm[1,2] <- mcom[h]
      fm[1,3] <- '<3 Obs' 
      fm2 <- rbind(fm2, fm)
      #print(nzM)
      next
    }else{
      print(nzM)
      fm <- data.frame(matrix(ncol=3, nrow = 0))
      colnames(fm) <- c('Year', 'metcom', 'results')
      if(nrow(nzM)>3 & all(nzM[-1,'conc'] == nzM[1,'conc'])==FALSE){
        swm <-(shapiro.test(nzM$conc)$p.value)
        fm[1,1] <- yr[i]
        fm[1,2] <- mcom
        if(swm<0.05){
          fm[1,3] <- 'fail' 
        }else{
          fm[1,3] <- 'pass' 
        }
        fm2 <- rbind(fm2, fm)
      }else{
        fm[1,1] <- yr[i]
        fm[1,2] <- mcom
        fm[1,3] <- '<3 Obs' 
        fm2 <- rbind(fm2, fm)
      }
      sumnz<- summarySE(nzM, measure= "conc", groupvars =c("Tissue","Year"),.drop = FALSE)
      compar <- data.frame(matrix(ncol = 5, nrow = 0))
      cldtab <- data.frame(matrix(ncol = 4, nrow = 0))
      temp <-setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("Tissue", "pvalue"))
      tempno <-setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("Tissue", "pvalue"))
      tempy <-setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("Tissue", "pvalue"))
      pvals <-setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("Tissue", "pvalue"))
    }
    for(i in 1:length(tis)) {
      if((tis[i]%in%nzM$Tissue)==FALSE){
        i<-i+1
      }else{
        tiss <- as.data.frame(filter(nzM, Tissue == tis[i]))
        inter <- interaction(tiss[,3], tiss[,1])
        print(inter)
        if((nrow(fm2)!=0 & 'fail' %in% fm2[,'results']==TRUE & nrow(tiss)==1|length(levels(factor((tiss$Year))))==1)==FALSE){
          ctr0 <- data.frame(matrix(ncol = 10))
          kw<- kruskal.test(conc ~ inter, data = tiss)
          temp[1,1] <- tis[i]
          temp[1,2] <- kw[["p.value"]]
          ctr0[1,1] <- mcom[h]
          ctr0[1,2] <- min(nzM$conc)
          ctr0[1,3] <- max(nzM$conc)
          ctr0[1,4] <- format(signif(mean(nzM$conc),3), scientific=FALSE)
          ctr0[1,5] <- median(nzY$conc)
          ctr0[1,6] <- paste('chisq=', signif(kw.model$statistic[[1]],3), sep = "")
          ctr0[1,7] <- kw.model$parameter[[1]]
          ctr0[1,8] <- signif(kw.model$p.value, 3)
          ctr0[1,9] <- NA
          ctr0[1,10] <- NA
          ctr0[1,11] <- res
          pvals <- rbind(pvals,temp)
          if (is.nan(kw[["p.value"]])){
            if((tis[i]=='inksac')==TRUE){
              cldtnan <- data.frame(matrix(ncol = 4, nrow = 4))
              cnames <-c("Tissue","Year","Letter","MonoLetter")
              colnames(cldtnan) <-cnames
              yrI <- levels(factor(tiss[,1]))
              cldtnan[c(1:2),1]<- tis[i]#change with year
              cldtnan[c(1:2),2]<- yrI #lev19#change with year
              cldtnan[c(1:2),c(3:4)]<- 'a'
            }else{
              cldtnan <- data.frame(matrix(ncol = 4, nrow = 5))
              cnames <-c("Tissue","Year","Letter","MonoLetter")
              colnames(cldtnan) <-cnames
              cldtnan[c(1:3),1]<- tis[i]#change with year
              cldtnan[c(1:3),2]<- yr #lev19#change with year
              cldtnan[c(1:3),c(3:4)]<- 'a'
            }
            cldtab <- rbind(cldtab,cldtnan)
            comparnan <- data.frame(matrix(ncol = 4, nrow = 30))
            comnames <-c("Comparison" ,"Z","P.unadj","P.adj")
            colnames(comparnan) <-comnames
            comparnan[c(1:30),1]<- tis[i]#change with year
            comparnan[c(1:30),c(2:4)]<- 0
            compar <- rbind(compar,comparnan)
            next
          }
        }else{
          tempno[1,1] <- tis[i]
          tempno[1,2] <- NA
          pvals <- rbind(pvals,tempno)
          if((tis[i]=='inksac')==TRUE){
            cldtnan <- data.frame(matrix(ncol = 4, nrow = 4))
            cnames <-c("Tissue","Year","Letter","MonoLetter")
            colnames(cldtnan) <-cnames
            yrI <- levels(factor(tiss[,1]))
            cldtnan[c(1:2),1]<- tis[i]#change with year
            cldtnan[c(1:2),2]<- yrI #lev19#change with year
            cldtnan[c(1:2),c(3:4)]<- 'a'
          }else{
            cldtnan <- data.frame(matrix(ncol = 4, nrow = 5))
            cnames <-c("Tissue","Year","Letter","MonoLetter")
            colnames(cldtnan) <-cnames
            cldtnan[c(1:3),1]<- tis[i]#change with year
            cldtnan[c(1:3),2]<- yr #lev19#change with year
            cldtnan[c(1:3),c(3:4)]<- 'a'
          }
          cldtab <- rbind(cldtab,cldtnan)
          comparnan <- data.frame(matrix(ncol = 4, nrow = 30))
          comnames <-c("Comparison" ,"Z","P.unadj","P.adj")
          colnames(comparnan) <-comnames
          comparnan[c(1:30),1]<- tis[i]#change with year
          comparnan[c(1:30),c(2:4)]<- 0
          compar <- rbind(compar,comparnan)
          next
        }
        if(length(levels(factor((tiss$Year)))) < 3){
          dtt<-dunn.test (tiss$conc, inter, method=p.adjustment.methods, kw=TRUE)
          dtresI <- data.frame(matrix(ncol = 4, nrow=1))
          comnames <-c("Comparison" ,"Z","P.unadj","P.adj")
          colnames(dtresI) <-comnames
          dtresI[,1] <- dtt$comparisons
          dtresI[,2] <- dtt$Z
          dtresI[,3] <- dtt$P
          dtresI[,4] <- dtt$P.adjusted
          compar <- rbind(compar,dtresI)
          cldtt <- cldList(P.adj ~ Comparison, data = dtresI,threshold = 0.05, remove.zero = FALSE)
          cldtt <-cldtt %>% separate(Group, c("Tissue", "Year"))
          cldtab <- rbind(cldtab,cldtt)
        }else{
          dt <- dunnTest(conc ~ inter,data=tiss,method="bh")
          dtres <- dt$res
          compar <- rbind(compar,dtres)
          cldt <- cldList(P.adj ~ Comparison, data = dtres,threshold = 0.05, remove.zero = FALSE)
          #cldt <-cldt %>% separate(Group, c("Tissue", "Area"))
          cldt <-cldt %>% separate(Group, c("Tissue", "Year"))
          cldtab <- rbind(cldtab,cldt)
        }
      }
    }
    #For icons----
    if(file.exists(paste0(filename,mcom,".png"))==TRUE){
      icon <- file.exists(paste0(filename,mcom,".png"))
      icons = png::readPNG(paste0(filename,mcom,".png")) %>%
        rasterGrob(x= 0.95, y=1.03,interpolate = TRUE, height = 0.11, width = 0.06)}else{
          icon <- FALSE
        }
    #_________________________________0
    if(file.exists(paste0(filename,mcom,"1",".png"))==TRUE){
      icon1 <- file.exists(paste0(filename,mcom,"1",".png"))
      iconz = png::readPNG(paste0(filename,mcom,"1",".png")) %>%
        rasterGrob(x= 0.88, y=1.03,interpolate = TRUE, height = 0.11, width = 0.06)}else{
          icon1 <- FALSE
        } 
    #__________________________________________1
    if(file.exists(paste0(filename,mcom,"2",".png"))==TRUE){
      icon2 <- file.exists(paste0(filename,mcom,"2",".png"))
      iconsz = png::readPNG(paste0(filename,mcom,"2",".png")) %>%
        rasterGrob(x= 0.80, y=1.03,interpolate = TRUE, height = 0.11, width = 0.06)}else{
          icon2 <- FALSE
        } 
    print(mcom)
    pvalfl <- setNames(data.frame(matrix(ncol = 2, nrow = 0)), c("Tissue", "pvalue"))
    pvalfl <- rbind(pvalfl,pvals)
    pvalstv<-append(list(pvalfl),pvalstv, 0)
    pvaln <-paste(mcom,"tmpvar", sep = "")
    pvalsnames <- append(pvalsnames,pvaln)
    cldtabt <- data.frame(matrix(ncol = 4, nrow = 0))
    cldtabt <- rbind(cldtabt,cldtab)
    list1<-append(list(data.frame(cldtabt)),list1, 0)
    name1 <-paste(mcom,'restv', sep = "")
    list1names <-append(list1names, name1)
    #______________________________________________________________________
    list0<-append( list(data.frame(compar)),list0, 0)
    name0<- paste(mcom,'restv', sep = "")
    list0names <- append(list0names,name0)
    #______________________________________________________________________
    #res1 <- res%>% left_join(cldtab, by=c('Area'='Area','Tissue'='Tissue'))
    #lev19 <- c('43', '04', '01', '88', '87')
    res1 <- sumnz%>% left_join(cldtabt, by=c('Year'='Year','Tissue'='Tissue'))
    res1[,2] <- factor(res1[,2], levels=yr)#change with year
    colnames(res1)[4] <-mcom
    list2<-append(list(data.frame(res1)),list2, 0)
    name2 <- paste(mcom,"restv", sep = "")
    #names(list2)[[h]]<-name2
    list2names <- append(list2names,name2)
    #____________________________________________________________________
    if (all(res1[-1, 8] == res1[1, 8])==FALSE){
      #print(mcom)
      Colors <-setNames( c('red', 'green','blue'),yr)
      plot <- print(ggplot(res1, aes(x=Tissue, y=!! rlang::sym(paste0(mcom)), fill=factor(Year, levels=c('2019','2020','2021')))) +
                      geom_bar(stat='identity', color="black", position=position_dodge()) +scale_fill_manual(values=Colors) +
                      geom_errorbar(aes(ymin=!! rlang::sym(paste0(mcom)),ymax=!! rlang::sym(paste0(mcom))+se),width=.1,position=position_dodge(.8))+
                      labs(title = paste0(mcom))+
                      {if(icon==TRUE)annotation_custom(icons)}+
                      {if(icon1==TRUE)annotation_custom(iconz)}+
                      {if(icon2==TRUE)annotation_custom(iconsz)}+
                      coord_cartesian(clip = 'off')+
                      theme(plot.title = element_text(hjust = 0.5))+
                      labs(y = "Concentration mg/kg", x = "Tissue")+
                      labs(fill = "Years") +
                      geom_text(aes(label=Letter), position = position_dodge(1), size = 5,vjust=-0.2, hjust=-0.005, colour = "black")+ {if((rml[,2])>max(res1[,paste(mcom)]))annotate('text', x=max(as.numeric(as.factor(unique(res1$Tissue))))*.78, y=max(res1[,paste(mcom)]*1.40)*.87, label= rml[,4],fontface='bold', size=2.2, color="red") else geom_hline(yintercept=rml[,2], linetype="dashed", color = "red")})
      # {if((is.na(rml[,2])==FALSE))geom_hline(yintercept=rml[,2], linetype="dashed", color = "red")}+
      # {if((is.na(rml[,3])==FALSE))geom_hline(yintercept=rml[,3], linetype="dashed", color = "red")})
      list3<-append(list(plot),list3, 0)
      name3 <-paste(mcom,"plottv", sep = "")
      #names(list3)[[h]]<-name3
      list3names <- append(list3names,name3)
    } else {
      #print(mcom)
      plotnl <- print(ggplot(res1, aes(x=Tissue, y=!! rlang::sym(paste0(mcom)), fill=factor(Year, levels=c('2019', '2020', '2021')))) +
                        geom_bar(stat='identity', color="black", position=position_dodge()) +scale_fill_manual(values=Colors) +
                        geom_errorbar(aes(ymin=!! rlang::sym(paste0(mcom)), ymax=!! rlang::sym(paste0(mcom))+se), width=.1, position=position_dodge(.8))+
                        labs(title = paste0(mcom))+
                        {if(icon==TRUE)annotation_custom(icons)}+
                        {if(icon1==TRUE)annotation_custom(iconz)}+
                        {if(icon2==TRUE)annotation_custom(iconsz)}+
                        coord_cartesian(clip = 'off')+
                        theme(plot.title = element_text(hjust = 0.5))+
                        labs(y = "Concentration mg/kg", x = "Tissue")+
                        labs(fill = "Years")+
                        {if((rml[,2])>max(res1[,paste(mcom)]))annotate('text', x=max(as.numeric(as.factor(unique(res1$Tissue))))*.78, y=max(res1[,paste(mcom)]*1.40)*.87, label= rml[,4],fontface='bold', size=2.2, color="red") else geom_hline(yintercept=rml[,2], linetype="dashed", color = "red")})
      # {if((is.na(rml[,2])==FALSE))geom_hline(yintercept=rml[,2], linetype="dashed", color = "red")}+{if((is.na(rml[,3])==FALSE))geom_hline(yintercept=rml[,3], linetype="dashed", color = "red")})
      # + geom_text(aes(label=Letter), position = position_dodge(1), size = 3,vjust=-0.2, hjust=-0.005, colour = "black")
      list3<-append(list(plotnl),list3, 0)
      name3 <-paste(mcom,"plottv", sep = "")
      list3names <- append(list3names,name3)
      
    }
    names(list0)<-list0names
    names(list1)<-list1names
    names(list2)<-list2names
    names(list3)<-list3names
    names(pvalstv)<-pvalsnames
  }
  
  return(list (comparisons=list0, cldt=list1, summarymcom=list2, plots=list3, pvals=pvalstv))
}
tv <- temp_var(hmfull, remove.zeroes = TRUE)    
do.call("grid.arrange", c(tv$plots[c(6,7,2,4,1,3,10,5)], ncol=3)) #HM

do.call("grid.arrange", c(tv$plots[c(1,2,3,4,8)], ncol=3)) #SM
#saving data from dataframes in list----
sink("tv.txt")
print(tv[c(1,2,3,5)])
sink()
