library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library("writexl")
# loading the data


files<- data.frame(read_excel("C:/Users/elwakal/OneDrive - Oxy/Downloads/Advent_code_2022_7.xlsx",     sheet = "Sheet1"))

 
 files$size <- as.numeric(gsub("([0-9]+).*$", "\\1", files$Data)) # extracting the file siez
 
 sum( files$size, na.rm =TRUE)
 #files$name[3] <- as.numeric(gsub( files$size[3] ,"test" , files$Data[3])) # extracting the file siez

 
 files$LEVEL <-1
 files$CRNT_DIR <-NA
 files$PRNT_DIR <-NA
 files$PRNT_DIR2 <-NA
 files$PRNT_DIR3 <-NA
 files$PRNT_DIR4<-NA
 files$PRNT_DIR5 <-NA
 files$PRNT_DIR6 <-NA
 files$PRNT_DIR7 <-NA
 files$PRNT_DIR8 <-NA
 files$PRNT_DIR9 <-NA
 files$PRNT_DIR0 <-NA
 files$path <- NA
 
 length(unique(files$CRNT_DIR))
 up =0
 dwn=0
 for (x in 1:nrow(files) )
   
 {

  if( "$ cd /" %in% files$Data[x]){ files$CRNT_DIR[x]<-"MAIN"; files$PRNT_DIR[x]<-"MAIN"; files$PRNT_DIR2[x]<-"MAIN"; files$PRNT_DIR3[x]<-"MAIN";files$PRNT_DIR4[x]<-"MAIN";files$PRNT_DIR5[x]<-"MAIN";files$PRNT_DIR6[x]<-"MAIN";files$PRNT_DIR7[x]<-"MAIN";files$PRNT_DIR8[x]<-"MAIN";files$PRNT_DIR9[x]<-"MAIN";files$PRNT_DIR0[x]<-"MAIN"} # main
  
  else if( substring(files$Data[x],1,7) =="$ cd ..")# going up one level
  { 
    files$LEVEL[x]= as.numeric(files$LEVEL[x-1]) -1
    files$CRNT_DIR[x] <- files$PRNT_DIR[x-1]
    files$PRNT_DIR[x] <- files$PRNT_DIR2[x-1]
    files$PRNT_DIR2[x]<- files$PRNT_DIR3[x-1]
    files$PRNT_DIR3[x]<- files$PRNT_DIR4[x-1]
    files$PRNT_DIR4[x]<- files$PRNT_DIR5[x-1]
    files$PRNT_DIR5[x]<- files$PRNT_DIR6[x-1]
    files$PRNT_DIR6[x]<- files$PRNT_DIR7[x-1]
    files$PRNT_DIR7[x]<- files$PRNT_DIR8[x-1]
    files$PRNT_DIR8[x]<- files$PRNT_DIR9[x-1]
    files$PRNT_DIR9[x]<- files$PRNT_DIR0[x-1]
    files$PRNT_DIR0[x]<-"MAIN"
    up =up +1

  }
  else if( substring(files$Data[x],1,4) =="$ cd")# activating a new dir
  { 
    files$LEVEL[x]= as.numeric(files$LEVEL[x-1]) + 1
      # current gets new one 
    files$PRNT_DIR0[x] <- files$PRNT_DIR9[x-1]
    files$PRNT_DIR9[x] <- files$PRNT_DIR8[x-1]
    files$PRNT_DIR8[x] <- files$PRNT_DIR7[x-1]
    files$PRNT_DIR7[x] <- files$PRNT_DIR6[x-1]
    files$PRNT_DIR6[x] <- files$PRNT_DIR5[x-1]
    files$PRNT_DIR5[x] <- files$PRNT_DIR4[x-1]
    files$PRNT_DIR4[x] <- files$PRNT_DIR3[x-1]
    files$PRNT_DIR3[x] <- files$PRNT_DIR2[x-1]
    files$PRNT_DIR2[x] <- files$PRNT_DIR[x-1]
    files$PRNT_DIR[x]  <- files$CRNT_DIR[x-1]
    files$CRNT_DIR[x]<-substring(files$Data[x], 6,nchar(files$Data[x]))

    dwn =dwn +1
  }
   
   else # keeping the same DIR
   { 
    files$CRNT_DIR[x]<-files$CRNT_DIR[x-1]  
    files$PRNT_DIR[x] <- files$PRNT_DIR[x-1]
    files$PRNT_DIR2[x]<-files$PRNT_DIR2[x-1] 
    files$PRNT_DIR3[x]<-files$PRNT_DIR3[x-1] 
    files$PRNT_DIR4[x]<-files$PRNT_DIR4[x-1] 
    files$PRNT_DIR5[x]<-files$PRNT_DIR5[x-1] 
    files$PRNT_DIR6[x]<-files$PRNT_DIR6[x-1] 
    files$PRNT_DIR7[x]<-files$PRNT_DIR7[x-1] 
    files$PRNT_DIR8[x]<-files$PRNT_DIR8[x-1] 
    files$PRNT_DIR9[x]<-files$PRNT_DIR9[x-1] 
    files$PRNT_DIR0[x]<-files$PRNT_DIR0[x-1] 
    files$LEVEL[x]=files$LEVEL[x-1]  
   }

   
 } 

 for (x in 1:nrow(files) )
   
 {
   
 if (files$LEVEL[x] ==1) {files$path[x] <- paste( "MAIN", sep= "/")}
 if (files$LEVEL[x] ==2) {files$path[x] <- paste( "MAIN",files$CRNT_DIR[x], sep= "/")} 
 if (files$LEVEL[x] ==3) {files$path[x] <- paste( "MAIN",files$PRNT_DIR[x],files$CRNT_DIR[x], sep= "/")} 
 if (files$LEVEL[x] ==4) {files$path[x] <- paste( "MAIN",files$PRNT_DIR2[x],files$PRNT_DIR[x],files$CRNT_DIR[x], sep= "/")} 
 if (files$LEVEL[x] ==5) {files$path[x] <- paste( "MAIN",files$PRNT_DIR3[x],files$PRNT_DIR2[x],files$PRNT_DIR[x],files$CRNT_DIR[x], sep= "/")} 
 if (files$LEVEL[x] ==6) {files$path[x] <- paste( "MAIN",files$PRNT_DIR4[x],files$PRNT_DIR3[x],files$PRNT_DIR2[x],files$PRNT_DIR[x],files$CRNT_DIR[x], sep= "/")}  
 if (files$LEVEL[x] ==7) {files$path[x] <- paste( "MAIN",files$PRNT_DIR5[x],files$PRNT_DIR4[x],files$PRNT_DIR3[x],files$PRNT_DIR2[x],files$PRNT_DIR[x],files$CRNT_DIR[x], sep= "/")} 
 if (files$LEVEL[x] ==8) {files$path[x] <- paste( "MAIN",files$PRNT_DIR6[x],files$PRNT_DIR5[x],files$PRNT_DIR4[x],files$PRNT_DIR3[x],files$PRNT_DIR2[x],files$PRNT_DIR[x],files$CRNT_DIR[x] , sep= "/")} 
 if (files$LEVEL[x] ==9) {files$path[x] <- paste( "MAIN",files$PRNT_DIR7[x],files$PRNT_DIR6[x],files$PRNT_DIR5[x],files$PRNT_DIR4[x],files$PRNT_DIR3[x],files$PRNT_DIR2[x],files$PRNT_DIR[x],files$CRNT_DIR[x], sep= "/")} 
 if (files$LEVEL[x] ==10) {files$path[x] <- paste( "MAIN",files$PRNT_DIR8[x],files$PRNT_DIR7[x],files$PRNT_DIR6[x],files$PRNT_DIR5[x],files$PRNT_DIR4[x],files$PRNT_DIR3[x],files$PRNT_DIR2[x],files$PRNT_DIR[x],files$CRNT_DIR[x], sep= "/")} 
 
   
 }
 
 DIR_SIZE = data.frame(files %>% group_by(DIR = path) %>%  dplyr::summarise(sum_size = sum(as.numeric(size),na.rm =TRUE) ,  .groups = 'drop'))
 results <- sum(DIR_SIZE[DIR_SIZE$sum_size <=100000,2])
 
  

 

#
# getting each dir size
#DIR_SIZE = data.frame(files %>% group_by(DIR = CRNT_DIR) %>%  dplyr::summarise(sum_size = sum(as.numeric(size),na.rm =TRUE) ,  .groups = 'drop'))
#DIR_SIZE1 = data.frame(files %>% group_by(DIR =PRNT_DIR) %>%  dplyr::summarise(sum_size = sum(as.numeric(size),na.rm =TRUE) ,  .groups = 'drop'))
#DIR_SIZE2 = data.frame(files %>% group_by(DIR =PRNT_DIR2) %>%  dplyr::summarise(sum_size = sum(as.numeric(size),na.rm =TRUE) ,  .groups = 'drop'))
#DIR_SIZE3 = data.frame(files %>% group_by(DIR =PRNT_DIR3) %>%  dplyr::summarise(sum_size = sum(as.numeric(size),na.rm =TRUE) ,  .groups = 'drop'))
#DIR_SIZE4 = data.frame(files %>% group_by(DIR =PRNT_DIR4) %>%  dplyr::summarise(sum_size = sum(as.numeric(size),na.rm =TRUE) ,  .groups = 'drop'))
#DIR_SIZE5 = data.frame(files %>% group_by(DIR =PRNT_DIR5) %>%  dplyr::summarise(sum_size = sum(as.numeric(size),na.rm =TRUE) ,  .groups = 'drop'))
#DIR_SIZE6 = data.frame(files %>% group_by(DIR =PRNT_DIR6) %>%  dplyr::summarise(sum_size = sum(as.numeric(size),na.rm =TRUE) ,  .groups = 'drop'))
#DIR_SIZE7 = data.frame(files %>% group_by(DIR =PRNT_DIR7) %>%  dplyr::summarise(sum_size = sum(as.numeric(size),na.rm =TRUE) ,  .groups = 'drop'))
#DIR_SIZE8 = data.frame(files %>% group_by(DIR =PRNT_DIR8) %>%  dplyr::summarise(sum_size = sum(as.numeric(size),na.rm =TRUE) ,  .groups = 'drop'))
#DIR_SIZE9= data.frame(files %>% group_by(DIR =PRNT_DIR9) %>%  dplyr::summarise(sum_size = sum(as.numeric(size),na.rm =TRUE) ,  .groups = 'drop'))
#DIR_SIZE0 = data.frame(files %>% group_by(DIR =PRNT_DIR0) %>%  dplyr::summarise(sum_size = sum(as.numeric(size),na.rm =TRUE) ,  .groups = 'drop'))

Files_all <- rbind(DIR_SIZE,DIR_SIZE1,DIR_SIZE2,DIR_SIZE3,DIR_SIZE4,DIR_SIZE5,DIR_SIZE6,DIR_SIZE7,DIR_SIZE8,DIR_SIZE9,DIR_SIZE0)
DIR_SIZE_total = data.frame(Files_all %>% group_by(DIR) %>%  dplyr::summarise(sum_size = sum(as.numeric(sum_size),na.rm =TRUE) ,  .groups = 'drop'))

results <- sum(DIR_SIZE_total[DIR_SIZE_total$sum_size <=100000,2])
write_xlsx(files ,"C:/Users/elwakal/OneDrive - Oxy/Downloads/Advent_code_2022_7_out3.xlsx")
 