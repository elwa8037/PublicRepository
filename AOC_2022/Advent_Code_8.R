library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library("writexl")
library(stringi)
# loading the data
#options(scipen=999) # to stop the scintific notation

#data <- readChar(read.table(file = "C:/Users/elwakal/OneDrive - Oxy/Downloads/advent of code_2022_8.txt", header = FALSE,sep=","),99)

library(readr)
advent_of_code_2022_8 <- read_csv("C:/Users/elwakal/OneDrive - Oxy/Downloads/advent of code_2022_8.txt",   col_names = FALSE)

##################################################################################################
#fileName<-"C:/Users/elwakal/OneDrive - Oxy/Downloads/advent of code_2022_8.txt"
#data <-data.frame(readChar(fileName, file.info(fileName)$size))

#############################################################################################
data <-data.frame(advent_of_code_2022_8)

df <- data.frame(list((  strsplit(as.character(data$X1[]),"")  )),row.names= NULL,fix.empty.names =FALSE)# seprating the numbers


#df <-matrix(as.numeric(unlist(strsplit(data, ""))), byrow = TRUE)

#for (x in 1:l){length(tree_list[[1]][[x]]) <- 100} # keeping all the length consistant

#df <- data.frame(tree_list,row.names= NULL,fix.empty.names =FALSE)

names(df) <- NULL#(WORSK) taking out the names of the df


nr <- nrow(df) 
nc <- ncol(df) 
df_results <- df





###############################################################################################
#part 1
#for (x in 1:nr)
#  {
 #   for (y in 1:nc)
 #     {

#      if (x ==1 || x ==99 || y ==1|| y ==99 )                                            {df_results[x,y] <- 'v'} #edge
 #     else  if  
  #    ( as.integer(df[x,y])  >  max(as.integer(unlist(df[x,1:y-1])), na.rm = TRUE))      {df_results[y,x] <- 'v'} # north
 #     else  if 
   #   (as.integer(df[x,y])  >  max(as.integer(unlist(df[x,(y+1):99])), na.rm = TRUE))  {df_results[y,x] <- 'v'} # south
   #    else  if        
   #   (as.integer(df[x,y])  >   max(as.integer(unlist(df[1:x-1,y])), na.rm = TRUE))  {df_results[y,x] <- 'v'} # west
   #    else  if 
   #   (as.integer(df[x,y]) > max(as.integer(unlist(df[(x+1):nc,y])), na.rm = TRUE))   {df_results[y,x] <- 'v'}        #east
  #    else {df_results[y,x] <- 'h'}
      
 #   }
    
 # } 

#df[,1]
#sum(as.integer(unlist(df_results)), na.rm = TRUE)== 'v'

#print(sum(data$col3 == 'manoj'))
#capture.output((tree_list), file = "C:/Users/elwakal/OneDrive - Oxy/Downloads/Advent_code_2022_8_out.txt")
write_xlsx(df_results ,"C:/Users/elwakal/OneDrive - Oxy/Downloads/Advent_code_2022_8.xlsx") # using coutif in excel ==1845

#write.table(tree_list,"C:/Users/elwakal/OneDrive - Oxy/Downloads/Advent_code_2022_8.xlsx", sep=",")
#write.table(dt, file="C:/Users/elwakal/OneDrive - Oxy/Downloads/Advent_code_2022_8.xlsx",sep=",",row.names=F)

#-- part 2
df_results2 <- df

for (x in 1:nr)
{
    for (y in 1:nc)
    {
      if (x != 1 && x !=99 && y !=1 && y !=99 )  {
        #x=8
        #y=3
    
        it <-as.integer(df[x,y])
        
        for (N in 1:(y-1))
        {
        print(df[x,(y-N)])
         if(as.integer(df[x,y])<=as.integer(df[x,(y-N)] ) ){
           break
           }
        }
        for (S in 1:(nc-y))
        {
          print(df[x,S])
          if(as.integer(df[x,y])<=as.integer(df[x,S+y]) ){
            break
            }
        }
        for (E in 1:(nr-x))
        {
              if(as.integer(df[x,y])<=as.integer(df[E+x,y]) ){
            break
            }
        }
        for (W in 1:(x-1))
        {
          if(as.integer(df[x,y])<=as.integer(df[x-W,y]) ){
            break
            }
        }
    
    df_results2[x,y] <- N * S * E * W
    print(as.integer(df_results2[x,y]))
     }
  }
  
} 
max(as.numeric(unlist(df_results2)))

write_xlsx(df_results2 ,"C:/Users/elwakal/OneDrive - Oxy/Downloads/Advent_code_2022_8.xlsx")