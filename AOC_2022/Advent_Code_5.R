library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library("writexl")
library(tibble)

moves <- read_excel("C:/Users/elwakal/OneDrive - Oxy/Downloads/Advent_code_2022_6.xlsx",     sheet = "moves")
carts <- data.frame(read_excel("C:/Users/elwakal/OneDrive - Oxy/Downloads/Advent_code_2022_6.xlsx",     sheet = "crats"))

carts_orginal <- carts

nr_moves <- nrow(moves) 

nrow_carts <- nrow(carts)

#colnames(carts) <- ""

nr_carts <- ncol(carts) 


col1<-  na.omit(carts[,1])
col2<-  na.omit(carts[,2])
col3<-  na.omit(carts[,3])
col4<-  na.omit(carts[,4])
col5<-  na.omit(carts[,5])
col6<-  na.omit(carts[,6])
col7<-  na.omit(carts[,7])
col8<-  na.omit(carts[,8])
col9<-  na.omit(carts[,9])

carts_list <-list(col1,col2,col3,col4,col5,col6,col7,col8,col9)
n= 0

# ------------------------------------------------ PART 2
for (x in 1:nr_moves)
{
    print(x)

#  move 3 from 8 to 9
#  move 2 from 2 to 8
#  move 5 from 4 to 2
#  move 7 from 1 to 4
  
    m = moves$location[x]
    c1 =moves$x1[x]
    c2 =moves$x2[x]

      for (x in 1:m)
        
      {
        n = m - x + 1
        # adding (works)
        carts_list[[c2]] <-c(carts_list[[c1]][n],carts_list[[c2]])
        # deleting (works)
        carts_list[[c1]] <- carts_list[[c1]][-n]
      }
     n= 0
  #  print(carts_list)
        
  }
print(carts_list)
# ------------------------------------------------ PART 1
#for (x in 1:nr_moves)
#{
#  print(x)
  
  #  move 3 from 8 to 9
  #  move 2 from 2 to 8
  #  move 5 from 4 to 2
  #  move 7 from 1 to 4
  
#  m = moves$location[x]
#  c1 =moves$x1[x]
#  c2 =moves$x2[x]
#  
#  for (x in 1:m)
    
#  {
    
    # adding (works)
#    carts_list[[c2]] <-c(carts_list[[c1]][1],carts_list[[c2]])
#    # deleting (works)
 #   carts_list[[c1]] <- carts_list[[c1]][-1]
#  }
  
  #  print(carts_list)
  
#}
#print(carts_list)
