library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library("writexl")
library(stringi)
library(readr)

advent_of_code_2022_12<- read_csv("C:/Users/elwakal/OneDrive - Oxy/Downloads/Advent_code_2022_12.txt",   col_names = FALSE)

data <-data.frame(advent_of_code_2022_12)


df <- data.frame(list((  strsplit(as.character(data$X1[]),"")  )),row.names= NULL,fix.empty.names =FALSE)# seprating the numbers

names(df) <- NULL


nr <- nrow(df) 
nc <- ncol(df) 
# lets get the starting and the ending positions
for (x in 1:nr)
{
  for (y in 1:nc)
    {
    
    if(as.character(df[x,y]) =="S")
      {
        x_S = as.integer(x)
        y_S = as.integer(y)  
      } 
    
    if(as.character(df[x,y]) =='E'){
        x_E = x 
        y_E = y 
      } 
    
  }
}

df[x_S,y_S] ="a"
df[x_E,y_E] ="z"
########################################################################
conv=data.frame(L=letters[1:26],N=c(1:26))


which(as.character(conv$L)==as.character(df[x_S,y_S]))
which(as.character(conv$L)==as.character(df[x_E,y_E]))

d=((x_E-x_S)^2 +(y_E-y_S)^2)^.5

x_P = x_S
y_P = y_S
j=""

map <-rbind(c(x_P,y_P))

map2 <- paste(map[1],"_",map[2], sep ='')

for (i in 1:(nc*nr)) # step
  { # check 4 directions
  
      # check is it even bossible  to go in which direction
        if (
           y_P > 1 && j !="N" && 
           any(map2!=paste(x_P,"_",y_P-1, sep ='') ) &&
             (which(as.character(conv$L)==as.character(df[x_P,y_P]))+1 == which(as.character(conv$L)==as.character (df[x_P,y_P-1])) ||
              which(as.character(conv$L)==as.character(df[x_P,y_P]))   == which(as.character(conv$L)==as.character (df[x_P,y_P-1])) )  
             
             )    { N=1 }  else {N=0}# North check
        if ( y_P < nc && j !="S"&&
             any(map2!=paste(x_P,"_",y_P+1, sep ='') )&& 
             (which(as.character(conv$L)==as.character(df[x_P,y_P])) +1 ==   which(as.character(conv$L)==as.character(df[x_P,y_P+1]))  ||
              which(as.character(conv$L)==as.character(df[x_P,y_P])) ==   which(as.character(conv$L)==as.character(df[x_P,y_P+1]))   )
             
             )   {S=1  } else {S=0}# South check
        if ( x_P > 1 && j !="W" && any(map2!=paste(x_P-1,"_",y_P, sep ='') )&& 
             (which(as.character(conv$L)==as.character(df[x_P,y_P]))  +1 == which(as.character(conv$L)==as.character (df[x_P-1,y_P])) ||
              which(as.character(conv$L)==as.character(df[x_P,y_P]))   == which(as.character(conv$L)==as.character (df[x_P-1,y_P])))

             )   {W=1  }  else {W=0}# west check
        if ( x_P < nr && j !="E" && any(map2!=paste(x_P+1,"_",y_P-1, sep ='') )&& 
             (which(as.character(conv$L)==as.character(df[x_P,y_P])) == which(as.character(conv$L)==as.character(df[x_P+1,y_P])) ||
              which(as.character(conv$L)==as.character(df[x_P,y_P])) +1 == which(as.character(conv$L)==as.character(df[x_P+1,y_P]))) 
             ) {E=1  } else {E=0}# east check
      # find the direction that will get us closer 

      if( N==1){d_N=(((x_E-x_P)^2 +(y_E-(y_P-1))^2)^.5)}else {d_N =999}
      if( S==1){d_S=(((x_E-x_P)^2 +(y_E-(y_P+1))^2)^.5)}else {d_S =999}
      if( E==1){d_E=(((x_E-(x_P+1))^2 +(y_E-y_P)^2)^.5)}else {d_E =999}
      if( W==1){d_W=(((x_E-(x_P-1))^2 +(y_E-y_P)^2)^.5)}else {d_W =999}
      # finding where we going
      dir<-data.frame(des =c(d_N,d_S,d_E,d_W),direction = c("N","S","E","W"))
     # dir<- (dir  %>% filter(des != 0))# %>% summarise(mindes = min(des))
      dir<- dir[which.min(dir$des),]
      print(paste(x_P,"_",y_P,"_",dir,Sep =""))
      
           if(dir$direction =="S"){y_P=y_P+1 ; j="N"}
      else if(dir$direction =="N"){y_P=y_P-1 ; j="S"} 
      else if(dir$direction =="E"){x_P=x_P+1 ; j="W"} 
      else if(dir$direction =="W"){x_P=x_P-1 ; j="E"} 
      # keeping track
      map <-rbind(map,c(x_P,y_P))
      
      map2 <- paste(map[1],"_",map[2],"_",dir$direction, sep ='')
      
      
   # reached the end
      if(y_P == y_E  &&  x_P == x_E ){ ii = i
        break
      }
       
  }
      












