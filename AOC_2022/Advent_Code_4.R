library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library("writexl")
# loading the data
df <- data.frame(read_excel("C:/Users/elwakal/OneDrive - Oxy/Downloads/Advent_code_2022_3.xlsx"))

df$len <- str_length(df$data)

df$bag1 <-  substr(df$data, 1, df$len /2)

df$bag2 <-  substr(df$data, (df$len /2)+1, df$len )
#---------------------------------------------------------
#strsplit(df$bag2[1])

nr <- nrow(df) 
#------------------------------------------------Part 1
#for (x in 1:nr)
#{
  
#  df$test[x] <- intersect(as.list(strsplit(df$bag1[x], "")[[1]]),as.list(strsplit(df$bag2[x], "")[[1]]))
  
#}
#------------------------------------------------Part 2
x=0
for (n in 1:100)
  
{
 x =(3*n)-2
 print(x)
 print(as.list(df$data[x], "")[[1]])
 print(as.list(df$data[x+1], "")[[1]])
 print(as.list(df$data[x+2], "")[[1]])
  df$test2[x] <- intersect(intersect(as.list(strsplit(df$data[x], "")[[1]]),as.list(strsplit(df$data[x+1], "")[[1]])),as.list(strsplit(df$data[x+2], "")[[1]]))
  df$test2[x+1] <- df$test2[x]
  df$test2[x+2] <- df$test2[x]
  
}

#-----------------------------------------------
df$Letter <- df$test2


df$Letter<-gsub("a","1",df$Letter)
df$Letter<-gsub("b","2",df$Letter)
df$Letter<-gsub("c","3",df$Letter)
df$Letter<-gsub("d","4",df$Letter)
df$Letter<-gsub("e","5",df$Letter)
df$Letter<-gsub("f","6",df$Letter)
df$Letter<-gsub("g","7",df$Letter)
df$Letter<-gsub("h","8",df$Letter)
df$Letter<-gsub("i","9",df$Letter)
df$Letter<-gsub("j","10",df$Letter)
df$Letter<-gsub("K","11",df$Letter)
df$Letter<-gsub("l","12",df$Letter)
df$Letter<-gsub("m","13",df$Letter)
df$Letter<-gsub("n","14",df$Letter)
df$Letter<-gsub("o","15",df$Letter)
df$Letter<-gsub("p","16",df$Letter)
df$Letter<-gsub("q","17",df$Letter)
df$Letter<-gsub("r","18",df$Letter)
df$Letter<-gsub("s","19",df$Letter)
df$Letter<-gsub("t","20",df$Letter)
df$Letter<-gsub("u","21",df$Letter)
df$Letter<-gsub("v","22",df$Letter)
df$Letter<-gsub("w","23",df$Letter)
df$Letter<-gsub("x","24",df$Letter)
df$Letter<-gsub("y","25",df$Letter)
df$Letter<-gsub("z","26",df$Letter)
df$Letter<-gsub("A","27",df$Letter)
df$Letter<-gsub("B","28",df$Letter)
df$Letter<-gsub("C","29",df$Letter)
df$Letter<-gsub("D","30",df$Letter)
df$Letter<-gsub("E","31",df$Letter)
df$Letter<-gsub("F","32",df$Letter)
df$Letter<-gsub("G","33",df$Letter)
df$Letter<-gsub("H","34",df$Letter)
df$Letter<-gsub("I","35",df$Letter)
df$Letter<-gsub("J","36",df$Letter)
df$Letter<-gsub("K","37",df$Letter)
df$Letter<-gsub("L","38",df$Letter)
df$Letter<-gsub("M","39",df$Letter)
df$Letter<-gsub("N","40",df$Letter)
df$Letter<-gsub("O","41",df$Letter)
df$Letter<-gsub("P","42",df$Letter)
df$Letter<-gsub("Q","43",df$Letter)
df$Letter<-gsub("R","44",df$Letter)
df$Letter<-gsub("S","45",df$Letter)
df$Letter<-gsub("T","46",df$Letter)
df$Letter<-gsub("U","47",df$Letter)
df$Letter<-gsub("V","48",df$Letter)
df$Letter<-gsub("W","49",df$Letter)
df$Letter<-gsub("X","50",df$Letter)
df$Letter<-gsub("Y","51",df$Letter)
df$Letter<-gsub("Z","52",df$Letter)

# 8484 is too high
#10798 too high
sum(as.integer(df$Letter))/3

#write.csv(df, "C:/Users/elwakal/OneDrive - Oxy/Downloads/Advent_code_2022_6.csv", row.names=TRUE)





#df2 <-df

#write_xlsx(df2 ,"C:/Users/elwakal/OneDrive - Oxy/Downloads/Advent_code_2022_6.xlsx")



#--------------------Faild attempts
#df$test <-intersect(as.list(strsplit(df$bag2[], "")[[1]])
 #                  ,as.list(strsplit(df$bag1[2], "")[[1]])
 #         )

#df$test <- str_subset(df$bag1, coll(df$bag2, ignore_case = FALSE))
#df$test <- as.numeric(as.factor(df$bag1))

#df$test <- str_detect(df$bag1,df$bag2)
#df$test <-Reduce(intersect,df$bag1,df$bag2)

#df$test <-df$bag1 %in% df$bag2

#df$test <-  adist(df$bag1, df$bag2, partial = TRUE)

#charmatch(df$bag1,df$bag2)

#save(list=ls(), file="//ohoasdr1/data/Share/OP_PrimDev/STSS/R/DEBUG/AE_fuzz_match.RData", RFormat=TRUE)
#df$test <-as.character(match(df$bag1, LETTERS))

#as.list(strsplit(df$bag1[3], "")[[1]])

#intersect(as.list(strsplit(df$bag1[2], "")[[1]]),as.list(strsplit(df$bag2[2], "")[[1]]))

