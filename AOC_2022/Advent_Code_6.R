library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library("writexl")
# loading the data


data <- read.table(file = "C:/Users/elwakal/OneDrive - Oxy/Downloads/advent of code_2022_6.txt", header = FALSE)

code_text <- strsplit(data$V1, "")[[1]]

df <- data.frame(code_text)

df$counts <- 1

l <-length(code_text) 

for (x in 1:l )
  
{

  test <-df[x:(x+3),]
  
  code_grp = data.frame(test %>% group_by(code_text)  %>%  dplyr::summarise(sum_code = sum(counts),  .groups = 'drop'))
  
  if( max(code_grp$sum_code) ==1)
  {
     answer1= (x+3)
     break
    } 
  
}
#write_xlsx(df ,"C:/Users/elwakal/OneDrive - Oxy/Downloads/Advent_code_2022_6.xlsx")
print(answer1)