library(readxl)
library(dplyr)
library(stringr)
library(tidyr)

# loading the data
df <- data.frame(read_excel("C:/Users/elwakal/OneDrive - Oxy/Downloads/Advent_code_2022_2.xlsx"))

df2 <- df %>% separate(Data, c("A", "B", "C", "D")) # seprating the data
nr <- nrow(df2) 

for (x in 1:nr)
{
  
  # if empty value add 1 , else do nothing
  if ((df2$A[x]<=df2$C[x] & df2$B[x]>=df2$D[x]))# || (df2$C[x]<=df2$A[x] & df2$D[x]>=df2$B[x]) ) {
  {
    df2$stat[x] =  1
  } else {
    
    df2$stat[x] =  0
  }
  
}
print(sum(df2$stat))
write.csv(df2, "C:/Users/elwakal/OneDrive - Oxy/Downloads/Advent_code_2022_4.csv", row.names=FALSE)