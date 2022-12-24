library(readxl)
library(dplyr)

# loading the data
CAL_LIST <- read_excel("C:/Users/elwakal/OneDrive - Oxy/Downloads/Advent_code_2022.xlsx")

df <- data.frame(CAL_LIST)

nr <- nrow(df) 

df$ELFS= 1

y=0

for (x in 1:nr)
  {
  
  # if empty value add 1 , else do nothing
    if ( is.na(df$Calories[x])) {
      y= y+1 

      df$ELFS[x] =df$ELFS[x] + y
    } else {

      df$ELFS[x] = y+1
    }
    
}

# taking out the NA
 df <- na.omit(df)
  
# grouping the calaries by elfs
 df_grp_elfs = df %>% group_by(ELFS)  %>%
   summarise(Calories = sum(Calories),
             .groups = 'drop')
 
 max(df_grp_elfs$Calories) # answer for #1 =71124
 
 # adding rank column
 df_grp_elfs$RANK <-nrow(df_grp_elfs) -  rank(df_grp_elfs$Calories) +1 

 df_top_three <- df_grp_elfs[which(df_grp_elfs$RANK <4),]
 
 sum(df_top_three$Calories)# answer for #2 =204639
 
 