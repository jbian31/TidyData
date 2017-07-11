library(dplyr)
library(tidyr)
library(stringr)

#download
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", "data.zip")
unzip("data.zip",exdir="data")
setwd("./data/UCI HAR Dataset")

#test dataset
df1 <- read.table(paste(getwd(),"/test/X_test.txt", sep="")) %>% tbl_df()
df2 <- read.table(paste(getwd(),"/test/Y_test.txt", sep="")) %>% tbl_df() 
df3 <- read.table(paste(getwd(),"/test/subject_test.txt", sep="")) %>% tbl_df() 

dff <- read.table(paste(getwd(),"/features.txt", sep="")) %>% tbl_df() 
df_col <- dff[str_detect(dff$V2,(c("mean","std"))),]

df1 <-  df1[,df_col$V1]
names(df1) <- df_col$V2
df_test <- cbind(df3,df2,df1)
names(df_test) [1:2] <- c("Subject","Activity") 

#train dataset
df1 <- read.table(paste(getwd(),"/train/X_train.txt", sep="")) %>% tbl_df()
df2 <- read.table(paste(getwd(),"/train/Y_train.txt", sep="")) %>% tbl_df() 
df3 <- read.table(paste(getwd(),"/train/subject_train.txt", sep="")) %>% tbl_df() 

df1 <-  df1[,df_col$V1]
names(df1) <- df_col$V2
df_train <- cbind(df3,df2,df1)
names(df_train) [1:2] <- c("Subject","Activity") 

#combine train and test dataset
df <- rbind(df_test,df_train) %>% unique()
rm(df1,df2,df3)

#add activity name
df1 <- read.table(paste(getwd(),"/activity_labels.txt", sep="")) %>% tbl_df()
df <- merge(df1,df,by.x = "V1", by.y ="Activity", all.x = TRUE) %>% select(-1)

names(df)[1] = c("Activity")
rm(df1)

#tidy the columns
df_final <- gather(df,Feature_Signal_Axial,Measurement,-c(1:2)) %>%
       separate(col = Feature_Signal_Axial,c("Feature","Signal","Axial")) %>%
       unique() %>%      
       mutate(Rank=rank(Measurement, ties.method = "min")) %>%
       spread(Signal,Measurement)

#run statistic
df_result <- group_by(df_final,Activity,Subject,Feature,Axial) %>%
             summarize(mean_mean = mean(mean, na.rm = TRUE)
                       , mean_meanFreq = mean(meanFreq, na.rm = TRUE)
                       ,mean_std = mean(std, na.rm = TRUE))

write.table(df_result,paste(getwd(),"/Result_Data.txt", sep=""), row.name=FALSE) 
