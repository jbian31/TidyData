library(dplyr)
library(tidyr)
library(stringr)
#test dataset
df1 <- read.table("~/R/project1/test/x_test.txt") %>% tbl_df()
df2 <- read.table("~/R/project1/test/y_test.txt") %>% tbl_df() 
df3 <- read.table("~/R/project1/test/subject_test.txt") %>% tbl_df() 

dff <- read.table("~/R/project1/features.txt") %>% tbl_df() 
df_col <- dff[str_detect(dff$V2,(c("mean","std"))),]

df1 <-  df1[,df_col$V1]
names(df1) <- df_col$V2
df_test <- cbind(df3,df2,df1)
names(df_test) [1:2] <- c("Subject","Activity") 

#train dataset
df1 <- read.table("~/R/project1/train/x_train.txt") %>% tbl_df()
df2 <- read.table("~/R/project1/train/y_train.txt") %>% tbl_df() 
df3 <- read.table("~/R/project1/train/subject_train.txt") %>% tbl_df() 

df1 <-  df1[,df_col$V1]
names(df1) <- df_col$V2
df_train <- cbind(df3,df2,df1)
names(df_train) [1:2] <- c("Subject","Activity") 

#combine train and test dataset
df <- rbind(df_test,df_train) %>% unique()
rm(df1,df2,df3)

#add activity name
df1 <- read.table("~/R/project1/activity_labels.txt") %>% tbl_df()
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

View(df_result)  
