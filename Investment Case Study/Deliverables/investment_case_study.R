
# Checkpoint 1: Data Cleaning 1
install.packages("tidyr")
install.packages("dplyr")
install.packages("sqldf")
install.packages("xlsx")
install.packages("writexl")
install.packages("stringr")

library(tidyr)
library(dplyr)
library(sqldf)
library(xlsx)
library(writexl)
library("stringr")

# Checkpoint-1.1: Load the companies data into ‘companies’ data frame.

# Step1: Read companies.txt file in R console using following function;

companies<-read.delim("companies.txt",header=TRUE,stringsAsFactors=FALSE)

# Checkpoint-1.2: Load the rounds2 data into ‘rounds2’ data frame.

# Step 1: Read rounds2.csv file in R console using following function;

rounds2 <- read.csv("rounds2.csv", header=TRUE,stringsAsFactors = FALSE)

# Checkpoint-1.3: How many unique companies are present in rounds2?

# Step 1: Convert ‘company_permalink’ column data into lower case from rounds2 data frame.

rounds2_lower <- rounds2 %>%  mutate(company_permalink = tolower(company_permalink))

# Step 2: Find the distinct records in ‘company_permalink’ column

distinct_count_companies_rounds2 <- sqldf("select count(distinct(company_permalink)) from rounds2_lower")


# Checkpoint- 1.4: How many unique companies present in ‘companies’ dataframe.

# Step 1: Convert ‘permalink’ column into lower case from ‘companies’ data frame.

companies_lower <- companies %>%  mutate(permalink = tolower(permalink))

# Step 2: Find the distinct records in ‘permalink’ column from ‘companies’ data frame.

distinct_count_companies <- sqldf("select count(distinct(permalink)) from companies")


# Checkpoint- 1.5: Are there any companies in the rounds2 file which are not present in companies? Answer yes or no: Y/N

# Step 1: This function will compare column from rounds2 & companies, if it matches then it will return 0

length(rounds2_lower$company_permalink[!(rounds2_lower$company_permalink %in% companies_lower$permalink)])

# Checkpoint- 1.6: Merge the two data frames so that all variables (columns) in the companies frame are added to the rounds2 data frame. Name the merged frame master_frame. How many observations are present in master_frame?

master_frame <- merge(rounds2_lower, companies_lower, by.x=c("company_permalink"),by.y=c("permalink"),all.x=TRUE)


# Checkpoint 2: Funding Type Analysis
# Checkpoint- 2.1: 

# Step 1: Calculate the mean master_frame$raised_amount_usd

mean(master_frame$raised_amount_usd,na.rm=T)

# Step 2: assign all the mean values in the NA values

master_frame$raised_amount_usd[is.na(master_frame$raised_amount_usd)] <- 10426869

# Step 3: find the average funding based on funding type;
average_funding <-aggregate(raised_amount_usd~funding_round_type,master_frame,mean)

# Step 4: Average funding amount of venture type
average_funding_venture<- filter(average_funding,funding_round_type=="venture")


# Step 5: Average funding amount of seed type
average_funding_seed<- filter(average_funding,funding_round_type=="seed")

# Step 6: Average funding amount of angel type
average_funding_angel<- filter(average_funding,funding_round_type=="angel")


# Step 7: Average funding amount of private equity type
average_funding_private_equity<- filter(average_funding,funding_round_type=="private_equity")


# Step 8: Find funding type which has funding >5000000 and funding <15000000
average_funding_type <- sqldf('select funding_round_type, raised_amount_usd from average_funding where raised_amount_usd > 5000000 AND raised_amount_usd < 15000000 AND funding_round_type IN ("seed", "venture", "angel", "private equity")')


# Checkpoint 3: Country Analysis


# Checkpoint 3.1- Spark Funds wants to see the top nine countries which have received the highest total funding (across ALL sectors for the chosen investment type)

# Step 1: Filter master_frame based on venture

master_frame <- filter(master_frame,funding_round_type=="venture")

# Step 2: Finding out the country raised highest amount 
top9 <-aggregate(raised_amount_usd~country_code,master_frame,sum)

# Step 3: Decreasing by their value
top9 <- top9[order(top9$raised_amount_usd,decreasing = T),]
# Step 4: Removing the blank values 
top9 <-top9[!(top9$country_code==""),]
# Step 5: Top 9 countries
top9 <- head(top9,9)


# Checkpoint 4: Sector Analysis 1


# Considering funding type investment as 'Venture'


# Checkpoint 4.1- Extract the primary sector of each category list from the category_list column

# Step 1: Analysis to create dataframe with primary_sector from master_frame.

master_frame1 <- separate(master_frame, category_list, into = c("primary_sector", "other_sector"), sep = "\\|", extra = "merge", fill = "right")

# Checkpoint 4.2- Use the mapping file 'mapping.csv' to map each primary sector to one of the eight main sectors (Note that ‘Others’ is also considered one of the main sectors)


# Step 1: read mapping.csv file from current set directory;

mapping <- read.csv("mapping.csv", header = TRUE, stringsAsFactors = FALSE)

# Step 2: convert wide to long data format

mapping_long <- gather(mapping, main_sector, my_value, "Automotive...Sports":"Social..Finance..Analytics..Advertising")

# Step 3: remove '0' from my_value

mapping_long <- mapping_long[!(mapping_long$my_value==0),]

# Step 4: checked my_value has only '1' as value so that i can remove from dataframe.
filter(mapping_long, my_value!=1)

# Step 5: remove 3rd column 'my_value' from mapping_long_splitsector dataframe
mapping_long <- mapping_long[ ,-3]  

# Step 6: there are 0 character in string on category_list, for eg: a0lytics, ma0gement. to clean this dataframe, we use the following function;

mapping_long[] <- lapply(mapping_long, function(x) as.character(gsub("[0,]", "na", x)))

mapping_long$category_list<-str_replace(mapping_long$category_list,"Enterprise 2.na","Enterprise 2.0")


# Step 7: Remove "Blank" from main_sector column

mapping_long_remove_blank <- mapping_long[!(mapping_long$main_sector == "Blanks"),]
	
# Step 8:  convert ‘category_list’ column to lower case from ‘mapping_long_remove_blank’ data frame.

mapping_long_lower <- mapping_long_remove_blank %>%  mutate(category_list = tolower(category_list))

# Step 9: Now convert ‘primary_sector’ column to lower case from ‘master_frame1’ data frame.

master_frame1_lower <- master_frame1 %>%  mutate(primary_sector = tolower(primary_sector))


# Step 10: merge ‘mapping_long_lower’ data frame with ‘master_frame1_lower’, so that sectors are mapped with ‘primary category’.


master_frame2 <- merge(master_frame1_lower, mapping_long_lower, by.x=c("primary_sector"),by.y=c("category_list"),all.x=TRUE
)

# Step 11: Check total count of sector;

sqldf("select distinct(main_sector) from master_frame2")


# Step 12: Remove empty spaces from primary_sector column

master_frame3<- master_frame2[!(!is.na(master_frame2$primary_sector) & master_frame2$primary_sector ==""), ]

# Step 13: Drop NA from "main_sector" column

master_frame3 <- master_frame3 %>% drop_na(main_sector)


# Checkpoint 5: Sector Analysis 2


# Checkpoint 5.1-Create three separate data frames D1, D2 and D3 for each of the three countries containing the observations of funding type FT falling within the 5-15 million USD range. The three data frames should contain:



# Step 1: 

D1 <- sqldf('select * from master_frame3 where country_code = "USA" AND raised_amount_usd > 5000000 AND raised_amount_usd < 15000000  ')


# Step 2: 

D2 <- sqldf('select * from master_frame3 where country_code = "GBR" AND raised_amount_usd > 5000000 AND raised_amount_usd < 15000000  ')

# Step 3: 

D3 <- sqldf('select * from master_frame3 where country_code = "IND" AND raised_amount_usd > 5000000 AND raised_amount_usd < 15000000  ')


# Step 4: To find the total number investment count for each main sector for D1.

d1_tot_inv_count<- D1 %>% group_by(main_sector) %>% summarise(raised_amount_usd = n())

# Step 5: arrange 'raised_amount_usd count in descending order;

arrange(d1_tot_inv_count, desc(raised_amount_usd))

# Step 6: Finally find the total number investment count for D1
sum(d1_tot_inv_count$raised_amount_usd)


# Step 7: To find the total # of investment count for D2.

d2_tot_inv_count<- D2 %>% group_by(main_sector) %>% summarise(raised_amount_usd = n())

# Step 8: arrange 'raised_amount_usd count in descending order;
arrange(d2_tot_inv_count, desc(raised_amount_usd))


# Step 9: Finally find the total # of investment count for D2
sum(d2_tot_inv_count$raised_amount_usd)


# Step 10: To find the total # of investment count for D3.

d3_tot_inv_count<- D3 %>% group_by(main_sector) %>% 
summarise(raised_amount_usd = n()) 

# Step 11: arrange 'raised_amount_usd count in descending order;

arrange(d3_tot_inv_count, desc(raised_amount_usd))


# Step 12: Finally find the total# of investment count for D3
sum(d3_tot_inv_count$raised_amount_usd)


# Step 13: Find the total amount invested sector wise for D1.
# Step 13.1: Group by main_sector for D1
d1_groupby_main_sector <- group_by(D1, main_sector)

# Step 13.2: Find total amount invested;
d1_tot_amt_inv <- summarise(d1_groupby_main_sector, sum(raised_amount_usd, na.rm= TRUE))

# Step 13.3: change the end column name to tot_amt_invested

colnames(d1_tot_amt_inv)[2] <- "amt_invested"


# Step 13.4: arrange in descending order of "amt_invested"

d1_tot_amt_inv <- arrange(d1_tot_amt_inv, desc(amt_invested))


# Step 13.5: Finally find the total amount investment main sector wise for D1
sum(d1_tot_amt_inv$amt_invested)

# Step 14: Find the total amount invested main sector wise for D2.
# Step 14.1: Group by main_sector for D2
d2_groupby_main_sector <- group_by(D2, main_sector)

# Step 14.2: Find total amount invested;
d2_tot_amt_inv <- summarise(d2_groupby_main_sector, sum(raised_amount_usd, na.rm= TRUE))

# Step 14.3: change the end column name to amt_invested

colnames(d2_tot_amt_inv)[2] <- "amt_invested"


# Step 14.4: arrange in descending order of 'funding'

d2_tot_amt_inv <- arrange(d2_tot_amt_inv, desc(amt_invested))


# Step 14.5: Finally find the total amount investment main sector wise for D2
sum(d2_tot_amt_inv$amt_invested)


# Step 15: Find the total amount invested sector wise for D3.
# Step 15.1: Group by main_sector for D3
d3_groupby_main_sector <- group_by(D3, main_sector)

# Step 15.2: Find total amount invested;
d3_tot_amt_inv <- summarise(d3_groupby_main_sector, sum(raised_amount_usd, na.rm= TRUE))

# Step 15.3: change the end column name to tot_amt_invested

colnames(d3_tot_amt_inv)[2] <- "amt_invested"


# Step 15.4: arrange in descending order of 'funding'

d3_tot_amt_inv <- arrange(d3_tot_amt_inv, desc(amt_invested))

# Step 15.5: Finally find the total amount investment main sector wise for D3
sum(d3_tot_amt_inv$amt_invested)


# Step 16: Find which company received the highest investment (Top sector count wise) for D1

# Step 16.1 filter D1 based on top sector i.e “Others”;

filter_others_d1 <- sqldf('select * from D1 where main_sector = "Others" ')

# Step 16.1.1 group by name

filter_others_d1_group <- group_by(filter_others_d1, name)


# Step 16.2: Find the company with highest no. of investment; 

filter_others_d1_highest_investment <- summarise(filter_others_d1_group, sum(raised_amount_usd, na.rm= TRUE))
# Step 16.3: change the end column name to tot_amt_invested

colnames(filter_others_d1_highest_investment)[2] <- "amt_invested"

filter_others_d1_highest_investment <- arrange(filter_others_d1_highest_investment, desc(amt_invested))



# Step 17: Find which company received the highest investment (Top sector count wise) for D2

# Step 17.1 filter D2 based on top sector i.e “Others”;

filter_others_d2 <- sqldf('select * from D2 where main_sector = "Others" ')

# Step 17.1.1 group by name

filter_others_d2_group <- group_by(filter_others_d2, name)


# Step 17.2: Find the company with highest no. of investment; 

filter_others_d2_highest_investment <- summarise(filter_others_d2_group, sum(raised_amount_usd, na.rm= TRUE))
# Step 17.3: change the end column name to tot_amt_invested

colnames(filter_others_d2_highest_investment)[2] <- "amt_invested"

filter_others_d2_highest_investment <- arrange(filter_others_d2_highest_investment, desc(amt_invested))


# Step 18: Find which company received the highest investment (Top sector count wise) for D3

# Step 18.1 filter D3 based on top sector i.e “Others”;

filter_others_d3 <- sqldf('select * from D3 where main_sector = "Others" ')

# Step 18.1.1 group by name

filter_others_d3_group <- group_by(filter_others_d3, name)


# Step 18.2: Find the company with highest no. of investment; 

filter_others_d3_highest_investment <- summarise(filter_others_d3_group, sum(raised_amount_usd, na.rm= TRUE))
# Step 18.3: change the end column name to tot_amt_invested

colnames(filter_others_d3_highest_investment)[2] <- "amt_invested"

filter_others_d3_highest_investment <- arrange(filter_others_d3_highest_investment, desc(amt_invested))

# Step 19: Find which company received the highest investment (2nd Top sector count wise) for D1

# Step 19.1 filter D1 based on top sector i.e “Social..Finance..Analytics..Advertising”;

filter_social_d1 <- sqldf('select * from D1 where main_sector = "Social..Finance..Analytics..Advertising" ')

# Step 19.1.1 group by name

filter_social_d1_group <- group_by(filter_social_d1, name)


# Step 19.2: Find the company with highest no. of investment; 

filter_social_d1_highest_investment <- summarise(filter_social_d1_group, sum(raised_amount_usd, na.rm= TRUE))
# Step 19.3: change the end column name to tot_amt_invested

colnames(filter_social_d1_highest_investment)[2] <- "amt_invested"

filter_social_d1_highest_investment <- arrange(filter_others_d1_highest_investment, desc(amt_invested))



# Step 20: Find which company received the highest investment (Top sector count wise) for D2

# Step 20.1 filter D2 based on top sector i.e “Social..Finance..Analytics..Advertising”;

filter_social_d2 <- sqldf('select * from D2 where main_sector = "Social..Finance..Analytics..Advertising" ')

# Step 20.1.1 group by name

filter_social_d2_group <- group_by(filter_social_d2, name)


# Step 20.2: Find the company with highest no. of investment; 

filter_social_d2_highest_investment <- summarise(filter_social_d2_group, sum(raised_amount_usd, na.rm= TRUE))
# Step 20.3: change the end column name to tot_amt_invested

colnames(filter_social_d2_highest_investment)[2] <- "amt_invested"

filter_social_d2_highest_investment <- arrange(filter_social_d2_highest_investment, desc(amt_invested))


# Step 21: Find which company received the highest investment (Top sector count wise) for D3

# Step 21.1 filter D3 based on top sector i.e “Social..Finance..Analytics..Advertising”;

filter_social_d3 <- sqldf('select * from D3 where main_sector = "Social..Finance..Analytics..Advertising" ')

# Step 21.1.1 group by name

filter_social_d3_group <- group_by(filter_social_d3, name)


# Step 21.2: Find the company with highest no. of investment; 

filter_social_d3_highest_investment <- summarise(filter_social_d3_group, sum(raised_amount_usd, na.rm= TRUE))
# Step 21.3: change the end column name to tot_amt_invested

colnames(filter_social_d3_highest_investment)[2] <- "amt_invested"

filter_social_d3_highest_investment <- arrange(filter_social_d3_highest_investment, desc(amt_invested))















































