setwd("D:/Learning/UpGrad/Investment Case Group Project")

minInvestimentValue <- 5000000
maxInvestimentValue <- 15000000

# read the datafiles to dataframes
companies <- read.table("companies.txt", header = TRUE, sep = "\t", quote = "", fill = TRUE, stringsAsFactors = F)
rounds2 <- read.csv("rounds2.csv", header = TRUE, quote = "", stringsAsFactors = F)

# permalink in companies and company_permalink in round2 has been identified as key
# make permalink and company_permalink to to upper case and create a new variable, not to modify the existing data
companies$permalink_upper <- toupper(companies$permalink)
rounds2$company_permalink_upper <- toupper(rounds2$company_permalink)


################ Checkpoint 1: Data Cleaning 1 #####################

# 1. How many unique companies are present in rounds2?
Unique_companies_round2 <- unique(rounds2$company_permalink_upper)
length(Unique_companies_round2)

# 2. How many unique companies are present in companies ?
Unique_companies <- unique(companies$permalink_upper)
length(Unique_companies)

# 3. In the companies data frame, which column can be used as the unique key for each company ? Write the name of the column.
#permalink

# 4. Are there any companies in the rounds2 file which are not present in companies? Answer yes or no: Y/N
setdiff(Unique_companies_round2, Unique_companies)

# output -0, Hence answer is No

# 5. Merge the two data frames so that all variables (columns) in the companies frame are added to the rounds2 data frame.
#Name the merged frame master_frame.How many observations are present in master_frame ?

# Change the company_permalink column name to permalink in rounds2 dataset
colnames(rounds2)[7] <- "permalink_Upper"
colnames(companies)[11] <- "permalink_Upper"

# merging the two frames
master_frame <- merge(companies, rounds2, by = "permalink_Upper")

################ Checkpoint 1: END #####################



################ Checkpoint 2: Funding Type Analysis #####################

# 1. Average funding amount of venture type
# We are not taking the venture with NA values in the raised amount.
venture_Companies <- subset(master_frame, funding_round_type == "venture")
mean(venture_Companies$raised_amount_usd, na.rm = TRUE)

# 2. Average funding amount of angel type
angel_Companies <- subset(master_frame, funding_round_type == "angel")
mean(angel_Companies$raised_amount_usd, na.rm = TRUE)


# 3. Average funding amount of seed type
seed_Companies <- subset(master_frame, funding_round_type == "seed")
mean(seed_Companies$raised_amount_usd, na.rm = TRUE)

# 4. Average funding amount of private equity type
private_equity_Companies <- subset(master_frame, funding_round_type == "private_equity")
mean(private_equity_Companies$raised_amount_usd, na.rm = TRUE)

# 5. Considering that Spark Funds wants to invest between 5 to 15 million USD per investment round, which investment type is the most suitable for it?
Group_by_funding_Type <- aggregate(master_frame$raised_amount_usd, list(master_frame$funding_round_type), mean, na.rm = TRUE)
rows_between_values <- which(Group_by_funding_Type$x >= minInvestimentValue & Group_by_funding_Type$x <= maxInvestimentValue)
funding_type_selected = Group_by_funding_Type[rows_between_values, 1]

################ Checkpoint 2: END #####################


################ Checkpoint 3: START #####################

Venture_Companies_Grouped_By_Country_Code <- aggregate(venture_Companies$raised_amount_usd, by = list(Category = venture_Companies$country_code), FUN = sum, na.rm = TRUE)
venture_Companies_Sorted <- Venture_Companies_Grouped_By_Country_Code[order(Venture_Companies_Grouped_By_Country_Code$x, decreasing = TRUE),]
top9 <- venture_Companies_Sorted[1:9,]

# ------------- Find English speaking countries ---------------#
# -------- find english speaking countries by code -----------#
install.packages("countrycode")
library(countrycode)
install.packages("readr")
library(readr)
english_speaking_countries <- scan("EngSpeakingCountries.txt", character(), sep ="\n")
country_code <- countrycode(c(english_speaking_countries),"country.name", "iso3c",nomatch = NA)
View(country_code)


## Look at which country from top 9 is english speaking 
top9_english_speaking_countries <- subset(top9, top9$Category %in% country_code)
top9_english_speaking_countries

top_english_speaking_country <- subset(top9_english_speaking_countries, x== max(x))
top_english_speaking_country$Category


second_english_speaking_country <- subset(top9_english_speaking_countries, x== max(x[x!=max(x)]))
second_english_speaking_country$Category 

third_english_speaking_country <- subset(top9_english_speaking_countries, x== min(x[x!=min(x)]))
third_english_speaking_country$Category
# --------------- Find English speaking countries END ---------------#
# ANS : USA,GBR,IND

################ Checkpoint 3: END #####################


################ Checkpoint 4: START #####################
library(tidyr)
master_frame_category_seperated <- separate(master_frame,category_list,into = c("category_list","Secondary Sector"),sep = "\\|",na.rm = TRUE)
master_frame_category_seperated$category_list_Upper <- toupper(master_frame_category_seperated$category_list)

mapping <- read.csv("mapping.csv", header = TRUE, stringsAsFactors = F)

#----------------- DATA cLEANING ---------------------------#
mapping$category_list <- gsub('0',"na",mapping[,1])
mapping$category_list[mapping$category_list == "Enterprise 2.na"] <- "Enterprise 2.0"

# -------------------- wide format to long format ----------------------#
mapping_long_format <- gather(mapping,MainCategory,MainCategory_value,2:10)
mapping_long_format <- mapping_long_format[!(mapping_long_format$MainCategory_value == 0),]
mapping_long_format$category_list_Upper <- toupper(mapping_long_format$category_list)

# merging the two frames
master_frame_Mapping <- merge(master_frame_category_seperated, mapping_long_format, by = "category_list_Upper",all.x = TRUE)

################ Checkpoint 4: END #####################


################ Checkpoint 5: START #####################
library("dplyr")

USA_Frame <-master_frame_Mapping[master_frame_Mapping$country_code == "USA" & master_frame_Mapping$funding_round_type == funding_type_selected & master_frame_Mapping$MainCategory != "Blanks" & !is.na(master_frame_Mapping$MainCategory),]
GBR_Frame <-master_frame_Mapping[master_frame_Mapping$country_code == "GBR" & master_frame_Mapping$funding_round_type == funding_type_selected & master_frame_Mapping$MainCategory != "Blanks" & !is.na(master_frame_Mapping$MainCategory),]
IND_Frame <-master_frame_Mapping[master_frame_Mapping$country_code == "IND" & master_frame_Mapping$funding_round_type == funding_type_selected & master_frame_Mapping$MainCategory != "Blanks" & !is.na(master_frame_Mapping$MainCategory),]


USA_Row_between_values <- which(USA_Frame$raised_amount_usd >= minInvestimentValue & USA_Frame$raised_amount_usd <= maxInvestimentValue)
D1 <- USA_Frame[USA_Row_between_values, ]

GBR_Row_between_values <- which(GBR_Frame$raised_amount_usd >= minInvestimentValue & GBR_Frame$raised_amount_usd <= maxInvestimentValue)
D2 <- GBR_Frame[GBR_Row_between_values, ]

IND_Row_between_values <- which(IND_Frame$raised_amount_usd >= minInvestimentValue & IND_Frame$raised_amount_usd <= maxInvestimentValue)
D3 <- IND_Frame[IND_Row_between_values, ]

D1_MainCategory_Fund_count <- aggregate(D1$MainCategory_value, by = list(D1$MainCategory), FUN = sum, na.rm = TRUE)
D2_MainCategory_Fund_count <- aggregate(D2$MainCategory_value, by = list(D2$MainCategory), FUN = sum, na.rm = TRUE)
D3_MainCategory_Fund_count <- aggregate(D3$MainCategory_value, by = list(D3$MainCategory), FUN = sum, na.rm = TRUE)

D1_MainCategory_Total_Amount_Invested <- aggregate(D1$raised_amount_usd, by = list(D1$MainCategory), FUN = sum)
D2_MainCategory_Total_Amount_Invested <- aggregate(D2$raised_amount_usd, by = list(D2$MainCategory), FUN = sum)
D3_MainCategory_Total_Amount_Invested <- aggregate(D3$raised_amount_usd, by = list(D3$MainCategory), FUN = sum)

# 1.Total number of Investments (count)
sum(D1_MainCategory_Fund_count$x)
sum(D2_MainCategory_Fund_count$x)
sum(D3_MainCategory_Fund_count$x)

# 2.Total amount of investment (USD)
sum(D1_MainCategory_Total_Amount_Invested$x)
sum(D2_MainCategory_Total_Amount_Invested$x)
sum(D3_MainCategory_Total_Amount_Invested$x)

# 3.Top Sector name (no. of investment-wise)
D1_Top_Sector <- D1_MainCategory_Fund_count[order(D1_MainCategory_Fund_count$x, decreasing = TRUE),][1,1]
D2_Top_Sector <- D2_MainCategory_Fund_count[order(D2_MainCategory_Fund_count$x, decreasing = TRUE),][1,1]
D3_Top_Sector <- D3_MainCategory_Fund_count[order(D3_MainCategory_Fund_count$x, decreasing = TRUE),][1,1]

# 4.Second Sector name (no. of investment-wise)
D1_MainCategory_Fund_count[order(D1_MainCategory_Fund_count$x, decreasing = TRUE),][2,1]
D2_MainCategory_Fund_count[order(D2_MainCategory_Fund_count$x, decreasing = TRUE),][2,1]
D3_MainCategory_Fund_count[order(D3_MainCategory_Fund_count$x, decreasing = TRUE),][2,1]

# 5.Third Sector name (no. of investment-wise)
D1_MainCategory_Fund_count[order(D1_MainCategory_Fund_count$x, decreasing = TRUE),][3,1]
D2_MainCategory_Fund_count[order(D2_MainCategory_Fund_count$x, decreasing = TRUE),][3,1]
D3_MainCategory_Fund_count[order(D3_MainCategory_Fund_count$x, decreasing = TRUE),][3,1]

# 6.Number of investments in top sector (3)
D1_MainCategory_Fund_count[order(D1_MainCategory_Fund_count$x, decreasing = TRUE),][1,2]
D2_MainCategory_Fund_count[order(D2_MainCategory_Fund_count$x, decreasing = TRUE),][1,2]
D3_MainCategory_Fund_count[order(D3_MainCategory_Fund_count$x, decreasing = TRUE),][1,2]

# 7.Number of investments in second sector (4)
D1_MainCategory_Fund_count[order(D1_MainCategory_Fund_count$x, decreasing = TRUE),][2,2]
D2_MainCategory_Fund_count[order(D2_MainCategory_Fund_count$x, decreasing = TRUE),][2,2]
D3_MainCategory_Fund_count[order(D3_MainCategory_Fund_count$x, decreasing = TRUE),][2,2]


# 8.Number of investments in third sector (5)
D1_MainCategory_Fund_count[order(D1_MainCategory_Fund_count$x, decreasing = TRUE),][3,2]
D2_MainCategory_Fund_count[order(D2_MainCategory_Fund_count$x, decreasing = TRUE),][3,2]
D3_MainCategory_Fund_count[order(D3_MainCategory_Fund_count$x, decreasing = TRUE),][3,2]

# 9.For point 3 (top sector count-wise), which company received the highest investment?
USA_Frame_Top_Sector <-D1[D1$MainCategory == D1_Top_Sector,]
GBR_Frame_Top_Sector <-D2[D2$MainCategory == D2_Top_Sector,]
IND_Frame_Top_Sector <-D3[D3$MainCategory == D3_Top_Sector,]

D1_Raised_Fund <- aggregate(USA_Frame_Top_Sector$raised_amount_usd, by = list(USA_Frame_Top_Sector$name), FUN = sum)
D2_Raised_Fund <- aggregate(GBR_Frame_Top_Sector$raised_amount_usd, by = list(GBR_Frame_Top_Sector$name), FUN = sum)
D3_Raised_Fund <- aggregate(IND_Frame_Top_Sector$raised_amount_usd, by = list(IND_Frame_Top_Sector$name), FUN = sum)

D1_Raised_Fund[order(D1_Raised_Fund$x, decreasing = TRUE),][1,1]
D2_Raised_Fund[order(D2_Raised_Fund$x, decreasing = TRUE),][1,1]
D3_Raised_Fund[order(D3_Raised_Fund$x, decreasing = TRUE),][1,1]


# 10.For point 4 (second best sector count-wise), which company received the highest investment?
D1_Raised_Fund[order(D1_Raised_Fund$x, decreasing = TRUE),][2,1]
D2_Raised_Fund[order(D2_Raised_Fund$x, decreasing = TRUE),][2,1]
D3_Raised_Fund[order(D3_Raised_Fund$x, decreasing = TRUE),][2,1]

################ Checkpoint 5: END #####################