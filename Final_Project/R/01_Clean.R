# Cleaning

## Setup
library(tidyverse)
library(tidyr)
library(tidyverse)
library(skimr)

#read in Data/Updated_DSR.csv
df <- read.csv("./Data/Updated_DSR.csv")

#take out that first row of dates
df <- df %>% 
  slice(-1)

View(df)

#lengthen data to make column named Block and DSR
df_long <- df %>% 
  pivot_longer(
    cols = starts_with("B"),
    names_to = "Block",
    values_to = "DSR"
  )

#create new column called "Season". 
#assign 1 for B1:B3.1
df_long <- df_long %>% 
  mutate(Season = ifelse(Block %in% c("B1", "B2", "B3", "B1.1", "B2.1", "B3.1"), 1, 2)) %>% 
  mutate(Season = as.factor(Season)) %>% #make growing season into a factor
  relocate(Season, .after = X) %>% 
  rename(Accession_ID = X) 

#assign the "B1.1" etc to a replicate - that's essentially what they are
df_long <- df_long %>% 
  mutate(Rep = case_when(
    Block %in% c("B1", "B2", "B3") ~ "1",
    Block %in% c("B1.1", "B2.1", "B3.1") ~ "2",
    Block %in% c("B1.2", "B2.2", "B3.2") ~ "3",
    Block %in% c("B1.3", "B2.3", "B3.3") ~ "4",
    Block %in% c("B1.4", "B2.4", "B3.4") ~ "5",
  ))

#now let's get rid of the .whatever in the Block column
df_long <- df_long %>%
  mutate(Block = sub("\\..*", "", Block))

#now let's relocate the rep column
df_long <- df_long %>% 
  relocate(Rep, .after = Block)

#remove B prefix in Block
df_long$Block <- gsub("^B", "", df_long$Block)

#look over df_long for problems
skim(data = df_long)

#Make a cleaned csv
write_csv(df_long, "./Data/cleaned_DSR.csv")

#now that DSR is cleaned, let's import and tidy the origin sheet
#this shows, ID, Accession Number, Origin, and Species


#Import Origin_sheet
origin_df <- read_csv("./Data/Origin_Sheet.csv")

View(origin_df)

#rename ID
origin_df <- origin_df %>% 
  rename(ID = 'ID #') 

#remove"S24-" prefix from the ID column
origin_df$ID <- str_replace(origin_df$ID, "^S24-", "")

#remove Brassica prefix from species column
origin_df$Species <- str_replace(origin_df$Species, "^Brassica ","")

#keep a copy of Accession for table later on
Accession <- origin_df

#drop accession column, not necessary for this - only useful for book keeping
origin_df <- origin_df %>% 
  dplyr::select(-Accession)

#save cleaned origin data set
write.csv(origin_df, "./Data/cleaned_origin_sheet.csv", row.names = FALSE)
#save copy with long accession number
write.csv(Accession, "./Data/cleaned_origin_sheet_Accession.csv", row.names = FALSE)

#combine the two 
#read in the two
df_origin <- read.csv("./Data/cleaned_origin_sheet.csv", stringsAsFactors = FALSE)
df_dsr <- read.csv("./Data/cleaned_DSR.csv", stringsAsFactors = FALSE)

#merge the two
df_merged <- df_dsr %>%
  left_join(df_origin, by = c("Accession_ID" = "ID"))

#check up to see how it looks
glimpse(df_merged)

#save the merged data set
write.csv(df_merged, "./Data/Merged_DSR_Origin_Dataset.csv", row.names = FALSE)


#repeat this for one with Accession included:
df_accession <- read.csv("./Data/cleaned_origin_sheet_Accession.csv", stringsAsFactors = FALSE)
df_dsr <- read.csv("./Data/cleaned_DSR.csv", stringsAsFactors = FALSE)

# Convert ID columns to the same type (character)
df_accession$ID <- as.character(df_accession$ID)
df_dsr$Accession_ID <- as.character(df_dsr$Accession_ID)

#merge the two
df_merged_1 <- df_dsr %>%
  left_join(df_accession, by = c("Accession_ID" = "ID"))

#save the merged data set
write.csv(df_merged_1, "./Data/Merged_DSR_Origin_Dataset_Accession.csv", row.names = FALSE)



#give me a list of the accessions that had the lowest average DSR
low_dsr <- df_merged_1 %>%
  filter(!is.na(DSR), !is.na(Species)) %>%
  group_by(Accession_ID, Species) %>%
  summarise(Avg_DSR = mean(DSR, na.rm = TRUE), .groups = "drop") %>%
  arrange(Avg_DSR)

write.csv(low_dsr, "./Data/low_dsr.csv", row.names = FALSE)

#combine Accession in Merged with low DSR
# Merge to get Accession names
merged_low_dsr <- low_dsr %>%
  left_join(df_merged_1 %>% select(Accession_ID, Accession), by = "Accession_ID") %>%
  distinct(Accession_ID, .keep_all = TRUE)

# View result
print(merged_low_dsr)

#save this to make table later on
write.csv(merged_low_dsr, "./Data/low_dsr_with_accession", row.names = FALSE)
