library(tidyverse)
library(here)
library(janitor)

#READ IN DATA

#SB county plant observations
plants <- read_csv(here("data","all_sb_county_plant_observations.csv"))

#General characteristics of plants in SB County
char <- read_csv(here("data","all sb county plants characteristics.csv"))

#UCANR toxic plants - skin irritants
poison <- read_csv(here("data","UCANR Skin Irritant Plants Clean.csv"))

#join the characteristics to the observations
plant_char <- left_join(plants,char)

#import toxin code data, correct typo in codes
poison_codes <- read_csv(here("data","UCANR Poisonous Plants Metadata Key.csv"))|>
  mutate(Code = replace(Code, Meaning == "alkaloid", "A"))
  
############################################

#create data frame of toxic plants
#replacing toxin and plant part codes with meanings
poison_w_meaning <- poison |>
  left_join(filter(poison_codes, Column == "Toxic Part"), join_by("Toxic part 1" == "Code")) |>
  select(-"Toxic part 1", -Column) |>
  rename("Toxic part 1" = Meaning) |>
  left_join(filter(poison_codes, Column == "Toxic Part"), join_by("Toxic part 2" == "Code")) |>
  select(-"Toxic part 2", -Column) |>
  rename("Toxic part 2" = Meaning) |>
  left_join(filter(poison_codes, Column == "Toxic Part"), join_by("Toxic part 3" == "Code")) |>
  select(-"Toxic part 3", -Column) |>
  rename("Toxic part 3" = Meaning) |>
  left_join(filter(poison_codes, Column == "Toxins"), join_by("Toxin 1" == "Code")) |>
  select(-"Toxin 1", -Column) |>
  rename("Toxin 1" = Meaning) |>
  left_join(filter(poison_codes, Column == "Toxins"), join_by("Toxin 2" == "Code")) |>
  select(-"Toxin 2", -Column) |>
  rename("Toxin 2" = Meaning)

#add additional columns that combine all toxins or toxic parts
poison_w_meaning <- poison_w_meaning |>
  unite("Toxins", "Toxin 1", "Toxin 2", sep = ", ", na.rm = TRUE, remove = FALSE) |>
  unite("Toxic parts", "Toxic part 1", "Toxic part 2", "Toxic part 3", sep = ", ", na.rm = TRUE, remove = FALSE)

############################################

#create filtered data frames to use for joining
pwm_spp <- poison_w_meaning |>
  filter(Species == "spp.")

pwm_not_spp <- poison_w_meaning |>
  filter(Species != "spp.") |>
  select(-"Scientific name")

############################################

#ADDING TOXINS INFO TO OBSERVATIONS

#remove weird question mark character in observations data
plant_char$Taxon <- str_replace_all(plant_char$Taxon, "�", "")
plant_char$Species <- str_replace_all(plant_char$Species, "�", "")
  
#for toxic plants described by UCANR at the genus level,
#add toxins data to all species in the genus
plant_char2 <- plant_char |>
  left_join(pwm_spp, join_by(Genus), suffix = c("",".y")) |>
  select(-ends_with(".y"), -"Scientific name")

#for toxic plants described by UCANR at the species level,
#add toxins data based on the genus and species
plant_char3 <- plant_char2 |>
  rows_patch(pwm_not_spp, by = c("Genus", "Species"), unmatched = "ignore")

#replace blank cells with NAs
plant_char3[plant_char3 == ""] <- NA

#write to csv
write_csv(plant_char3, here("data","sb_obs_w_characteristics_toxins.csv"))

############################################

#ADDING TOXINS INFO TO CHARACTERISTICS

#remove weird question mark character in observations data
char$Taxon <- str_replace_all(char$Taxon, "�", "")
char$Species <- str_replace_all(char$Species, "�", "")
  
#for toxic plants described by UCANR at the genus level,
#add toxins data to all species in the genus
char2 <- char |>
  left_join(pwm_spp, join_by(Genus), suffix = c("",".y")) |>
  select(-ends_with(".y"), -"Scientific name") |>
#for toxic plants described by UCANR at the species level,
#add toxins data based on the genus and species
  rows_patch(pwm_not_spp, by = c("Genus", "Species"), unmatched = "ignore")

#replace blank cells with NAs
char2[char2 == ""] <- NA

#write to csv
write_csv(char2, here("data","sb_species_w_characteristics_toxins.csv"))
