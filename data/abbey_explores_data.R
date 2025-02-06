library(tidyverse)
library(here)

plants <- read_csv(here("data","all_sb_county_plant_observations.csv"))

char <- read_csv(here("data","all sb county plants characteristics.csv"))

plant_char <- left_join(plants,char)

poison <- read_csv(here("data","UCANR Skin Irritant Plants Clean.csv"))

poison_codes <- read_csv(here("data","UCANR Poisonous Plants Metadata Key.csv"))

poison_long <- poison |>
  pivot_longer(starts_with("Toxi"),
               names_to="Column",
               values_to="Code"
               ) |>
  mutate(Column=ifelse(Column %in% c("Toxic part 1","Toxic part 2","Toxic part 3"), "Toxic Part", "Toxins")) |>
  drop_na()

poison_w_code <- left_join(poison_long,poison_codes)