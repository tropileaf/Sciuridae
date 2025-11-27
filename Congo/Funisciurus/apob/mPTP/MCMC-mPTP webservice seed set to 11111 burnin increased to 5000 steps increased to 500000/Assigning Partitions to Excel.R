### Assigning Congolese Funisciurus spp MOTUS based on ASAP and mPTP outputs

## Only strictly overlapping sequences representing unique haplotypes
## APOB and CYTB

library(readxl)
library(dplyr)
library(openxlsx)
library(stringr)

### Need to reset wd to subfolders where necessary
### Need to convert mPTP output files to csv

### ---- Helper function to clean sequence names ---- ###
clean_name <- function(x) {
  x %>%
    toupper() %>%          # standardize case
    str_trim() %>%         # remove whitespace
    str_replace("_.*$", "")  # remove everything after first "_"
} #manually remove any '?'

### ---- 1.Load files ---- ###

# Excel (contains Sample_ID and empty APOB_ASAP_OTU)
df_excel <- read_excel("Corrections Article Genetique Ecureuils LF_PB_LF_jm2_LF2.xlsx") # or latest version

#APOB_ASAP

# CSV (no header)
df_csv <- read.csv("Unique_Overlapping_One_Outgroup.fas.Partition_1.csv", header = FALSE, stringsAsFactors = FALSE) # or the ASAP partition file you need
colnames(df_csv) <- c("SeqName", "SpeciesNum")

### ---- 2. Clean names ---- ###

df_excel <- df_excel %>%
  mutate(TrimmedName = clean_name(Sample_ID)) # not needed once your excel file contains the trimmed names

df_csv <- df_csv %>%
  mutate(TrimmedName = clean_name(SeqName))

### ---- 3. Species number → species label ---- ###

species_map <- c( # adjust as shown on partition file
  "1" = "Fa",
  "2" = "Fp_RB",
  "3" = "Fx",
  "4" = "Fb",
  "5" = "P_LB"
)

df_csv$SpeciesLabel <- species_map[as.character(df_csv$SpeciesNum)]

### ---- 4. Exact matching ---- ###

matched <- df_excel %>%
  left_join(df_csv %>% select(TrimmedName, SpeciesLabel),
            by = "TrimmedName")

### ---- 5. Report unmatched CSV names ---- ###

unmatched <- anti_join(df_csv, df_excel, by = "TrimmedName")

if (nrow(unmatched) > 0) {
  message("The following CSV sequence names could NOT be matched:\n") # manually fill in excel file at the end
  print(unmatched)
} else {
  message("All CSV names matched successfully!")
}

### ---- 6. Fill APOB_ASAP_OTU with species label ---- ###

df_excel$APOB_ASAP_OTU <- matched$SpeciesLabel

### ---- 7. Save output ---- ###

write.xlsx(df_excel, "excel_with_species_filled.xlsx", overwrite = TRUE)








### ---- 1. Repeat with APOB_mPTP ---- ###

df_excel <- read_excel("excel_with_species_filled.xlsx")

# CSV (no header)
APOBmPTP <- read_delim("APOBmPTP.csv", delim = ";", 
                       escape_double = FALSE, trim_ws = TRUE)
View(APOBmPTP)
df_csv <-APOBmPTP

#df_csv <- read.csv("APOBmPTP.csv", header = FALSE, stringsAsFactors = FALSE)
colnames(df_csv) <- c("SeqName", "SpeciesNum")

### ---- 2. Clean names ---- ###

#df_excel <- df_excel %>%
 # mutate(TrimmedName = clean_name(Sequence_Name))

df_csv <- df_csv %>%
  mutate(TrimmedName = clean_name(SeqName))

### ---- 3. Species number → species label ---- ###

species_map <- c(
  "2" = "Fa",
  "3" = "Fb",
  "1" = "P_LB"
)

df_csv$SpeciesLabel <- species_map[as.character(df_csv$SpeciesNum)]

### ---- 4. Exact matching ---- ###

matched <- df_excel %>%
  left_join(df_csv %>% select(TrimmedName, SpeciesLabel),
            by = "TrimmedName")

### ---- 5. Report unmatched CSV names ---- ###

unmatched <- anti_join(df_csv, df_excel, by = "TrimmedName")

if (nrow(unmatched) > 0) {
  message("The following CSV sequence names could NOT be matched:\n") # manually fill in excel file at the end
  print(unmatched)
} else {
  message("All CSV names matched successfully!")
}

### ---- 6. Fill APOB_ASAP_OTU with species label ---- ###

df_excel$APOB_mPTP_OTU <- matched$SpeciesLabel

### ---- 7. Save output ---- ###

write.xlsx(df_excel, "excel_with_species_filled_APOB.xlsx", overwrite = TRUE)





### ----- 1. Repeat with CYTB ASAP ----- ###

setwd("~/Documents/GitHub/Sciuridae/Congo/Funisciurus/cytb including missing sequences/asap abgd/eb8f1076-7786-4440-969e-6c45eedab308_20251126/asap")

# Excel (contains Sample_ID and empty CYTB_ASAP_OTU)
df_excel <- read_excel("excel_with_species_filled_APOB.xlsx")

#CYTB_ASAP

# CSV (no header)
df_csv <- read.csv("Unique_Missing_Overlap.fas.Partition_1.csv", header = FALSE, stringsAsFactors = FALSE)
colnames(df_csv) <- c("SeqName", "SpeciesNum")

### ---- 2. Clean names ---- ###

#df_excel <- df_excel %>%
 # mutate(TrimmedName = clean_name(Sample_ID))

df_csv <- df_csv %>%
  mutate(TrimmedName = clean_name(SeqName))

### ---- 3. Species number → species label ---- ###

species_map <- c(
  "1" = "Fa_LB",
  "2" = "Fa_RB",
  "3" = "Fc",
  "4" = "Fp_RB",
  "6" = "GenBank",
  "8" = "Fx",
  "9" = "P_LB",
  "7" = "Fb_sp1",
  "5" = "Fb_sp2"
)

df_csv$SpeciesLabel <- species_map[as.character(df_csv$SpeciesNum)]

### ---- 4. Exact matching ---- ###

matched <- df_excel %>%
  left_join(df_csv %>% select(TrimmedName, SpeciesLabel),
            by = "TrimmedName")

### ---- 5. Report unmatched CSV names ---- ###

unmatched <- anti_join(df_csv, df_excel, by = "TrimmedName")

if (nrow(unmatched) > 0) {
  message("The following CSV sequence names could NOT be matched:\n") # manually fill in excel file at the end
  print(unmatched)
} else {
  message("All CSV names matched successfully!")
}

### ---- 6. Fill APOB_ASAP_OTU with species label ---- ###

df_excel$CYTB_ASAP_OTU <- matched$SpeciesLabel

### ---- 7. Save output ---- ###

write.xlsx(df_excel, "excel_with_species_filled_CYTP_ASAP.xlsx", overwrite = TRUE)








### ---- 8. Repeat with CYTB_mPTP ---- ###

df_excel <- read_excel("excel_with_species_filled_CYTP_ASAP.xlsx")

# CSV (no header)
CYTBmPTP <- read_delim("CYTBmPTP.csv", delim = ";", # first, manually make csv using the output file
                       escape_double = FALSE, trim_ws = TRUE)
View(CYTBmPTP)
df_csv <-CYTBmPTP

#df_csv <- read.csv("APOBmPTP.csv", header = FALSE, stringsAsFactors = FALSE)
colnames(df_csv) <- c("SeqName", "SpeciesNum")

### ---- 2. Clean names ---- ###

#df_excel <- df_excel %>%
 # mutate(TrimmedName = clean_name(Sequence_Name))

df_csv <- df_csv %>%
  mutate(TrimmedName = clean_name(SeqName))

### ---- 3. Species number → species label ---- ###

species_map <- c(
  "2" = "Fx",
  "3" = "Fa_LB",
  "4" = "Fa_RB",
  "5" = "Fc",
  "6" = "Fp_RB",
  "7" = "Fb_sp2",
  "8" = "Genbank",
  "9" = "Fb_sp1",
  "1" = "P_LB"
)

df_csv$SpeciesLabel <- species_map[as.character(df_csv$SpeciesNum)]

### ---- 4. Exact matching ---- ###

matched <- df_excel %>%
  left_join(df_csv %>% select(TrimmedName, SpeciesLabel),
            by = "TrimmedName")

### ---- 5. Report unmatched CSV names ---- ###

unmatched <- anti_join(df_csv, df_excel, by = "TrimmedName")

if (nrow(unmatched) > 0) {
  message("The following CSV sequence names could NOT be matched:\n") # manually fill in excel file at the end
  print(unmatched)
} else {
  message("All CSV names matched successfully!")
}

### ---- 6. Fill APOB_ASAP_OTU with species label ---- ###

df_excel$CYTB_mPTP_OTU <- matched$SpeciesLabel

### ---- 7. Save output ---- ###

write.xlsx(df_excel, "excel_with_species_filled_all_analyses.xlsx", overwrite = TRUE)


