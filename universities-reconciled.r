
setwd("C:/Users/martinop/OneDrive - Universite de Montreal/perso/en_cours/JDR50bibliometrie/JDR50bibliometrie")

library(dplyr)

ZOTEROLIB <- read.table("My library.csv", header = TRUE, sep = ",", encoding = "UTF-8")

subset2_ZOTEROLIB <- subset(ZOTEROLIB,
                            (Item.Type == "thesis" &
                               grepl("_TTRPG", Manual.Tags)))

DF <- subset2_ZOTEROLIB

# Remove all Type of thesis = Bachelor stuff (starts by B or R of D or H or I or C or S)
DF <- DF %>%
  filter(!grepl("^[BRDHCS]", Type))

# Normalized Type field into TypeNormalized
# if Type starts by T P or D, then its a PhD
# if Type starts by M, then its a Master
DF <- DF %>%
  mutate(TypeNormalized = case_when(
    grepl("^[TPD]", Type) ~ "PhD",
    grepl("^M", Type) ~ "Master",
    TRUE ~ Type
  ))

# Now select only the PhD and the Master
DF <- DF %>%
  filter(TypeNormalized %in% c("PhD", "Master"))












# Read the universities reconciled CSV file
DFU <- read.csv("universities-reconciled.csv", stringsAsFactors = FALSE)

# Assuming DF is your existing dataframe with Publisher field
# Get unique publishers from DF that are not in DFU$Universities
new_universities <- unique(DF$Publisher[!(DF$Publisher %in% DFU$Universities)])

# Remove any NA values
new_universities <- new_universities[!is.na(new_universities)]

# If there are new universities to add
if(length(new_universities) > 0) {
  # Create a data frame with the same structure as DFU for the new entries
  # Assuming DFU has the same number of columns as the original file
  new_rows <- data.frame(
    Universities = new_universities,
    # Fill other columns with NA or appropriate default values
    matrix(NA,
           nrow = length(new_universities),
           ncol = ncol(DFU) - 1
    )
  )
  names(new_rows) <- names(DFU)

  # Append the new rows to DFU
  DFU <- rbind(DFU, new_rows)
}

# remove rows with NA in the first column
DFU <- DFU %>% filter(!is.na(across(1)))

# Optional: Write the updated dataframe back to a CSV file
write.csv(DFU, "universities-reconciled.csv", row.names = FALSE)

#Reconcile

# Reimport to verify
library(readr)
universities_reconciled_updated <- read_csv("universities-reconciled.csv")
View(universities_reconciled_updated)


