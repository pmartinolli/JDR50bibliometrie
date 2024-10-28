
setwd("C:/Users/martinop/OneDrive - Universite de Montreal/perso/en_cours/JDR50bibliometrie/JDR50bibliometrie")

library(dplyr)

ZOTEROLIB <- read.table("My library.csv", header = TRUE, sep = ",", encoding = "UTF-8")

# Create a first subset based on your criteria
subset1_ZOTEROLIB <- subset(ZOTEROLIB,
                            Item.Type == "journalArticle" &
                              grepl("_peer reviewed", Manual.Tags) &
                              grepl("_TTRPG", Manual.Tags))

# Assuming your data frame is named DF with a column Publication.Year
DF <- subset1_ZOTEROLIB

# Read the universities reconciled CSV file
DFJ <- read.csv("journals-reconciled.csv", stringsAsFactors = FALSE)

# Assuming DF is your existing dataframe with Publication.Title field
# Get unique Publication.Title from DF that are not in DFJ$Publication.Title
new_journals <- unique(DF$Publication.Title[!(DF$Publication.Title %in% DFJ$Publication.Title)])

# Remove any NA values
new_journals <- new_journals[!is.na(new_journals)]

# If there are new universities to add
if(length(new_journals) > 0) {
  # Create a data frame with the same structure as DFJ for the new entries
  new_rows <- data.frame(
    Publication.Title = new_journals,
    # Fill other columns with NA or appropriate default values, except Inception
    matrix(NA,
           nrow = length(new_journals),
           ncol = ncol(DFJ) - 2  # Subtract 2 to account for both Title and Inception
    )
  )

  # Add the Inception column with existing values from DFJ
  new_rows$Inception <- DFJ$Inception[1:length(new_journals)]

  # Set column names to match DFJ
  names(new_rows) <- names(DFJ)

  # Append the new rows to DFJ
  DFJ <- rbind(DFJ, new_rows)
}

# remove rows with NA in the Publication.Title
DFJ <- DFJ %>%
  filter(!is.na(Publication.Title) &
           trimws(Publication.Title) != "")

# Sort DFJ
DFJ <- DFJ %>%
  arrange(desc(Publication.Title))

# Optional: Write the updated dataframe back to a CSV file
write.csv(DFJ, "journals-reconciled.csv", row.names = FALSE)

#Reconcile

# Reimport to verify
library(readr)
journal_reconciled_updated <- read_csv("journals-reconciled.csv")
View(journal_reconciled_updated)


