
setwd("C:/Users/martinop/OneDrive - Universite de Montreal/perso/en_cours/JDR50bibliometrie/JDR50bibliometrie")

library(dplyr)


ZOTEROLIB <- read.table("My library.csv", header = TRUE, sep = ",", encoding = "UTF-8")

# Create a first subset based on your criteria
subset1_ZOTEROLIB <- subset(ZOTEROLIB,
                            Item.Type == "journalArticle" &
                              grepl("_peer reviewed", Manual.Tags) &
                              grepl("_TTRPG", Manual.Tags))

# Assuming your data frame is named DF with a column Language
DF <- subset1_ZOTEROLIB









# Function to extract given names from author strings
extract_given_names <- function(author_string) {
  # Split authors by semicolon
  authors <- unlist(strsplit(as.character(author_string), ";"))

  # For each author, extract the given name (after comma)
  given_names <- sapply(authors, function(x) {
    # Clean the string
    x <- trimws(x)
    # Check if there's a comma
    if(grepl(",", x)) {
      # Extract and trim the part after the comma
      name <- trimws(str_extract(x, "(?<=,).*"))
      return(name)
    } else {
      return(NA)
    }
  })

  return(given_names)
}






# Create new dataframe with individual authors
DFA <- DF %>%
  # Create a row for each author-year combination
  rowwise() %>%
  mutate(
    given_names = list(extract_given_names(Author))
  ) %>%
  unnest(given_names) %>%
  # Keep only relevant columns and remove NA values
  select(given_names, Publication.Year) %>%
  filter(!is.na(given_names)) %>%
  # Remove any empty strings
  filter(given_names != "") %>%
  # Clean up given names
  mutate(given_names = trimws(given_names))





# Read the gender reconciled CSV file
DFG <- read.csv("gender-name-reconciled.csv", stringsAsFactors = FALSE)

# Assuming DFA is your existing dataframe with given_names field
# Get unique given_names from DFA that are not in DFG$givenname
new_givennames <- unique(DFA$given_names[!(DFA$given_names %in% DFG$givenname)])

# Remove any NA values
new_givennames <- new_givennames[!is.na(new_givennames)]

# If there are new givennames to add
if(length(new_givennames) > 0) {
  # Create a data frame with all NA columns first
  new_rows <- as.data.frame(matrix(NA,
                                   nrow = length(new_givennames),
                                   ncol = ncol(DFG)))

  # Set the column names to match DFG
  names(new_rows) <- names(DFG)

  # Now specifically set the givenname column
  new_rows$givenname <- new_givennames

  # Append the new rows to DFG
  DFG <- rbind(DFG, new_rows)
}

# Optional: Write the updated dataframe back to a CSV file
write.csv(DFG, "gender-name-reconciled.csv", row.names = FALSE)

#Reconcile

# Reimport to verify
library(readr)
gender_reconciled_updated <- read_csv("gender-name-reconciled.csv")
View(gender_reconciled_updated)


