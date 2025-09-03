#Open ELAN
#Create a new Domain
#Search --> Search Multiple EAF... --> Define Search Domain --> New Domain... --> Select files then click >> OK --> New name "CompLing"
#ELAN Main Screen (blank) --> File --> Export Multiple Files As --> Praat TextGrid... --> Select Files from Domain --> CompLing --> Load --> Select All --> Next --> choose directory --> Finish
#Praat TextGrids have the absolute best underlying XML format in my experience. Open up a file in EditPad Lite. 
#size = number of rows
#name = Tier name
#xmin = time at the beginning of each annotation
#xmax = time at the end of each annotation
#text = the text in the annotation

#We're going to extract this information and create a dataset from all the annotations
# Load required libraries
library(stringr)  # for string manipulation (e.g., str_match, regex)
library(dplyr)    # for data manipulation (group_by, summarise, etc.)

# Set folder where .TextGrid files are stored
folder <- "C:\\Users\\mikey\\Dropbox\\Courses\\Computational Linguistics - LING 349 Fall\\EAF Files\\"

# List all TextGrid files in the folder (case-insensitive match)
files <- list.files(folder, pattern = "\\.TextGrid$", full.names = TRUE)

# Function to extract all relevant intervals from a TextGrid file
extract_all_intervals <- function(file_path) {
  # Read lines of the file into a character vector
  lines <- readLines(file_path, encoding = "UTF-8")
  
  # Extract just the filename, removing the .TextGrid extension (case-insensitive)
  filename <- basename(file_path)
  filename <- sub("\\.TextGrid$", "", filename, ignore.case = TRUE)
  
  # Identify lines where tiers are defined
  tier_lines <- grep('name = "', lines)
  
  # Extract tier names using regex
  tier_names <- str_match(lines[tier_lines], 'name = "(.*?)"')[,2]
  
  results <- list()  # list to hold tier-level data
  
  # Iterate through each tier
  for (i in seq_along(tier_lines)) {
    start_line <- tier_lines[i]
    end_line <- if (i < length(tier_lines)) tier_lines[i + 1] - 1 else length(lines)
    tier_block <- lines[start_line:end_line]
    
    interval_starts <- grep("intervals \\[", tier_block)
    tier_name <- tier_names[i]
    intervals <- list()
    
    # Iterate through each interval within the tier
    for (j in seq_along(interval_starts)) {
      start <- interval_starts[j]
      end <- if (j < length(interval_starts)) interval_starts[j + 1] - 1 else length(tier_block)
      chunk <- tier_block[start:end]
      
      # Extract xmin timestamp and text from the interval
      xmin <- as.numeric(str_match(chunk[grep("xmin =", chunk)], "xmin = ([0-9.]+)")[,2])
      text <- str_match(chunk[grep('text = ', chunk)], 'text = "(.*)"')[,2]
      
      # Only include non-empty annotations
      if (!is.na(text) && text != "") {
        intervals[[length(intervals) + 1]] <- data.frame(
          xmin = round(xmin, 3),
          tier = tier_name,
          text = text,
          file = filename,
          stringsAsFactors = FALSE
        )
      }
    }
    
    # Combine intervals from this tier
    if (length(intervals) > 0) {
      results[[length(results) + 1]] <- do.call(rbind, intervals)
    }
  }
  
  # Combine all tier results into one data frame
  if (length(results) > 0) {
    return(do.call(rbind, results))
  } else {
    return(NULL)
  }
}

# Process all files and extract intervals
all_blocks <- lapply(files, extract_all_intervals)

# Combine all file results into one data frame
combined <- do.call(rbind, all_blocks)

# Format output: group annotations by timestamp and file
output <- combined %>%
  group_by(file, xmin) %>%
  arrange(factor(tier, levels = unique(combined$tier))) %>%
  summarise(
    block = paste0(paste0(tier, ": ", text), collapse = "\n"),
    .groups = "drop"
  ) %>%
  mutate(block = paste0(block, "\nFilename: ", file))  # append file info to block

# Save formatted output to text file
writeLines(paste(output$block, collapse = "\n\n"), file.path(folder, "combined_output.txt"))


##########
#Translating and adding name entry
##########
library(stringr)
library(dplyr)

# Load the file (change path as needed)
input_path <- "C:\\Users\\mikey\\Dropbox\\Courses\\Computational Linguistics - LING 349 Fall\\EAF Files\\combined_output.txt"
output_path <- "C:\\Users\\mikey\\Dropbox\\Courses\\Computational Linguistics - LING 349 Fall\\EAF Files\\combined_output_cleaned.txt"
lines <- readLines(input_path, encoding = "UTF-8")

# Function to clean a block
clean_block <- function(block_lines) {
  # Join lines into one block
  block <- paste(block_lines, collapse = "\n")

  # Remove everything before colon in specified labels
  block <- str_replace_all(block, "Segmentación\\+IPA[^:]*:", "Segmentation+IPA:")
  block <- str_replace_all(block, "Español Estándar[^:]*:", "Standard Spanish:")
  block <- str_replace_all(block, "Quichua[^:]*:", "Kichwa:")
  block <- str_replace_all(block, "Español[^:]*:", "Spanish:")
  block <- str_replace_all(block, "Inglés[^:]*:", "English:")
  block <- str_replace_all(block, "Oración Elicitada[^:]*:", "Elicited Sentence:")
  block <- str_replace_all(block, "Glosa[^:]*:", "Gloss:")

  # Split lines again
  lines <- unlist(strsplit(block, "\n"))

  # Find the speaker line (not one of the standard labels)
  standard_labels <- c("Segmentation+IPA:", "Standard Spanish:", "Spanish:", "Kichwa:", 
                       "English:", "Elicited Sentence:", "Gloss:", "Filename:")
  is_custom <- function(line) {
    any(str_detect(line, ":")) && !any(startsWith(line, standard_labels))
  }
  speaker_line <- lines[sapply(lines, is_custom)]
  speaker <- if (length(speaker_line) > 0) str_extract(speaker_line[1], "^[^:]+") else "UNKNOWN"

  # Replace the speaker line label with "Media Lengua"
  lines <- sapply(lines, function(line) {
    if (startsWith(line, speaker)) {
      return(str_replace(line, paste0("^", speaker), "Media Lengua"))
    } else {
      return(line)
    }
  })

  # Append the speaker at the end
  lines <- c(lines, paste0("Speaker: ", speaker))
  return(lines)
}

# Split input by blocks
block_indices <- which(lines == "")
block_starts <- c(1, block_indices + 1)
block_ends <- c(block_indices - 1, length(lines))

# Process all blocks
blocks <- mapply(function(start, end) {
  clean_block(lines[start:end])
}, block_starts, block_ends, SIMPLIFY = FALSE)

# Flatten and write to file
final_output <- unlist(lapply(blocks, function(b) c(b, "")))
writeLines(final_output, output_path)
 

######
#Formatting the data for the computer.
######

#Libraries
library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(writexl)
library(ggrepel)

# Read the file as a single character vector
lines <- readLines("C:\\Users\\mikey\\Dropbox\\Courses\\Computational Linguistics - LING 349 Fall\\EAF Files\\combined_output_cleaned.txt", encoding = "UTF-8")

# Split the lines into blocks based on empty lines
blocks <- list()
current_block <- c()

for (line in lines) {
  if (line == "") {
    if (length(current_block) > 0) {
      blocks[[length(blocks) + 1]] <- current_block
      current_block <- c()
    }
  } else {
    current_block <- c(current_block, line)
  }
}
# Add the last block
if (length(current_block) > 0) {
  blocks[[length(blocks) + 1]] <- current_block
}

# Function to process one block
extract_info <- function(block, entry_id) {
  segmentation <- NA
  gloss <- NA
  ml_sentence <- NA
  speaker <- NA
  filename <- NA
  translation <- FALSE
  word_count <- NA
  
  for (line in block) {
    line <- str_trim(gsub("\uFEFF", "", line))  # remove BOM and trim
    
    if (startsWith(line, "Segmentation+IPA:")) {
      segmentation <- str_trim(sub("Segmentation\\+IPA:", "", line))
    }
    if (startsWith(line, "Gloss:")) {
      gloss <- str_trim(sub("Gloss:", "", line))
      word_count <- length(str_split(gloss, " ")[[1]])
    }
    if (startsWith(line, "Media Lengua:")) {
      ml_sentence <- str_trim(sub("Media Lengua:", "", line))
    }
    if (startsWith(line, "Speaker:")) {
      speaker <- str_trim(sub("Speaker:", "", line))
    }
    if (startsWith(line, "Filename:")) {
      filename <- str_trim(sub("Filename:", "", line))
    }
    if (grepl("^(Spanish|Standard Spanish|Kichwa|English):", line)) {
      translation <- TRUE
    }
  }
  
  return(tibble(
    entry_id = entry_id,
    segmentation = segmentation,
    gloss = gloss,
    ml_sentence = ml_sentence,
    speaker = speaker,
    filename = filename,
    translation_available = translation,
    word_count = word_count
  ))
}

# Loop over blocks and apply the function
all_data <- tibble()
for (i in seq_along(blocks)) {
  row <- extract_info(blocks[[i]], i)
  all_data <- bind_rows(all_data, row)
}

# View the results
View(all_data)

#There are a number of Errors here, likely due to transfer errors from ELAN or transcription errors made a while back. We're doing to ditch rows where the gloss and segmentation columns do not have equal parses.
clean_df <- all_data %>%
  filter(grepl("[-=]", gloss) & grepl("[-=]", segmentation))

# Save as XLSX
write_xlsx(clean_df, "C:\\Users\\mikey\\Dropbox\\Courses\\Computational Linguistics - LING 349 Fall\\Assignment 1\\clean_df.xlsx")

#The purpose of this code is to extract and count morphemes from glossed Media Lengua sentences, so you can see which morphemes are most frequent across your dataset.
# Load Media Lengua data
ml_data <- read_excel("C:\\Users\\mikey\\Dropbox\\Courses\\Computational Linguistics - LING 349 Fall\\Assignment 1\\clean_df.xlsx")

#PART I
# Step 1: Select relevant columns
step1 <- ml_data %>% select(ml_sentence, gloss)
View(step1)

# Step 2: Add a space before hyphens and equal signs
step2 <- step1 %>%
  mutate(gloss = str_replace_all(gloss, "-", " -"),
         gloss = str_replace_all(gloss, "=", " ="))
View(step2)

# Step 3: Separate into morphemes
step3 <- step2 %>% separate_rows(gloss, sep = "\\s+")
View(step3)

# Step 4: Remove empty strings
step4 <- step3 %>% filter(gloss != "")
View(step4)

# Step 5: Classify morpheme type
step5 <- step4 %>%
  mutate(
    type = case_when(
      str_starts(gloss, "-") ~ "bound_morpheme",
      str_starts(gloss, "=") ~ "bound_clitic",
      TRUE ~ "headword"
    )
  )
View(step5)

step5$gloss_clean = str_remove(step5$gloss, "^[-=]")


# ALL IN ONE!
# Step 1: Tokenise gloss into morphemes, preserving structure
ordered_morph <- ml_data %>%
  select(ml_sentence, gloss) %>% #Selects these two columns
  mutate(gloss = str_replace_all(gloss, "-", " -"), # inserting a space
         gloss = str_replace_all(gloss, "=", " =")) %>% #inserting a space
  separate_rows(gloss, sep = "\\s+") %>%  #Now we can separate the morphemes
  filter(gloss != "") %>% #This gets rid of entries that are empty.
  mutate(type = case_when(  #Case_when is basically an if_else statement
    str_starts(gloss, "-") ~ "bound_morpheme", #~ creates a formula
    str_starts(gloss, "=") ~ "bound_clitic",
    TRUE ~ "headword" #TRUE basically means 'else' (seems counterintuitive to me)
  ),
  gloss_clean = str_remove(gloss, "^[-=]"))


#Step 2: Note: 'step5' is 'ordered_morphs'
ordered_morph$word_index <- cumsum(ordered_morph$type == "headword") #Counting each complex word 


#PART II
# Now build suffix chains, properly anchored to the headword
# Step 1: Filter out only relevant types
filtered_morphs <- ordered_morph %>%
  filter(type %in% c("bound_morpheme", "bound_clitic", "headword"))
View(filtered_morphs)

# Step 2: Group by sentence and word index
grouped_morphs <- filtered_morphs %>%
  group_by(ml_sentence, word_index)
# This is a grouped tibble, not a data frame yet
print(grouped_morphs)

# Step 3: Summarise within each sentence - this reconnects the morphemes in a single row. 
summarised_morphs <- grouped_morphs %>%
  summarise(
    word = first(gloss_clean[type == "headword"]),
    suffix_chain = paste(gloss_clean[type != "headword"], collapse = "-"),
    suffix_count = sum(type != "headword"),
    .groups = "drop"
  )
View(summarised_morphs)

summarised_morphs <- summarised_morphs %>%
  filter(!is.na(word))

#All in one!
suffix_order <- ordered_morph %>%
    filter(type %in% c("bound_morpheme", "bound_clitic", "headword")) %>%
    group_by(ml_sentence, word_index) %>%
    summarise(
        word = first(gloss_clean[type == "headword"]),
        suffix_chain = paste(gloss_clean[type != "headword"], collapse = "-"),
        suffix_count = sum(type != "headword"),
        #.groups = "drop"
    ) %>%
    filter(!is.na(word))  # remove any rows with no headword (just in case)
    
#Major clean up - 
# List of morphemes or patterns to exclude
#Deleting the entire row because of glossing issue that I don't want to deal with
cleaned_suffixes <- suffix_order %>%
  filter(
    !str_detect(suffix_chain, "3\\.FUT\\.NEG") &
    !str_detect(suffix_chain, "<") &
    !str_detect(suffix_chain, ">") &
    !str_detect(suffix_chain, "ɾka$") &
    !str_detect(suffix_chain, "\\]$") &
    !str_detect(suffix_chain, "\\[$") &
    !str_detect(suffix_chain, "decir$") &
    !str_detect(suffix_chain, "ʧun$") &
    !str_detect(suffix_chain, "AFFdecir$") &
    !str_detect(suffix_chain, "LIM\\.COND\\.SD") &
    !str_detect(suffix_chain, "COND\\.3p") &
    !str_detect(suffix_chain, "COP") &
    !str_detect(suffix_chain, "PUES") &
    !str_detect(suffix_chain, "n$") &
    !str_detect(suffix_chain, "pi$") &
    !str_detect(suffix_chain, "ʧaɾi$") &
    !str_detect(suffix_chain, "\\[ACC\\]") &
    !str_detect(suffix_chain, "a$") &
    !str_detect(suffix_chain, "encima$") &
    !str_detect(suffix_chain, "gu$") &
    !str_detect(suffix_chain, "j$") &
    !str_detect(suffix_chain, "kaʧi$") &
    !str_detect(suffix_chain, "ma$") &
    !str_detect(suffix_chain, "maɾe$") &
    !str_detect(suffix_chain, "mi$") &
   # !str_detect(suffix_chain, "ngi$") &
    !str_detect(suffix_chain, "nka$") &
    !str_detect(suffix_chain, "nʧi$") &
    !str_detect(suffix_chain, "pa$") &
    !str_detect(suffix_chain, "paʃ$") &
    !str_detect(suffix_chain, "so$") &
    !str_detect(suffix_chain, "wa$") &
   # !str_detect(suffix_chain, "xawa$") &
    !str_detect(suffix_chain, "xu$") &
   # !str_detect(suffix_chain, "xun$") &
    !str_detect(suffix_chain, "ɲaxu$") &
  #  !str_detect(suffix_chain, "ɾi$") &
  #  !str_detect(suffix_chain, "ʧu$") &
    !str_detect(suffix_chain, "PST\\.") &
    !str_detect(suffix_chain, "PL\\[") &
    !str_detect(suffix_chain, "PRTdecir$") &
    !str_detect(suffix_chain, "EF$") &
    !str_detect(suffix_chain, "TOT.LOC") & #Not Pijal
    !str_detect(suffix_chain, "3p") & #Not Pijal
    !str_detect(suffix_chain, "TOT2") 
  #  !str_detect(suffix_chain, "ʃpa$") &
  #  !str_detect(suffix_chain, "ta$")
  
  )

#Changing glossing issues for consisitency
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "1.DUB"] <- "DUB"
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "ta"] <- "ACC"
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "ʃpa"] <- "SS.CONV"
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "ngi"] <- "2"
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "ʧu"] <- "Q.POL//NEG"
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "xu"] <- "PROG"
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "ɾi"] <- "REFL"
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "Q.yn//NEG"] <- "Q.POL//NEG"
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "3P"] <- "3p"
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "Q.Wh"] <- "Q.CON"
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "FUT.1s"] <- "1.FUT"
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "BEN"] <- "POSS//BEN"
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "Q.POL"] <- "Q.POL//NEG"
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "POSS"] <- "POSS//BEN"
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "FUT.1p"] <- "1p.FUT"
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "PERF"] <- "PRT"
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "GRN"] <- "GER"
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "RELF"] <- "REFL"
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "SS.CON"] <- "SS.CONV"
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "INT"] <- "INST"
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "1."] <- "1"
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "DUB.1"] <- "1.DUB"
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "AC"] <- "ACC"
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "FUT.1"] <- "1.FUT"
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "TOT1"] <- "TOT"
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "DS.COND"] <- "DS.COND"
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "DS.CONJ"] <- "DS.COND"
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "COND.DS"] <- "COND.DS"
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "Q.NEG"] <- "Q.POL//NEG"
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "Wh.Q"] <- "Q.CON"
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "FUT.3"] <- "3.FUT"
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "1P.FUT"] <- "1p.FUT"
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "p"] <- "2p"
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "FUT.IMD"] <- "FUT"
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "COND.3p"] <- "COND.3p(Sp)"
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "AGT"] <- "DS.CONV"
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "1P"] <- "1p"
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "VERB"] <- "VERBALIZER"
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "PURP"] <- "SS.PURP"
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "PERF.COND"] <- "PRT"
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "IMP.IMFORM"] <- "IMP"
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "AB"] <- "SUPRA"
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "AB"] <- "SUPRA"
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "LIM.COND.SD"] <- "LIM.COND.DS"
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "ETC//COND"] <- "ETC"
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "DS.COND"] <- "DS.CONV"
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "COND.DS"] <- "DS.CONV"
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "1.OBJ"] <- "OBJ.1"
cleaned_suffixes$suffix_chain[cleaned_suffixes$suffix_chain == "xawa"] <- "SUPRA"

View(cleaned_suffixes)

# COUNT: Calculate morpheme frequency
#This doesn't change the appearance in View() much, but it tells R to treat rows with the same suffix_chain as a group for the next operation. You’ll see this in the printed output as a "grouped tibble" (a tibble is a dataframe)
grouped_suffixes <- cleaned_suffixes %>%
  group_by(suffix_chain)
View(grouped_suffixes) 

#Summarizes how often each morpheme occurs
suffix_summary <- grouped_suffixes %>%
  summarise(count = n(), .groups = "drop")
View(suffix_summary)

#See how much data we're dealing with
sum(suffix_summary$count)

#Reorder the data from highest to lowest
morpheme_counts <- suffix_summary %>%
  arrange(desc(count))
View(morpheme_counts)

#All in one!
morpheme_counts <- cleaned_suffixes %>%
  group_by(suffix_chain) %>%
  summarise(count = n(), .groups = "drop") %>%
  arrange(desc(count))

# PLOT: Bar graph of most frequent morphemes - This isn't a great visualization because topic is soooo frequent 
ggplot(morpheme_counts, aes(x = fct_reorder(suffix_chain, count), y = count)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Most Frequent Morphemes (Cleaned)",
    x = "Morpheme",
    y = "Frequency"
  ) +
  theme_minimal()

#Square root - this is a better visalization with count converted to square root.
ggplot(morpheme_counts, aes(x = fct_reorder(suffix_chain, count), y = sqrt(count))) +
  geom_col(fill = "darkblue") +
  coord_flip() +
  labs(
    title = "Most Frequent Morphemes (Cleaned)",
    x = "Morpheme",
    y = "Frequency"
  ) +
  theme_minimal()

#PART IV
#Which morpheme appears most frequently in each position. 

#Tokenize suffixes by position
#Step 1: Filter out words with no suffixes
step1 <- cleaned_suffixes %>%
  filter(suffix_count > 0)
View(step1)

#Step 2: Split suffix_chain into multiple rows
step2 <- step1 %>%
  separate_rows(suffix_chain, sep = "-")
View(step2)

#Step 3: Group by word - So we can assign a slot number to each morpheme within a single word (i.e., by word_index).
step3 <- step2 %>%
  group_by(word_index)
print(step3)  # Shows grouping
View(step3)

#Step 4: Assign a slot number (i.e., position in the chain)
step4 <- step3 %>%
  mutate(slot = row_number())
View(step4)

# All together! 
suffix_slots <- cleaned_suffixes %>%
  filter(suffix_count > 0) %>%
  separate_rows(suffix_chain, sep = "-") %>%
  group_by(word_index) %>%
  mutate(slot = row_number()) %>%
  ungroup()
###

#Count morphemes in each position
# Step 1: Count morphemes by slot
step1 <- suffix_slots %>%
  count(slot, suffix_chain, sort = TRUE, name = "count")
View(step1)

#Step 2: Group by slot so we can compute frequencies - we'll want to calculate the proportion of each morpheme within each slot.
step2 <- step1 %>%
  group_by(slot)
print(step2)   # shows it's grouped
View(step2)

#Step 3: Calculate relative frequency within each slot
step3 <- step2 %>%
  mutate(freq = count / sum(count))
View(step3)

# All together!
suffix_position_counts <- suffix_slots %>%
  count(slot, suffix_chain, sort = TRUE, name = "count") %>%
  group_by(slot) %>%
  mutate(freq = count / sum(count)) %>%
  ungroup()
###

#Select top 10 suffixes per slot


# STEP 3: Select top 10 suffixes per slot
#Step 1: Group by slot - Treat each slot (1st, 2nd, 3rd morpheme position, etc.) as its own group (You're telling R to do this)
step1 <- suffix_position_counts %>%
  group_by(slot)
View(step1)

#Step 2: Extract top 10 rows per group
step2 <- step1 %>%
  slice_max(order_by = freq, n = 10, with_ties = FALSE)
View(step2)

# All together!
top_suffixes <- suffix_position_counts %>%
  group_by(slot) %>%
  slice_max(order_by = freq, n = 10, with_ties = FALSE) %>%
  ungroup()

# STEP 4: Plot
ggplot(top_suffixes, aes(x = reorder(suffix_chain, -freq), y = freq)) +
  geom_col(fill = "steelblue") +
  facet_wrap(~slot, scales = "free_x") +
  labs(
    title = "Most Frequent Morphemes by Slot Position (Cleaned)",
    x = "Morpheme",
    y = "Proportion in Slot"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#Part V
#Morpheme order based on frequency
# STEP 1: Prepare suffix chain data
# Split each suffix_chain into multiple rows, preserving word index

#This filters out any rows in your cleaned_suffixes data where there are no suffixes — i.e., only keeps words with at least one suffix. suffix_count must be a column in your dataset.
step1 <- cleaned_suffixes %>%
  filter(suffix_count > 0)
View(step1)

#This takes each multi-morpheme string in suffix_chain and splits it into separate rows wherever there’s a -. For example:
step2 <- step1 %>%
  separate_rows(suffix_chain, sep = "-")
View(step2)

#Groups the data by word — assuming word_index uniquely identifies each word. This is needed so the next step (row_number()) operates within each word, not across the whole dataset.
step3 <- step2 %>%
  group_by(word_index)
  
#Assigns a position number (slot) to each suffix in order from left to right, within each word. So the first suffix gets 1, the second 2, etc.
step4 <- step3 %>%
  mutate(slot = row_number())
View(step4)

#Removes the grouping so future operations don’t accidentally stay grouped by word_index.
suffix_slots <- step4 %>%
  ungroup()

#Step 1 all together!
suffix_slots <- cleaned_suffixes %>%
    filter(suffix_count > 0) %>%
    separate_rows(suffix_chain, sep = "-") %>%
    group_by(word_index) %>%
    mutate(slot = row_number()) %>%
    ungroup()

#
# STEP 2: For each word, get morpheme order
morpheme_order <- suffix_slots %>%
    select(word_index, slot, suffix_chain)
View(morpheme_order)

# STEP 3: For each instance of a morpheme, count how many morphemes precede and follow it
#This counts how many morphemes are in each word (i.e. how many rows share the same word_index). Count is stored in n_suffixes
step1 <- morpheme_order %>%
  group_by(word_index) %>%
  mutate(n_suffixes = n())
View(step1)

#Removes the grouping by word_index to allow global operations in the next step.
step2 <- step1 %>%
  ungroup()

preceding_following <- step2 %>%
  mutate(
    preceding = slot - 1,
    following = n_suffixes - slot
  )
View(preceding_following)

#All in one!
preceding_following <- morpheme_order %>%
    group_by(word_index) %>%
    mutate(n_suffixes = n()) %>%
    ungroup() %>%
    mutate(
        preceding = slot - 1,
        following = n_suffixes - slot
    )

# STEP 4: Summarise by morpheme across all instances
#group rows by morpheme
grouped_morphemes <- preceding_following %>%
  group_by(suffix_chain)

#Calculate position statistics for each morpheme
summary_stats <- grouped_morphemes %>%
  summarise(
    avg_preceding = mean(preceding),         # average number of morphemes before it
    avg_following = mean(following),         # average number after it
    total_occurrences = n()                  # how many times this morpheme appears
  )

#All in one!
morpheme_position_summary <- preceding_following %>%
    group_by(suffix_chain) %>%
    summarise(
        avg_preceding = mean(preceding),
        avg_following = mean(following),
        total_occurrences = n()
    ) %>%
    ungroup()

# STEP 5: Plot the morpheme continuum
ggplot(morpheme_position_summary, aes(x = avg_preceding, y = avg_following, label = suffix_chain)) +
  geom_point(aes(
    size = ifelse(total_occurrences >= 9, 4, 2),  # size: 4 if 9+, else 2
    color = ifelse(total_occurrences >= 9, "#067c63", "#d3244a")  # colour: green if 9+, else red
  ), alpha = 0.7) +
  geom_text_repel(
    aes(label = suffix_chain),
    size = 3,
    max.overlaps = Inf,
    force = 1,
    direction = "y",
    segment.size = 0.2
  ) +
  labs(
    title = "Morpheme Continuum: Root-Proximal to Peripheral",
    x = "Average Number of Morphemes Before",
    y = "Average Number of Morphemes After"
  ) +
  scale_size_identity() +  # so size values are treated as-is
  scale_color_identity() +  # so manual colours are used as-is
  theme_minimal()
  
#Part VI: Most common roots
# Extract root as the first element before - or =
df_roots <- clean_df %>%
  mutate(root = str_extract(segmentation, "^[^-=]+")) %>%  # extract root
  count(root, sort = TRUE) %>%                             # count frequency
  slice_max(n, n = 50)                                     # top 50
  
library(ggplot2)

# Assuming you already created df_roots as shown earlier
ggplot(df_roots, aes(x = reorder(root, n), y = n)) +
  geom_col(fill = "#6670f0") +
  coord_flip() +
  labs(
    title = "Top 50 Most Common Roots",
    x = "Root",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 12)

#Zipf's law?
df_roots %>%
  mutate(rank = row_number()) %>%
  ggplot(aes(x = log10(rank), y = log10(n))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Zipf's Law: Log(Frequency) vs Log(Rank)",
    x = "log10(Rank)",
    y = "log10(Frequency)"
  ) +
  theme_minimal()