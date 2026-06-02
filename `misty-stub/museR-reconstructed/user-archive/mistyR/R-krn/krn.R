###############################################################
#' kern2df
#' Takes a .krn spline and converts into tidy dataframe of note rows
#'
#' @param spline a .krn score file
#' @return tidy frame of note rows
#
# 
kern2df <- function(spline){
  data <- readLines(spline) # Read the lines of the spline as a vector of strings
  data <- data[-grep("^!|\\*", data)] # Removes extra text and information - lines that start with *
  notes_df <- .k2df_tidy_measures(data) # Get dataframe with tidy rows - move measure lines and make them columns
  print(notes_df)
  # Tidy the dataframe
  notes_df <- .k2df_resolve_chords(notes_df) # Resolve the note pattern chords into seperate tidy note columns
  # Useful values
  max_notes <- ncol(notes_df) - 1 # Get the number of notes in the largest chord  
  num_notes <- nrow(notes_df) # Get the number of notes
  # Add piece information to the dataframe
  notes_df$key <- .k2df_get_key(data); notes_df$meter <- .k2df_get_meter(data) # Add the piece information
  notes_df <- notes_df[,c(c((ncol(notes_df) - 1):(ncol(notes_df)), 1:(1+max_notes)))] # Reorder the columns
  # Save the key, meter, and measure columns
  piece_info <- notes_df[,1:3]
  # Resolve each note column into rhythm and note value/name
  note_columns <- notes_df %>% select(contains("note"))
  notes_df <- map2_dfc(note_columns, 1:ncol(note_columns), .k2df_resolve_notes)
  notes_df <- as.data.frame(lapply(notes_df, function(y) gsub("K", "", y)))
  # Return the piece info and the resolved note columns
  cbind(piece_info, notes_df)
}

#========================================================#
#                     HELPER FUNCTIONS
#========================================================#

# Gets the key of a .krn piece
.k2df_get_key <- function(data){
  key_v <- grep("^\\*.*[^I]:$",data,value = T)
  # In case not immediately found
  if(identical(key_v,character(0))){
    key_v <- grep("\\*{1}k\\[",data,value = T)
  }
  key_v<- gsub("\t.*","",key_v)
  key_v
}

# Gets the meter of a .krn piece
.k2df_get_meter <- function(data){
  meter <- grep("\\*{1}M[0-9]",data,value = T)
  meter
}

# Tidies the rows of the lines from a .krn piece into a tidy dataframe with a measure column 
.k2df_tidy_measures <- function(data){
  # Helper: Returns the interval of rows for measure i
  .measure_interval <- function(i, measure_bds){ (1 + measure_bds[i]) : measure_bds[i+1] }
  # Get rows with measure boundaries - lines that start with an =
  measure_bds <- grep("=",data, value = F)
  # Get the length of each measure by calling length() on the helper function
  measure_lengths <- map_dbl(1:(length(measure_bds)-1), function(i){length(.measure_interval(i, measure_bds))})
  # Get number of measures
  num_measures <- length(measure_lengths)
  # Generate the column designating which measure a note is in by using rep(measure_number, measure_length)
  measure_col <- do.call("c",lapply(1:num_measures, function(m){rep(m, measure_lengths[m])}))
  # Remove the measure boundaries
  measure_col <- measure_col[-measure_bds]
  # Get column of notes
  notes_df <- data.frame(measure = measure_col, note_pattern = data[-measure_bds])
  #print(class(notes_df$note_pattern)) # Causing bug?
  notes_df
}

#Resolve the note pattern chords into seperate tidy note columns
.k2df_resolve_chords <- function(notes_df){
  # Helper: Resolves a .krn string with multiple notes (a chord) into a vector of single note patterns
  .resolve_chord_notes <- function(chord){unlist(strsplit(chord,"\\s"))}
  # Resolve the .krn strings with multiple notes (a chord) into a list of vectors of single note patterns
  resolved_notes <- map(as.character(notes_df$note_pattern), .resolve_chord_notes)
  max_notes <- max(lengths(resolved_notes)) # Get the number of notes in the largest chord  
  num_notes <- nrow(notes_df) # Get the number of notes
  # Intialize an empty array with number of columns to fit maximum number of notes needed
  notes_df_ <- map_dfc(1:max_notes, function(foo){data.frame(V = rep(NA_character_, num_notes))})
  # Populate the dataframe
  for(i in 1:num_notes){
    vec_notes <- resolved_notes[[i]]
    notes_df_[i,1:length(vec_notes)] <- vec_notes 
  }
  # Relabel each note column
  colnames(notes_df_) <- purrr::map_chr(1:max_notes, .f = function(i){paste("note",i,sep="")})
  # Add the measure column
  notes_df_ <- cbind(notes_df$measure, notes_df_); colnames(notes_df_)[1] <- "measure"
  # Purify the note pattern 
  notes_df_ <- as.data.frame(lapply(notes_df_, function(y) gsub("L|J|K", "", y)))
  notes_df_ <- as.data.frame(lapply(notes_df_, function(y) gsub("'", "", y)))
  notes_df_ <- as.data.frame(lapply(notes_df_, function(y) gsub("\\[|\\]|\\\\|\\/", "", y)))
  # Return the tidied dataframe
  notes_df_
}

.k2df_resolve_notes <- function(note_column, i){
  # Initialize the dataframe of note with resolved rhythm and note 
  resolved_note_df <- data.frame(pattern = note_column)
  # Get the rhythm value
  resolved_note_df$rhythm <- stringr::str_extract(note_column,"[0-9]{1,2}(\\.*)")
  # Get the note pattern strings
  notei <- stringr::str_extract(note_column,"[A-z]{1,2}.*")
  # Parse the note pattern strings for a name and value (ex. 'ee' = E, 9)
  parsed_notes <- do.call("rbind",lapply(notei, function(i){krn_note(i)}))
  # Add the columns to the dataframe 
  resolved_note_df$note_name <- parsed_notes[,1]
  resolved_note_df$note_value <- parsed_notes[,2]
  # Drop the pattern string column
  resolved_note_df <- resolved_note_df %>% select(!contains("pattern"))
  # Paste the note index onto the column names
  colnames(resolved_note_df) <- paste(colnames(resolved_note_df), i, sep="")
  # Return the tidy note with its resolved values of rhythm, note name, and note value
  resolved_note_df 
}

#========================================================#
#                         GLOBAL
#========================================================#
# Returns a note hash table for parsing .krn files
.note_hashtable <- function(){
  # Vectors of .krn syntax
  musical_alphabet_krn <- c("[Aa]","[Bb]","[Cc]","[Dd]","[Ee]","[Ff]","[Gg]") # The musical alphabet
  incidentals_krn <- c("-","(?!#|-)[kTp;n\\)_]*$","#") # Corrospond to flat, natural, and sharp respectively
  # Vectors for museR syntax
  musical_alphabet <- c("A","B","C","D","E","F","G") # The musical alphabet
  incidentals <- c("-","","#") # Corrospond to flat, natural, and sharp respectively
  # Helper: returns a character vector of size 3 applying each incidental
  .apply_incidentals <- function(note_letter, incidentals_){paste(note_letter, incidentals_, sep = "")}
  # Get notes and their values
  note_patterns <- do.call("c", lapply(musical_alphabet_krn, .apply_incidentals, incidentals_krn))
  note_labels <- do.call("c", lapply(musical_alphabet, .apply_incidentals, incidentals))
  note_values <- c(1,2,3,3,4,5,4,5,6,6,7,8,8,9,10,9,10,11,11,12,1)
  # Hash table of notes
  note_table <- data.frame(pattern = note_patterns, label = note_labels, value = note_values)
  # Make hash table columns characters
  note_table$pattern <- as.character(note_table$pattern); note_table$label <- as.character(note_table$label)
  return(note_table)
}
# GLOBAL VARIABLE: the note hash table
note_table <- .note_hashtable()

#========================================================#
#                       .KRN NOTE
#========================================================#
#### n.v_n.n #### 
#' Identifies the note and note value of a .krn note
#'  
#' @param note .krn file 
#' @return NNV and DNV
#' 
# Input a krn note string
krn_note <- function(note){
  # Check edge cases first
  if(is.na(note)){ return(rep(NA, 2)) }
  # Iterate over our table to try and find a match
  for(i in 1:nrow(note_table)){
    pattern <- as.character(note_table$pattern[i])
    if(stringr::str_detect(note, pattern)){ return(c(note_table$label[i],note_table$value[i])) }
  }
  if(stringr::str_detect(note, "r")){ return(c("rest", NA)) }
  else if(stringr::str_detect(note, "\\.")){ return(rep(".", 2)) }
  else{ return(c(note, NA)) } # If no edge cases or notes can be matched, catch corner cases by returning raw note with NA
}