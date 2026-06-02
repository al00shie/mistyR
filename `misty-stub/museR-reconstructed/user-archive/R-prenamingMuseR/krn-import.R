#========================================================#
#                       PIECE_DF
#========================================================#
#' piece_df
#' Using multiple .krn files, calls kern2df to create piece_df: a dataframe for the entire piece
#'
#' @param krn_files string vector of .krn filenames
#' @param instruments string vector of instruments
#' @return a dataframe containing all instruments for the whole piece

piece_df <- function(krn_files, instruments){
  # Get the piece info from one of the files ONCE
  piece_info <- .piece_df_pieceinfo(krn_files)
  # Get the labeled note columns for each instrument by column binding each one iteratively
  notes_df <- purrr::map2_dfc(krn_files, instruments, .kern2df_instrument)
  # Generate the piece_df by column binding the two dataframes
  piece_df <- cbind(piece_info, notes_df)
  return(piece_df)
}
#========================================================#
# Given a vector of krn files, get their common piece info once
.piece_df_pieceinfo <- function(krn_files){
  # Get one of the pieces (doesn't matter which)
  piece_df <- kern2df(krn_files[1])
  # Obtain the piece information columns using the _p regex
  piece_info <- piece_df %>% select(contains("_p"))
  return(piece_info)
}
#========================================================#
# Given a filename, returns the note columns labelled with that instrument
.kern2df_instrument <- function(spline, instrument){
  # Get the current dataframe
  piece_df <- kern2df(spline)
  # Remove the piece information columns
  notes_df <- piece_df %>% select(!contains("_p"))
  # Apply the instrument name to the note columns
  colnames(notes_df) <- paste(instrument, colnames(notes_df), sep = ".")
  return(notes_df)
}

#========================================================#
#                         KERN2DF
#========================================================#
#' kern2df
#' Takes a .krn spline and converts into tidy dataframe of note rows
#'
#' @param spline a .krn score file
#' @return tidy frame of note rows
#
# 
kern2df <- function(spline){
  raw_data <- readLines(spline) # Read the lines of the spline as a vector of strings
  data <- raw_data[-grep("^!|\\*", raw_data)] # Removes extra text and information - lines that start with *
  notes_df <- .k2df_tidy_measures(data) # Get dataframe with tidy rows - move measure lines and make them columns
  #print(notes_df)
  # Tidy the dataframe
  notes_df <- .k2df_resolve_chords(notes_df) # Resolve the note pattern chords into seperate tidy note columns
  # Useful values
  max_notes <- ncol(notes_df) - 1 # Get the number of notes in the largest chord  
  num_notes <- nrow(notes_df) # Get the number of notes
  # Add piece information to the dataframe
  notes_df$key_p <- .k2df_get_key(raw_data)
  notes_df$meter_p <- .k2df_get_meter(raw_data)
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
#                K2DF: PIECE INFORMATION
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
#========================================================#
# Gets the meter of a .krn piece
.k2df_get_meter <- function(data){
  meter <- grep("\\*{1}M[0-9]",data,value = T)
  meter
}
#========================================================#
#                K2DF: NOTE WRANGLING
#========================================================#
# Tidies the rows of the lines from a .krn piece into a tidy dataframe with a measure column 
.k2df_tidy_measures <- function(data){
  # Helper: Returns the interval of rows for measure i
  .measure_interval <- function(i, measure_bds){ (1 + measure_bds[i]) : measure_bds[i+1] }
  # Get rows with measure boundaries - lines that start with an =
  measure_bds <- grep("=", data, value = F)
  # Get the length of each measure by calling length() on the helper function
  measure_lengths <- map_dbl(1:(length(measure_bds)-1), function(i){length(.measure_interval(i, measure_bds))})
  # Get number of measures
  num_measures <- length(measure_lengths)
  # Generate the column designating which measure a note is in by using rep(measure_number, measure_length)
  measure_col <- do.call("c",lapply(1:num_measures, function(m){rep(m, measure_lengths[m])}))
  # Remove the measure boundaries
  measure_col <- measure_col[-measure_bds]
  # Make sure we only have notes
  note_patterns <- data[-measure_bds]
  # If the composer is in the note patterns, remove them
  if(str_detect(note_patterns[1], "COM")){ note_patterns <- note_patterns[-c(1)] }
  # Get column of notes
  notes_df <- data.frame(measure_p = measure_col, note_pattern = note_patterns)
  return(notes_df)
}
#========================================================#
# Resolve the note pattern chords into seperate tidy note columns
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
  notes_df_ <- cbind(notes_df$measure_p, notes_df_); colnames(notes_df_)[1] <- "measure_p"
  # Purify the note pattern 
  notes_df_ <- as.data.frame(lapply(notes_df_, function(y) gsub("L|J|K", "", y)))
  notes_df_ <- as.data.frame(lapply(notes_df_, function(y) gsub("'", "", y)))
  notes_df_ <- as.data.frame(lapply(notes_df_, function(y) gsub("\\[|\\]|\\\\|\\/", "", y)))
  # Return the tidied dataframe
  return(notes_df_)
}
#========================================================#
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
#### PREVIOUSLY CALLED: n.v_n.n #### 
## krn_note: Identifies the note and note value of a .krn note using the note_hashtable()
#'  
#' @param note .krn note pattern string 
#' @return note name and note value
#' 
# Input a krn note string
krn_note <- function(note){
  # Check edge cases first
  if(is.na(note)){ return(rep(NA, 2)) }
  # Iterate over our hash table to try and find a match
  for(i in 1:nrow(note_table)){
    pattern <- as.character(note_table$pattern[i])
    # Match found! Return note name and note value.
    if(stringr::str_detect(note, pattern)){ return(c(note_table$label[i], note_table$value[i])) }
  }
  if(stringr::str_detect(note, "r")){ return(c("rest", NA)) }
  else if(stringr::str_detect(note, "\\.")){ return(rep(".", 2)) }
  else{ return(c(note, NA)) } # If no edge cases or notes can be matched, catch corner cases by returning raw note with NA
}

#========================================================#
#                      RELATIVE KEY
#========================================================#
#' Figure out the relative key of a piece (previously called Major_minor)
#' 
#' @param piece A piece of .krn music
#' @import tidyverse
#' 
relative_key <- function(piece){
  krn_key <- piece[1,1]
  if(stringr::str_detect(krn_key,"\\:")){
    krn_key <- gsub("\\*","",krn_key)
    krn_key <- gsub("\\:","",krn_key)
    if(krn_key == toupper(krn_key)){ c(krn_key,"Major") }
    else(c(krn_key,"minor"))
  } else{
    krn_key <- gsub("\t.*","",krn_key)
    key_s <- gsub("\\*k","",krn_key)
    key_s <- gsub("\\[","",key_s)
    key_s <- gsub("\\]","",key_s)
    note_cols <- grep("n\\.n", colnames(piece),value = T)
    note_df <-  piece[,note_cols]
    if(key_s ==""){key_s <- "nosf"}
    m_m <- key[,key_s] %>% unname() 
    tonics <- scales[1,m_m] %>% unlist()%>% unname() %>% as.character()
    fifths <- scales[5,m_m] %>% unlist()%>% unname() %>% as.character()
    tonics_fifths_count <- rep(0,2)
    for(i in 1:ncol(note_df)){
      a <- table(note_df[,i])
      mmtonic <- a[tonics] %>% as.vector
      mmtonic[is.na(mmtonic)]<- 0
      mmfifth <- a[fifths] %>% as.vector
      mmfifth[is.na(mmfifth)]<- 0
      tonics_fifths_count <- tonics_fifths_count + mmtonic + mmfifth
    }
    names(tonics_fifths_count) <- tonics
    Major <- tonics_fifths_count[1] %>% unname
    minor <- tonics_fifths_count[2] %>% unname
    if(Major >= minor){x <- c(tonics[1],"Major")
    }else{x <- c(tonics[2],"minor")}
    x
  }
}


