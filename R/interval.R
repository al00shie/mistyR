#============================================#
#          INTERVAL CONSTRUCTION
#============================================#
.intervalNote <- function(tonic, degree){
  # Coerce the degree as a numeric 
  degree_n <- parse_number(degree)
  # Get the number of semitones from the scale degree
  semitones <- SCALE_DEGREES[[degree]]
  # Get the target note letter (by stepping up n - 1 times)
  white <- tonic %STEP+% (degree_n - 1)
  # Find the semitones spanned
  semitones_white <- .WKsemitones(tonic, degree_n)
  # Use accidentals to find the difference
  accidental <- .accidentalSemitones(semitones - semitones_white)
  # Paste accidental the correct target note
  note <- paste(white, accidental, sep = "")
  # Return the interval
  note
}

#============================================#
#            WHITE KEY SEMITONES
#============================================#
# Find the white-key range number of semitones between a note and a given scale degree
.WKsemitones <- function(note, degree){
  # Get the alphabet range for the chosen note and degree
  note_idx <- note_index(note)
  range <- .ALPH_RANGE(note_idx, degree)
  letter_range <- ALPHABET[range,]
  # Get the number of flats in range
  FLATS <- sum(letter_range[2:degree,]$HAS_FL)
  # Get the number of white keys in the range (exclude tonic)
  WHITES <- degree - 1
  # Return the number of semitones
  semitones <- FLATS + WHITES
  semitones
}

#============================================#
#            SEMITONE ARITHMETIC
#============================================#
`%ST+%` <- function(note, semitones){
  if(semitones == 1){ return(note %ACC% "#") }
  else if(semitones == 2){ return((note %ACC% "#") %ACC% "#")}
}
#============================================#
`%ST-%` <- function(note, semitones){
  if(semitones == 1){ return(note %ACC% "b") }
  else if(semitones == 2){ return((note %ACC% "b") %ACC% "b")}
}


