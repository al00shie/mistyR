
#============================================#
#            SIMPLE ENHARMONICS
#============================================#
enharmonic <- function(note, above = TRUE, simplify = FALSE){
  # Simplify the enharmonic if prompted
  # if(simplify){ note <- .simpler_enharmonic(note) }
  # Get the current accidental and note letter
  note_letter <- .dropAccidental(note)
  note_acc <- .detectAccidental(note)
  # If the accidental is a black key, cycle enharmonics
  if(.isBlackNote(note)){ return(.cycle_enharmonic(note)) }
  # If it is natural, return a double enharmonic
  else if(note_acc == "natural"){
    # If prompted for letter above, return double flat
    if(above){ return(((MUS_ALPH %STEPUP% note) %ACC% "b") %ACC% "b")}
    # Otherwise, return letter below, double sharp
    else{ return(((MUS_ALPH %STEPDOWN% note) %ACC% "#") %ACC% "#")}
  }
  else if(note_acc == "2flat" || note_acc == "2sharp"){
    return(.simpler_enharmonic(note))
  }
  else{ return(.simpler_enharmonic(note)) }
}
#============================================#
# Given a black key, cycle between the two basic enharmonics
.cycle_enharmonic <- function(black_note){
  # Get the current accidental and note letter
  note_letter <- .dropAccidental(black_note)
  note_acc <- .detectAccidental(black_note)
  # If the accidental is a single sharp or flat, cycle enharmonics
  if(note_acc == "sharp"){ return((MUS_ALPH %STEPUP% note_letter) %ACC% "b") }
  else if(note_acc == "flat"){ return((MUS_ALPH %STEPDOWN% note_letter) %ACC% "#") }
}
#============================================#
#            COMPLEX ENHARMONICS
#============================================#
# Given a note, finds an enharmonic with less accidentals
.simpler_enharmonic <- function(note){
  # Detect note accidental
  accidental <- .detectAccidental(note)
  # Get note letter and its index in the alphabet
  note_letter <- .dropAccidental(note)
  note_idx <- note_index(note_letter)
  # If the accidental is a flat/sharp and is a letter with none, return white key enharmonic
  # Ex/ {Fb and Cb} or {E# and B#}
  if(accidental == "flat" && !(note_letter %in% HAS_FLATS)){
    return( MUS_ALPH %STEPDOWN% note_letter )
  }
  else if(accidental == "sharp" && !(note_letter %in% HAS_SHARPS)){
    return( MUS_ALPH %STEPUP% note_letter )
  }
  # Now, handle cases of double accidentals
  # Ex/ {Fbb and Abb} or {E## and D##}
  if(accidental == "2flat"){
    # If the letter name has no flats, then we may simplify
    if(!(note_letter %in% HAS_FLATS)){ return ((enharmonic(note_letter %ACC% "b")) %ACC% "b") }
    # Otherwise, just go to the letter below
    return(MUS_ALPH %STEPDOWN% note_letter)
  }
  else if(accidental == "2sharp"){
    # If the letter name has no flats, then we may simplify
    if(!(note_letter %in% HAS_SHARPS)){ return ((enharmonic(note_letter %ACC% "#")) %ACC% "#") }
    # Otherwise, just go to the letter above
    return(MUS_ALPH %STEPUP% note_letter)
  }
}
#============================================#
#             HELPER FUNCTIONS
#============================================#
# Given a note, determine whether it is a "black" key (on the piano)
.isBlackNote <- function(note){
  # Get the note accidental and note letter
  note_acc <- .detectAccidental(note)
  note_letter <- .dropAccidental(note)
  # Depending on accidental, check whether it is a black key
  if(note_acc == "flat"){ return(note_letter %in% HAS_FLATS) }
  else if(note_acc == "sharp"){ return(note_letter %in% HAS_SHARPS) }
  FALSE
}
#============================================#
# Given a note, determine whether it is a "white" key (on the piano)
.isWhiteNote <- function(note){
  !.isBlackNote(note)
}

#============================================#
#          ACCIDENTAL INCRMENTATION
#============================================#
# A wrapper infix function that increments a note with an accidental (simple/letter-preserving)
`%ACC%` <- function(tonic, accidental){
  if(accidental == "#"){
    return(.addAccidental(tonic, "sharp"))
  }
  else if(accidental == "b"){
    return(.addAccidental(tonic, "flat"))
  }
}
#============================================#
# Given a note and an accidental, adds up to a double accidental (simple/letter-preserving; no enharmonics)
.addAccidental <- function(note, accidental){
  # Get note letter
  note_letter <- .dropAccidental(note)
  # See if the note already has an accidental
  note_acc <- .detectAccidental(note)
  # Find appropriate accidental
  if(accidental == "flat"){
    if(note_acc == "flat"){ accidental <- "&" }
    else if(note_acc == "natural"){ accidental <- "b" }
    else if(note_acc == "sharp"){ accidental <- "" }
  }
  else if(accidental == "sharp"){
    if(note_acc == "sharp"){ accidental <- "x" }
    else if(note_acc == "natural"){ accidental <- "#" }
    else if(note_acc == "flat"){ accidental <- "" }
  }
  # Otherwise, print a warning and return no accidental
  else{
    print("Warning: possible misuse; accidental not found.")
    accidental <- ""
  }
  # Return note with added accidental
  return(paste(note_letter, accidental, sep = ""))
}
#============================================#
# Given a note, drop the accidental and obtain the note letter
.dropAccidental <- function(note){ substring(note, first = 1, last = 1) }

#============================================#
#           ACCIDENTAL DETECTION
#============================================#
# Given a note, use the extended accidental hash table to detect accidental
.detectAccidental <- function(note){
  # Use str_detect to see if the note has an accidental and return match
  for(i in 1:nrow(ACC_HASH)){
    # Warning: for ## and bb; might confound with # and b
    if(str_detect(note, pattern = ACC_HASH$symbol[i])){ return(ACC_HASH$accidental[i]) }
  }
  # Otherwise, return "natural" or no accidental
  return("natural")
}
#============================================#
#           ACCIDENTAL ARITHMETIC
#============================================#
# Return either the number of semitones of a accidental or accidental from semitones
.accidentalSemitones <- function(x, inverse = T){
  ACCIDENTAL <- c("&", "-", "", "#", "x")
  VALUE <- c(-2,-1,0,1,2)
  if(inverse) { return(ACCIDENTAL[which(VALUE == x)]) }
  VALUE[which(ACCIDENTAL == x)]
}
