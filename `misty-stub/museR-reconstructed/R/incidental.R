
#============================================#
#          INCIDENTAL INCRMENTATION
#============================================#
# A wrapper infix function that increments a note with an incidental (simple/letter-preserving)
`%INC%` <- function(tonic, incidental){
  if(incidental == "#"){
    return(.addIncidental(tonic, "sharp"))
  }
  else if(incidental == "b"){
    return(.addIncidental(tonic, "flat"))
  }
}
#============================================#
# Given a note and an incidental, adds up to a double incidental (simple/letter-preserving; no enharmonics)
.addIncidental <- function(note, incidental){
  # Get note letter
  note_letter <- .dropIncidental(note)
  # See if the note already has an incidental
  note_inc <- .detectIncidental(note)
  # Find appropriate incidental
  if(incidental == "flat"){ 
    if(note_inc == "flat"){ incidental <- "&" } 
    else if(note_inc == "natural"){ incidental <- "b" }
    else if(note_inc == "sharp"){ incidental <- "" }
  }
  else if(incidental == "sharp"){
    if(note_inc == "sharp"){ incidental <- "x" } 
    else if(note_inc == "natural"){ incidental <- "#" }
    else if(note_inc == "flat"){ incidental <- "" }
  }
  # Otherwise, print a warning and return no incidental
  else{
    print("Warning: possible misuse; incidental not found.")
    incidental <- ""
  }
  # Return note with added incidental
  return(paste(note_letter, incidental, sep = ""))
}
#============================================#
# Given a note, drop the incidental and obtain the note letter
.dropIncidental <- function(note){ substring(note, first = 1, last = 1) }

#============================================#
#           INCIDENTAL DETECTION
#============================================#
# Given a note, use the extended incidental hash table to detect incidental
.detectIncidental <- function(note){
  # Use str_detect to see if the note has an incidental and return match
  for(i in 1:nrow(INC_HASH)){
    # Warning: for ## and bb; might confound with # and b
    if(str_detect(note, pattern = INC_HASH$symbol[i])){ return(INC_HASH$incidental[i]) }
  }
  # Otherwise, return "natural" or no incidental
  return("natural")
}
#============================================#
#           INCIDENTAL ARITHMETIC
#============================================#
# Return either the number of semitones of a incidental or incidental from semitones
.incidentalSemitones <- function(x, inverse = T){
  INCIDENTAL <- c("&", "b", "", "#", "x")
  VALUE <- c(-2,-1,0,1,2)
  if(inverse) { return(INCIDENTAL[which(VALUE == x)]) }
  VALUE[which(INCIDENTAL == x)]
}
