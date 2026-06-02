#==============================================================
#                         RHYTHMS 
#==============================================================
# Declare global variable
RHYTHMS <- c("2","2.","4","4.","8","8.","16","16.","32")
#==============================================================
#                       NOTE HASH
#==============================================================
# Returns a note hash table for parsing .krn files
.KRN_NOTE_HASH <- function(){
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
  KRN_NOTE_HASH <- data.frame(pattern = note_patterns, label = note_labels, value = note_values)
  # Make hash table columns characters
  KRN_NOTE_HASH$pattern <- as.character(KRN_NOTE_HASH$pattern); KRN_NOTE_HASH$label <- as.character(KRN_NOTE_HASH$label)
  return(KRN_NOTE_HASH)
}
#==============================================================
#                         KEYS
#==============================================================
.KEYS_TABLE <- function(){    
  key_sigs <- c("nosf","f#","f#c#","f#c#g#","f#c#g#d#","f#c#g#d#a#",
                "f#c#g#d#a#e#","f#c#g#d#a#e#b#",
                "b-e-a-d-g-c-f-","b-e-a-d-g-c-","b-e-a-d-g-",
                "b-e-a-d-","b-e-a-","b-e-","b-")
  major_keys <- c("C","G","D","A","E","B","F#","C#",
                  "C-","G-","D-","A-","E-","B-","F")
  minor_keys <- c("a","e","b","f#","c#","g#","d#","a#","a-",
                  "e-","b-","f","c","g","d")
  notes <- as.factor(c("A","A-","A#","B","B-","B#","C","C-","C#",
                       "D","D-","D#","E","E-","E#","F","F-","F#",
                       "G","G-","G#","rest"))
  key <- (data.frame(key_sig = key_sigs, key = major_keys))
  #colnames(key) <- key_sigs
  # Return key table
  return(key)
}
#==============================================================
#                         SCALES
#==============================================================
.SCALES_TABLE <- function(){
  scale_degree_names <- c("Tonic","Supertonic","Mediant",
                          "Subdominant",
                          "Dominant","Submediant",
                          "Leading Tone")
  # solfege <- c("Do","Di","Ra","Re","Ri","Me","Mi","Fa",
  #              "Fi","Se","Sol","Si","Le","La","Li","Te","Ti")
  # 
   scale_degree_roman_major <- c("I","ii","iii","IV","V","vi","viid")
   scale_degree_roman_minor <- c("i","iid","III","iv","V","VI","viid")
  
  # major scales
  C <- c("C","D","E","F","G","A","B")
  G <- c("G","A","B", "C","D","E","F#")
  D <- c("D","E","F#","G","A","B","C#")
  A <- c("A","B","C#","D","E","F#","G#")
  E <- c("E","F#","G#","A","B","C#","D#")
  B <- c("B","C#","D#","E","F#","G#","A#")
  Fs <- c("F#","G#","A#","B","C#","D#","E#")
  Cs <- c("C#","D#","E#","F#","G#","A#","B#")
  Cb <- c("C-","D-","E-","F-","G-","A-","B-")
  Gb <- c("G-","A-","B-","C-","D-","E-","F")
  Db <- c("D-","E-","F","G-","A-","B-","C")
  Ab <- c("A-","B-","C","D-","E-","F","G")
  Eb <- c("E-","F","G","A-","B-","C","D")
  Bb <- c("B-","C","D","E-","F","G","A")
  FM <- c("F","G","A","B-","C","D","E")
  
  
  # minor scales - natrual
  a <- c("A","B","C","D","E","F","G")
  e <- c("E","F#","G","A","B","C","D")
  b <- c("B","C#","D","E","F#","G","A")
  fs <- c("F#","G#","A","B","C#","D","E")
  cs <- c("C#","D#","E","F#","G#","A","B")
  gs <- c("G#","A#","B","C","D#","E","F#")
  ds <- c("D#","E#","F#","G#","A#","B","C#")
  as <- c("A#","B#","C#","D#","E#","F#","G#")
  ab <- c("A-","B-","C-","D-","E-","F-","G-")
  eb <- c("E-","F","G-","A-","B-","C-","D-")
  bb <- c("B-","C","D-","E-","F","G-","A-")
  f <- c("F","G","A-","B-","C","D-","E-")
  c <- c("C","D","E-","F","G","A-","B-")
  g <- c("G","A","B-","C","D","E-","F")
  d <- c("D","E","F","G","A","B-","C")
  
  scales <- data.frame(scale_degree_names,scale_degree_roman_major,
                       C,G,D,A,E,B,Fs,Cs,
                       Cb,Gb,Db,Ab,Eb,Bb,FM,scale_degree_roman_minor,
                       a,e,b,fs,cs,gs,ds,as,ab,eb,bb,f,c,g,d)
  colnames(scales) <- c("scale_degree_names","scale_degree_roman_major",
                        "C","G","D","A","E","B","F#","C#",
                        "C-","G-","D-","A-","E-","B-","F",
                        "scale_degree_roman_minor",
                        "a","e","b","f#","c#","g#","d#","a#",
                        "a-","e-","b-","f","c","g","d")
  # Return scales table
  return(scales)
}
#==============================================================

