
#========================================================#
#                       CHORDS
#========================================================#
#' Extract_chord_name_beat
#' This function gives all the harmonic notes that are attacked
#' at the same time
#' @param piece A piece of music in R's piece_df
#' @return A list of all the row by voiced chords
extract_chord_name_beat <- function(piece){
  note_cols <- grep("n\\.n", colnames(piece),value = T)
  note_df <-  piece[,note_cols]
  chords <- list()
  note_df <- map_df(note_df,function(x){gsub("rest",NA,x)})
  x <- !map_df(note_df,is.na)
  for(i in 1:nrow(x)){
    chords[[i]] <- note_df[i,x[i,]] %>%unname() %>% as.character()
  }
  chords
}

#=========================================================
#=========================================================
#' extract_chord_value_beat
#' Extracts..
#' @param piece
#' @return list of all note value chords

extract_chord_value_beat <- function(piece){
  piece <- assign_sd(piece)
  note_cols <- grep("n\\.v", colnames(piece),value = T)
  note_df <-  piece[,note_cols]
  chords <- list()
  note_df <- map_df(note_df,function(x){gsub("rest",NA,x)})
  x <- !map_df(note_df,is.na)
  for(i in 1:nrow(x)){
    chords[[i]] <- note_df[i,x[i,]] %>%unname() %>% as.numeric()
  }
  return(chords)
}

#=========================================================
#=========================================================
#' one_chord_harms
#' Given a chord, this returns the intervals between chord notes.
#' @param chord A vector of chord notes
#' @return A vector of the intervals between chord notes
one_chord_harms <- function(chord){
  l <- length(chord)
  if(l < 2){return(chord)}
  c <- 0
  for(i in 2:l){
    c[i-1] <- (chord[i] - chord[i-1]) %% 12
  }
  c
}

#=========================================================
#' chord_harms
#' Given a piece, this returns a list of intervals between chord notes.
#' @param piece
#' @return A list of chords spelled out by interval
chord_harms <- function(piece){
  chords <- extract_chord_value_beat(piece)
  harm_form <- map(chords,one_chord_harms)
  harm_form
}

#=========================================================
#' harm_ints
#'
#' @param piece
#' @return

harm_ints <- function(piece){
  chords <- chord_harms(piece)
  l <- length(chords)
  ls <- map(chords,length) %>% unlist
  harms <- which(ls == 1)
  twos <- chords[harms] %>% unlist
  ints <- c("unison","m2", "M2","m3", "M3","p4","tt",
            "p5", "m6","M6","m7","M7")
  m <- table(twos)/sum(table(twos))
  names(m) <- ints
  m
}


#=========================================================
#' freq_chord_size
#'
#' @param piece
#' @param type harmonic interval = 2, triad = 3, seventh = 4
#' @return
#'
freq_chord_size <- function(piece,type){
  chords <- chord_harms(piece)
  l <- length(chords)
  ls <- map(chords,length) %>% unlist
  two <- sum(ls ==type)
  freq <- two/l
  freq
}



#========================================================#
#                 MELODIC INTERVALS
#========================================================#
#' tot_mel_int
#' How many melodic intervals each spline has
#' 
#' @param spline .krn file - grouped by spline number
#' @return how many melodic intervals each spline has
#' 
#' @examples 
#' 
#'
# NOTE ONLY CONSIDERING DIM AND MINOR - NO AGUMENTED
tot_mel_int <- function(spline){
  sum(is.na(spline))
}

#=========================================================
#' is_mel_int
#' Checks if two notes is a specified melodic interval
#' 
#' @param n1 one note
#' @param n2 second note
#' @param int what interval we are chekcing for
#' @return T or F 
#' 

is_mel_int <- function(n1,n2, int){ 
  ifelse(n1$V-n2$V == int,T,F)
}  

#=========================================================
#' top_line2
#' 
#' @param piece piece of krn 
#' @param col one instrument name name 
#' @return gives the top melodic line of a instrument
#'
top_line2 <- function(piece,inst){
  inst_cols <- grep(inst,colnames(piece),value = T)
  note_cols <- grep("n\\.n", inst_cols,value = T)
  n <- piece[,note_cols]
  if(length(note_cols)==1){
    x <- map_lgl(n,is.na)
    x <- data.frame(rep(F,length(x)),x)
  }else{
    x <- map_df(n,is.na)
    x <- cbind(rep(F,nrow(x)),x)
  }
  f <- function(i){max(which(i == F))-1}
  notes <- purrrlyr::by_row(x,f, .collate = "cols",
                            .labels = F)[[1]]
  notes
}
#=========================================================
#' voice_mel_ints
#' 
#' @param piece One instrument 
#' @param col name 
#' @return vector of counts for melodic intervals
#' 

mel_ints <- function(piece,col){
  mel <- top_line2(piece,col)
  mel <- as.numeric(as.vector(na.omit(mel)))
  mel_dif <- c()
  for(i in 1:length(mel)-1){
    mel_dif[i] <- abs(mel[i]-mel[i+1] %% 12)
    #min(max(mel[i],mel[i+1]) - min(mel[i],mel[i+1]),
    #                min(mel[i],mel[i+1]) + 12 - max(mel[i],mel[i+1]))
  }
  mel_dif <- mel_dif + 1 # Change indexing to start at 1
  ints <- c("unison","m2", "M2","m3", "M3","p4","tt",
            "p5", "m6","M6","m7","M7")
  mel_fac <- factor(ints[mel_dif], levels = ints, ordered = T)
  m <- table(mel_fac)/sum(table(mel_fac))
  m
}

#=========================================================
#' consonances
#' 
#' @param piece One piece
#' @param col name of intstrument
#' @return vector of counts for consonances
#' 
consonances <- function(piece,col){
  mel <- mel_ints(piece,col)
  perfect <- sum(mel[c(1,6,8)])
  imperfect <- sum(mel[c(4,5,9,10)])
  dissonant <- sum(mel[c(2,3,7,11,12)])
  c(perfect,imperfect,dissonant)
}

#========================================================#
#                    SCALE DEGREE
#========================================================#
#' scale_degree_freq
#' @param piece
#' @return the frequency of occurance of all the scale degrees
#'
scale_degree_freq <- function(piece){
  note_cols <- grep("n\\.n", colnames(piece),value = T)
  note_df <-  piece[,note_cols]
  keyx <- Major_minor(piece)[1]
  scale_degrees <- scales[,keyx] %>% as.vector()
  degrees_count <- rep(0,length(scale_degrees))
  tot <- 0
  for(i in 1:ncol(note_df)){
    a <- table(note_df[,1]) # is this really a better useage?
    tot <- tot + sum(a, na.rm = T)
    scale_deg <- a[scale_degrees] %>% as.vector
    scale_deg[is.na(scale_deg)]<- 0
    degrees_count <- degrees_count + scale_deg
  }
  freq <- degrees_count/tot
  data.frame(scale_degrees,degrees_count,freq)
}

#=========================================================
#=========================================================
#' scale_degree_freq
#' @param piece
#' @return the scale degrees for each note
#'

assign_sd <- function(piece){
  key2 <- Major_minor(piece)[1]
  scalez <- c("G#","A-","A","A#","B-","B","B#","C-","C","C#",
              "D-","D","D#","E-","E","E#","F-","F","F#","G-","G")
  scalez <- c(scalez,scalez)
  start <- min(which(scalez == key2))
  scale_deg_s <- scalez[start:(start+20)]
  scdv <- c(0,0,1,2,2,3,4,3,4,5,5,6,7,7,8,9,8,9,10,10,11) + 1
  scdv <- c(scdv,scdv+scdv[start])
  scdv_key <- scdv[start:(start+20)]
  scdv_key <- scdv_key - (scdv_key[1]-1)
  
  scale_deg_values <- rbind(scale_deg_s,scdv_key)
  scale_deg_values <- cbind(scale_deg_values,c(".",NA),c("rest",NA))
  colnames(scale_deg_values) <- c(scale_deg_s,".","rest")
  
  note_name_cols <- grep("n\\.n", colnames(piece),value = T)
  note_value_cols <- grep("n\\.v", colnames(piece),value = T)
  for(j in 1:length(note_name_cols)){
    for(i in 1:nrow(piece)){
      if(is.na(piece[i,note_name_cols[j]])){
        sd <- NA
      }else{
        note <- piece[i,note_name_cols[j]] %>% unname() %>% as.character()
        sd <- scale_deg_values[2,note]
      }
      piece[i,note_value_cols[j]] <- sd
    }
  }
  piece
}
