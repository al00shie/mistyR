
#========================================================#
#                   RYHTHM FREQUENCY
#========================================================#
#' rythem freq
#'
#' @param piece
#' @return Vector of frequencies for all rhythmic values
#'

rhythm_freq <- function(piece){
  r_cols <- measure_col <- grep("r\\.v", colnames(piece),value = T)
  r_df <-  piece[,r_cols]
  tot <- rep(0,10)
  names(tot) <- c("2","2.","4","4.","8","8.","16","16.","32")
  a <- list()
  for(i in 1:ncol(r_df)){
    a <- table(r_df[,i])
    b <- c(tot,a)
    tot <- tapply(b,names(b),sum)
  }
  freqs <- tot/sum(!is.na(r_df))
  freqs
}

#==============================================================
#' dot freq
#'
#' @param piece
#' @return Frequency of dotted rhythms in the piece
#'

dot_freq <- function(piece){
  f <- rhythm_freq(piece)
  s <- f["16."] + f["2."] + f["8."]
  s
}

#============================================================
#' topline rhythem freqs
#'
#' @param piece
#' @return Frequencies
#'

top_rhythm_freq <- function(piece){
  r_cols <- measure_col <- grep("r\\.v", colnames(piece),value = T)
  r_df <-  piece[,r_cols]
  tot <- rep(0,10)
  names(tot) <- c("2","2.","4","4.","8","8.","16","16.","32")
  a <- table(r_df[,1])
  freqs <- a/sum(!is.na(r_df))
  freqs
}

#==============================================================
#' top dot freq
#'
#' @param piece
#' @return Frequency of dotted rhythms in the piece
#'

top_dot_freq <- function(piece){
  f <- top_rhythm_freq(piece)
  s <- f["16."] + f["2."] + f["8."]
  s
}

#========================================================#
#                         DENSITY
#========================================================#
#' beat_density
#' 
#' @param piece in raw data frame
#' @return density for each measure
#'

beat_density <- function(piece){
  note_cols <- grep("measure|n\\.n", colnames(piece) ,value = T)
  note_df <- piece[,note_cols]
  onote_df <- note_df[,-1]
  d <- vector()
  for(i in 1:nrow(onote_df)){
    d[i] <- sum(!is.na(onote_df[i,]))
  }
  d <- d[which(d != 0)]
  c(mean(d),sd(d))
}

#==============================================================
#' note_duration
#' 
#' @param piece in raw data frame
#' @return density for each measure
#'

note_duration <- function(piece,inst){
  rhy_cols <- grep("measure|r\\.v_1", colnames(piece) ,value = T)
  rhy_df <- piece[,note_cols]
  onote_df <- note_df[,-1]
  rhy_no_na <- as.numeric(as.vector(na.omit()))
}

#========================================================#
#                     RHYTHM ENTROPY
#========================================================#
#' rhythm entropy
#'
#' @param piece
#' @return Time signature of the piece
#'
rhy_entropy <- function(piece){
  r_cols <- measure_col <- grep("r\\.v", colnames(piece),value = T)
  r_df <-  piece[,r_cols]
  changes <- 0
  for(j in 1:ncol(r_df)){
    rv <- as.numeric(as.vector(na.omit(r_df[,j])))
    changes_j <- 0
    for(i in 2:length(rv)){
      c <- rv[i]-rv[i-1]
      changes_j[i-1] <-ifelse(c ==0,0,1)
    }
    changes[j] <- mean(changes_j)
  }
  changes <- mean(changes,na.rm = T)
  changes
}

#========================================================#
#                       DURATION
#========================================================#
#' Duration
#' 
#' @param piece
#' @return piece that has duration included
#' 

durration_df <- function(piece){
  cols <- grep("n\\.n|n\\.o|r\\.v", colnames(piece),value = T)
  for(j in 1:length(cols)){
    for(i in 2:nrow(piece)){
      if(!is.na(piece[i,cols[j]])){
        if(piece[i,cols[j]] == "." ){
          piece[i,cols[j]] <- piece[i-1,cols[j]]
        }
      }
    }
  }
  piece
}

#========================================================#
#                       LENGTH
#========================================================#
#' Length
#' 
#' 

length_measures <- function(piece){
  measure_col <- grep("measure", colnames(piece),value = T)
  measure <-  piece[,measure_col] %>% as.numeric()
  max_m <- max(measure)
  max_m
}

#========================================================#
#                        METER
#========================================================#
#' Meter
#'
#' @param piece
#' @return Time signature of the piece
#'

meter <- function(piece){
  m <- piece[1,2]
  met <- regmatches(m, regexpr("M.{,2}",m))
  met <- sub(met, "", "M")
  met
}