#### n.v_n.n #### 
#' Identifies the note and note value of a .krn note
#'  
#' @param note .krn file 
#' @return NNV and DNV
#' 

n.v_n.n <- function(note){
  if(is.na(note)){
    v <- NA
    val <- NA
  } else if(stringr::str_detect(note, "[Aa]-")){
    v <- "A-"
    val <- 1
  } else if(stringr::str_detect(note, "[Aa](?!#|-)[kTp;n\\)_]*$")){
    v <- "A"
    val <- 2
  } else if(stringr::str_detect(note, "[Aa]#")){
    v <- "A#"
    val <- 3
  } else if(stringr::str_detect(note, "[Bb]-")){
    v <- "B-"
    val <- 3
  } else if (stringr::str_detect(note, "[Bb](?!#|-)[kTp;n\\)_]*$")){
    v <- "B"
    val <- 4
  }else if (stringr::str_detect(note, "[Bb]#")){
    v <- "B#"
    val <- 5
  } else if (stringr::str_detect(note, "[Cc]-")){
    v <- "C-"
    val <- 4
  }  else if (stringr::str_detect(note, "[Cc](?!#|-)[kTp;n\\)_]*$")){
    v <- "C"
    val <- 5
  } else if (stringr::str_detect(note, "[Cc]#")){
    v <- "C#"
    val <- 6
  } else if (stringr::str_detect(note, "[Dd]-")){
    v <- "D-"
    val <- 6
  } else if (stringr::str_detect(note, "[Dd](?!#|-)[kTp;n\\)_]*$")){
    v <- "D"
    val <- 7
  } else if (stringr::str_detect(note, "[Dd]#")){
    v <- "D#"
    val <- 8
  } else if (stringr::str_detect(note, "[Ee]-")){
    v <- "E-"
    val <- 8
  } else if (stringr::str_detect(note, "[Ee](?!#|-)[kTp;n\\)_]*$")){
    v <- "E"
    val <- 9
  } else if (stringr::str_detect(note, "[Ee]#")){
    v <- "E#"
    val <- 10
  } else if (stringr::str_detect(note, "[Ff]-")){
    v <- "F-"
    val <- 9
  } else if (stringr::str_detect(note, "[Ff](?!#|-)[kTp;n\\)_]*$")){
    v <- "F"
    val <- 10
  } else if (stringr::str_detect(note, "[Ff]#")){
    v <- "F#"
    val <- 11
  } else if (stringr::str_detect(note, "[Gg]-")){
    v <- "G-"
    val <- 11
  } else if (stringr::str_detect(note, "[Gg](?!#|-)[kTp;n\\)_]*$")){
    v <- "G"
    val <- 12
  } else if (stringr::str_detect(note, "[Gg]#")){
    v <- "G#"
    val <- 1
  } else if (stringr::str_detect(note, "r")){
    v <- "rest"
    val <- NA
  } else if (stringr::str_detect(note,"\\.")){
    v <- "."
    val <- "."
  }else {
    v <- note
    val <- NA
  }
  r <- c(v,val)
  return(r)
}
#==============================================================
#### note_value #### 
#' add_n.v_n.n
#' Add note value note name
#' Adds columns with more easily analyzable note names
#'  
#' @param notes one note line for one instrument
#' @return data frame with orriginal piece and NNV and DNV
#' 

add_n.v_n.n <- function(notez){
  df <- data.frame(colnames(c("n.n","n.v")))
  #v <- vector()
  for(i in 1:length(notez)){
    #v[i] <- n.v_n.n(notes[i])
    df[i,1] <- n.v_n.n(notez[i])[1]
    df[i,2] <- n.v_n.n(notez[i])[2] 
  }
  df
}  


#==============================================================
#' r.n
#' 
#' Gives the name of a given rhythm value
#'
#' @param note one note
#' @return rhythm name
#'
#'
r.n <- function(note){
  if(is.na(note)){
    n <- NA
  }else if(note == 4){
    n <- "Quarter note" 
  }else{
    n <- "Tbd"
  }
  return(n)
}
#==============================================================
#' Takes result of kern_2_df and changes
#' rhythms to name values (ie changes 4 to quarter note)
#'
#' @param ri individual note vector
#' @return columns with rhythm names
#'
#'
add_r.n <- function(ri){
  v <- vector()
  for(i in 1:length(ri)){
   v[i] <- r.n(ri[i]) 
  }
  v
}
  
#========================================================#
#                        KEY
#========================================================#
#' Figure out the key of a piece
#' 
#' @param piece A piece of .krn music
#' @import tidyverse
#' 
Major_minor <- function(piece){
  krn_key <- piece[1,1]
  if(stringr::str_detect(krn_key,"\\:")){
    krn_key <- gsub("\\*","",krn_key)
    krn_key <- gsub("\\:","",krn_key)
    if(krn_key == toupper(krn_key)){
      c(krn_key,"Major")
    }else(c(krn_key,"minor"))
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

  
  