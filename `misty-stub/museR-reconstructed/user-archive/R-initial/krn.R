#========================================================#
#                       KRN FUNCTIONS
#========================================================#
#' piece_df
#' Takes splines in .krn and calls kern2df
#' to create one df for entire piece
#'
#' @param v vector with splines .krn file strings
#' @param instruments vector of same lenght as v,
#' including instrument names for each spline
#' @return dataframe for entire piece - combined splines

piece_df_initial <- function(v, instruments){
  len <- length(v)
  c <- vector()
  first <- T
  for(i in 1:len){
    Ai <- kern2df_initial(v[i])
    if(first){
      piece <- Ai
    }
    if(!first){
      piece <- cbind(piece,Ai[,-(1:3)])
    }
    max_notes <- (ncol(Ai)-3)/4
    for(k in 1:max_notes){
      if(first){cols <- c("key","meter","measure","r.v","n.o","n.n","n.v")}
      if(!first){cols <- c("r.v","n.o","n.n","n.v")}
      c_i <- vector()
      for(j in 1:length(cols)){
        c_i[j] <- paste(instruments[i],"_",cols[j],k,sep = "",collapse="")
      }
      c <- c(c,c_i)
      first <- F
    }
  }
  colnames(piece) <- c
  piece
}
###############################################################
###############################################################
#' kern2df
#' Kern 2 data frame
#' Takes a .krn spline and converts into data frame
#'
#' @param spline .krn file
#' @return dataframe of stuff

kern2df_initial <- function(spline){ # takes a spline(".krn") as input
  data <- readLines(spline)
  key_v <- grep("^\\*.*[^I]:$",data,value = T)
  if(identical(key_v,character(0))){
    key_v <- grep("\\*{1}k\\[",data,value = T)
  }
  key_v<- gsub("\t.*","",key_v)
  meter <- grep("\\*{1}M[0-9]",data,value = T)
  data <- data[-grep("^!|\\*", data)] #removes extra text and info
  measures <- grep("=",data, value = F) # measures in .krn start wtih = 1
  val_list_notes <- data[-measures]
  if(!grepl("=1-",data[1])){measures <- c(0,measures)}
  measure_numbers <- 0:(length(measures)) # how many measures there are
  measure_column <- vector() # make a vector of which row each is in.
  for(i in 2:length(measures)){
    len <- as.numeric(measures[i]-measures[(i-1)])-1
    val <- measure_numbers[i]
    measure_column <- c(measure_column, rep(val, len))
  }
  sep_chord <- list() #creates a list that seperates 4E 8n into two
  for(i in 1:length(val_list_notes)){
    sep_chord[[i]] <- unlist(strsplit(val_list_notes[i],"\\s"))
  }
  max_notes <- max(lengths(sep_chord))
  n <- data.frame()
  for( j in 1:max_notes){
    for( i in 1:length(val_list_notes)){
      n[i,j]<-sep_chord[[i]][j]
    }
  }
  piece <- cbind(measure_column, n)
  piece <- as.data.frame(lapply(piece, function(y) gsub("L|J|K", "", y)))
  piece <- as.data.frame(lapply(piece, function(y) gsub("'", "", y)))
  piece <- as.data.frame(lapply(piece, function(y) gsub("\\[|\\]|\\\\|\\/", "", y)))
  spline_df <- data.frame(rep(key_v,nrow(piece)),
                          rep(meter,nrow(piece)),
                          measure_column)
  for(i in 2:(max_notes+1)){
    #ri <- stringr::str_extract(piece[,i],"[0-9]{1,2}(\\.*)|\\.") # rhythem value
    ri <- stringr::str_extract(piece[,i],"[0-9]{1,2}(\\.*)")
    #rin <- add_r.n(ri) # rhythem name
    #notei <- stringr::str_extract(piece[,i],"[A-z]{1,2}.*|^\\.")
    notei <- stringr::str_extract(piece[,i],"[A-z]{1,2}.*")
    notei_nv <- add_n.v_n.n(notei)
    spline_df <- cbind(spline_df,ri,notei,notei_nv)
  }
  spline_df <- as.data.frame(lapply(spline_df,
                                    function(y) gsub("K", "", y)))
  spline_df
}

#========================================================#
#                       .KRN NOTE
#========================================================#
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
#           INCOMPLETE. IGNORE R.N.
#==============================================================
#' r.n
#' 
#' Gives the name of a given rhythm value
#'
#' @param note one note
#' @return rhythm name
#'
#'
# r.n <- function(note){
#   if(is.na(note)){
#     n <- NA
#   }else if(note == 4){
#     n <- "Quarter note" 
#   }else{
#     n <- "Tbd"
#   }
#   return(n)
# }
#==============================================================
#' Takes result of kern_2_df and changes
#' rhythms to name values (ie changes 4 to quarter note)
#'
#' @param ri individual note vector
#' @return columns with rhythm names
#'
#'
# add_r.n <- function(ri){
#   v <- vector()
#   for(i in 1:length(ri)){
#     v[i] <- r.n(ri[i]) 
#   }
#   v
# }

#========================================================#
#                      RELATIVE KEY
#========================================================#
#' Figure out the relative key of a piece (previously called Major_minor)
#' 
#' @param piece A piece of .krn music
#' @import tidyverse
#' 
#' ALI ---- I called this relative_key in R-clean
#' 
#' 
Major_minor <- function(piece){
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


