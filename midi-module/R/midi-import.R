
#========================================================#
#                        MIDI2DF
#========================================================#
#' midi2df
#' Reads a .mid file into the same tidy piece frame that kern2df
#' produces: key_p, meter_p, measure_p + one (rhythmN, note_nameN,
#' note_valueN) triplet per simultaneous voice slot. This gives the
#' kern-side analysis suite (analyzeScore, rhythm_freq, rhythm_entropy,
#' beat_density, chord_harms, scale_degree_freq, ...) a MIDI front end
#' with no changes on the analysis side.
#'
#' Notes attacked at the same tick are treated as one chord (kern's
#' simultaneity convention). Note spelling is key-aware per key window:
#' MIDI supports mid-piece key changes, and each note is spelled with
#' the flats/sharps of the key signature in force at its attack time.
#'
#' @param midi_file path to a .mid file
#' @return tidy piece dataframe (kern2df-compatible)
midi2df <- function(midi_file){
  # Read the event stream and the note stream
  midi_events <- tuneR::readMidi(midi_file)
  notes <- tuneR::getMidiNotes(midi_events)
  # File-level parameters
  division <- .m2df_get_division(midi_file)     # ticks per quarter note
  meter <- .m2df_get_meter(midi_events)         # c(numerator, denominator)
  bar_ticks <- division * (4 / meter[2]) * meter[1]
  # Key windows (start tick -> misty major-key label)
  key_table <- .m2df_get_keys(midi_events)
  # Group notes into simultaneity chords by attack tick
  attack_ticks <- sort(unique(notes$time))
  chords <- split(notes, factor(notes$time, levels = attack_ticks))
  max_notes <- max(purrr::map_int(chords, nrow))
  # Piece information columns
  key_col <- key_table$key[findInterval(attack_ticks, key_table$time)]
  piece_info <- data.frame(
    key_p = key_col,
    meter_p = paste("*M", meter[1], "/", meter[2], sep = ""),
    measure_p = 1 + floor(attack_ticks / bar_ticks)
  )
  # Resolve each voice slot into (rhythm, note_name, note_value) columns
  notes_df <- purrr::map_dfc(1:max_notes, .m2df_resolve_slot,
                             chords = chords, key_col = key_col, division = division)
  # Return the piece info and the resolved note columns (kern2df order)
  cbind(piece_info, notes_df)
}

#========================================================#
#                M2DF: PIECE INFORMATION
#========================================================#
# Ticks per quarter note live in bytes 13-14 of the MIDI header
# (big-endian); tuneR::readMidi does not expose them.
.m2df_get_division <- function(midi_file){
  con <- file(midi_file, "rb")
  header <- readBin(con, "raw", n = 14)
  close(con)
  as.integer(header[13]) * 256 + as.integer(header[14])
}
#========================================================#
# Parse the first Time Signature event into c(numerator, denominator);
# default to 4/4 when the file carries none.
.m2df_get_meter <- function(midi_events){
  ts <- midi_events[midi_events$event == "Time Signature", "parameterMetaSystem"]
  if(length(ts) == 0){ return(c(4, 4)) }
  frac <- strsplit(strsplit(ts[1], ",")[[1]][1], "/")[[1]]
  as.numeric(frac)
}
#========================================================#
# Build the key-window table: one row per Key Signature event, with the
# signature converted to the misty major-key label (same convention as
# .k2df_get_key: minor signatures map to their relative major, and
# relative_key() re-votes major vs. minor downstream).
.m2df_get_keys <- function(midi_events){
  ks <- midi_events[midi_events$event == "Key Signature", c("time", "parameterMetaSystem")]
  if(nrow(ks) == 0){ return(data.frame(time = 0, key = "C")) }
  keys <- purrr::map_chr(ks$parameterMetaSystem, .m2df_parse_keysig)
  key_table <- data.frame(time = ks$time, key = keys)
  # Ensure the first window covers the head of the piece
  key_table$time[1] <- 0
  key_table
}
#========================================================#
# "Db major" -> "D-"; "bb minor" -> relative major "D-"; "F# major" -> "F#"
.m2df_parse_keysig <- function(keysig){
  parts <- strsplit(trimws(keysig), "\\s+")[[1]]
  name <- parts[1]; quality <- ifelse(length(parts) > 1, tolower(parts[2]), "major")
  # tuneR spells flats with a trailing 'b'; misty uses the kern '-'
  name <- sub("b$", "-", name)
  if(quality == "minor"){
    # Map the (lowercase) minor key to its relative major
    idx <- match(tolower(name), KRN_KEYS$relative_minor)
    if(!is.na(idx)){ return(KRN_KEYS$key[idx]) }
  }
  name
}

#========================================================#
#                M2DF: NOTE WRANGLING
#========================================================#
# Pitch-class spelling tables (misty note labels, kern '-' flats)
.M2DF_SHARP_NAMES <- c("C","C#","D","D#","E","F","F#","G","G#","A","A#","B")
.M2DF_FLAT_NAMES  <- c("C","D-","D","E-","E","F","G-","G","A-","A","B-","B")
#========================================================#
# A key spells flats iff its label is flatted (or is F major)
.m2df_flat_key <- function(key){ grepl("-", key) || key == "F" }
#========================================================#
# MIDI note number -> misty note label, spelled for the active key
.m2df_note_name <- function(midi_note, key){
  pc <- midi_note %% 12
  if(.m2df_flat_key(key)){ .M2DF_FLAT_NAMES[pc + 1] } else { .M2DF_SHARP_NAMES[pc + 1] }
}
#========================================================#
# MIDI note number -> misty note value (KRN_NOTES numbering, A- = 1)
.m2df_note_value <- function(midi_note){ ((midi_note %% 12) + 4) %% 12 + 1 }
#========================================================#
# Tick length -> kern-style reciprocal rhythm token ("4" = quarter,
# "8." = dotted eighth, ...), snapped to the nearest standard duration
# on a log scale (MIDI lengths are rarely exact).
.m2df_rhythm <- function(ticks, division){
  RHYTHM_LABELS   <- c("1","2","2.","4","4.","8","8.","16","16.","32")
  RHYTHM_QUARTERS <- c( 4,  2,  3,  1, 1.5, 0.5, 0.75, 0.25, 0.375, 0.125)
  quarters <- ticks / division
  if(is.na(quarters) || quarters <= 0){ return(NA_character_) }
  RHYTHM_LABELS[which.min(abs(log(quarters / RHYTHM_QUARTERS)))]
}
#========================================================#
# Resolve voice slot i across all chords into its column triplet
# (rhythmN, note_nameN, note_valueN) - mirrors .k2df_resolve_notes
.m2df_resolve_slot <- function(i, chords, key_col, division){
  num_chords <- length(chords)
  rhythm <- rep(NA_character_, num_chords)
  note_name <- rep(NA_character_, num_chords)
  note_value <- rep(NA_real_, num_chords)
  for(j in 1:num_chords){
    chord <- chords[[j]]
    if(nrow(chord) >= i){
      # Slot order: lowest pitch first (bass in slot 1, like kern splines)
      chord <- chord[order(chord$note), ]
      rhythm[j] <- .m2df_rhythm(chord$length[i], division)
      note_name[j] <- .m2df_note_name(chord$note[i], key_col[j])
      note_value[j] <- .m2df_note_value(chord$note[i])
    }
  }
  slot_df <- data.frame(rhythm, note_name, note_value)
  colnames(slot_df) <- paste(colnames(slot_df), i, sep = "")
  slot_df
}
