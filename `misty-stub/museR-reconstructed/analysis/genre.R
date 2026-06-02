# Returns a list of kern scores by genre
byGenre <- function(genre, dir_path = "../data-krn/genre/", loud = T){
  # Go to the genre file folder
  dir_path <- paste(dir_path, genre, "/", sep = "")
  # Get score file names and get their path
  genre_scores <- dir(path = dir_path, pattern = "\\.krn$")
  # Print the piece names
  if(loud) { print(genre_scores) }
  # Helper function to parse string name
  .ADDprefix <- function(filename, dirpath){paste(dirpath, filename, sep = "", collapse = "")}
  score_paths <- as.list(purrr::map_chr(genre_scores, .ADDprefix, dir_path))
  # Read the kern files and return a list 
  purrr::map(score_paths, kern2df)
}

# List the scores found in the directory by the genre
find_genre_scores <- function(genre){
  # Find genre-sorted directory
  dir_path = "../data-krn/genre/"
  # Go to the genre file folder
  dir_path <- paste(dir_path, genre, "/", sep = "")
  # Get script file names and get their path
  genre_scores <- dir(path = dir_path, pattern = "\\.krn$")
  # Return genre scores
  genre_scores
}

# Using the genre score filename, outputs the relevant kern file
genre_score <- function(genre, krn_file){
  # Find genre-sorted directory
  dir_path = "../data-krn/genre/"
  # Go to the genre file folder
  dir_path <- paste(dir_path, genre, "/", sep = "")
  # Get the kern score for the genre piece
  score_path <- paste(dir_path, krn_file, sep = "")
  # Get the data frame for the score
  kern2df(score_path)
}