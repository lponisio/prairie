## function to clean up white-space in a column of data (replaces all
## instances of white-space with " " and empty cells with ""
fix.white.space <- function(d) {
  d <- as.character(d)
  remove.first <- function(s) substr(s, 2, nchar(s))
  d <- gsub("      ", " ", d, fixed=TRUE)
  d <- gsub("     ", " ", d, fixed=TRUE)
  d <- gsub("    ", " ", d, fixed=TRUE)
  d <- gsub("   ", " ", d, fixed=TRUE)
  d <- gsub("  ", " ", d, fixed=TRUE)

  tmp <- strsplit(as.character(d), " ")
  d <- sapply(tmp, function(x) paste(x, collapse=" "))

  first <- substr(d, 1, 1)
  d[first==" "] <- remove.first(d[first==" "])
  d
}

## Function to convert temperatures from F to C
fahrenheit_to_celsius <- function(fahrenheit) {
  celsius <- (fahrenheit - 32) * (5/9)
  return(celsius)
}

