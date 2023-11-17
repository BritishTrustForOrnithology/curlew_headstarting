


# FUNCTION to convert coordinates in degrees, minutes, seconds to decimal degrees

dms2dec <- function(dms, separators = c("º", "°", "\'", "’", "’’", "\"", "\'\'", "\\?", "′")) {
  
  # version 1.4 (2 Feb 2022)
  # dms: a vector of latitude or longitude in degrees-minutes-seconds-hemisfere, e.g. 41° 34' 10.956" N (with or without spaces)
  # separators: the characters that are separating degrees, minutes and seconds in 'dms'; mind these are taken in the order in which they appear and not interpreted individually, i.e. 7'3º will be taken as 7 degrees, 3 minutes! input data are assumed to be properly formatted
  
  dms <- as.character(dms)
  dms <- gsub(pattern = " ", replacement = "", x = dms)
  for (s in separators) dms <- gsub(pattern = s, replacement = "_splitHere_", x = dms)
  
  splits <- strsplit(dms, split = "_splitHere_")
  n <- length(dms)
  deg <- min <- sec <- hem <- vector("character", n)
  
  for (i in 1:n) {
    deg[i] <- splits[[i]][1]
    min[i] <- splits[[i]][2]
    
    if (length(splits[[i]]) < 4) {
      hem[i] <- splits[[i]][3]
    } else {
      sec[i] <- splits[[i]][3]
      hem[i] <- splits[[i]][4]
    }
  }
  
  dec <- colSums(rbind(as.numeric(deg), (as.numeric(min) / 60), (as.numeric(sec) / 3600)), na.rm = TRUE)
  sign <- ifelse (hem %in% c("N", "E"), 1, -1)
  hem_miss <- which(is.na(hem))
  if (length(hem_miss) > 0) {
    warning("Hemisphere not specified at position(s) ", hem_miss, ", so the sign of the resulting coordinates may be wrong.")
  }
  dec <- sign * dec
  return(dec)
}  # end dms2dec function