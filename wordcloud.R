# Read stopList file 
stopList <- readLines("common-english-words.txt", warn = FALSE)

# Read and Process the Raw Data 
parse_file <- function(file) {
  isUrl <- length(grep("^https?:", file)) == 1
  isFile <- length(grep("[.][[:alnum:]]{2,7}$", file)) == 1
  if (isUrl || isFile) {
    text <- readLines(file, warn = FALSE)
    text <- paste(text, collapse = "")
  } 
  return(text)
}

# Split text into words, remove blanks and "words" containing numbers, and convert to lower case 
tokenize <- function(text) {
  re1 <- "[^[:alpha:]][-][^[:alpha:]]"
  re2 <- "[-[:space:]!@#$%^&*()_+=\'\":;<>.,?/\\]+"
  re <- paste0("(", re1, "|", re2, ")")
  text <- strsplit(text, re) 
  text <- tolower(unlist(text))
  text <- text[nchar(text) > 2]
  text <- grep("[0-9]", text, invert = TRUE, value = TRUE)
  text <- gsub("\\d+","", text)
  return(text)
}

# Filter stopwords 
filter_stopwords <- function(text) {
  stopList <- strsplit(stopList, ",")[[1]]
  # Exclude stop words
  text.rmstop <- Filter(function(word) { !(word %in% stopList) }, text)
  return(text.rmstop)
}

# Determine the Weight (or Weigths) for Each Word and sort in descreasing order 
uniquify_weight_counts_sort <- function(words) {
  table_words <- table(words)
  table_words <- sort(table_words, decreasing = TRUE)
  return(data.frame(words = names(table_words), counts = table_words))
}

# Compute Properties of the Displayed Text Strings 
# Find size of each Text String 
dimensions <- function(text, size) {
  height <- strheight(text, cex = size)
  width <- strwidth(text, cex = size)
  return(c(width, height))
}

# Check for Overlaps between Words 
stroverlap <- function(x1,y1,x2,y2,text1,text2,size1,size2) {
  box1 <- dimensions(text1,size1)
  box2 <- dimensions(text2,size2)
  sw1 <- box1[1]/2
  sh1 <- box1[2]/2
  sw2 <- box2[1]/2
  sh2 <- box2[2]/2
  overlap <- FALSE
  if (x1 < x2) { 
    overlap <- x1 + sw1 > x2 - sw2
  } else {
    overlap <- x2 + sw2 > x1 - sw1
  }
  if (y1 < y2) {
    overlap <- overlap & (y1 + sh1 > y2 - sh2)
  } else { 
    overlap <- overlap & (y2 + sh2 > y1 - sh1)
  }
  return(overlap)
}

# Word Placement using a Randomized, greedy strategy 
place_words <- function(text, counts.Freq, scale = c(1,0.1)) {
  par(mar = c(0,0,0,0))
  plot(c(0,1), c(0,1), ann = F, bty = "n", type = "n", xaxt = "n", yaxt = "n")
  size <- (scale[1] - scale[2]) * counts.Freq + scale[2]
  size <- counts.Freq - min(counts.Freq) + 0.1

  size <- size * 250/sum(nchar(text)*size)
    
  text(0.5, 0.5, text[1], cex = size[1], offset = 0)
  x_in_canvas <- numeric(length(text))
  y_in_canvas <- numeric(length(text))
  x_in_canvas[1] <- 0.5
  y_in_canvas[1] <- 0.5
  text_in_canvas = rep(NA, length(text))
  size_in_canvas = rep(NA, length(text))
  text_in_canvas <- text[1]
  size_in_canvas <- size[1]
  
  for (i in (2:length(text))) {
    boxes <- dimensions(text[i], size[i])
    
    xcord <- runif(1, 0 + boxes[1]/2, 1 - boxes[1]/2)
    ycord <- runif(1, 0 + boxes[2]/2, 1 - boxes[2]/2)

    initial_xcord <- xcord
    initial_ycord <- ycord

    theta <- 0.2
    increment <- 0.1
    overlap_bool <- overlaps_with_any(x_in_canvas[1:(i - 1)], y_in_canvas[1:(i - 1)], 
                                     text_in_canvas[1:(i - 1)], size_in_canvas[1:(i - 1)], 
                                     text[i], size[i], xcord, ycord)
    
    while (overlap_bool) {
      r <- (theta) ^ 1.2
      xcord <- initial_xcord + r * cos(theta)/500
      ycord <- initial_ycord + r * sin(theta)/500
      
      if (xcord< boxes[1]/2 | xcord > 1 - boxes[1]/2 | 
          ycord< boxes[2]/2 | ycord > 1 - boxes[2]/2 |
          theta > 500) {
        initial_xcord <- runif(1, 0 + boxes[1]/2, 1 - boxes[1]/2)
        initial_ycord <- runif(1, 0 + boxes[2]/2, 1 - boxes[2]/2)
        xcord <- initial_xcord
        ycord <- initial_ycord
        theta <- 0.2
      }
      
      
      overlap_bool <- overlaps_with_any(x_in_canvas[1:(i - 1)], y_in_canvas[1:(i - 1)], 
                                       text_in_canvas[1:(i - 1)], size_in_canvas[1:(i - 1)], 
                                       text[i], size[i], xcord, ycord)
      theta <- theta + increment
    }
    
    print(text[i])
    text(xcord, ycord, text[i], cex = size[i], offset = 0)
    
    x_in_canvas[i] <- xcord
    y_in_canvas[i] <- ycord 
    text_in_canvas[i] <- text[i]
    size_in_canvas[i] <- size[i]
  }
}

overlaps_with_any <- function(x_in_canvas, y_in_canvas, 
                              text_in_canvas, size_in_canvas,
                              text, size, xcord, ycord) {
 
  for (j in seq_along(x_in_canvas)) {
    overlap <- stroverlap(x_in_canvas[j], y_in_canvas[j], 
                          xcord, ycord, 
                          text_in_canvas[j], text, 
                          size_in_canvas[j], size)
    # don't compare booleans, just use them 
    if (overlap) {
      return(TRUE)
    }
  }
  return(FALSE)
}