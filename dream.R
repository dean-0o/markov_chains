
dream <- function(corpus_path, 
                  max_iter = 300,
                  max_ngrams = 3){
  
  library(stringr)
  library(ngram)
  library(RWeka)
  options(stringsAsFactors = F)
  
  # retrieve the subconscious and find a starting place
  subconscious <- ngram_asweka(
    
    tolower(paste(unlist(readLines(corpus_path)), collapse = " ")),
    min = 2,
    max = max_ngrams 
    
  ) # end subconscious
  subconscious <- gsub("\\s+", " ", subconscious) # sub single space for multiples
  vision <- sample(subconscious, size = 1)
  
  # spin a dream from available material
  for(i in 1:sample(100:max_iter, size = 1)){
    
    # get the last word of the poem so far using stringr function 'word'
    connector <- word(vision, -1)
    
    # get all ngrams that start with that word
    # not using a unique function, so that more frequent constructions are used more frequently
    myblocks <- subconscious[grepl(paste0("^", connector, " "), subconscious)]
    
    # choose one block randomly, and eliminate the connecting word
    nextPhrase <- sample(myblocks, size = 1)
    readyPhrase <- paste(unlist(str_split(nextPhrase, boundary("word")))[-1], collapse = " ")
    
    vision <- paste(vision, readyPhrase)
    
  } # end dream creation loop
  
  # transform punctuation back into symbols
  
  vision <- gsub(pattern = " xherequotationstart ",
                 replacement = " '",
                 x = vision)
  
  vision <- gsub(pattern = " xherequotationend ",
                 replacement = "' ",
                 x = vision)
  
  vision <- gsub(pattern = " XHEREperiod| xhereperiod", 
                 replacement = "\\.\n\n",
                 x = vision)
  
  vision <- gsub(pattern = " xhereapostrophe ", 
                 replacement = "'",
                 x = vision)
  
  vision <- gsub(pattern = "XHEREdash|xheredash", 
                 replacement = "-",
                 x = vision)
  
  vision <- gsub(pattern = " XHEREcomma| xherecomma", 
                 replacement = ",",
                 x = vision)
  
  vision <- gsub(pattern = " XHEREcolon| xherecolon", 
                 replacement = ":",
                 x = vision)
  
  vision <- gsub(pattern = " XHEREsemicolon| xheresemicolon", 
                 replacement = ";",
                 x = vision)
  
  vision <- gsub(pattern = "XHEREampersand|xhereampersand", 
                 replacement = "\\&",
                 x = vision)
  
  vision <- gsub(pattern = "XHEREopenparenthesis |xhereopenparenthesis ", 
                 replacement = "\\(",
                 x = vision)
  
  vision <- gsub(pattern = " XHEREcloseparenthesis| xherecloseparenthesis", 
                 replacement = "\\)",
                 x = vision)
  
  vision <- gsub(pattern = " XHEREexclamationpoint| xhereexclamationpoint", 
                 replacement = "!",
                 x = vision)
  
  vision <- gsub(pattern = " XHEREquestionmark| xherequestionmark", 
                 replacement = "\\?",
                 x = vision)
  
  vision <- gsub(pattern = "XHERElinebreak|xherelinebreak", 
                 replacement = "\n",
                 x = vision)
  
  vision <- gsub(pattern = "XHEREend|xhereend", 
                 replacement = "",
                 x = vision)
  
  cat(vision)
  
  return(vision)
  
} # end function
