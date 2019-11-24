
# prepare corpus function

prepare_corpus <- function(corpus_path, out_dir){
  
  vision <- tolower(paste(unlist(readLines(corpus_path)), collapse = " "))
  
  vision <- gsub(pattern = "\\\"", 
                 replacement = "",
                 x = vision)
  
  vision <- gsub(pattern = "\\[\\d*\\]",
                 replacement = "",
                 x = vision)
  
  vision <- gsub(pattern = "\\s*\\([^\\)]+\\)",
                 replacement = "",
                 x = vision)
  
  vision <- gsub(pattern = "\\.", 
                 replacement = " xhereperiod ",
                 x = vision)
  
  vision <- gsub(pattern = "-", 
                 replacement = " xheredash ",
                 x = vision)
  
  vision <- gsub(pattern = "'", 
                 replacement = " xhereapostrophe ",
                 x = vision)
  
  vision <- gsub(pattern = ",", 
                 replacement = " xherecomma ",
                 x = vision)
  
  vision <- gsub(pattern = ":", 
                 replacement = " xherecolon ",
                 x = vision)
  
  vision <- gsub(pattern = ";", 
                 replacement = " xheresemicolon ",
                 x = vision)
  
  vision <- gsub(pattern = "\\&", 
                 replacement = " xhereampersand ",
                 x = vision)
  
  vision <- gsub(pattern = "_",
                 replacement = "",
                 x = vision)
  
  vision <- gsub(pattern = "\\(", 
                 replacement = " xhereopenparenthesis ",
                 x = vision)
  
  vision <- gsub(pattern = "\\)", 
                 replacement = " xherecloseparenthesis ",
                 x = vision)
  
  vision <- gsub(pattern = "!", 
                 replacement = " xhereexclamationpoint ",
                 x = vision)
  
  vision <- gsub(pattern = "\\?", 
                 replacement = " xherequestionmark ",
                 x = vision)
  
  vision <- gsub(pattern = "\\s+",
                 replacement = " ",
                 x = vision)
  
  writeLines(text = vision, con = paste0(out_dir, "/", basename(corpus_path)))
  
} # end function
