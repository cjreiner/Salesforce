to18Id <- function(inputId){
  if(nchar(inputId)==15){
    #split 15 digit into 3 parts
    chunk1 <- substr(inputId,1,5)
    chunk2 <- substr(inputId,6,10)
    chunk3 <- substr(inputId,11,15)
    
    #reverse each part
    splits <- strsplit(chunk1,"")[[1]]
    chunk1r <- rev(splits)
    splits <- strsplit(chunk2,"")[[1]]
    chunk2r <- rev(splits)
    splits <- strsplit(chunk3,"")[[1]]
    chunk3r <- rev(splits)
    rm(splits)
    
    #replace Uppercase A-Z with 1, all others to 0
    chunk1r[1] <- if_else(grepl("[A-Z]",chunk1r[1]),1,0)
    chunk1r[2] <- if_else(grepl("[A-Z]",chunk1r[2]),1,0)
    chunk1r[3] <- if_else(grepl("[A-Z]",chunk1r[3]),1,0)
    chunk1r[4] <- if_else(grepl("[A-Z]",chunk1r[4]),1,0)
    chunk1r[5] <- if_else(grepl("[A-Z]",chunk1r[5]),1,0)
    chunk2r[1] <- if_else(grepl("[A-Z]",chunk2r[1]),1,0)
    chunk2r[2] <- if_else(grepl("[A-Z]",chunk2r[2]),1,0)
    chunk2r[3] <- if_else(grepl("[A-Z]",chunk2r[3]),1,0)
    chunk2r[4] <- if_else(grepl("[A-Z]",chunk2r[4]),1,0)
    chunk2r[5] <- if_else(grepl("[A-Z]",chunk2r[5]),1,0)
    chunk3r[1] <- if_else(grepl("[A-Z]",chunk3r[1]),1,0)
    chunk3r[2] <- if_else(grepl("[A-Z]",chunk3r[2]),1,0)
    chunk3r[3] <- if_else(grepl("[A-Z]",chunk3r[3]),1,0)
    chunk3r[4] <- if_else(grepl("[A-Z]",chunk3r[4]),1,0)
    chunk3r[5] <- if_else(grepl("[A-Z]",chunk3r[5]),1,0)
    
    #paste back to a single chunks
    chunk1r <- paste(chunk1r, collapse = "")  
    chunk2r <- paste(chunk2r, collapse = "")    
    chunk3r <- paste(chunk3r, collapse = "")  
    
    #lookup tables
    vals <- c("A","B","C","D","E","F","G","H",
              "I","J","K","L","M","N","O","P",
              "Q","R","S","T","U","V","W","X",
              "Y","Z",0,1,2,3,4,5)
    bin_vals <- c("00000","00001","00010","00011","00100",
                  "00101","00110","00111","01000","01001",
                  "01010","01011","01100","01101","01110",
                  "01111","10000","10001","10010","10011",
                  "10100","10101","10110","10111","11000",
                  "11001","11010","11011","11100","11101",
                  "11110","11111")
    
    newIdsuffix <- paste0(vals[match(chunk1r,bin_vals)],vals[match(chunk2r,bin_vals)], vals[match(chunk3r,bin_vals)])
    return(paste0(inputId,newIdsuffix))
  }
  return(inputId)
}
