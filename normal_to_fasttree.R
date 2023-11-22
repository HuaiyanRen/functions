filename <- "WAG.txt"

fasttree <- function(filename){
  # read file and split the text to matrix part and F vector part
  model_text <- readLines(filename)
  index_Q <- grep("^> Q$", model_text)
  index_F <- grep("^> F$", model_text)

  # generate R matrix and F vector from texts 
  R_matrix <- matrix(nrow = 20, ncol = 20)
  for (r in c(1:20)){
    row<- as.numeric(strsplit(model_text[r+index_Q+1], "\\s+")[[1]])
    R_matrix[r,] <- row[2:21]
  } 
  F_vector <- as.numeric(strsplit(model_text[index_F+2], "\\s+")[[1]])
  
  # calculate the Q matrix
  Q_matrix <- matrix(nrow = 20, ncol = 20)
  for (i in c(1:20)){
    for (j in c(1:20)){
      if (i != j){
        Q_matrix[i,j] <- F_vector[j]*R_matrix[i,j]
      }
    }
  }
  
  # add diagonal values 
  dia <- rowSums(Q_matrix, na.rm = TRUE)
  for (d in c(1:20)){
    Q_matrix[d,d] <- -dia[d]
  }
  
  # normalize the Q matrix
  norm_Q <- Q_matrix/sum(F_vector*dia)
  
  # transpose the Q matrix and add a row for F vertor
  trans_Q <- t(norm_Q)
  fasttree_matrix <- cbind(trans_Q, F_vector)
  
  # add column and row names
  row_names <- c("A", "R", "N", "D", "C", "Q", "E", "G", "H", "I", "L", "K", "M", "F", "P", "S", "T", "W", "Y", "V")
  col_names <- c("A", "R", "N", "D", "C", "Q", "E", "G", "H", "I", "L", "K", "M", "F", "P", "S", "T", "W", "Y", "V", "*")
  rownames(fasttree_matrix) <- row_names
  colnames(fasttree_matrix) <- col_names
  
  # write output file
  write.table(fasttree_matrix, paste0("output_", filename), sep = "\t", quote = FALSE, row.names = TRUE, col.names = TRUE)
} 

fasttree(filename)
