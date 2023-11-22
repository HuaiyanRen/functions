library(sem)
filename <- "WAG"

fasttree <- function(filename){
  # read file and generate R matrix and F vector from texts
  Q <- readMoments(filename, diag=F)
  pi <- Q[nrow(Q), 1:(ncol(Q)-1)]
  Q <- Q[1:(nrow(Q)-1), 1:(ncol(Q)-1)]
  Q <- (Q + t(Q))
  diag(Q) <- 0
  
  # calculate the Q matrix
  Q_matrix <- matrix(nrow = 20, ncol = 20)
  for (i in c(1:20)){
    for (j in c(1:20)){
      if (i != j){
        Q_matrix[i,j] <- pi[j]*Q[i,j]
      }
    }
  }
  
  # add diagonal values 
  dia <- rowSums(Q_matrix, na.rm = TRUE)
  diag(Q_matrix) <- -dia
  
  # normalize the Q matrix
  norm_Q <- Q_matrix/sum(pi*dia)
  
  # transpose the Q matrix and add a row for F vertor
  trans_Q <- t(norm_Q)
  Q_fasttree <- cbind(trans_Q, pi)
  
  # add column and row names
  row_names <- c("A", "R", "N", "D", "C", "Q", "E", "G", "H", "I", "L", "K", "M", "F", "P", "S", "T", "W", "Y", "V")
  col_names <- c("A", "R", "N", "D", "C", "Q", "E", "G", "H", "I", "L", "K", "M", "F", "P", "S", "T", "W", "Y", "V", "*")
  rownames(Q_fasttree) <- row_names
  colnames(Q_fasttree) <- col_names
  
  # write output file
  write.table(Q_fasttree, paste0("output_", filename), sep = "\t", quote = FALSE, row.names = TRUE, col.names = TRUE)
} 

fasttree(filename)
