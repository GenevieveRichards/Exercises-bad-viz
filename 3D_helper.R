matrix_data <- function(matrixes, beta) {

  b <- matrix (0, 25, 1, dimnames = list(NULL, c("Observation")))
  r <- 1
  c <- 1

  for (col in 1:nrow(matrixes)) {
    matrixes[col,1] <- (as.numeric(matrixes[col,1]) * (beta * (r * 5) + beta * (c * 5)))
    if (r < 5) {
      r <- r + 1
    } else {
      r <- 1
      c <- c + 1
    }
  }

  T <- sum(matrixes)
  matrixes <- matrixes/T
  # print(matrixes)

  for (row in 1:nrow(b)) {
    b[row, 1] <- as.numeric(matrixes[row, 1] * 1000)
  }

  return (b)
}

returned_Matrix <- function(beta) {
  
  a <- matrix (0, 25, 1, dimnames = list(NULL, c( "Observation")))
  
  #Create Initial Matrix 
  initial <- (1/(25))
  for (col in 1:nrow(a)) {
    a[col,1] <- initial
  }
  
  ten <- as.data.frame(a)
  nine <- matrix_data(ten, 1)
  eight <- matrix_data(data.matrix(nine), 2)
  seven <- matrix_data(data.matrix(eight), 3)
  six <- matrix_data(data.matrix(seven), 4)
  five <- matrix_data(data.matrix(six), 5)
  four <- matrix_data(data.matrix(five), 6)
  three <- matrix_data(data.matrix(four), 7)
  two <- matrix_data(data.matrix(three), 8)
  one <- matrix_data(data.matrix(two), 12)
  
  types <- c("One", "Two", "Three", "Four", "Five")
  para <- matrix (0, 25, 2, dimnames = list(NULL, c("Type_1", "Type_2")))
  k <- 1
  l <- 1
  for (row in 1:nrow(para)) {
    para[row, 1] <- types[k]
    para[row, 2] <- types[l]
    if (k < 5) {
      k <- k + 1
    } else {
      k <- 1
      l <- l + 1
    }
  }
  
  if (beta == 1) {
    return(as.data.frame(cbind(para,one)))
  } else if (beta == 2) {
    return(as.data.frame(cbind(para,two)))
  } else if (beta == 3) {
    return(as.data.frame(cbind(para,three)))
  } else if (beta == 4) {
    return(as.data.frame(cbind(para,four)))
  } else if (beta == 5) {
    return(as.data.frame(cbind(para,five)))
  } else if (beta == 6) {
    return(as.data.frame(cbind(para,six)))
  } else if (beta == 7) {
    return(as.data.frame(cbind(para,eight)))
  } else if (beta == 8) {
    return(as.data.frame(cbind(para,nine)))
  } else {
    return(as.data.frame(cbind(para,ten)))
  }
}

returned_Matrix(8)

