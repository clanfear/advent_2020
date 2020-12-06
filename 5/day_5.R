# Part 1

input <- readLines("./5/input.txt")

# Construct matrix of seats
col_ind            <- 0:7
row_ind            <- 0:127
seat_mat           <- outer(row_ind*8, col_ind, "+")
colnames(seat_mat) <- col_ind
rownames(seat_mat) <- row_ind

# Function to halve matrices

get_half <- function(mat, x){
  if(x=="F") return(mat[1:(.5*nrow(mat)), , drop=FALSE])
  if(x=="B") return(mat[(1 + .5*nrow(mat)):nrow(mat), , drop=FALSE])
  if(x=="L") return(mat[ , 1:(.5*ncol(mat)), drop=FALSE])
  if(x=="R") return(mat[ , (1 + .5*ncol(mat)):ncol(mat), drop=FALSE])
}

# Function to iteratively halve matrix based on pattern

get_id <- function(pattern){
  current_mat <- seat_mat
  for(char in unlist(strsplit(pattern, ""))){
    current_mat <- get_half(current_mat, char)
  }
  return(current_mat)
}
occupied_seats <- sort(sapply(input, get_id))

# Answer 1: Highest seat ID
max(occupied_seats)

# Part 2
# Trivial solution: My seat is one ID number below the only seat which has an 
# id number two higher than the occupied seat before it.

# Answer 2: My seat
occupied_seats[(occupied_seats-lag(occupied_seats)!=1)][-1]-1 # [-1] drops the NA
