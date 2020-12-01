input_vec <- read.table("./input.txt")[,1]

# Find product of pair of numbers that sum to 2020

get_2020_product_2d <- function(x){
  mult_mat  <- x %o% x
  add_mat   <- outer(x, x, FUN = "+") * upper.tri(mult_mat)
  return(mult_mat[add_mat==2020])
}

get_2020_product_2d(input_vec)

# Find product of triplet of numbers that sum to 2020

get_2020_product_3d <- function(x){
  mult_array  <- x %o% x %o% x
  add_array   <- outer(outer(x, x, FUN = "+"), x, FUN = "+")
  return(unique(mult_array[add_array==2020])) # Crude
}

get_2020_product_3d(input_vec)

