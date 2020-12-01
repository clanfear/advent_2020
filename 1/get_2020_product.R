input_vec <- read.table("./input.txt")[,1]

# Find product of pair of numbers that sum to 2020

get_2020_product_2d <- function(x){
  ex_mat <- matrix(x, ncol=length(x), nrow=length(x))
  return((ex_mat * t(ex_mat))[upper.tri(ex_mat) & ((ex_mat + t(ex_mat))==2020)])
}

get_2020_product_2d(input_vec)

# Find product of triplet of numbers that sum to 2020

get_2020_product_3d <- function(x){
  add_array   <- outer(outer(x, x, FUN = "+"), x, FUN = "+")
  mult_array  <- x %o% x %o% x
  return(unique(mult_array[add_array==2020])) # Crude
}

get_2020_product_3d(input_vec)

