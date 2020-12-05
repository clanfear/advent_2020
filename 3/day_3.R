# Load data
tree_lines <- do.call(rbind, strsplit(readLines("./3/input.txt"), ""))

# Part 1
## Clunky solution but it works
get_path <- function(trees, x, y){
  its   <- (nrow(trees)-1) %/% y
  width <- ((its*x) %/% ncol(trees))+1
  trees <- matrix(trees, ncol = ncol(trees)*width, nrow = nrow(trees))
  mat   <- cbind(seq(x, x*its, by = x)+1,
                 seq(y, y*its, by = y)+1)
  path  <- apply(mat, 1, function(mat) `[`(trees, mat[2], mat[1]))
  return(sum(path == "#"))
}
get_path(tree_lines, 3, 1)

# Part 2
## At least it applies nicely
patterns <- data.frame(x = c(1,3,5,7,1),
                       y = c(1,1,1,1,2))

prod(apply(patterns, 1, function(x){ get_path(tree_lines, x["x"], x["y"]) }))
