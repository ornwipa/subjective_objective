power_preserve_sign <- function(x_vector, k) {
  x_vector_transformed <- c()
  for (i in 1:length(x_vector)) {
    if (x_vector[i]>=0) {
      c = 1
    } else {
      c = -1
    }
    x_vector_transformed[i] = x_transformed = (abs(x_vector[i])^k)*c
  }
  return(x_vector_transformed)
}
