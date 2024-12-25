library(GA)

weights <- c(4, 7, 5)
values <- c(4, 6, 3)
capacity <- 10
num_items <- length(weights)

fitness_function <- function(chromosome, weights, values, capacity) {
  total_weight <- sum(chromosome * weights)
  total_value <- sum(chromosome * values)
  if (total_weight > capacity) return(0)
  return(total_value)
}

result <- ga(
  type = "binary",
  fitness = function(chromosome) fitness_function(chromosome, weights, values, capacity),
  nBits = num_items,
  maxiter = 100,
  popSize = 50,
  pmutation = 0.1
)

best_solution <- as.numeric(summary(result)$solution[1, ])
selected_items <- which(best_solution == 1)

cat(sprintf("Case 1: %d\n", sum(best_solution * values)))
cat(sprintf("%d\n", length(selected_items))) 

for (i in selected_items) {
  cat(sprintf("%d %d\n", weights[i], values[i]))
}
