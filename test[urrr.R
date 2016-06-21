library(purrr)
library(dplyr)

random_group <- function(n, probs) {
  probs <- probs / sum(probs)
  g <- findInterval(seq(0, 1, length = n), c(0, cumsum(probs)),
                    rightmost.closed = TRUE)
  names(probs)[sample(g)]
}
partition <- function(df, n, probs) {
  n %>%
    replicate(split(df, random_group(nrow(df), probs)), FALSE) %>%
    transpose() %>%
    as_data_frame()
}

msd <- function(x, y) sqrt(mean((x - y) ^ 2))

# Genearte 100 random test-training splits,
cv <- mtcars %>%
  partition(100, c(training = 0.8, test = 0.2)) %>%
  mutate(
    # Fit the model
    model = map(training, ~ lm(mpg ~ wt, data = .)),
    # Make predictions on test data
    pred = map2(model, test, predict),
    # Calculate mean squared difference
    diff = map2(pred, test %>% map("mpg"), msd) %>% flatten()
  )
cv