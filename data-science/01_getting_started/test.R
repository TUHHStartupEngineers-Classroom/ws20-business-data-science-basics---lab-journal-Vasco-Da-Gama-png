roll <- function() {
  die <- 1:6
  dice <- sample(die, size = 2, replace = TRUE)
  sum(dice)
}

roll2 <- function(faces = 6, number = 2) {
  dice <- sample(1:faces, size = number, replace = TRUE)
  sum(dice)
}

roll3 <- function(number = 2) {
  probabilities <- c(0.1, 0.1, 0.1, 0.1, 0.1, 0.5)
  dice <- sample(1:6, size = number, replace = TRUE, prob = probabilities)
  dice
  #sum(dice)
}

EOQ <- function(D = 1000) {
K <- 5
h <- 0.25
sqrt((2*D*K)/h)
}