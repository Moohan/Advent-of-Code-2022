input <- readr::read_csv("day1_1.txt",
                         col_types = "i",
                         col_names = FALSE,
                         skip_empty_rows = FALSE)[[1]]

test <- c(
  1000,
  2000,
  3000,
  NA,
  4000,
  NA,
  5000,
  6000,
  NA,
  7000,
  8000,
  9000,
  NA,
  10000
)

day1_1 <- function(input) {
  elves <- vector("integer")
  calories <- 0L

  for (line in input) {
    if (is.na(line)) {
      elves <- append(elves, calories)
      calories <- 0L
    } else {
      calories <- calories + line
    }
  }

  # Deal with last line not NA
  if(!is.na(input[length(input)])) {
    elves <- append(elves, calories)
  }

  return(
    invisible(
    list(
      elves = elves,
    answer = max(unlist(elves))
  )
  )
  )
}

result <- day1_1(input)
result$answer


day1_2 <- function(input) {

  elves <- day1_1(input)$elves

  top_3_index <- which(rank(elves) %in% c((length(elves) - 2):length(elves)))

  top_3 <- elves[top_3_index]

  return(list(
    elves = elves,
    top_3 = top_3,
    answer = sum(top_3)))
}

day1_2(input)
