#' Count the ones in a number in binary
#'
#' @param num An integer.
#'
#' @return The result of how many "1"s in the number when it is switched to binary
#' @export
#'
#' @examples
#' binary_count(5)
binary_count <- function(num) {
  stopifnot(is.numeric(num), num %% 1 == 0)
  int_num <- as.integer(num)
  count <- 0
  while (num > 0) {
    count <- count + num %% 2
    num <- num %/% 2
  }
  return(count)
}
