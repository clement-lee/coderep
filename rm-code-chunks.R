library(stringr)
lines0 <- readLines("wisconsin.Rmd")
begin0 <- which(str_detect(lines0, "```\\{r"))
end0 <- which(str_detect(lines0, "```$"))
if (length(begin0) != length(end0)) {
  stop("The number of code chunk beginnings and ends don't match.")
}
n <- length(begin0)
indices0 <- c()
for (i in 1:n) {
  seq0 <- seq(begin0[i], end0[i], by = 1L)
  indices0 <- append(indices0, seq0)
}
lines1 <- lines0[-indices0]
writeLines(lines1, "wisconsin_no_code_chunks.md")
