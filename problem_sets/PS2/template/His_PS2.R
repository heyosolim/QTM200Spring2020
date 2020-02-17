Observed <- matrix(c(14, 6, 7, 7, 7, 1), byrow = T, nrow = 2) # Recreating the table of data
rownames(Observed) <- c("Upper class", "Lower class") # Row names
colnames(Observed) <- c("Not stopped", "Bribe requested", "Stopped/given warning") # Col names
Observed

grandSum <- sum(Observed)
rowSum <- rowSums(Observed)
colSum <- colSums(Observed)

rowSum
colSum

expected <- outer(rowSum, colSum, "*") / grandSum
# dimnames(Observed) isn't necessary??

test_stat <- sum((abs - expected)) ^^2
