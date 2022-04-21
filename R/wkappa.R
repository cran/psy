#' @export
wkappa <- function(r, weights = "squared")
{
  r <- na.omit(r)
  r1 <- r[, 1]
  r2 <- r[, 2]
  lev <- min(c(r1, r2)):max(c(r1, r2))
  p <- length(lev)
  tab <- matrix(nrow = p, ncol = p)
  dimnames(tab) <- list(lev, lev)
  weight <- matrix(nrow = p, ncol = p)
  for(i in 1:p) for(j in 1:p)
  {
    tab[i,j] <- sum(r1==lev[i]&r2==lev[j])
    if (weights == "squared")
      weight[i, j] <- 1 - (i - j)^2/(p - 1)^2
    else weight[i, j] <- 1 - abs(i - j)/abs(p - 1)
  }
  tsum <- sum(tab)
  ttab <- tab/tsum
  agreeP <- sum(ttab * weight)
  tm1 <- apply(ttab, 1, sum)
  tm2 <- apply(ttab, 2, sum)
  ttabchance <- tm1 %*% t(tm2)
  chanceP <- sum(ttabchance * weight)
  kappa2 <- (agreeP - chanceP)/(1 - chanceP)
  result <- list(table = tab, weights = weights, kappa = kappa2)
  result
}

