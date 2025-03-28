N = 1000
B = 8
M = 100
A = 500

set.seed(42)

all_returns <- matrix(0, M, N)

for (j in 1:M) {
  x <- runif(N)
  
  returns <- rep(0, N)
  
  for (i in 1:N) {
    if (x[i] <= 0.6935123) {
      returns[i] = 0.441935 * B
    }
    else if (x[i] > 0.6935123) {
      returns[i] = -B
    }
  }
  all_returns[j, ] = returns
  
}

k <- 1

valor_teorico <- A +  cumsum(rep((k - 1) * B, 1000))

cumsum_mat <- A + t(apply(all_returns, 1, cumsum))

row_means_cumsum <- A + cumsum(colMeans(all_returns))

matplot(
  t(cumsum_mat),
  type = "l",
  pch = 1:3,
  col = 2:3,
  xlab = "Número de jogos",
  ylab = "Valor monetário",
  main = "Soma cumulativa em 1000 apostas com odd justa",
  ylim = c(0, 800)
)

lines(row_means_cumsum,
      type = "l",
      lwd = 3,
      col = "blue")


legend("topleft", 
       legend = c("Média das somas cumulativas", "Valor teórico"),
       col = c( "blue", "black"),
       #lty = c(1:3, 1, 1),
       #pch = c(1:3, NA, NA),
       lwd = c(2),
       #ncol = 1,
       bg = "white")


lines(valor_teorico,
      type = "l",
      lwd = 3,
      col = "black")


# -----------------------------------------------------------------------------
rm(list = ls())


N = 1000
B = 8
M = 100
A = 500
all_returns <- matrix(0, M, N)

for (j in 1:M) {
  x <- runif(N)
  
  returns <- rep(0, N)
  
  for (i in 1:N) {
    if (x[i] <= 0.6935123) { # 0.8744395
      returns[i] = 0.37 * B # 0.12
    }
    else if (x[i] > 0.6935123) {
      returns[i] = -B
    }
  }
  all_returns[j, ] = returns
  
}

cumsum_mat <-  A +  t(apply(all_returns, 1, cumsum))

row_means_cumsum <-  A + cumsum(colMeans(all_returns))

matplot(
  t(cumsum_mat),
  type = "l",
  pch = 1:3,
  col = 2:3,
  xlab = "Número de jogos",
  ylab = "Valor monetário",
  main = "Soma cumulativa em 1000 apostas com odd real",
  ylim = c(0, 800)
  
)

lines(row_means_cumsum,
      type = "l",
      lwd = 3,
      col = "blue")

legend("topleft", 
       legend = c("Média das somas cumulativas", "Valor teórico"),
       col = c( "blue", "black"),
       #lty = c(1:3, 1, 1),
       #pch = c(1:3, NA, NA),
       lwd = c(2),
       #ncol = 1,
       bg = "white")


k <- 0.9501119

valor_teorico <- A + cumsum(rep((k - 1) * B, 1000))

lines(valor_teorico,
      type = "l",
      lwd = 3,
      col = "black")

#plot(valor_teorico, type = "l", lwd = 3, col = "black")


#plot(cumsum(all_returns[1,]))
#plot(cumsum(all_returns), type='l')

#legend("topleft", legend = rownames(all_returns), pch = 1:3, col = 1:3)
