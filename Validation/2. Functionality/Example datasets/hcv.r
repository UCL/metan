library(meta)


## Load dataset
##
hcv <- read.csv("hcv.csv")


## Harmonic mean of sample sizes
##
1 / mean(1 / hcv$n)


## Meta-analyses using classic meta-analysis method
##
m1.pft <- metaprop(event, n, data = hcv, studlab = Name,
                   sm = "PFT", pscale = 1000, method.ci = "NAsm",
                   method.tau = "REML")
##
m1.pas <- update(m1.pft, sm = "PAS")
##
m1.plogit <- update(m1.pft, sm = "PLOGIT")


## Meta-analyses using generalized linear mixed model
## and Clopper-Pearson confidence intervals for individual studies
##
m1.glmm <- update(m1.pft, method = "GLMM", method.ci = "CP")


## Print meta-analysis results (without back-transformation)
##
print(summary(m1.pas, backtransf = FALSE), digits = 3)
print(summary(m1.pft, backtransf = FALSE), digits = 3)
print(summary(m1.plogit, backtransf = FALSE), digits = 3)
print(summary(m1.glmm, backtransf = FALSE), digits = 3)


## Print meta-analysis results (with back-transformation)
##
print(summary(m1.pas), digits = 2)
print(summary(m1.pft), digits = 2)
print(summary(m1.plogit), digits = 2)
print(summary(m1.glmm), digits = 2)


## Between-study heterogeneity standard deviations
##
m1.pas$tau
m1.pft$tau
m1.plogit$tau
m1.glmm$tau


## Figure 1
##
pdf("forest1.pdf", width = 9, height = 3)
forest(m1.pft,
       leftlabs = c("Study", "HCV\nInfections", "Total"),
       digits = 3,
       print.pval.Q = FALSE,
       xlim = c(0.025, 0.075), at = c(0.025, 0.05, 0.075),
       backtransf = FALSE)
dev.off()


## Figure 2
##
pdf("forest2.pdf", width = 9, height = 3)
forest(m1.pft,
       leftlabs = c("Study", "HCV\nInfections", "Total"),
       smlab = "HCV infections per\n1000 observations",
       print.pval.Q = FALSE,
       xlim = c(0, 15), at = c(0, 5, 10, 15))
dev.off()


## Figure 4
##
pdf("forest3.pdf", width = 9, height = 3)
forest(m1.plogit,
       leftlabs = c("Study", "HCV\nInfections", "Total"),
       smlab = "HCV infections per\n1000 observations",
       print.pval.Q = FALSE,
       xlim = c(0, 15), at = c(0, 5, 10, 15))
dev.off()


## Figure 5
##
pdf("forest4.pdf", width = 9, height = 3)
forest(update(m1.glmm, method.ci = "CP"),
       leftlabs = c("Study", "HCV\nInfections", "Total"),
       smlab = "HCV infections per\n1000 observations",
       print.pval.Q = FALSE,
       xlim = c(0, 15), at = c(0, 5, 10, 15))
dev.off()



## Figure 3
##
N <- c(seq(10, 2000, 10))
##
pas.fixed <- pas.random <- pft.fixed <- pft.random <- rep_len(NA, length(N))
##
for (i in seq_along(N)) {
  pft.fixed[i]  <- 1000 * meta:::backtransf(m1.pft$TE.fixed,
                                            "PFT", "mean", N[i])
  pft.random[i] <- 1000 * meta:::backtransf(m1.pft$TE.random,
                                            "PFT", "mean", N[i])
  pas.fixed[i]  <- 1000 * meta:::backtransf(m1.pas$TE.fixed,
                                            "PAS", "mean")
  pas.random[i] <- 1000 * meta:::backtransf(m1.pas$TE.random,
                                            "PAS", "mean")
}
##
pdf("samplesize.pdf", height = 8, width = 10)
##
plot(N, pft.fixed, type = "l",
     xlab = "Sample size used in back-transformation",
     ylab = "Events per 1000 observations",
     ylim = c(0, 2),
     cex = 1.5, cex.axis = 1.5, cex.lab = 1.5,
     lwd = 2)
lines(N, pft.random, col = "darkgray", lwd = 2)
##
lines(N, pas.fixed, lty = 2, lwd = 2)
lines(N, pas.random, lty = 3, col = "darkgray", lwd = 2)
##
legend("bottomright",
       c("Double arcsine - fixed effect model",
         "Double arcsine - random effects model",
         "Arcsine - fixed effect model",
         "Arcsine - random effects model"),
       lty = c(1, 1, 2, 3), lwd = 2,
       col = c("black", "darkgray",
               "black", "darkgray"),
       cex = 1.5)
##
n.harm <- 1 / mean(1 / hcv$n)
text(n.harm, 0.4, "Harmonic\nmean",
     cex = 1.5, adj = 0.5)
arrows(n.harm, 0.275, n.harm, 0.01,
       lwd = 1.25, length = 0.1)
##
dev.off()





##
## Supplementary Table
##
n.harm <- round(1 / mean(1 / hcv$n))
n.geom <- round(exp(mean(log(hcv$n))))
n.arit <- round(mean(hcv$n))
##
N <- c(n.harm, 500, 1000, n.geom, 10000, n.arit, 100000, 1000000)
##
pft.f <- pft.r <- rep_len(NA, length(N))
##
for (i in seq_along(N)) {
  pft.f[i]  <- 1000 * meta:::backtransf(m1.pft$TE.fixed,
                                        sm = "PFT", "mean", N[i])
  pft.r[i] <- 1000 * meta:::backtransf(m1.pft$TE.random,
                                       sm = "PFT", "mean", N[i])
}
##
pas.f <- 1000 * meta:::backtransf(m1.pas$TE.fixed,
                                  sm = "PAS", "mean")
pas.r <- 1000 * meta:::backtransf(m1.pas$TE.random,
                                  sm = "PAS", "mean")
pft.pas.f  <- 1000 * meta:::backtransf(m1.pft$TE.fixed,
                                       sm = "PAS", "mean")
pft.pas.r <- 1000 * meta:::backtransf(m1.pft$TE.random,
                                      sm = "PAS", "mean")
##
pft.f  <- round(pft.f, 3)
pft.pas.f <- round(pft.pas.f, 3)
pas.f  <- round(pas.f, 3)
pft.r <- round(pft.r, 3)
pft.pas.r <- round(pft.pas.r, 3)
pas.r <- round(pas.r, 3)


sink("hcv.txt")
##
cat(paste("Harmonic mean:     ",  round(n.harm), "\n"))
cat(paste("Geometric mean:  ",  round(n.geom), "\n"))
cat(paste("Arithmetic mean:",  round(n.arit), "\n\n"))
##
cat(paste("Events per 1000 persons (Arcsine transformation and back-transformation):\n",
          "Fixed effect model (FE):   ", pas.f, "\n",
          "Random effects model (RE): ", pas.r, "\n\n"))
##
cat(paste("Events per 1000 persons (Freeman-Tukey transformation with arcsine back-transformation):\n",
          "Fixed effect model (FE):   ", pft.pas.f, "\n",
          "Random effects model (RE): ", pft.pas.r, "\n\n"))
##
cat("Events per 1000 persons (Freeman-Tukey transformation and back-transformation):\n")
##
data.frame(N, FE = pft.f, RE = pft.r,
           Mean = c("harmonic", "", "", "geometric", "", "arithmetic", "", ""))
##
sink()





## Miller (1978)
##
n.miller <- c(11, 17, 21, 6)
##
n.harm <- 1 / mean(1 / n.miller)
n.geom <- exp(mean(log(n.miller)))
n.arit <- mean(n.miller)
## Harmonic mean
n.harm
meta:::backtransf(1.3093 / 2, "PFT", "mean", n.harm)
## Geometric mean
n.geom
meta:::backtransf(1.3093 / 2, "PFT", "mean", n.geom)
## Arithmetic mean
n.arit
meta:::backtransf(1.3093 / 2, "PFT", "mean", n.arit)
