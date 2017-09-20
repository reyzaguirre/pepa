#' Analysis for a Line x Tester genetic design.
#'
#' This is a modified version of the \code{"lineXtester"} function of
#' package \code{"agricolae"} for the analysis of a Lina x Tester genetic design.
#'
#' @param trait The trait to analyze.
#' @param lines The lines.
#' @param testers The testers.
#' @param rep The replication.
#' @param data The name of the data frame containing the data.
#' @author Raul Eyzaguirre.
#' @details Data must correspond with a line by tester design and must be balanced.
#' If there are missing values they are not estimated and results can be dubious.
#' @return It returns the analysis for the line by tester genetic design.
#' @examples
#' aov.lxt("yield", "line", "tester", "rep", lxt)
#' @importFrom stats aov pf
#' @export

aov.lxt <- function(trait, lines, testers, rep, data) {

  # Internal data frame

  ddd <- data.frame(trait = data[, trait],
                    lines = data[, lines],
                    testers = data[, testers],
                    rep = data[, rep])

  # Everything as factor

  ddd$lines <- factor(ddd$lines)
  ddd$testers <- factor(ddd$testers)
  ddd$rep <- factor(ddd$rep)

  # Number of lines, testers and reps

  nl <- nlevels(ddd$lines)
  nt <- nlevels(ddd$testers)
  nr <- nlevels(ddd$rep)

  # Models

  ddd$treat <- as.factor(paste(ddd$lines, ddd$testers))

  model.1 <- aov(trait ~ rep + treat, data = ddd)
  anova.1 <- as.matrix(anova(model.1))

  model.4 <- aov(trait ~ lines * testers, data = ddd)
  anova.4 <- as.matrix(anova(model.4))

  # SCA and GCA

  ddd.2 <- na.omit(ddd)

  mm <- tapply(ddd.2$trait, ddd.2[, c("lines", "testers")], mean, na.rm = TRUE)
  cmm <- ncol(mm)
  rmm <- nrow(mm)
  SCA <- mm
  for (i in 1:rmm)
    for (j in 1:cmm)
      SCA[i, j] <- mm[i, j] - mean(mm[, j], na.rm = TRUE) -
                   mean(mm[i, ], na.rm = TRUE) + mean(mm, na.rm = TRUE)

  mm <- tapply(ddd.2$trait, ddd.2$lines, mean, na.rm = TRUE)
  GCA.lines <- mm - mean(ddd.2$trait, na.rm = TRUE)

  mm <- tapply(ddd.2$trait, ddd.2$testers, mean, na.rm = TRUE)
  GCA.testers <- mm - mean(ddd.2$trait, na.rm = TRUE)

  # More anovas

  model.3 <- aov(trait ~ treat, data = ddd.2)
  anova.3 <- as.matrix(anova(model.3))

  ddd.3 <- ddd[is.na(ddd$lines) | is.na(ddd$testers), ]

  model.2 <- aov(trait ~ treat, data = ddd.3)
  anova.2 <- as.matrix(anova(model.2))
  anova.5 <- anova.1[2, ] - anova.2[1, ] - anova.3[1, ]

  matriz <- rbind(anova.1[1:2, ], anova.2[1, ], anova.5, anova.3[1, ],
                  anova.4[1:3, ], anova.1[3, ])

  total.1 <- sum(anova.1[, 1])
  total.2 <- sum(anova.1[, 2])
  matriz <- rbind(matriz, c(total.1, total.2, NA, NA, NA))

  for (i in 1:9) {
    matriz[i, 3] <- matriz[i, 2] / matriz[i, 1]
    matriz[i, 4] <- round(matriz[i, 3] / matriz[9, 3], 3)
    matriz[i, 5] <- round(1 - pf(matriz[i, 4], matriz[i, 1], matriz[9, 1]), 4)
    if (i == 6 | i == 7) {
      matriz[i, 4] <- round(matriz[i, 3] / matriz[8, 3], 3)
      matriz[i, 5] <- round(1 - pf(matriz[i, 4], matriz[i, 1], matriz[8, 1]), 4)
    }
  }
  matriz[9, 4] <- NA
  matriz[9, 5] <- NA
  rownames(matriz) <- c("Replications", "Treatments", "Parents",
                        "Parents vs. Crosses", "Crosses", "Lines", "Testers",
                        "Lines x Testers", "Error", "Total")
  cm <- matriz[9, 3]
  s1 <- sqrt(cm / (nr * nt))
  s2 <- sqrt(cm / (nr * nl))
  s3 <- sqrt(cm / nr)
  s4 <- sqrt(2 * cm / (nr * nt))
  s5 <- sqrt(2 * cm / (nr * nl))
  s6 <- sqrt(2 * cm / nr)
  cov1 <- (matriz[6, 3] - matriz[8, 3]) / (nr * nt)
  cov2 <- (matriz[7, 3] - matriz[8, 3]) / (nr * nl)
  cov3 <- (((nl - 1) * matriz[6, 3] + (nt - 1) * matriz[7, 3]) / (nl + nt - 2)
           - matriz[8, 3]) / (nr * (2 * nl * nt - nl - nt))
  cov4 <- ((matriz[6, 3] - matriz[9, 3]) + (matriz[7, 3] - matriz[9, 3]) +
             (matriz[8, 3] - matriz[9, 3])) / (3 * nr) +
          (6 * nr * cov3 - nr * (nl + nt) * cov3) / (3 * nr)
  var.A0 <- cov3 * 4
  var.D0 <- ((matriz[8, 3] - matriz[9, 3]) / nr) * 2
  var.A1 <- cov3 * 4 / 2
  var.D1 <- ((matriz[8, 3] - matriz[9, 3]) / nr)
  c1 <- matriz[6, 2] * 100 / matriz[5, 2]
  c2 <- matriz[7, 2] * 100 / matriz[5, 2]
  c3 <- matriz[8, 2] * 100 / matriz[5, 2]

  # P values

  GCA.lines.p <- round(pt(abs(GCA.lines) / s1, matriz[9, 1], lower.tail = F) * 2, 4)
  GCA.testers.p <- round(pt(abs(GCA.testers) / s2, matriz[9, 1], lower.tail = F) * 2, 4)

  GCA.le <- t(matrix(c(GCA.lines, GCA.lines.p), nrow = 2, byrow = T))
  rownames(GCA.le) <- names(GCA.lines)
  colnames(GCA.le) <- c("Effects", "p-values")

  GCA.te <- t(matrix(c(GCA.testers, GCA.testers.p), nrow = 2, byrow = T))
  rownames(GCA.te) <- names(GCA.testers)
  colnames(GCA.te) <- c("Effects", "p-values")

  SCA.p <- round(pt(abs(SCA) / s3, matriz[9, 1], lower.tail = F) * 2, 4)

  cat("ANOVA for parents and crosses",
    "\n=============================\n")
  matriz1 <- matriz[c(1, 2, 3, 4, 5, 9, 10), ]
  print(matriz1, na.print = "")
  cat("\nANOVA for line x tester",
      "\n=======================\n")
  matriz1 <- matriz[6:9, ]
  print(matriz1, na.print = "")
  cat("\nANOVA for line x tester including parents",
      "\n=========================================\n")
  print(matriz, na.print = "")
  cat("\nGCA effects",
      "\n===========")
  cat("\nLines:\n")
  print(GCA.le)
  cat("\nTesters:\n")
  print(GCA.te)
  cat("\nSCA effects",
      "\n===========\n")
  cat("\nEffects:\n")
  print(SCA)
  cat("\np-values:\n")
  print(SCA.p)
  cat("\nStandard errors for combining ability effects",
      "\n=============================================")
  cat("\nS.E. (gca for lines)   :", s1)
  cat("\nS.E. (gca for testers) :", s2)
  cat("\nS.E. (sca effect)      :", s3)
  cat("\nS.E. (gi - gj)line     :", s4)
  cat("\nS.E. (gi - gj)tester   :", s5)
  cat("\nS.E. (sij - skl)tester :", s6, "\n")
  cat("\nGenetic components",
      "\n==================")
  cat("\nCov H.S. (line)   :", cov1)
  cat("\nCov H.S. (tester) :", cov2)
  cat("\nCov H.S. (average):", cov3)
  cat("\nCov F.S. (average):", cov4)
  cat("\nF = 0, adittive genetic variance:", var.A0)
  cat("\nF = 1, adittive genetic variance:", var.A1)
  cat("\nF = 0, variance due to dominance:", var.D0)
  cat("\nF = 1, variance due to dominance:", var.D1, "\n")
  cat("\nProportional contributions to total variance",
      "\n============================================")
  cat("\nContributions of lines  :", c1)
  cat("\nContributions of testers:", c2)
  cat("\nContributions of lxt    :", c3, "\n")
}
