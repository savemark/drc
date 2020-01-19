# Detta program beräknar diskonteringsräntekurvor både från idag och framåt i tiden,
# för användning till exempelvis framtida balansräkningar. Kurvorna kan enkelt skrivas till 
# Excel i tabellformat eller plottas som ytor.
# Indata är marknadsnoterade swapräntor.

discountFactors <- function(t = length(part), part) {
  i <- 2
  df <- numeric(t)
  df[1] <- 1/(1+part[1])
  while (i <= t) {
    df[i] <- (1-part[i]*sum(df[1:(i-1)]))/(1+part[i])
    i <- i+1
  }
  return(df)
}

zeroCouponRates <- function(t = 1:length(df), df) {
  (1/df[t])^(1/t)-1
}

forwardRates <- function(t, df) {
  if (t == 1) {
    return(1/df[t]-1)
  } else {
    return(df[t-1]/df[t]-1)
  }
}
vforwardRates <- Vectorize(forwardRates, vectorize.args = "t")

weight <- function(t, T1 = 10, T2 = T1+10) {
  # T1 längsta Loptiden
  # T2 tidpunkten för konvergens
  ifelse(t<=T1, 0, 
         ifelse(t>T1 & t<=T2, (t-T1)/(T2-T1+1), 1))
}

weightedForwardRate <- function(fwdr, w, T, UFR = 0.042) {
  # fwdr vektor av terminsräntor
  # w viktningsfunktion
  # T längsta Loptid
  res <- numeric(T)
  for (t in seq_len(T)) {
    res[t] <- ifelse(t<=length(fwdr), (1-w(t))*fwdr[t]+w(t)*UFR, UFR)
  } 
  return(res)
}

logLinearDFRelationshipClosure <- function(t, df) {
  # log-linjära relationer mellan diskonteringsfaktorer.
  # t är en 3-dimensionell vektor (t1, t2, t3) med t1 < t2 < t3.
  # denna funktion returnerar en funktion (det är en funktional dvs.)
  d <- t[3]-t[1]
  f <- function(x) {
    exp(((d-(t[2]-t[1]))/d)*log(df[t[1]])+((d-(t[3]-t[2]))/d)*log(1/((1+x)^t[3])))
  }
  return(f)
}

addFunctions <- function(x, l) {
  # x variabel
  # l lista av funktioner
  sum(sapply(l, function(f) f(x)))
}

multiplyFunctions <- function(x, l) {
  # x variabel
  # l lista av funktioner
  prod(sapply(l, function(f) f(x)))
}

objectiveFunction <- function(x, s, t, part, df) {
  # Målfunktion som ska minimeras.
  i <- (s+1):(t-1)
  logLinearDFRelationships <- lapply(i, function (x) logLinearDFRelationshipClosure(c(s, x, t), df))
  res <- (part[t]*(sum(df[1:s])+addFunctions(x, logLinearDFRelationships)+1/((1+x)^t))-(1-1/((1+x)^t)))^2
  return(res)
}

interestRateSwap <- function(par_t, credit.adj = -0.0035, allow.negative.rates = TRUE) {
  par_t.adj <- par_t + credit.adj
  if (!allow.negative.rates) par_t.adj[par_t.adj < 0] <- 0
  df <- discountFactors(part = par_t.adj)
  zcr <- zeroCouponRates(df = df)
  fwdr <- vforwardRates(1:length(par_t.adj), df)
  while (any(is.na(df))) {
    naindex <- min(which(is.na(df))) # Least index of NAs in df
    t_1 <- naindex-1 # Largest non-NA index before NA in df
    index <- which(!is.na(par_t.adj)) # Indices that are not NA in par_t
    t_3 <- index[min(which(index>naindex))] # The smallest non-NA index that is larger than naindex
    j <- naindex:(t_3-1) # NA indices in DF
    logLinearDFRelationships <- lapply(j, function (x) logLinearDFRelationshipClosure(c(t_1, x, t_3), df))
    zcr[t_3] <- optim(par = 0.01, fn = objectiveFunction, s = t_1, t = t_3, part = par_t.adj, df = df, method = "Brent", lower = -2, upper = 2)$par
    df[t_3] <- 1/(1+zcr[t_3])^(t_3)
    m <- 1
    for (k in naindex:(t_3-1)) {
      df[k] <- logLinearDFRelationships[[m]](zcr[t_3])
      m <- m+1
    }
    fwdr[naindex:t_3] <- ((df[t_1]/df[t_3])^(1/(t_3-t_1)))-1
  }
  zcr <- zeroCouponRates(1:length(par_t.adj), df)
  datafr <- data.frame("Loptid" = 1:length(par_t), "Marknadsnoteringar" = par_t, "Marknadsnoteringar med kred. just." = par_t.adj, "Nollkupongranta swap" = zcr, "Diskonteringsfaktor" = df, "Terminsranta swap" = fwdr)
  return(datafr)
}

discountFactors_2 <- function(t, fwdr) {
  i <- 2
  df <- numeric(t)
  df[1] <- 1/(1+fwdr[1])
  while (i <= t) {
    df[i] <- df[i-1]/(1+fwdr[i])
    i <- i+1
  }
  return(df)
}

nextDiscountFactor <- function(df, fwdr) {
  df_next <- df/(1+fwdr)
  return(df_next)
}

zeroCouponRate <- function(t, df) {
  return((1/df)^(1/t)-1)
}

weightedInterestSwap <- function(par_t, T, credit.adj = -0.0035, allow.negative.rates = FALSE, ...) {
  # Längsta Loptid
  # par_t marknadsnoteringar för swapräntor
  wfwdr <- weightedForwardRate(interestRateSwap(par_t, credit.adj = credit.adj, allow.negative.rates = allow.negative.rates)[ , 6], w = weight, T = T, ...)
  wdf <- discountFactors_2(T, wfwdr)
  wzcr <- zeroCouponRates(df = wdf)
  datafr <- data.frame("Löptid" = seq(wfwdr), "Terminsränta viktad" = wfwdr, "Diskonteringsfaktor" = wdf, "Nollkupongränta" = wzcr)
  return(datafr)
}

scenarioGenerator <- function(par_t, start.year, T, UFR = 0.042, ...) {
  # Denna funktion diskonterar alla framtida kassaflöden till första året,
  # även för framtida kontrakt. Bör endast användas i sällsynta fall?
  # Output är en lista innehållandes tre matriser.
  # par_t är marknadsnoteringar för ränteswapavtal.
  # start.year är år man börjar.
  # T är antal år framåt i tiden.
  # UFR = ultimate forward rate
  vector.is.empty <- function(x) return(length(x) == 0)
  M <- matrix(NA, T, T) # Terminsräntor
  N <- matrix(NA, T, T) # Diskonteringsfaktorer
  K <- matrix(NA, T, T) # Nollkupongräntor
  wfwdr <- weightedInterestSwap(par_t, T = T, ...)[ , 2]
  M[1, ] <- wfwdr
  row <- 2
  while (!(vector.is.empty(wfwdr)) & row<=T) {
    wfwdr <- wfwdr[-1]
    M[row, seq(wfwdr)] <- wfwdr
    row <- row+1
  }
  M[is.na(M)] <- UFR
  N[, 1] <- discountFactors_2(T, M[, 1])
  N[1, ] <- discountFactors_2(T, M[1, ])
  for (i in 2:T) {
    for (j in 2:T) {
      N[i, j] <- nextDiscountFactor(N[i, j-1], M[i, j])
    }
  }
  for (i in seq(T)) {
    for (j in seq(T)) {
      if (i == 1 & j == 1) {
        K[1, 1] <- M[1, 1]
      } else {
        K[i, j] <- zeroCouponRate(t = ((i-1)+(j-1)+1), df = N[i, j])
      }
    }
  }
  rownames(M) <- rownames(N) <- rownames(K) <- start.year:(start.year+T-1)
  colnames(M) <- colnames(N) <- colnames(K) <- seq(T)
  return(list("Terminsränta viktad" = M, "Diskonteringsfaktor" = N, "Nollkupongränta" = K))
}

scenarioGenerator_2 <- function(par_t, start.year, T, stress = NULL, UFR = 0.042, ...) {
  # Denna funktion diskonterar bara till varje nytt år.
  # (används t.ex. för diskontering vid framräkning av framtida balansräkningar)
  # Output är en lista innehållandes tre matriser.
  # par_t är marknadsnoteringar för ränteswapavtal.
  # start.year är år man börjar.
  # T är antal år framåt i tiden.
  # stress är en stressvektor vars längd måste vara lika med T.
  # UFR = ultimate forward rate
  if (!is.null(stress) & length(stress) != T)
    stop("Stress vector is not the same length as T")
  vector.is.empty <- function(x) return(length(x) == 0)
  M <- matrix(NA, T, T) # Terminsräntor
  N <- matrix(NA, T, T) # Diskonteringsfaktorer
  K <- matrix(NA, T, T) # Nollkupongräntor
  wfwdr <- weightedInterestSwap(par_t, T = T, ...)[ , 2]
  M[1, ] <- wfwdr
  row <- 2
  while (!(vector.is.empty(wfwdr)) & row<=T) {
    wfwdr <- wfwdr[-1]
    M[row, seq(wfwdr)] <- wfwdr
    row <- row+1
  }
  M[is.na(M)] <- UFR
  for (i in seq(T)) {
    N[i, ] <-  discountFactors_2(T, M[i, ])
  }
  K[1, ] <- M[1, ]
  K[, 1] <- M[, 1]
  for (i in 1:T) {
    for (j in 2:T) {
      K[i, j] <- ((1+M[i, j])*(1+K[i, j-1])^(j-1))^(1/j)-1
    }
  }
  if (!is.null(stress)) {
    stress.last <- stress[length(stress)]
    K[1, ] <- K[1, ]+stress
    row <- 2
    while (row<=T) {
      stress <- c(stress[-1], stress.last)
      K[row, ] <- K[row, ]+stress
      row <- row+1
    }
  }
  rownames(M) <- rownames(N) <- rownames(K) <- start.year:(start.year+T-1)
  colnames(M) <- colnames(N) <- colnames(K) <- seq(T)
  return(list("Terminsränta viktad" = M, "Diskonteringsfaktor" = N, "Nollkupongränta" = K))
}

# Exempel 1
# Detta exempel användes för att validera koden mot PROMEMORIA 2013-12-01 [FI Dnr 13-11409]
# par_t är marknadsnoteringarna. Observera att man måste inkludera NAs.
# par_t <- (1/100)*c(1.3200, 1.5275, 1.77, 2.008, 2.208, 2.3630, 2.49, 2.5930, 2.678, 2.745, NA, 2.8450, NA, NA, 2.9400, NA, NA, NA, NA, 3.04)
# cat("\n", "räntekurva swappar", "2013-06-30", "\n")
# irs <- interestRateSwap(par_t)
# print(irs)
# cat("\n", "Diskonteringsräntekurva", "2013-06-30", "\n")
# wirs <- weightedInterestSwap(par_t, T = 100)
# print(wirs)
# plot(wirs[, "Nollkupongränta"], xlab = "Loptid", ylab = "Nollkupongränta", type = "l", main = "Diskonteringsräntekurva 2013-06-30")

# Exempel 2-1
# Detta exempel användes för att validera koden mot en publicerad räntekurva, framräknad
# i en redan existerande Excel-snurra.
# par_t_2 <- (1/100)*c(-0.026, 0.088, 0.204, 0.325, 0.452, 0.574, 0.692, 0.802, 0.903, 0.997, NA, 1.17, NA, NA, 1.335, NA, NA, NA, NA, 1.488)
# cat("\n", "räntekurva swappar", "2019-02-28", "\n")
# irs_2 <- interestRateSwap(par_t_2)
# print(irs_2)
# cat("\n", "Diskonteringsräntekurva", "2019-02-28", "\n")
# wirs_2 <- weightedInterestSwap(par_t_2, T = 100)
# print(wirs_2)
# plot(wirs_2[, "Nollkupongränta"], xlab = "Loptid", ylab = "Nollkupongränta", type = "l", main = "Diskonteringsräntekurva 2019-02-28")

# Exempel 2-2
# Detta exempel användes för att validera koden mot en publicerad räntekurva, framräknad
# i en redan existerande Excel-snurra.
# par_t_2_2 <- (1/100)*c(-0.021, 0.025, 0.085, 0.148, 0.235, 0.323, 0.415, 0.505, 0.588, 0.672, NA, 0.83, NA, NA, 0.988, NA, NA, NA, NA, 1.144)
# cat("\n", "räntekurva swappar", "2019-05-31", "\n")
# irs_2_2 <- interestRateSwap(par_t_2_2)
# print(irs_2_2)
# cat("\n", "Diskonteringsräntekurva", "2019-05-31", "\n")
# wirs_2_2 <- weightedInterestSwap(par_t_2_2, T = 100)
# print(wirs_2_2)
# plot(wirs_2_2[, "Nollkupongränta"], xlab = "Loptid", ylab = "Nollkupongränta", type = "l", main = "Diskonteringsräntekurva 2019-05-31")

# Exempel 3
# Detta exempel användes i Eiopas stresstest för tjänstepensionskassor år 2019.
# Metoden skiljer sig från Swith-Wilson (den svenska är enklare).
# par_t_3 <- (1/100)*c(-0.085, 0.046, 0.193, 0.34, 0.494, 0.646, 0.788, 0.914, 1.022, 1.116, NA, 1.278, NA, NA, 1.446, NA, NA, NA, NA, 1.591)
# cat("\n", "räntekurva swappar", "2018-12-28", "\n")
# irs_3 <- interestRateSwap(par_t_3)
# print(irs_3)
# cat("\n", "Diskonteringsräntekurva", "2018-12-28", "\n")
# wirs_3 <- weightedInterestSwap(par_t_3, T = 100, allow.negative.rates = FALSE)
# print(wirs_3)
# plot(wirs_3[, "Nollkupongränta"], xlab = "Loptid", ylab = "Nollkupongränta", type = "l", main = "Diskonteringsräntekurva 2018-12-28")    

# Diskonteringsränteytor, med ett stressat scenario.
# Detta exempel användes i Eiopas stresstest för tjänstepensionskassor år 2019.
# Stressvektorn bör i framtida scenarion definieras via funktion, istället för som en konstant vektor!
# scenario_base <- scenarioGenerator_2(par_t_3, 2019, 100, allow.negative.rates = TRUE)
# stress <- (1/100)*c(0.40, 0.55, 0.70, 0.84, 1.01, 0.94, 0.88, 0.81, 0.74, 0.68, 0.65, 0.62, 0.60, 0.57, 0.55, 0.52, 0.50, 0.48, 0.46, 0.44, rep(0.44, times = 80))
# scenario_adverse <- scenarioGenerator_2(par_t_3, 2019, 100, stress = stress, allow.negative.rates = TRUE)

# Plottar för validering
# persp(x = 2019:(2019+100-1), y = 1:100, z = scenario_base$'Terminsränta viktad', main = "Basscenario IORP ST 2019", xlab = "år", ylab = "Loptid", zlab = "Terminsränta viktad", ticktype = "detailed", theta = -30, phi = 30)
# persp(x = 2019:(2019+100-1), y = 1:100, z = scenario_base$'Diskonteringsfaktor', main = "Basscenario IORP ST 2019", xlab = "år", ylab = "Loptid", zlab = "Diskonteringsfaktor", ticktype = "detailed", theta = 30, phi = 30)
# persp(y = 2019:(2019+100-1), x = 1:100, z = t(scenario_base$'Nollkupongränta'), main = "Basscenario IORP ST 2019", ylab = "år", xlab = "Loptid", zlab = "Nollkupongränta", ticktype = "detailed", theta = 30, phi = 30)
# persp(x = 2019:(2019+100-1), y = 1:100, z = scenario_adverse$'Terminsränta viktad', main = "Stressat scenario IORP ST 2019", xlab = "år", ylab = "Loptid", zlab = "Terminsränta viktad", ticktype = "detailed", theta = -30, phi = 30)
# persp(x = 2019:(2019+100-1), y = 1:100, z = scenario_adverse$'Diskonteringsfaktor', main = "Stressat scenario IORP ST 2019", xlab = "år", ylab = "Loptid", zlab = "Diskonteringsfaktor", ticktype = "detailed", theta = 30, phi = 30)
# persp(y = 2019:(2019+100-1), x = 1:100, z = t(scenario_adverse$'Nollkupongränta'), main = "Stressat scenario IORP ST 2019", ylab = "år", xlab = "Loptid", zlab = "Nollkupongränta", ticktype = "detailed", theta = 30, phi = 30)

# Skriv till Excel
# Scenarion för stresstest av IORP:ar 2019
# "iht" = Input Helper Tool
# scenario_baseline_iht <- scenario_base$'Nollkupongränta'
# scenario_baseline_iht[scenario_baseline_iht < 0] <- 0.000
# scenario_adverse_iht <- scenario_adverse$'Nollkupongränta'
# scenario_adverse_iht[scenario_adverse_iht < 0] <- 0.000
# scenario_baseline_iht <- data.frame(scenario_baseline_iht)
# names(scenario_baseline_iht) <- 1:100
# scenario_adverse_iht <- data.frame(scenario_adverse_iht)
# names(scenario_adverse_iht) <- 1:100
#install.packages("xlsx")
#library("xlsx")
#write.xlsx(scenario_baseline_iht, file = "Inputs helper tool Sverige.xlsx", sheetName = "Future RFR Baseline", append = FALSE)
#write.xlsx(scenario_adverse_iht, file = "Inputs helper tool Sverige.xlsx", sheetName = "Futute RFR Adverse", append = TRUE)