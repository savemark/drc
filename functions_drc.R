# Detta program beräknar diskonteringsräntekurvor både från idag och framåt i tiden,
# för användning till exempelvis framtida balansräkningar. Kurvorna kan enkelt skrivas till 
# Excel i tabellformat eller plottas som ytor.
# Indata är marknadsnoterade swapräntor.

# Se exempel från PROMEMORIA 2013-12-01 [FI Dnr 13-11409].
# Programmerad av Christian Savemark (maj 2019)
# Ändringar: Namn (datum)
# Ändringar består i:

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

zeroCouponRates <- function(t = 1:length(df), df) {
  (1/df[t])^(1/t)-1
}

nextDiscountFactor <- function(df, fwdr) {
  df_next <- df/(1+fwdr)
  return(df_next)
}

zeroCouponRate <- function(t, df) {
  return((1/df)^(1/t)-1)
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
  # UFR Ultimate Forward Rate
  res <- numeric(T)
  for (t in seq_len(T)) {
    res[t] <- ifelse(t<=length(fwdr), (1-w(t))*fwdr[t]+w(t)*UFR, UFR)
  } 
  return(res)
}

logLinearDFRelationshipClosure <- function(t, df) {
  # exponerade log-linjära relationer mellan diskonteringsfaktorer.
  # t är en 3-dimensionell vektor (t1, t2, t3) med t1 < t2 < t3.
  # denna funktion returnerar en funktion (closure)
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

stress <- function(x, 
                   type = c("none", "absolute_up", "absolute_down", "relative_up", "relative_down"),
                   t = NULL,
                   y = NULL) {
  # type teckensträng, exempelvis "absolute_up"
  type <- match.arg(type)
  if (is.null(t)) t <- c(1:10, 12, 15, 20)
  none = function(x) {
    return(x)
  }
  absolute_up = function(x) {
    if (is.null(y)) y <- (1/10000)*c(50, 53, 56, 60, 62, 64, 65, 66, 67, 68, 69, 70, 70)
    f <- approxfun(t, y, rule = 2)
    x[which(x>0)] <- x[which(x>0)]+f(which(x>0))
    return(x)
  }
  absolute_down = function(x) {
    if (is.null(y)) y <- (1/10000)*c(50, 53, 56, 60, 62, 64, 65, 66, 67, 68, 69, 70, 70)
    f <- approxfun(t, y, rule = 2)
    index_positive <- which(x>0)
    x_cut <- x[index_positive]
    x_cut_stressed <- x_cut-f(index_positive)
    x_cut_stressed[x_cut_stressed<0] <- 0
    x[index_positive] <- x_cut_stressed
    return(x)
  }
  relative_up = function(x) {
    if (is.null(y)) y <- (1/100)*c(41, 38, 36, 33, 32, 30, 28, 27, 26, 25, 23, 22, 20)
    f <- approxfun(t, y, rule = 2)
    x[which(x>0)] <- x[which(x>0)]*(1+f(which(x>0)))
    return(x)
  }
  relative_down = function(x) {
    if (is.null(y)) y <- (1/100)*c(41, 38, 36, 33, 32, 30, 28, 27, 26, 25, 23, 22, 20)
    f <- approxfun(t, y, rule = 2)
    x[which(x>0)] <- x[which(x>0)]*(1-f(which(x>0)))
    return(x)
  }
  switch(type,
         none = none(x),
         absolute_up = absolute_up(x),
         absolute_down = absolute_down(x),
         relative_up = relative_up(x),
         relative_down = relative_down(x)
  )
}

interestRateSwap <- function(par_t, credit.adj = -0.0035, allow.negative.rates = TRUE, stress = NULL, args.stress = list()) {
  par_t.adj <- par_t + credit.adj
  if (!allow.negative.rates) par_t.adj[par_t.adj < 0] <- 0
  if (!is.null(stress)) {
    par_t.adj.stressed <- do.call(stress, c(list(x = par_t.adj), args.stress))
  } else {
    par_t.adj.stressed <- par_t.adj
  }
  df <- discountFactors(part = par_t.adj.stressed)
  zcr <- zeroCouponRates(df = df)
  fwdr <- vforwardRates(1:length(par_t), df)
  while (any(is.na(df))) {
    naindex <- min(which(is.na(df))) # Smallest index of NAs in df
    t_1 <- naindex-1 # Largest non-NA index before NA in df
    index <- which(!is.na(par_t.adj.stressed)) # Indices that are not NA in par_t
    t_3 <- index[min(which(index>naindex))] # The smallest non-NA index that is larger than naindex
    j <- naindex:(t_3-1) # NA indices in DF
    logLinearDFRelationships <- lapply(j, function (x) logLinearDFRelationshipClosure(c(t_1, x, t_3), df))
    zcr[t_3] <- optim(par = 0.001, fn = objectiveFunction, s = t_1, t = t_3, part = par_t.adj.stressed, df = df, method = "Brent", lower = -0.5, upper = 0.5)$par
    df[t_3] <- 1/(1+zcr[t_3])^(t_3)
    m <- 1
    for (k in naindex:(t_3-1)) {
      df[k] <- logLinearDFRelationships[[m]](zcr[t_3])
      m <- m+1
    }
    fwdr[naindex:t_3] <- ((df[t_1]/df[t_3])^(1/(t_3-t_1)))-1
  }
  zcr <- zeroCouponRates(1:length(par_t.adj.stressed), df)
  datafr <- data.frame(1:length(par_t), 
                       par_t, 
                       par_t.adj, 
                       par_t.adj.stressed,
                       fwdr, 
                       df, 
                       zcr)
  names(datafr) <- c("L\u00F6ptid", 
                     "Marknadsnot.", 
                     "Marknadsnot. kred. just.", 
                     "Marknadsnot. kred. just. stressade",
                     "Terminsr\u00E4nta swap", 
                     "Diskonteringsfaktor", 
                     "Nollkupongr\u00E4nta")
  return(datafr)
}

weightedInterestSwap <- function(par_t, 
                                 T, 
                                 credit.adj = -0.0035, 
                                 allow.negative.rates = FALSE, 
                                 UFR = 0.042,
                                 stress = NULL, 
                                 args.stress = list()) {
  # T Längsta Loptid
  # par_t marknadsnoteringar för swapräntor
  par_t_long <- rep_len(NA, length.out = T)
  par_t_long.adj <- rep_len(NA, length.out = T)
  par_t_long.adj.stressed <- rep_len(NA, length.out = T)
  fwdr_long <- rep_len(NA, length.out = T)
  irs <- interestRateSwap(par_t, 
                          credit.adj = credit.adj, 
                          allow.negative.rates = allow.negative.rates, 
                          stress = stress, 
                          args.stress = args.stress)
  par_t.adj <- irs[ , 3]
  par_t.adj.stressed <- irs[ , 4]
  fwdr <- irs[ , 5]
  wfwdr <- weightedForwardRate(fwdr, 
                               w = weight, 
                               T = T, 
                               UFR = UFR)
  wdf <- discountFactors_2(T, wfwdr)
  wzcr <- zeroCouponRates(df = wdf)

  par_t_long[seq(par_t)] <- par_t
  par_t_long.adj[seq(par_t.adj)] <- par_t.adj
  par_t_long.adj.stressed[seq(par_t.adj.stressed)] <- par_t.adj.stressed
  fwdr_long[seq(fwdr)] <- fwdr
  datafr <- data.frame(seq(wfwdr), 
                       par_t_long, 
                       par_t_long.adj,
                       par_t_long.adj.stressed,
                       fwdr_long,
                       wfwdr, 
                       wdf, 
                       wzcr)
  names(datafr) <- c("L\u00F6ptid", 
                     "Marknadsnot. ", 
                     "Marknadsnot. kred. just.",
                     "Marknadsnot. kred. just. str.",
                     "Terminsr\u00E4nta",
                     "Terminsr\u00E4nta viktad", 
                     "Diskonteringsfaktor", 
                     "Nollkupongr\u00E4nta")
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
  return(list("Terminsr\u00E4nta viktad" = M, "Diskonteringsfaktor" = N, "Nollkupongr\u00E4nta" = K))
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
  return(list("Terminsr\u00E4nta viktad" = M, "Diskonteringsfaktor" = N, "Nollkupongr\u00E4nta" = K))
}
