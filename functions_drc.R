# Detta program beräknar diskonteringsräntekurvor både från idag och framåt i tiden,
# för användning till exempelvis framtida balansräkningar. Kurvorna kan enkelt skrivas till 
# Excel i tabellformat eller plottas som ytor.
# Indata är marknadsnoterade swapräntor.
# Se exempel från PROMEMORIA 2013-12-01 [FI Dnr 13-11409]

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

weightedForwardRate <- function(fwdr, w, T, UFR = 0) {
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
  # denna funktion returnerar en funktion (closure) som beror av x
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
                   type = c("identity", 
                            "no_negative",
                            "absolute_up", 
                            "absolute_up_100bps", 
                            "absolute_up_50bps",
                            "absolute_down", 
                            "absolute_down_100bps", 
                            "absolute_down_50bps",
                            "relative_up", 
                            "relative_down"),
                   t = NULL,
                   y = NULL) {
  # x swapräntor
  # type teckensträng, exempelvis "absolute_up"
  # t löptider
  # y stresser
  type <- match.arg(type)
  if (is.null(t)) t <- c(1:10, 12, 15, 20)
  no_negative = function(x) {
    x[which(x<0)] <- 0.0
    return(x)
  }
  absolute_up = function(x) {
    if (is.null(y)) y <- (1/10000)*c(50, 53, 56, 60, 62, 64, 65, 66, 67, 68, 69, 70, 70)
    f <- approxfun(t, y, rule = 2)
    x[which(x>0)] <- x[which(x>0)]+f(which(x>0))
    return(x)
  }
  absolute_up_100bps = function(x) {
    if (is.null(y)) y <- (1/10000)*c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100)
    f <- approxfun(t, y, rule = 2)
    x <- ifelse(x+f(x)<0, 0.0, x+f(x))
    return(x)
  }
  absolute_up_50bps = function(x) {
    if (is.null(y)) y <- (1/10000)*c(50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50)
    f <- approxfun(t, y, rule = 2)
    x <- ifelse(x+f(x)<0, 0.0, x+f(x))
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
  absolute_down_100bps = function(x) {
    if (is.null(y)) y <- (1/10000)*c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100)
    f <- approxfun(t, y, rule = 2)
    x <- ifelse(x-f(x)<0, 0.0, x-f(x))
    return(x)
  }
  absolute_down_50bps = function(x) {
    if (is.null(y)) y <- (1/10000)*c(50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50, 50)
    f <- approxfun(t, y, rule = 2)
    x <- ifelse(x-f(x)<0, 0.0, x-f(x))
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
         identity = I(x),
         no_negative = no_negative(x),
         absolute_up = absolute_up(x),
         absolute_up_100bps = absolute_up_100bps(x),
         absolute_up_50bps = absolute_up_50bps(x),
         absolute_down = absolute_down(x),
         absolute_down_100bps = absolute_down_100bps(x),
         absolute_down_50bps = absolute_down_50bps(x),
         relative_up = relative_up(x),
         relative_down = relative_down(x)
  )
}

interestRateSwap <- function(x, shift = 0, transformation = I, args.transformation = list()) {
  if (missing(x)) stop("Missing swap rates")
  if (is.na(x[1])) stop("First swap rate is NA")
  if (is.na(x[length(x)])) stop("Last swap rate is NA")
  x.adj <- x + shift
  x.adj.trans <- do.call(transformation, c(list(x = x.adj), args.transformation))
  df <- discountFactors(part = x.adj.trans)
  zcr <- zeroCouponRates(df = df)
  fwdr <- vforwardRates(1:length(x), df)
  while (any(is.na(df))) {
    naindex <- min(which(is.na(df))) # Smallest index of NAs in df
    t_1 <- naindex-1 # Largest non-NA index before NA in df
    index <- which(!is.na(x.adj.trans)) # Indices that are not NA in x
    t_3 <- index[min(which(index>naindex))] # The smallest non-NA index that is larger than naindex
    j <- naindex:(t_3-1) # NA indices in DF
    logLinearDFRelationships <- lapply(j, function (x) logLinearDFRelationshipClosure(c(t_1, x, t_3), df))
    zcr[t_3] <- optim(par = 0.001, fn = objectiveFunction, s = t_1, t = t_3, part = x.adj.trans, df = df, method = "Brent", lower = -0.5, upper = 0.5, control = list(reltol = .Machine$double.eps))$par
    df[t_3] <- 1/(1+zcr[t_3])^(t_3)
    m <- 1
    for (k in naindex:(t_3-1)) {
      df[k] <- logLinearDFRelationships[[m]](zcr[t_3])
      m <- m+1
    }
    fwdr[naindex:t_3] <- ((df[t_1]/df[t_3])^(1/(t_3-t_1)))-1
  }
  zcr <- zeroCouponRates(1:length(x.adj.trans), df)
  datafr <- data.frame(1:length(x), 
                       x, 
                       x.adj, 
                       x.adj.trans,
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

interestRateSwap_2 <- function(x, shift = 0, transformation = I, args.transformation = list()) {
  if (missing(x)) stop("Missing swap rates")
  if (is.na(x[1])) stop("First swap rate is NA")
  if (is.na(x[length(x)])) stop("Last swap rate is NA")
  x.adj <- x + shift
  x.adj.trans <- do.call(transformation, c(list(x = x.adj), args.transformation))
  df <- discountFactors(part = x.adj.trans)
  zcr <- zeroCouponRates(df = df)
  fwdr <- vforwardRates(1:length(x), df)
  while (any(is.na(df))) {
    naindex <- min(which(is.na(df))) # Smallest index of NAs in df
    t_1 <- naindex-1 # Largest non-NA index before NA in df
    index <- which(!is.na(x.adj.trans)) # Indices that are not NA in x
    t_3 <- index[min(which(index>naindex))] # The smallest non-NA index that is larger than naindex
    j <- naindex:(t_3-1) # NA indices in DF between t_1 and t_3
    logLinearDFRelationships <- lapply(j, function (x) logLinearDFRelationshipClosure(c(t_1, x, t_3), df))
    zcr[t_3] <- optim(par = 0.001, fn = objectiveFunction, s = t_1, t = t_3, part = x.adj.trans, df = df, method = "Brent", lower = -0.5, upper = 0.5, control = list(reltol = .Machine$double.eps))$par
    df[t_3] <- 1/(1+zcr[t_3])^(t_3)
    m <- 1
    for (k in naindex:(t_3-1)) {
      df[k] <- logLinearDFRelationships[[m]](zcr[t_3])
      m <- m+1
    }
    fwdr[naindex:t_3] <- ((df[t_1]/df[t_3])^(1/(t_3-t_1)))-1
  }
  zcr <- zeroCouponRates(1:length(x.adj.trans), df)
  datafr <- data.frame(1:length(x), 
                       x, 
                       x.adj, 
                       x.adj.trans,
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

weightedInterestRateSwap <- function(x, 
                                     T, 
                                     shift = 0,
                                     UFR = 0,
                                     transformation = I, 
                                     args.transformation = list()) {
  # T Längsta Loptid
  # x marknadsnoteringar för swapräntor
  x_long <- rep_len(NA, length.out = T)
  x_long.adj <- rep_len(NA, length.out = T)
  x_long.adj.trans <- rep_len(NA, length.out = T)
  fwdr_long <- rep_len(NA, length.out = T)
  irs <- interestRateSwap(x, 
                          shift = shift, 
                          transformation = transformation, 
                          args.transformation = args.transformation)
  x.adj <- irs[ , 3]
  x.adj.trans <- irs[ , 4]
  fwdr <- irs[ , 5]
  wfwdr <- weightedForwardRate(fwdr, 
                               w = weight, 
                               T = T, 
                               UFR = UFR)
  wdf <- discountFactors_2(T, wfwdr)
  wzcr <- zeroCouponRates(df = wdf)
  
  x_long[seq(x)] <- x
  x_long.adj[seq(x.adj)] <- x.adj
  x_long.adj.trans[seq(x.adj.trans)] <- x.adj.trans
  fwdr_long[seq(fwdr)] <- fwdr
  datafr <- data.frame(seq(wfwdr), 
                       x_long, 
                       x_long.adj,
                       x_long.adj.trans,
                       fwdr_long,
                       wfwdr, 
                       wdf, 
                       wzcr)
  names(datafr) <- c("L\u00F6ptid", 
                     "Marknadsnot. ", 
                     "Marknadsnot. kred. just.",
                     "Marknadsnot. kred. just. stressade",
                     "Terminsr\u00E4nta",
                     "Terminsr\u00E4nta viktad", 
                     "Diskonteringsfaktor", 
                     "Nollkupongr\u00E4nta")
  return(datafr)
}

scenarioGenerator <- function(x, start.year, T, UFR = 0, stress = NULL, ...) {
  # Denna funktion räknar ut diskonteringsfaktorer till varje nytt år.
  # (används t.ex. för diskontering vid framräkning av framtida balansräkningar)
  # Output är en lista innehållandes tre matriser.
  # x är marknadsnoteringar för ränteswapavtal.
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
  wfwdr <- weightedInterestRateSwap(x, T = T, UFR = UFR, ...)[ , 6]
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

scenarioGenerator_2 <- function(x, start.year, T, UFR = 0, ...) {
  # Denna funktion kan användas till att diskontera alla framtida kassaflöden till första året,
  # även för framtida kontrakt. Bör endast användas i sällsynta fall?
  # Output är en lista innehållandes tre matriser.
  # x är marknadsnoteringar för ränteswapavtal.
  # start.year är år man börjar.
  # T är antal år framåt i tiden.
  # UFR = ultimate forward rate
  vector.is.empty <- function(x) return(length(x) == 0)
  M <- matrix(NA, T, T) # Terminsräntor
  N <- matrix(NA, T, T) # Diskonteringsfaktorer
  K <- matrix(NA, T, T) # Nollkupongräntor
  wfwdr <- weightedInterestRateSwap(x, T = T, UFR = UFR, ...)[ , 6]
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
