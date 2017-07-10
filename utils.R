# helpers for probability calibration class

# pull packages

pkgs <- c('mlbench', 'caret', 'data.table',
          'MLmetrics', 'localexpeRt', 'gbm')
purrr::walk(pkgs, library, character.only = TRUE, warn.conflicts = FALSE)

data("PimaIndiansDiabetes")

## useful functions

ReliabilityPlot <- function(obs, pred, bins = 10) {
  # save and reset default graphical parameters
  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))
  plot(
    c(0, 1),
    c(0, 1),
    col = "grey",
    type = "l",
    xlab = expression('s'[i]),
    ylab = expression('p(c'[i]*')')
  )
  
  # scale
  minp <- min(pred)
  maxp <- max(pred)
  pdiff <- maxp - minp
  pred <- (pred - minp) / pdiff
  
  # bins
  bin.pred <- cut(pred, breaks = bins)
  
  # ratio
  k <- plyr::ldply(levels(bin.pred), function(x) {
    idx <- x == bin.pred
    c(sum(obs[idx]) / length(obs[idx]), mean(pred[idx]))
  })
  
  # replace NAs
  is.nan.idx <- !is.nan(k$V2)
  k <- k[is.nan.idx, ]
  
  lines(k$V2, k$V1, col = 'red', type = 'o', lwd = 4)
}

ExtractBestPreds <- function(model, col = 'pred') {
  best.pred.key <-
    data.table::data.table(model$bestTune, key = names(model$bestTune)) # save this
  all.preds <-
    data.table::data.table(model$pred, key = names(model$bestTune))
  best.pred <- all.preds[best.pred.key,] # save this, and reorder?
  data.table::setorderv(best.pred, c("rowIndex", "Resample"))
  return(best.pred[[eval(col)]])
}

bootstrap_iso <- function(preds, y, NBOOT = 500, SAMP = 0.5){

  samp_iso <- function(x, y, N, SAMP) {
    rows <- sample(seq(N), as.integer(N * SAMP), replace = TRUE)
    return(as.stepfun(isoreg(x[rows], y[rows])))
  }
  
  repeated_iso <- function(NBOOT, x, y, N, SAMP){
    return(lapply(seq(NBOOT), function(i) samp_iso(x=x, y=y, N=N, SAMP=SAMP)))
  }
  
  N <- NROW(preds)
  
  return(repeated_iso(NBOOT = NBOOT, x = preds, y = y, N = N, SAMP = SAMP))
}

predict_bootiso <- function(models, newdata){
  levels <- length(models)
  out <- data.table(tmp = seq(nrow(newdata)))
  
  if (length(levels) > 1) {
  for (i in seq(levels)){
    out[, paste0('col',i) := apply(sapply(models[[i]], function(x){
      x(newdata[[i]])}), 1, mean)]
  }
  out[, tmp:=NULL]
  
  return(out)}

  else{
    out <- apply(sapply(models, function(x){
      x(newdata)}), 1, mean)

    return(out)
  }
}