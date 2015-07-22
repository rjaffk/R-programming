best <- function(state, outc) {
  ha = na.omit(data.frame(outcome[2],outcome[11],outcome[7]))
  hf = na.omit(data.frame(outcome[2],outcome[17],outcome[7]))
  pn = na.omit(data.frame(outcome[2],outcome[23],outcome[7]))
  if (!(state %in% outcome$State)) stop("Invalid state")
  if (!(outc %in% c("heart attack","heart failure","pneumonia"))) stop("Invalid outcome")
  if (outc=="heart attack"){data = subset(ha, ha[3]==state)
                            mn = which.min(as.double(data[,2]))}
  if (outc=="heart failure") {data = subset(hf, hf[3]==state)
                              mn = which.min(as.double(data[,2]))}
  if (outc=="pneumonia") {data = subset(pn, pn[3]==state)
                          mn = which.min(as.double(data[,2]))}
  data[mn,1]
}
