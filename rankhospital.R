rankhospital <- function(state, outc, num = "best") { 
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
  if (!state %in% outcome$State) {stop("invalid state")}
  if(!outc %in% c("heart attack", "heart failure", "pneumonia")) {stop("invalid outcome")}
  if (outc == "heart attack"){
    ha = na.omit(data.frame(outcome[7],outcome[2], outcome[11]))
    data = subset(ha, ha[1]==state)
    data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack = as.double(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    data = data[order(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, data$Hospital.Name, decreasing = F),] 
    if (num == "best") num = 1
    if (num == "worst") num = length(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    d = data[num,2]
  }
if (outc == "heart failure"){
  hf = na.omit(data.frame(outcome[7],outcome[2], outcome[17]))
  data = subset(hf, hf[1]==state)
  data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure = as.double(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
  data = data[order(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, data$Hospital.Name, decreasing = F),] 
  if (num == "best") num = 1
  if (num == "worst") num = length(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
  d = data[num,2]
}
if (outc == "pneumonia"){
  pn = na.omit(data.frame(outcome[7],outcome[2], outcome[23]))
  data = subset(pn, pn[1]==state)
  data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia = as.double(data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
  data = data[order(data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, data$Hospital.Name, decreasing = F),] 
  if (num == "best") num = 1
  if (num == "worst") num = length(data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
  d = data[num,2]
}
d
  
}