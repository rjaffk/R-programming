rankall <- function(outc, num = "best") {
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings = "Not Available")
  if(!outc %in% c("heart attack", "heart failure", "pneumonia")) {stop("invalid outcome")}
  ret = data.frame(hospital = as.character(), state = as.character())
  
  
  if (outc == "heart attack"){
  ha = na.omit(data.frame(outcome[7],outcome[2], outcome[11]))
  ha$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack = as.double(ha$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
  ha = ha[order(ha$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, ha$Hospital.Name, decreasing = F),] 
  un = unique(ha$State)
    for (i in un){
    data = subset(ha,ha$State==i)
    if (num=="best"){name = data.frame(hospital = data[1,2], state = i)
    ret = rbind(ret,name)}
    if (num=="worst"){name = data.frame(hospital = tail(data[2],1), state = i)
    ret = rbind(ret,name)}
    if (!num %in% c("worst", "best")){
    name = data.frame(hospital = data[num,2], state = i)
    ret = rbind(ret,name)}
    } 
  return(ret)}

  if (outc == "heart failure"){
    hf = na.omit(data.frame(outcome[7],outcome[2], outcome[17]))
    hf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure = as.double(hf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
    hf = hf[order(hf$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, hf$Hospital.Name, decreasing = F),] 
    un = unique(hf$State)
    for (i in un){
      data = subset(hf,hf$State==i)
      if (num=="best"){name = data.frame(hospital = data[1,2], state = i)
      ret = rbind(ret,name)}
      if (num=="worst"){name = data.frame(hospital = tail(data[2],1), state = i)
      ret = rbind(ret,name)}
      if (!num %in% c("worst", "best")){
        name = data.frame(hospital = data[num,2], state = i)
        ret = rbind(ret,name)}
    } 
    return(ret)}
  
  if (outc == "pneumonia"){
    pn = na.omit(data.frame(outcome[7],outcome[2], outcome[23]))
    pn$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia = as.double(pn$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
    pn = pn[order(pn$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, pn$Hospital.Name, decreasing = F),] 
    un = unique(pn$State)
    for (i in un){
      data = subset(pn,pn$State==i)
      if (num=="best"){name = data.frame(hospital = data[1,2], state = i)
      ret = rbind(ret,name)}
      if (num=="worst"){
      n = length(data$State)
      name = data.frame(hospital = data[n,2], state = i)
      ret = rbind(ret,name)}
      if (!num %in% c("worst", "best")){
        name = data.frame(hospital = data[num,2], state = i)
        ret = rbind(ret,name)
       }
    } 
    
    return(ret)}

}