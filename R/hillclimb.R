hillclimb = function(budget, inputs, connections=1, iterations=1, coeffs.upper=1, coeffs.lower=-1, coeffs.upper.square = 1, coeffs.lower.square = -1, coeffs.upper.cross=1, coeffs.lower.cross=-1){
  connections1 = connections
  # Error Messages for impossible inputs
  if(max(connections1)>inputs){
    stop("connections exceeds inputs in profit function")
  }
  if(coeffs.upper<=coeffs.lower){
    stop("coeffs.upper <= coeffs.lower")
  }
  if(budget < 0 | round(budget) != budget){
    stop("budget must be a whole number")
  }
 if(round(budget) != budget |all(round(connections1) != connections1) | round(inputs)!=inputs | round(iterations) != iterations){
   stop("parameters must be integers")
 }
  
  connections1=c(connections1)
  s.store=NULL
  m.store=NULL
  l.store=NULL
  s.moves=NULL
  m.moves=NULL
  l.moves=NULL
  results.table.total=NULL
  c = 1

  while(c <= connections1[length(connections1)]){
  for(g in 1:iterations){
    connections = connections1[c]
  
    y=NULL
    x=budget
    # Random allocation of the budget
    for(i in 1:inputs-1){
      y[i]=sample(0:x, 1)
      x=budget-sum(y)
    }
    Xall1=append(y, x)
    xi.i=matrix(data = NA, nrow=(inputs*connections), ncol=inputs)
    baseCase = Xall1
    for(i in 1:(inputs*connections)){
      xi.i[i,]=Xall1
    }
    baseXall =Xall1
    # The below loop returns a matrix with all possible reallocations of the budget given a certain number of inputs and connections
    ind = 0
    for(j in 1:inputs){
      if(j + connections > inputs){
        for(h in 1:connections){
          xi.i[j + ind,j]=Xall1[j]-1
          if((j + h) <= inputs){
            xi.i[j + ind, j+h]= Xall1[j+h]+1
            ind = ind + 1
          } else{
            xi.i[j + ind, (j+h)-inputs] = Xall1[(j+h)-inputs] + 1
            ind = ind + 1
          }
        }
        ind = ind - 1
      }
      else{
        for(h in 1:connections){
          xi.i[j + ind,j]=Xall1[j]-1
          xi.i[j + ind, j+h]= Xall1[j+h]+1
          ind = ind + 1
        }
        ind = ind - 1
      }
    }
    # Add Xall1 into the matrix of potential budget allocations to compare profit levels entire neighborhoods with current profit position
    xi.i = rbind(xi.i, Xall1)
    #
    ## Remove rows with negative inputs due to impossibility
    h = 1
    while(h <= nrow(xi.i)){
      p = 1
      while (p <= ncol(xi.i)){
        if(xi.i[h, p]<0){
          xi.i = xi.i[-h,]
          p = 1
        } else{
          p = p + 1
        }
      }
      h = h + 1
    }
    ##
    # 
    profitFun = matrix(data=NA, nrow=nrow(xi.i), ncol=2*ncol(xi.i)+(inputs*(inputs-1))/2)
    for(z in 1:nrow(xi.i)){
      index=1
      xixj=rep(NA, (inputs*(inputs-1))/2)
      for(i in 1:(inputs-1)){
        for(j in (i+1):inputs){
          xixj[index]= xi.i[z,i]*xi.i[z,j]
          index = index + 1
        }
      }
      xisqr= rep(NA, inputs)
      for(i in 1:inputs){
        xisqr[i]= xi.i[z,i]^2
      }
      profitFun[z,] = c(xi.i[z,], xisqr, xixj)
    }
    if(c == 1){
      coeffs=c(runif(inputs, coeffs.lower, coeffs.upper), runif(inputs, coeffs.lower.square, coeffs.upper.square), runif((ncol(profitFun)-(2*inputs)), coeffs.lower.cross, coeffs.upper.cross))
    }
    profit = rep(NA, nrow(profitFun))
    for(z in 1:nrow(profitFun)){
      profit.temp = rep(NA, ncol(profitFun))
      profit.temp = coeffs*profitFun[z,]
      profit[z]=sum(profit.temp)
    }
    xi.i.base = xi.i
    profit1 = profit
    # Ascent Strategies: paths correspond to the row in matrix xi.i with desired movement on profit landscape under given strategy#
    
    path.steepAscent = which.max(profit)
    ## path.medianAscent ## Extra code to account for when length(profit) = even number
    if(max(profit)>profit[length(profit)]){
      order = order(subset(profit, profit>profit[length(profit)]))
      if(length(order)%%2 == 0) {
        med1 = match((median(order) + 0.5), order)
        path.medianAscent = match(subset(profit, profit>profit[length(profit)])[med1],profit)
        m = profit[path.medianAscent]
      } else{
        path.medianAscent = match(median(subset(profit, profit>profit[length(profit)])), profit)
        m = profit[path.medianAscent]
      }
    } else {
      m = max(profit, na.rm=TRUE)
      path.medianAscent = length(profit)
    }
    ## path.leastAscent ##
    if(max(profit)>profit[length(profit)]){
      path.leastAscent = match(min(subset(profit, profit>profit[length(profit)])), profit)
      l = profit[path.leastAscent]
    } else{
      l = max(profit)
      path.leastAscent = length(profit)
    }
    
    ## Median Ascent ##
    Xall1 = baseXall
    xi.i = xi.i.base
    m.index = NULL
    m.index.counter = 1
    m = profit[length(profit)]+1
    while(m > profit[length(profit)] ) {
      Xall1 = xi.i[path.medianAscent,] 
      xi.i=matrix(data = NA, nrow=(inputs*connections), ncol=inputs)
      for(i in 1:(inputs*connections)){
        xi.i[i,]=Xall1
      }
      ind = 0
      for(j in 1:inputs){
        if(j + connections > inputs){
          for(h in 1:connections){
            xi.i[j + ind,j]=Xall1[j]-1
            if((j + h) <= inputs){
              xi.i[j + ind, j+h]= Xall1[j+h]+1
              ind = ind + 1
            } else{
              xi.i[j + ind, (j+h)-inputs] = Xall1[(j+h)-inputs] + 1
              ind = ind + 1
            }
          }
          ind = ind - 1
        }
        else{
          for(h in 1:connections){
            xi.i[j + ind,j]=Xall1[j]-1
            xi.i[j + ind, j+h]= Xall1[j+h]+1
            ind = ind + 1
          }
          ind = ind - 1
        }
      }
      xi.i = rbind(xi.i, Xall1)
      ## Remove rows with negative inputs due to impossibility
      h = 1
      while(h <= nrow(xi.i)){
        p = 1
        while (p <= ncol(xi.i)){
          if(xi.i[h, p]<0){
            xi.i = xi.i[-h,]
            p = 1
          } else{
            p = p + 1
          }
        }
        h = h + 1
      }
      ##
      profitFun = matrix(data=NA, nrow=nrow(xi.i), ncol=2*ncol(xi.i)+(inputs*(inputs-1))/2)
      for(z in 1:nrow(xi.i)){
        index=1
        xixj=rep(NA, (inputs*(inputs-1))/2)
        for(i in 1:(inputs-1)){
          for(j in (i+1):inputs){
            xixj[index]= xi.i[z,i]*xi.i[z,j]
            index = index + 1
          }
        }
        xisqr= rep(NA, inputs)
        for(i in 1:inputs){
          xisqr[i]= xi.i[z,i]^2
        }
        profitFun[z,] = c(xi.i[z,], xisqr, xixj)
      }
      profit = rep(NA, nrow(profitFun))
      for(z in 1:nrow(profitFun)){
        profit.temp = rep(NA, ncol(profitFun))
        profit.temp = coeffs*profitFun[z,]
        profit[z]=sum(profit.temp)
      }
      if(max(profit)>profit[length(profit)]){
        order = order(subset(profit, profit>profit[length(profit)]))
        if(length(order)%%2 == 0) {
          med1 = match((median(order) + 0.5), order)
          path.medianAscent = match(subset(profit, profit>profit[length(profit)])[med1],profit)
          m = profit[path.medianAscent]
        } else{
          path.medianAscent = match(median(subset(profit, profit>profit[length(profit)])), profit)
          m = profit[path.medianAscent]
        }
      } else {
        m = max(profit, na.rm=TRUE)
      }
      m.index[m.index.counter] = m
      m.index.counter =  m.index.counter + 1
    }
    
    # Steepest Ascent
    Xall1 = baseXall
    profit = profit1
    xi.i = xi.i.base
    s.index = NULL
    s.index.counter = 1
    s = profit[length(profit)]+1
    while(s > profit[length(profit)] ) {
      Xall1 = xi.i[path.steepAscent,]
      xi.i=matrix(data = NA, nrow=(inputs*connections), ncol=inputs)
      for(i in 1:(inputs*connections)){
        xi.i[i,]=Xall1
      }
      ind = 0
      for(j in 1:inputs){
        if(j + connections > inputs){
          for(h in 1:connections){
            xi.i[j + ind,j]=Xall1[j]-1
            if((j + h) <= inputs){
              xi.i[j + ind, j+h]= Xall1[j+h]+1
              ind = ind + 1
            } else{
              xi.i[j + ind, (j+h)-inputs] = Xall1[(j+h)-inputs] + 1
              ind = ind + 1
            }
          }
          ind = ind - 1
        }
        else{
          for(h in 1:connections){
            xi.i[j + ind,j]=Xall1[j]-1
            xi.i[j + ind, j+h]= Xall1[j+h]+1
            ind = ind + 1
          }
          ind = ind - 1
        }
      }
      xi.i = rbind(xi.i, Xall1)
      ## Remove rows with negative inputs due to impossibility
      h = 1
      while(h <= nrow(xi.i)){
        p = 1
        while (p <= ncol(xi.i)){
          if(xi.i[h, p]<0){
            xi.i = xi.i[-h,]
            p = 1
          } else{
            p = p + 1
          }
        }
        h = h + 1
      }
      ##
      profitFun = matrix(data=NA, nrow=nrow(xi.i), ncol=2*ncol(xi.i)+(inputs*(inputs-1))/2)
      for(z in 1:nrow(xi.i)){
        index=1
        xixj=rep(NA, (inputs*(inputs-1))/2)
        for(i in 1:(inputs-1)){
          for(j in (i+1):inputs){
            xixj[index]= xi.i[z,i]*xi.i[z,j]
            index = index + 1
          }
        }
        xisqr= rep(NA, inputs)
        for(i in 1:inputs){
          xisqr[i]= xi.i[z,i]^2
        }
        
        profitFun[z,] = c(xi.i[z,], xisqr, xixj)
      }
      profit = rep(NA, nrow(profitFun))
      for(z in 1:nrow(profitFun)){
        profit.temp = rep(NA, ncol(profitFun))
        profit.temp = coeffs*profitFun[z,]
        profit[z]=sum(profit.temp)
      }
      path.steepAscent = match(max(profit, na.rm = TRUE), profit)
      s = max(profit, na.rm = TRUE)
      s.index[s.index.counter] = s
      s.index.counter =  s.index.counter + 1
    }
    
    ## Least Ascent ##
    Xall1 = baseXall
    xi.i = xi.i.base
    profit = profit1
    l.index = NULL
    l.index.counter = 1
    l = profit[length(profit)]+1
    while(l > profit[length(profit)] ) {
      Xall1 = xi.i[path.leastAscent,] 
      xi.i=matrix(data = NA, nrow=(inputs*connections), ncol=inputs)
      for(i in 1:(inputs*connections)){
        xi.i[i,]=Xall1
      }
      ind = 0
      for(j in 1:inputs){
        if(j + connections > inputs){
          for(h in 1:connections){
            xi.i[j + ind,j]=Xall1[j]-1
            if((j + h) <= inputs){
              xi.i[j + ind, j+h]= Xall1[j+h]+1
              ind = ind + 1
            } else{
              xi.i[j + ind, (j+h)-inputs] = Xall1[(j+h)-inputs] + 1
              ind = ind + 1
            }
          }
          ind = ind - 1
        }
        else{
          for(h in 1:connections){
            xi.i[j + ind,j]=Xall1[j]-1
            xi.i[j + ind, j+h]= Xall1[j+h]+1
            ind = ind + 1
          }
          ind = ind - 1
        }
      }
      xi.i = rbind(xi.i, Xall1)
      ## Remove rows with negative inputs due to impossibility
      h = 1
      while(h <= nrow(xi.i)){
        p = 1
        while (p <= ncol(xi.i)){
          if(xi.i[h, p]<0){
            xi.i = xi.i[-h,]
            p = 1
          } else{
            p = p + 1
          }
        }
        h = h + 1
      }
      ##
      profitFun = matrix(data=NA, nrow=nrow(xi.i), ncol=2*ncol(xi.i)+(inputs*(inputs-1))/2)
      for(z in 1:nrow(xi.i)){
        index=1
        xixj=rep(NA, (inputs*(inputs-1))/2)
        for(i in 1:(inputs-1)){
          for(j in (i+1):inputs){
            xixj[index]= xi.i[z,i]*xi.i[z,j]
            index = index + 1
          }
        }
        xisqr= rep(NA, inputs)
        for(i in 1:inputs){
          xisqr[i]= xi.i[z,i]^2
        }
        
        profitFun[z,] = c(xi.i[z,], xisqr, xixj)
      }
      profit = rep(NA, nrow(profitFun))
      for(z in 1:nrow(profitFun)){
        profit.temp = rep(NA, ncol(profitFun))
        profit.temp = coeffs*profitFun[z,]
        profit[z]=sum(profit.temp)
      }
      if(max(profit)>profit[length(profit)]){
        path.leastAscent = match(min(subset(profit, profit>profit[nrow(profitFun)]), na.rm=TRUE), profit)
        l = profit[path.leastAscent]
      } else{
        l = max(profit)
      }
      l.index[l.index.counter] = l
      l.index.counter =  l.index.counter + 1
    }
    s.moves[g]=length(s.index)
    m.moves[g]=length(m.index)
    l.moves[g]=length(l.index)
    s.store[g]=s
    m.store[g]=m
    l.store[g]=l
  }
  
  s.store.norm = s.store / (budget + (budget^2))
  m.store.norm = m.store / (budget + (budget^2))
  l.store.norm = l.store / (budget + (budget^2))
  s.mean=round(mean(s.store.norm) * 100, 2)
  s.sd=round(sd(s.store.norm), 2)
  s.mean.moves = round(mean(s.moves), 2)
  m.mean=round(mean(m.store.norm) * 100, 2)
  m.sd=round(sd(m.store.norm), 2)
  m.mean.moves = round(mean(m.moves), 2)
  l.mean=round(mean(l.store.norm) * 100, 2)
  l.sd=round(sd(l.store.norm), 2)
  l.mean.moves = round(mean(l.moves), 2)

  Ascent.Strategy = c("Steepest Ascent", "Median Ascent", "Least Ascent")
  Mean = c(s.mean, m.mean, l.mean)
  Std.Error = c(s.sd, m.sd, l.sd)
  Num.Connects = c( connections1[c], connections1[c], connections1[c])
  Mean.Number.Moves = c(s.mean.moves, m.mean.moves, l.mean.moves)
  results.table = cbind(Num.Connects, Ascent.Strategy, Mean, Std.Error, Mean.Number.Moves)
  results.table.total = rbind(results.table.total, results.table)
  c = connections + 1
}
  print(as.table(results.table.total))
}
