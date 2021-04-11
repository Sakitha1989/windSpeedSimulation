cp01.144 <- matrix(list(),10,10)
for (i in 1:9) {
  for (j in i+1:10-i) {
    if(i<j){
      temp <- cpt.locations.multi(decompseData$residual[1:144,c(i,j)],0.01)
      if(!is.null(temp)){
        cp01.144[i,j][[1]] <- temp
      }
    }
  }
}

cp01.288 <- matrix(list(),10,10)
for (i in 1:9) {
  for (j in i+1:10-i) {
    if(i<j){
      temp <- cpt.locations.multi(decompseData$residual[1:288,c(i,j)],0.01)
      if(!is.null(temp)){
        cp01.288[i,j][[1]] <- temp
      }
    }
  }
}

cp01.576 <- matrix(list(),10,10)
for (i in 1:9) {
  for (j in i+1:10-i) {
    if(i<j){
      temp <- cpt.locations.multi(decompseData$residual[1:576,c(i,j)],0.01)
      if(!is.null(temp)){
        cp01.576[i,j][[1]] <- temp
      }
    }
  }
}

cp05.144 <- matrix(list(),10,10)
for (i in 1:9) {
  for (j in i+1:10-i) {
    if(i<j){
      temp <- cpt.locations.multi(decompseData$residual[1:144,c(i,j)],0.05)
      if(!is.null(temp)){
        cp05.144[i,j][[1]] <- temp
      }
    }
  }
}

cp05.288 <- matrix(list(),10,10)
for (i in 1:9) {
  for (j in i+1:10-i) {
    if(i<j){
      temp <- cpt.locations.multi(decompseData$residual[1:288,c(i,j)],0.05)
      if(!is.null(temp)){
        cp05.288[i,j][[1]] <- temp
      }
    }
  }
}

cp05.576 <- matrix(list(),10,10)
for (i in 1:9) {
  for (j in i+1:10-i) {
    if(i<j){
      temp <- cpt.locations.multi(decompseData$residual[1:576,c(i,j)],0.05)
      if(!is.null(temp)){
        cp05.576[i,j][[1]] <- temp
      }
    }
  }
}

cp10.144 <- matrix(list(),10,10)
for (i in 1:9) {
  for (j in i+1:10-i) {
    if(i<j){
      temp <- cpt.locations.multi(decompseData$residual[1:144,c(i,j)],0.10)
      if(!is.null(temp)){
        cp10.144[i,j][[1]] <- temp
      }
    }
  }
}

cp10.288 <- matrix(list(),10,10)
for (i in 1:9) {
  for (j in i+1:10-i) {
    if(i<j){
      temp <- cpt.locations.multi(decompseData$residual[1:288,c(i,j)],0.10)
      if(!is.null(temp)){
        cp10.288[i,j][[1]] <- temp
      }
    }
  }
}

cp10.576 <- matrix(list(),10,10)
for (i in 1:9) {
  for (j in i+1:10-i) {
    if(i<j){
      temp <- cpt.locations.multi(decompseData$residual[1:576,c(i,j)],0.10)
      if(!is.null(temp)){
        cp10.576[i,j][[1]] <- temp
      }
    }
  }
}

cp01.144.3 <- array(list(),dim=c(10,10,10))
for (l in 1:9) {
  for (i in l+1:10-l) {
    if(l<i){
      for (j in i+1:10-i) {
        if(i<j){
          temp <- cpt.locations.multi(decompseData$residual[1:144,c(i,j,l)],0.01)
          if(!is.null(temp)){
            cp01.144.3[l,i,j][[1]] <- temp
          }
        }
      }
    }
  }
}

cp01.288.3 <- array(list(),dim=c(10,10,10))
for (l in 1:9) {
  for (i in l+1:10-l) {
    if(l<i){
      for (j in i+1:10-i) {
        if(i<j){
          temp <- cpt.locations.multi(decompseData$residual[1:288,c(i,j,l)],0.01)
          if(!is.null(temp)){
            cp01.288.3[l,i,j][[1]] <- temp
          }
        }
      }
    }
  }
}

cp01.576.3 <- array(list(),dim=c(10,10,10))
for (l in 1:9) {
  for (i in l+1:10-l) {
    if(l<i){
      for (j in i+1:10-i) {
        if(i<j){
          temp <- cpt.locations.multi(decompseData$residual[1:576,c(i,j,l)],0.01)
          if(!is.null(temp)){
            cp01.576.3[l,i,j][[1]] <- temp
          }
        }
      }
    }
  }
}

cp05.144.3 <- array(list(),dim=c(10,10,10))
for (l in 1:9) {
  for (i in l+1:10-l) {
    if(l<i){
      for (j in i+1:10-i) {
        if(i<j){
          temp <- cpt.locations.multi(decompseData$residual[1:144,c(i,j,l)],0.05)
          if(!is.null(temp)){
            cp05.144.3[l,i,j][[1]] <- temp
          }
        }
      }
    }
  }
}

cp05.288.3 <- array(list(),dim=c(10,10,10))
for (l in 1:9) {
  for (i in l+1:10-l) {
    if(l<i){
      for (j in i+1:10-i) {
        if(i<j){
          temp <- cpt.locations.multi(decompseData$residual[1:288,c(i,j,l)],0.05)
          if(!is.null(temp)){
            cp05.288.3[l,i,j][[1]] <- temp
          }
        }
      }
    }
  }
}

cp05.576.3 <- array(list(),dim=c(10,10,10))
for (l in 1:9) {
  for (i in l+1:10-l) {
    if(l<i){
      for (j in i+1:10-i) {
        if(i<j){
          temp <- cpt.locations.multi(decompseData$residual[1:576,c(i,j,l)],0.05)
          if(!is.null(temp)){
            cp05.576.3[l,i,j][[1]] <- temp
          }
        }
      }
    }
  }
}

cp01.144.4 <- array(list(),dim=c(10,10,10,10))
for (k in 1:9) {
  for (l in k+1:10-k) {
    if(k<l){
      for (i in l+1:10-l) {
        if(l<i){
          for (j in i+1:10-i) {
            if(i<j){
              temp <- cpt.locations.multi(decompseData$residual[1:144,c(i,j,l,k)],0.01)
              if(!is.null(temp)){
                cp01.144.4[k,l,i,j][[1]] <- temp
              }
            }
          }
        }
      }
    }
  }
}

cp01.288.4 <- array(list(),dim=c(10,10,10,10))
for (k in 1:9) {
  for (l in k+1:10-k) {
    if(k<l){
      for (i in l+1:10-l) {
        if(l<i){
          for (j in i+1:10-i) {
            if(i<j){
              temp <- cpt.locations.multi(decompseData$residual[1:288,c(i,j,l,k)],0.01)
              if(!is.null(temp)){
                cp01.288.4[k,l,i,j][[1]] <- temp
              }
            }
          }
        }
      }
    }
  }
}

cp01.576.4 <- array(list(),dim=c(10,10,10,10))
for (k in 1:9) {
  for (l in k+1:10-k) {
    if(k<l){
      for (i in l+1:10-l) {
        if(l<i){
          for (j in i+1:10-i) {
            if(i<j){
              temp <- cpt.locations.multi(decompseData$residual[1:576,c(i,j,l,k)],0.01)
              if(!is.null(temp)){
                cp01.576.4[k,l,i,j][[1]] <- temp
              }
            }
          }
        }
      }
    }
  }
}

cp05.144.4 <- array(list(),dim=c(10,10,10,10))
for (k in 1:9) {
  for (l in k+1:10-k) {
    if(k<l){
      for (i in l+1:10-l) {
        if(l<i){
          for (j in i+1:10-i) {
            if(i<j){
              temp <- cpt.locations.multi(decompseData$residual[1:144,c(i,j,l,k)],0.05)
              if(!is.null(temp)){
                cp05.144.4[k,l,i,j][[1]] <- temp
              }
            }
          }
        }
      }
    }
  }
}