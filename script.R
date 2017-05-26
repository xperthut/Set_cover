##################################################
# Name: Md. Kamruzzaman
# WSU: 11481693
# Project 2
# Math 567
##################################################

##################################################
### Greedy function
##################################################
Greedy=function(matrix.data, fold, allowPreprocess=F){
  if(is.matrix(matrix.data)){
    d.adj.mat = matrix.data
    d.mat = matrix.data
    d.mat.tr = t(d.mat)
    
    # Create unit vector
    v = as.matrix(rep(1,ncol(d.mat)))
    
    # Soluition matrix
    solution.mat = matrix(0,nrow=0,ncol=ncol(d.mat))
    
    # Number of meters covered by a pole
    meter.per.pole = d.mat%*%v
    
    # Number of poles covered by a meter
    pole.per.meter = d.mat.tr%*%v
    
    # Start process
    st = Sys.time()
    
    # Preprocess controller
    preprocess=T
    
    while(sum(meter.per.pole) > 0){
      
      # Perform only once
      if(allowPreprocess & preprocess){
        preprocess=F
        ## Preprocessing steps
        # Step 1: remove singleton rows – rows that have a one in a single column, 
        # select the facility in question (corresponding to the column), and remove all rows 
        # covered by this selected column;
        
        if(length(which(pole.per.meter>0))>0){
          a = pole.per.meter[which(pole.per.meter>0)]
          if(min(a)==1){
            meter.index =which(pole.per.meter==1)
            
            for(i in meter.index){
              pole.index = which(d.mat[,i]>0)
              
              # Remove the pole and meters covered by this pole from matrix
              m.index = which(d.mat[pole.index,]>0)
              
              for(j in m.index){
                pIndex = which(d.mat[,j]>0)
                meter.per.pole[pIndex] = meter.per.pole[pIndex]-1
              }
              
              d.mat[,m.index]=0
              d.mat.tr[m.index,]=0
              pole.per.meter[m.index] = 0
              
              solution.mat = rbind(solution.mat, d.adj.mat[pole.index,])
            }
            
          }
          
        }
        
        # Step 2: remove rows that contain row j
        for(i in 1:nrow(d.mat.tr)){
          if(sum(d.mat.tr[i,])>0){
            mpp.i = which(d.mat.tr[i,]>0)
            ms.i = d.mat.tr[i,mpp.i]
            
            ind = mpp.i[1]
            m.ind = which(d.mat.tr[,ind]>0)
            
            for(j in m.ind){
              if(i!=j){
                ms = d.mat.tr[j,mpp.i]
                
                v1 = abs(ms-ms.i)
                
                if(sum(v1)==0){
                  
                  pIndex = which(d.mat[,j]>0)
                  meter.per.pole[pIndex] = meter.per.pole[pIndex]-1
                  
                  d.mat[,j]=0
                  d.mat.tr[j,]=0
                  pole.per.meter[j] = 0
                  
                }
              }
            }
          }
        }
        
        # Step 3: remove column i if there is another column that contains it.
        for(i in 1:nrow(d.mat)){
          if(sum(d.mat[i,])>0){
            mpp.i = which(d.mat[i,]>0)
            ms.i = d.mat[i,mpp.i]
            
            ind = mpp.i[1]
            p.ind = which(d.mat[,ind]>0)
            
            for(j in p.ind){
              if(i!=j){
                ms = d.mat[j,mpp.i]
                
                v1 = abs(ms-ms.i)
                
                if(sum(v1)==0){
                  d.mat[i,]=0
                  meter.per.pole[i] = 0
                  
                  mIndex = which(d.mat.tr[,i]>0)
                  pole.per.meter[mIndex] = pole.per.meter[mIndex]-1
                  d.mat.tr[,i]=0
                  
                  break
                } 
              }
            } 
          }
        }
        
      }
      
      if(sum(meter.per.pole)==0){
        break
      }
      
      # Find pole that is connected with maximum number of meters
      pole.of.max.meters = which(meter.per.pole==max(meter.per.pole))
      
      # Pick one pole
      pole.has.max.score = pole.of.max.meters[1]
      
      # add to final pole list
      solution.mat = rbind(solution.mat, d.adj.mat[pole.has.max.score,])
      
      mIndex = which(d.mat[pole.has.max.score,]>0)
      for(i in mIndex){
        pIndex = which(d.mat[,i]>0)
        meter.per.pole[pIndex] = meter.per.pole[pIndex]-1
      }
      
      # Remove the pole and meters covered by this pole from matrix
      d.mat[,which(d.mat[pole.has.max.score,]>0)]=0
      d.mat.tr[which(d.mat[pole.has.max.score,]>0),]=0
      pole.per.meter[which(d.mat[pole.has.max.score,]>0)]=0
      
      # cleanup
      if(nrow(solution.mat)%%fold == 0){
        solution.mat = clean_up(solution.mat)
        preprocess = T
      }
      
    }
    
    # Final cleanup
    solution.mat = clean_up(solution.mat)
    
    # End process
    et = Sys.time()
    t.diff=difftime(et, st, units = c("mins"))
    
    # Find all covered poles
    meter.per.pole = solution.mat%*%v
    meter.per.pole=sort(meter.per.pole, decreasing = T)
    pole = which(meter.per.pole>0)
    mc=0
    meter=c()
    for(i in pole){
      mc = mc + meter.per.pole[i]
      meter = c(meter, mc)
    }

    obj=list()
    obj$Time = t.diff
    obj$Poles = length(meter)
    obj$meter = meter
    
    return(obj)
  }
  
  return(NULL)
}

##################################################
### Modified greedy function
##################################################
Modified.greedy=function(matrix.data, fold, allowPreprocess=F, h=1, eps=0){
  if(is.matrix(matrix.data)){
    d.adj.mat = matrix.data
    d.mat = matrix.data
    d.mat.tr = t(d.mat)
    
    # Create unit vector
    v = as.matrix(rep(1,ncol(d.mat)))
    
    # Soluition matrix
    solution.mat = matrix(0,nrow=0,ncol=ncol(d.mat))
    
    # Number of meters covered by a pole
    meter.per.pole = d.mat%*%v
    
    # Number of poles covered by a meter
    pole.per.meter = d.mat.tr%*%v
    
    # Preprocess controller
    preprocess = T
    
    # Hard to cover
    k = h
    
    # Start process
    st = Sys.time()
    
    while(sum(meter.per.pole)>0){
      
      # Perform only once
      if(allowPreprocess & preprocess){
        preprocess=F
        ## Preprocessing steps
        # Step 1: remove singleton rows – rows that have a one in a single column, 
        # select the facility in question (corresponding to the column), and remove all rows 
        # covered by this selected column;
        
        if(length(which(pole.per.meter>0))>0){
          a = pole.per.meter[which(pole.per.meter>0)]
          if(min(a)==1){
            meter.index =which(pole.per.meter==1)
            
            for(i in meter.index){
              pole.index = which(d.mat[,i]>0)
              
              # Remove the pole and meters covered by this pole from matrix
              m.index = which(d.mat[pole.index,]>0)
              
              for(j in m.index){
                pIndex = which(d.mat[,j]>0)
                meter.per.pole[pIndex] = meter.per.pole[pIndex]-1
              }
              
              d.mat[,m.index]=0
              d.mat.tr[m.index,]=0
              pole.per.meter[m.index] = 0
              
              solution.mat = rbind(solution.mat, d.adj.mat[pole.index,])
              
            }
            
          }
          
        }
        
        # Step 2: remove rows that contain row j
        for(i in 1:nrow(d.mat.tr)){
          if(sum(d.mat.tr[i,])>0){
            mpp.i = which(d.mat.tr[i,]>0)
            ms.i = d.mat.tr[i,mpp.i]
            
            ind = mpp.i[1]
            m.ind = which(d.mat.tr[,ind]>0)
            
            for(j in m.ind){
              if(i!=j){
                ms = d.mat.tr[j,mpp.i]
                
                v1 = abs(ms-ms.i)
                
                if(sum(v1)==0){
                  
                  pIndex = which(d.mat[,j]>0)
                  meter.per.pole[pIndex] = meter.per.pole[pIndex]-1
                  
                  d.mat[,j]=0
                  d.mat.tr[j,]=0
                  pole.per.meter[j] = 0
                  
                }
              }
            }
          }
        }
        
        # Step 3: remove column i if there is another column that contains it.
        for(i in 1:nrow(d.mat)){
          if(sum(d.mat[i,])>0){
            mpp.i = which(d.mat[i,]>0)
            ms.i = d.mat[i,mpp.i]
            
            ind = mpp.i[1]
            p.ind = which(d.mat[,ind]>0)
            
            for(j in p.ind){
              if(i!=j){
                ms = d.mat[j,mpp.i]
                
                v1 = abs(ms-ms.i)
                
                if(sum(v1)==0){
                  d.mat[i,]=0
                  meter.per.pole[i] = 0
                  
                  mIndex = which(d.mat.tr[,i]>0)
                  pole.per.meter[mIndex] = pole.per.meter[mIndex]-1
                  d.mat.tr[,i]=0
                  
                  break
                } 
              }
            } 
          }
        }
        
      }
      
      if(sum(meter.per.pole)==0){
        break
      }
      
      
      # Find mp
      mp = min(pole.per.meter[which(pole.per.meter>0)])
      
      # Compute score(j,inf)
      score.pole = meter.per.pole
      
      for(t in 1:k){
        hard.to.cover.meter.indice = which(pole.per.meter>0&pole.per.meter<mp+t)
        
        # Create column vector where 1 is on the hard to cover meter indices only
        col.v = as.matrix(rep(0,nrow(d.adj.mat)))
        col.v[hard.to.cover.meter.indice]=1
        
        # Get poles which covers hard.to.cover meters
        pole.cover.hard.to.cover.meters = d.mat%*%col.v
        pole.cover.hard.to.cover.meters.indices = which(pole.cover.hard.to.cover.meters>0)
        
        # Reset score to zero which is not covered by t hard to cover meters
        score.pole[-c(pole.cover.hard.to.cover.meters.indices)]=0
        
        # Adjust score
        for(j in pole.cover.hard.to.cover.meters.indices){
          score.pole[j] = score.pole[j]*(pole.cover.hard.to.cover.meters[j]^(1/t))
        }
      }
      
      # Find score range based on epsilon
      max.score = max(score.pole)
      score.min = max.score-eps;
      score.max = max.score+eps;
      
      # Select all poles those scores are in the score range
      poles.have.max.score = which(score.pole>=score.min&score.pole<=score.max)
      
      # Select first pole from list
      pole.has.max.score = poles.have.max.score[1]
      
      # add to final pole list
      solution.mat = rbind(solution.mat, d.adj.mat[pole.has.max.score,])
      
      # Remove the pole and meters covered by this pole from matrix
      meters.covered.by.pole.has.max.score = d.mat[pole.has.max.score,]
      meters.covered.by.pole.has.max.score.indices = which(meters.covered.by.pole.has.max.score>0)
      
      mIndex = meters.covered.by.pole.has.max.score.indices
      for(i in mIndex){
        pIndex = which(d.mat[,i]>0)
        meter.per.pole[pIndex] = meter.per.pole[pIndex]-1
      }
      
      d.mat[,meters.covered.by.pole.has.max.score.indices]=0
      d.mat.tr[meters.covered.by.pole.has.max.score.indices,]=0
      pole.per.meter[meters.covered.by.pole.has.max.score.indices]=0
      
      if(nrow(solution.mat)%%fold == 0){
        solution.mat = clean_up(solution.mat)
        preprocess = T
      }
      
    }
    
    # Final cleanup
    solution.mat = clean_up(solution.mat)
    
    # End process
    et = Sys.time()
    t.diff=difftime(et, st, units = c("mins"))
    
    # Find all covered poles
    meter.per.pole = solution.mat%*%v
    meter.per.pole=sort(meter.per.pole, decreasing = T)
    pole = which(meter.per.pole>0)
    mc=0
    meter=c()
    for(i in pole){
      mc = mc + meter.per.pole[i]
      meter = c(meter, mc)
    }
    
    obj=list()
    obj$Time = t.diff
    obj$Poles = length(meter)
    obj$meter = meter
    
    return(obj)
  }
  
  return(NULL)
  
}

##################################################
### Cleanup function
##################################################
clean_up=function(s.mat){
  
  if(nrow(s.mat)>1){
    # unit vector
    v = as.matrix(rep(1,nrow(s.mat)))
    
    # transpose
    s.mat.tr = t(s.mat)
    
    # poles per meter
    ppm = s.mat.tr%*%v
    
    # Meters per pole
    v = as.matrix(rep(1,ncol(s.mat)))
    mpp = s.mat%*%v
    
    # Create matrix of pole and number of meters
    #pm = data.frame("pole"=c(1:nrow(s.mat)), "total.meters"=mpp)
    
    # Sort based on total.meters
    #pm = data.frame(pm[order(pm[,2]),])
    
    #for(i in 1:nrow(pm)){
    for(i in 1:nrow(s.mat)){
      
      # pole index 
      #p.i = pm[i,1]
      
      # get meter index of pole pm[i,1]
      meter.index=which(s.mat[i,]>0)
      meter.of.pole = s.mat[i,meter.index]
      
      # Subtract meters of pole from ppm and check the meter.index
      tempV = ppm[meter.index]-meter.of.pole
      tempV.index = which(tempV>0)
      
      # Reduce the number of meter per column if it shares by multiple poles
      if(length(tempV.index)>0){
        s.mat[i,meter.index[tempV.index]]=0
        ppm[meter.index[tempV.index]] = ppm[meter.index[tempV.index]]-1
      }
    }
    
    # remove all pols  which does not contains any meter
    v = as.matrix(rep(1,ncol(s.mat)))
    mpp = s.mat%*%v
    z = which(mpp==0)
    
    if(length(z)>0){
      s.mat[-z,]
    }
    
  }
  
  return(s.mat)
}



