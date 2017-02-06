## Stats 406 Hw 1 Alex Krencicki 

#1) 
  #a) 

    U1= runif(1000,0,1) 
    U2= runif(1000,0,1)  
    Z=sqrt(-2*log(U1))*(cos(2*pi*U2)) 
    vectorZ=Z
    vectorZ
    hist(Z, main="Z") 
    hist(vectorZ)
        
    #b) 
    
    less0=(length(which(vectorZ<0)))/length(vectorZ) 
    
    between01=length((which(vectorZ>0 & vectorZ<1)))/length(vectorZ)
    
    above1=length(which(vectorZ>1))/length(vectorZ) 
    
    
#2) 
    vec.Bx=vector(mode="numeric", length=21) 
    x=.2 
    n=20 
    k=c(0:21) 
    
    vec.Bx=abs(sin(2*pi*k/n))*choose(n,k)*(x^k)*((1-x)^(n-k))
    sum(vec.Bx)
        
#3)
    load('FrontRange.RData') 
    
    #a) 
    
    head(FR$info)
    names(FR$info)
    
    ncol(FR$info)
    max.elev=max(FR$info$elev) # 10,260
    min.elev=min(FR$info$elev) # 4,650
    which(FR$info[,3]==max.elev) # station #36
    which(FR$info[,3]==min.elev) # station #27
    
    plot(FR$time[[27]],FR$precip[[27]], xlab="ith day", ylab="Precip Measured mm", main="Lowest Elevation Daily Rainfail")
    plot(FR$time[[36]],FR$precip[[36]], xlab="ith day", ylab="Precip Measured mm", main="Highest Elevation Daily Rainfail")    

    
    #b) 
    length(which(FR$precip[[27]]>=10))/length(FR$precip[[27]]) # 11%
    length(which(FR$precip[[36]]>=10))/length(FR$precip[[36]]) # 26% 
    
    #c) 
    #input desired station, say 10 
    station_num=10 
    
    max_precip=max(FR$precip[[station_num]]) 
    max_precip_id=which(FR$precip[[station_num]]==max_precip) 
    time.matrix=FR$time[[station_num]]
    max_precip_date=(time.matrix[max_precip_id])

  
    
    
    
    
    