#David Henson
#Homework 1

#Problem 1(a)
vecZ = c(sqrt((-2)*log(runif(n=1000, min=0, max=1)))*cos(2*pi*runif(n=1000, min=0, max=1)));

#Problem 1(b)

#elements less than 0
length(vecZ[vecZ < 0])/length(vecZ);

#elements between 0 and 1
length(vecZ[vecZ > 0 & vecZ < 1])/length(vecZ);

#elements greater than 1
length(vecZ[vecZ > 1])/length(vecZ);

#Problem 2

Bn = function(x,k,n){
  return (abs(sin((2*pi*k)/n))*choose(n,k)*(x^k)*((1-x)^(n-k)));
}

Sum_func = function(vec,x,k,n){
  if(k<=n){
    vec[k+1]=Bn(x,k,n);
    Sum_func(vec,x,k+1,n);
  }
  else{
    return(sum(vec,na.rm=FALSE));
  }
}

#evaluation at x=0.2, n=20
vec = c();
Sum_func(vec,0.2,0,20);

#Problem 3(a)
load(FrontRange.RData);

#finding station with max elevation
max_station = which.max(FR$info$elev);

#finding station with min elevation
min_station = which.min(FR$info$elev);

#plotting precipitation over time for max elevation station
plot(FR$time[[max_station]], FR$precip[[max_station]]);

#plotting precipitation over time for min elevation station
plot(FR$time[[min_station]], FR$precip[[min_station]]);

#Problem 3(b)
max_length = length(FR$precip[[max_station]]);
max_vec = FR$precip[[max_station]];
length(max_vec[max_vec>10])/max_length;

min_length = length(FR$precip[[min_station]]);
min_vec = FR$precip[[min_station]];
length(min_vec[min_vec>10])/min_length;

#Problem 3(c)
Max_Precip = function(station){
  max = 0;
  max_pos = 1;
  precip_vec = FR$precip[[station]];
  time_vec = FR$precip[[station]];
  for(i in 1:length(precip_vec)){
    if(precip_vec[i] > max){
      max = precip_vec[i];
      max_pos = i;
    }
  }
  return(time_vec[max_pos]);
}

#example evaluation at station 10
Max_Precip(10);

    }
  }
}
  }
  }
}