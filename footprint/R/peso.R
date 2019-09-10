peso<-function(x){
  y<-sqrt((2.283*pi*(x-0.5))^2)
  if(y==0){j1=1}
  else{j1<-(2*besselJ(y,1)/y)^2}
  
  return(j1)}

