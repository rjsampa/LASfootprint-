LASftp<-function(ftpEddy,fetch = 500,distancia=10,A=5,B=5,C=2,D=2){
  a=2
  b=a+(dim(ftpEddy$Probability)[1]-1)
  c=2
  d=c+(dim(ftpEddy$Probability)[1]-1)
  
  vetor<-c(1:distancia)/distancia
  
  icm = matrix(0,dim(ftpEddy$Probability)[1]+distancia*5,
               dim(ftpEddy$Probability)[1]+distancia*5)
  
  for (i in 1:distancia){
    grade = matrix(0,dim(ftpEddy$Probability)[1]+distancia*5,
                   dim(ftpEddy$Probability)[1]+distancia*5)
    grade[a:b,c:d]=ftpEddy$Probability
    icm=icm+grade*peso(vetor[i])
    a=a+A
    b=b+B
    c=c+C
    d=d+D
  }
  linn = seq(99.5, -99.5, length = dim(icm)[1])                    # cria um vetor de coordenadas/pesos norte
  line = seq(-99.5, 99.5, length = dim(icm)[2])                    # cria um vetor de coordenadas este
  linn = linn * 2 * fetch/199                               # normaliza o vetor de coordenadas norte aos limites do intervalo de estudos
  line = line * 2 * fetch/199                               # normaliza o vetor de coordenadas estea aos limites do intervalo de estudos
  m = length(line)                                          # retira os comprimentos do grid
  n = length(linn)
  FPe = matrix(rep(line, each = n), nrow = n)               # construi matrizes 2d com os vetores coordenadas  
  FPn = matrix(rep(linn, m), nrow = n)
  
  returnList = list(LASfootprint = icm, FPe = FPe, FPn = FPn)
  return(returnList)
  #return(icm)
}