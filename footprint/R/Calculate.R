Calculate <-
  function (fetch = 500, height = 3, grid = 200, speed, direction, 
            uStar, zol, sigmaV){ 

    grid = 2 * floor(grid/2)                                        # cria grid
    sigmaY = matrix(0, nrow = grid, ncol = grid)                    # matrix de 
    twopisigma = matrix(0, nrow = grid, ncol = grid)
    DispY = matrix(0, nrow = grid, ncol = grid)
    Pfp = matrix(0, nrow = grid, ncol = grid)
    Pf = matrix(0, nrow = grid, ncol = grid)
    auPlume = matrix(1, nrow = grid, ncol = grid)
    linn = seq(99.5, -99.5, length = grid)                    # cria um vetor de coordenadas/pesos norte
    line = seq(-99.5, 99.5, length = grid)                    # cria um vetor de coordenadas este
    linn = linn * 2 * fetch/199                               # normaliza o vetor de coordenadas norte aos limites do intervalo de estudos
    line = line * 2 * fetch/199                               # normaliza o vetor de coordenadas estea aos limites do intervalo de estudos
    m = length(line)                                          # retira os comprimentos do grid
    n = length(linn)
    FPe = matrix(rep(line, each = n), nrow = n)               # construi matrizes 2d com os vetores coordenadas  
    FPn = matrix(rep(linn, m), nrow = n)
    FPd = sqrt(FPn^2 + FPe^2)                                 # constroi uma matriz com as varia??esdas coordenadas
    FPa = atan2(FPe, FPn) * 180/pi
    FPa = FPa - direction
    FPx = cos(FPa/57) * FPd
    FPy = sin(FPa/57) * FPd
    zt = ifelse(zol > 0, 0, (1 - 16 * zol)^0.25)
    phim = ifelse(zol > 0, 1 + 5 * zol, (1 - 16 * zol)^-0.25)
    phic = ifelse(zol > 0, 1 + 5 * zol, (1 - 16 * zol)^-0.5)
    psim = ifelse(zol > 0, 5 * zol, -2 * log((1 + zt)/2) - log((1 + 
                                                                  zt * zt)/2) + 2 * atan(zt) - pi/2)
    nn = ifelse(zol > 0, 1/phic, (1 - 24 * zol)/(1 - 16 * zol))
    eddydif = 0.41 * uStar * height/phic
    mm = uStar * phim/(0.41 * speed)
    rr = 2 + mm - nn
    mu = (1 + mm)/rr
    alpu = speed/(height^mm)
    alpk = eddydif/(height^nn)
    xi = alpu * height^rr/(((rr)^2) * alpk)
    fgamma = gamma(mu)
    idup = FPx > 0
    Pf[idup] = (1/fgamma) * (xi^mu)/(FPx[idup]^(1 + mu)) * exp(-1 * 
                                                                 xi/FPx[idup])
    uPlume = (gamma(mu)/gamma(1/rr))
    uPlume = uPlume * ((rr * rr * 0.4/speed)^(mm/rr))
    auPlume[idup] = uPlume * (speed * (FPx[idup]^(mm/rr)))
    sigmaY[idup] = sigmaV * FPx[idup]/auPlume[idup]
    twopisigma[idup] = 1/((sqrt(2 * pi) * sigmaY[idup]))
    DispY[idup] = twopisigma[idup] * exp(-1 * FPy[idup] * FPy[idup]/(2 * 
                                                                       sigmaY[idup] * sigmaY[idup]))
    Pfp[idup] = Pf[idup] * DispY[idup]
    footprint = Pfp/sum(sum(Pfp))
    returnList = list(footprint = footprint, FPe = FPe, FPn = FPn)
    return(returnList)
  }