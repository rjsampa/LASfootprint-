Average_mod <-
function (fetch = 500, height = 3, grid = 200, speed, direction, 
    uStar, zol, sigmaV, weights = NULL) 
{
    grid = 2 * floor(grid/2)                                            # deixa o grid par
    lens = c(length(speed), length(direction), length(uStar),           # define o numero de observacoes de cada variavel
        length(zol), length(sigmaV))
    if (!is.null(weights)) {                                            # condicao para o caso de pesos
        lens = c(lens, length(weights))
    }
    if (min(lens) == max(lens)) {                                       # confere se todas variaveis tem o mesmo numero de observacoes
        ids = !is.na(speed)                                             # define uma mascara para os valores nodata
        idd = !is.na(direction)
        idu = !is.na(uStar)
        idz = !is.na(zol)
        idv = !is.na(sigmaV)
    }
    idok = ids & idd & idu & idz & idv                                  # mascara para todo conjunto de dados
    if (!is.null(weights)) {
        idw = !is.na(weights)
        idok = idok & idw
    }
    speed = speed[idok]                                                 # aplica mascara 
    direction = direction[idok]
    uStar = uStar[idok]
    zol = zol[idok]
    sigmaV = sigmaV[idok]
    if (!is.null(weights)) {
        weights = weights[idok]
    }
    if (length(speed) >= 1) {                                          # inicia calculo do footprint, vel com menos de 1 obs nao tem footprint
        avg_footprint = list(Probability = matrix(0, nrow = grid,      #  cria a matriz com
            ncol = grid))
        if (!is.null(weights)) {
            avg_footprint = list(Probability = matrix(0, nrow = grid,  #cria a matriz com as probabilidades para os pesos
                ncol = grid), WeightedProbability = matrix(0, 
                nrow = grid, ncol = grid))
        }
        finput = t(mapply(list, speed, direction, uStar, zol, 
            sigmaV))                                                    # vetor com inputs
        icnt = 0
        for (i in 1:nrow(finput)) {                                     # for para calculo do footprint para cada obs
            ftp <- Calculate(fetch, height, grid, finput[[i, 
                1]], finput[[i, 2]], finput[[i, 3]], finput[[i, 
                4]], finput[[i, 5]])
            if (any(!is.na(ftp$footprint))) {                               # soma da footprint de cada obs
                avg_footprint$Probability = avg_footprint$Probability +
                  ftp$footprint
                if (!is.null(weights)) {                                    # soma  dos pesos 
                  avg_footprint$WeightedProbability = avg_footprint$WeightedProbability + 
                    ftp$footprint * weights[i]
                }
            }
            icnt = icnt + 1                                               # contagem de n 
        }
    }
    avg_footprint$Probability = avg_footprint$Probability/icnt           # calculando o peso medio 
    returnList = list(Probability = avg_footprint$Probability,              
        FPe = ftp$FPe, FPn = ftp$FPn, WeightedProbability = avg_footprint$WeightedProbability)
    return(returnList)
}
