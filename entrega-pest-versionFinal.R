mensaje_cifrado<-'HTSVBPX QTIVBHRXIXBSX XLNTBVBGVBGREX HVSBSXBPXIQVYRXIHTBSXBLTILRXILRVBCBSXB XGRJRTIBXQHXBSX XLNTBRILGACXBGVBGREX HVSBSXBLVYERV BSXB XGRJRTIBTBSXBL XXILRVBVQRBLTYTBGVBGREX HVSBSXBYVIRWXQHV BQAB XGRJRTIBTBQABL XXILRVBRISRZRSAVGBCBLTGXLHRZVYXIHXBHVIHTBXIBPAEGRLTBLTYTBXIBP RZVSTBPT BGVBXIQXMVIDVBGVBP VLHRLVBXGBLAGHTBCBGVBTEQX ZVILRVBHTSTBRISRZRSATBHRXIXBSX XLNTBVBGVBGREX HVSBSXBTPRIRTIBCBSXBXKP XQRTIBXQHXBSX XLNTBRILGACXBXGBSXBITBQX BYTGXQHVSTBVBLVAQVBSXBQAQBTPRIRTIXQBXGBSXBRIZXQHRJV BCB XLRER BRIWT YVLRTIXQBCBTPRIRTIXQBCBXGBSXBSRWAISR GVQBQRIBGRYRHVLRTIBSXBW TIHX VQBPT BLAVGUARX BYXSRTBSXBXKP XQRTI'

long_palabras_espanol <- read.csv("C:/Users/iglez/OneDrive - UVa/4-CURSO/PEST/DecodificacionMCMC/long_palabras_espanol.txt", sep="")
bigramas_espanol <- read.csv("C:/Users/iglez/OneDrive - UVa/4-CURSO/PEST/DecodificacionMCMC/bigramas_espanol.txt", sep="", encoding="UTF-8")
trigramas_espanol <- read.csv("C:/Users/iglez/OneDrive - UVa/4-CURSO/PEST/DecodificacionMCMC/trigramas_espanol.txt", sep="", encoding="UTF-8")

#correcciones
bigramas_espanol$bigrama[28]<-"NA"



##############################################################
##############################################################

letra_a_numero<-function(a) match(a,clave) 
numero_a_letra<-function(n) clave[n]

clave<-c(toupper(letters)[1:14],"Ñ",toupper(letters)[15:26]," ")
clave_numeros<-letra_a_numero(clave)




mensaje_a_numero<-function(mensaje){
  mensajenumerado<-NULL
  for(i in 1:nchar(mensaje)){
    mensajenumerado<-c(mensajenumerado,letra_a_numero(substring(mensaje,i,i)))  
  }
  return(mensajenumerado)
}




mensaje_a_letra<-function(mensaje){
  mensajetexto<-NULL
  for(i in 1:length(mensaje)){
    mensajetexto<-paste(mensajetexto,numero_a_letra(mensaje[i]),sep='')  
  }
  return(mensajetexto)
}


bigramas_palabra <- function(palabra) {
  aaa<-unlist(strsplit(palabra,''))
  nnn<-length(aaa)
  bigramas<-paste0(aaa[1:(nnn-1)],aaa[2:nnn])
  return(bigramas)
}

trigramas_palabra <- function(palabra) {
  aaa<-unlist(strsplit(palabra,''))
  nnn<-length(aaa)
  trigramas<-paste0(aaa[1:(nnn-2)],aaa[2:(nnn-1)],aaa[3:nnn])
  return(trigramas)
}



decodificador<-function(mensaje,ordenacion){
  fun.auxiliar<-function(i) ordenacion[i]
  mensaje_decodificado<-NULL
  for (j in 1:length(mensaje)) mensaje_decodificado<-c(mensaje_decodificado,fun.auxiliar(mensaje[j]))
  return(mensaje_decodificado)
}




score1<-function(palabras){
  freq<-0
  f<-0
  for(i in palabras){
    long_palabra<-nchar(i)
    if(long_palabra %in% 1:nrow(long_palabras_espanol)){
      f<-log(long_palabras_espanol$frecuencia[long_palabra])
    }else{
      f<-log(10^-8)
    }
    freq<-freq+f
  }
  return(freq)
}

score2<-function(palabras){
  bigramas<-c()
  f<-0
  freq<-0
  for(palabra in palabras){
    if(nchar(palabra)>1){
      bigramas<-c(bigramas, bigramas_palabra(palabra))
    }
  }
  #aqui tendriamos todas las palabras del mensaje divididas en bigramas
  #ahora para cada bigrama tengo que ver si esta en la tabla
  for(i in bigramas){
    if(i %in% bigramas_espanol$bigrama){
      f<-log(bigramas_espanol$frecuencia[bigramas_espanol$bigrama==i])
    }else{
      f<-log(10^-8)
    }
    freq<-freq+f
  }
  return(freq)
}

score3<-function(palabras){
  trigramas<-c()
  f<-0
  freq<-0
  for(palabra in palabras){
    if(nchar(palabra)>2){
      trigramas<-c(trigramas, trigramas_palabra(palabra))
    }
  }
  #aqui tendriamos todas las palabras del mensaje divididas en trigramas
  #ahora para cada bigrama tengo que ver si esta en la tabla
  for(i in trigramas){
    if(i %in% trigramas_espanol$V1){
      f<-log(trigramas_espanol$V2[trigramas_espanol$V1==i])
    }else{
      f<-log(10^-8)
    }
    freq<-freq+f
  }
  return(freq)
}



# funcion de calcula el score
# score = score1^3 * score2 * score3^(0.5)
# estamos operando con logaritmos, aplicando las propiedades de los logaritmos
# tendremos la siguiente función
# 3*score1 + score2 + 0.5*score3
score<-function(mensaje){
  palabras<-unlist(strsplit(mensaje, split = " "))
  score_valor<-3*score1(palabras)+score2(palabras)+0.5*score3(palabras)
  return(score_valor)
}

##############################################################
##############################################################


scoreAnterior<-score(mensaje_cifrado) #esto es el score inicial del texto
cifradoNuevo<-sample(clave_numeros)
#cifradoNuevo<-clave_numeros para probar al inicio fijo A,B,C,... como inicial
for(iteration in 1:5000){
  
  swaps<-sample(1:28,2)
  cifradoAnterior<-cifradoNuevo
  cifradoNuevo[swaps[1]] <- cifradoAnterior[swaps[2]]
  cifradoNuevo[swaps[2]] <- cifradoAnterior[swaps[1]]
  #aqui cifradoNuevo tiene dos letras cambiadas
  
  #ponemos el mensaje cifrado como secuencia de numeros
  mensaje_cifrado_num<-mensaje_a_numero(mensaje_cifrado)
  #aplicamos la codificacion a ese mensaje
  texto_propuesto_num<-decodificador(mensaje_cifrado_num,cifradoNuevo)
  #transformamos ese mensaje de vuelta a letras
  texto_propuesto<-mensaje_a_letra(texto_propuesto_num)
  
  #ahora con este mensaje decodificado calculamos el nuevo score
  scoreNuevo<-score(texto_propuesto)
  
  #aceptacion
  if (runif(1) > exp(scoreNuevo-scoreAnterior)){
    cifradoNuevo <- cifradoAnterior
  } else {
    scoreAnterior <- scoreNuevo
  }
  
  #para ver cada 100 iteraciones
  if ((iteration %%  100) == 0){
    cat(paste0("\n\nIteracion = ",iteration,"\n",scoreNuevo,"\n",texto_propuesto,"\n"))
  }

}
