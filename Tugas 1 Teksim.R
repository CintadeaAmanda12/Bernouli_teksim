#Nama: Cintadea Amanda Dwi Aryani
#Nim: B2A020027
#Kelas A

#Soalnimganjil
#Gunakan additive dan gunakan Bernouli_2
#zo=11123 a=35m=138 c=437 n=100 p=0.65

#Additive
Additive_RNG<-function(a,z0,c,m,n) {
  xi<-matrix(NA,n,3)
  colnames(xi)<-c("aZ(i-1)+c","Xi","Ui")
  for(i in 1:n)
  {
    xi[i,1]<-(a*z0+c)
    xi[i,2]<-xi[i,1]%%m
    xi[i,3]<-xi[i,2]/m
    z0<-xi[i,2]
  }
  hist(xi[,3])
  View(xi)
}
Additive_RNG(35,11123,437,138,100)

#Bernouli2
Bernouli_2<-function(n,p) {
  i<-n
  p<-p
  X<-runif(i)
  Y<-(X<=p)+0
  (tabel<-table(Y)/length(Y))
}
barplot(tabel,main="Bernoulli")
Bernouli_2(100,0.65)

dbWriteTable(xi, name = "hasil", value = Xi, row.names = FALSE, overwrite = TRUE)
dbListTables(xi)
