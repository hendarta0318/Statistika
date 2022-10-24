data = c(36,37,37,37,38,39,39,39,39,40,40,40,40,41,41,41,41,41,42,42,42,42,43,43,43,43,43,43,43,43,43,
         44,44,44,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,45,46,46,46,47,47,47,47,47,47,47,47,47,48,48,48,48,48,48,48,48,48,
         48,48,48,49,49,49,49,49,49,49,49,49,49,49,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,51,
         51,51,51,51,51,51,51,51,52,52,52,52,52,52,52,52,52,52,52,53,54,54,54,54,54,54,54,54,55,55,55,55,55,55,55,55,55,55,55,55,
         55,55,56,56,56,56,56,56,56,57,57,58,59,59,59,59,59,59,60,60,60,60,60,60,60,61,61,61,61,61,62,62,62,62,62,62,63,63,63,65,
         65,67,68,68,68,70,70,70,70)
sort(data)



n = length(data)
nmax = max(data)
nmin = min(data)
nmax
nmin
#menentukan jumlah kelas [K]
k = 1+(3.3*log10(n))
K= round(k)
K
#menetukan interval class [p]
p = (nmax-nmin)/K
p = ceiling(p)
p

frequensi = function(x,y,z){
  a =0
  for (i in 1:n){
    if(x[i]>=y&&x[i]<=z){
      a = a+1
    }
  }
  a
}
frek1= frequensi(data,36,40)
frek2= frequensi(data,41,45)
frek3= frequensi(data,46,50)
frek4= frequensi(data,51,55)
frek5= frequensi(data,56,60)
frek6= frequensi(data,61,65)
frek7= frequensi(data,66,70)
frek1
frek2
frek3
frek4
frek5
frek6
frek7
nilai_tengah <- list(38,43,48,53,58,63,68)

# mencari rata rata(mean)
rata= sum(data)/length(data)
rata

#mencari median data kelompok
median1= (36+40)/2
median2= (41+45)/2
median3= (46+50)/2
median4= (51+55)/2
median5= (56+60)/2
median6= (61 + 65)/2
median7= (66 + 70)/2
median6


#mencari Modus
tb <- 46 -0.5
d2 <- 60 - 43
d1 <- 60 - 37
p = 5
MODUS <- tb + (d1 / (d1+d2)) *p
MODUS

#mencai range
nilai_max = 68
nilai_min = 38
range = nilai_max - nilai_min
range

#simpangan rata-rata
fx1=frek1*((median1-rata)*-1)
fx2=frek2*((median2-rata)*-1)
fx3=frek3*((median3-rata)*-1)
fx4=frek4*(median4-rata)
fx5=frek5*(median5-rata)
fx6=frek6*(median6-rata)
fx7=frek7*(median7-rata)
fixi= fx1+fx2+fx3+fx4+fx5+fx6+fx7
fi=frek1+frek2+frek3+frek4+frek5+frek6+frek7
simpang.rata=fixi/fi
simpang.rata

#simpangan baku
fix1=frek1*((median1-rata)**2)
fix2=frek2*((median2-rata)**2)
fix3=frek3*((median3-rata)**2)
fix4=frek4*((median4-rata)**2)
fix5=frek5*((median5-rata)**2)
fix6=frek6*((median6-rata)**2)
fix7=frek7*((median7-rata)**2)
fixi2= fix1+fix2+fix3+fix4+fix5+fix6+fix7
simpang.baku = (fixi2/fi)**(1/2)
simpang.baku