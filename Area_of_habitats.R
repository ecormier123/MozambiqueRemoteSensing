#count the frequency of each class type (in number of cells)
f<-freq(refcl)
#add a new column that calculates the total area of each class in km2
#I ended up with .9 because each cell is 30m*30m=900m2 = 0.09hectares
p<-data.frame(f, a=(f[,2]*.09))
write.csv(p, "C:/Users/cormi/Desktop/p.csv")

