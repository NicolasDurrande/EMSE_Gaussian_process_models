library(tikzDevice)

notes_abs = c(12,0,
11.5,1,
7.5,1,
9.5,0,
9,0,
9.5,0,
7,1,
13.5,1,
9.5,2,
10,0,
13,0,
14.5,0,
6.5,3,
12.5,0,
8.5,0,
15.5,0,
11,0,
14.5,0,
12,0,
12.5,0,
14.5,0,
8.5,0,
12,0,
12.5,0,
8,3,
8.5,1,
8,1)


X = matrix(notes_abs,ncol=2,byrow=TRUE)

fileName <- 'notes_vs_abs.tex'
tikz(fileName, standAlone = TRUE, width=8, height=5)
par(mar=c(4.5,4.5,1.5,1.5),cex.axis=1.5,cex.lab=2)
plot(X[,2],X[,1], xlab="nb absence", ylab="exam mark",xaxp = c(0, 3, 3),ylim=c(5,17))
dev.off()
tools::texi2dvi(fileName,pdf=T,clean=TRUE)
file.remove(fileName)

