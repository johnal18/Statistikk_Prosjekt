
data = read.csv("data/table-2.csv", header = TRUE)

oslo = data[1:3653,]
stavanger = data[3654:7299,]

oslo = oslo[-c(883, 2970:2975),] # trekker fra de dagene som finnes i Oslo-datasettet, men ikke i Stavanger-datasettet

clean_row_and_id(oslo, stavanger)

levels(oslo$nedbor) = gsub(",", ".", levels(oslo$nedbor))
levels(stavanger$nedbor) = gsub(",", ".", levels(stavanger$nedbor))

oslo$nedbor = as.numeric(levels(oslo$nedbor)[as.numeric(oslo$nedbor)])
stavanger$nedbor = as.numeric(levels(stavanger$nedbor)[as.numeric(stavanger$nedbor)])

korrelasjon = find_correlation(oslo$nedbor, stavanger$nedbor, 3645)

plot(oslo$nedbor, stavanger$nedbor, xlim=c(0,10), ylim=c(0,10), main = "Linær regresjon", xlab = "Oslo", ylab = "Stavanger", col = "blue")
plot(x, Beta[1, 1] + Beta[2, 1]*x, xlim=c(0,30), ylim=c(0,30), col = "red", main = "Lineær regresjon", type = "l",xlab = "Oslo", ylab = "Stavanger")
lines(x, Beta[1, 1] + Beta[2, 1]*x, col = "red")
plot(oslo$nedbor, stavanger$nedbor, main = "Linær regresjon", xlab = "Oslo", ylab = "Stavanger", col = "blue", log="xy")

X = as.matrix(data.frame(1, oslo$nedbor))
y = as.matrix(data.frame(stavanger$nedbor))
X.t = as.matrix(t(data.frame(1, oslo$nedbor)))
XtX = X.t %*% X
Xty = X.t %*% y
library(matlib)
XtXI = inv(XtX)
Beta = XtXI %*% Xty

#x verdi for å plotte linjene
x = c(0:90)
lines(x, Beta[1,1] + Beta[2,1]*x, col = "red")

y.t = as.matrix(t(as.data.frame(y)))
Beta.t = as.matrix(t(as.data.frame(Beta)))
SSe = (y.t %*% y) - Beta.t %*% X.t %*% X %*% Beta
SSe = SSe[1,1]

v1 = 3644
s1 = SSe
sigma1 = sqrt(s1/v1)
SSx = sum((oslo$nedbor - mean(oslo$nedbor))^2)

tx = c(2000:2800)/100000
t = dgamma(tx, 1822, SSe/2)
plot(tx, t, main = "Gamma-fordeling for støyen", xlab = "", ylab = "", col = "blue", type = "l")

library(metRology)

#Plot for stigningstallet
bx = c(250:500)/1000
b = dt.scaled(bx, v1, Beta[2, 1], sigma1*sqrt(1/SSx))
plot(bx, b, main = "t-fordeling for stigningstallet b", xlab = "", ylab = "", col = "blue", type = "l")

#Plot for regresjonslinja med x=1
x1 = c(3000:4000)/1000
yx1 = dt.scaled(x1, v1, Beta[1,1] + Beta[2, 1]*1, sigma1*sqrt((1/3646) + (1/SSx)*(1-mean(oslo$nedbor))^2))
plot(x1, yx1, main = "t-fordeling for y(1)", xlab = "", ylab = "", col = "blue", type = "l")

#Plot for regresjonslinja med x=20
x20 = c(9000:12000)/1000
yx20 = dt.scaled(x20, v1, Beta[1,1] + Beta[2, 1]*20, sigma1*sqrt((1/3646) + (1/SSx)*(20-mean(oslo$nedbor))^2))
plot(x20, yx20, main = "t-fordeling for y(20)", xlab = "", ylab = "", col = "blue", type = "l")

#Plot for regresjonslinja med x=80
x80 = c(28000:37000)/1000
yx80 = dt.scaled(x80, v1, Beta[1,1] + Beta[2, 1]*80, sigma1*sqrt((1/3646) + (1/SSx)*(80-mean(oslo$nedbor))^2))
plot(x80, yx80, main = "t-fordeling for y(80)", xlab = "", ylab = "", col = "blue", type = "l")

#Plot for neste observasjon med x=1
yplussx = c(-20000:25000)/1000
ypluss = dt.scaled(yplussx, v1, Beta[1,1] + Beta[2, 1]*1, sigma1*sqrt(1 + (1/3646) + (1/SSx)*(1-mean(oslo$nedbor))^2))
plot(yplussx, ypluss, main = "t-fordeling for Y+(1)", xlab = "", ylab = "", col = "blue", type = "l")

#Plot for neste observasjon med x=80
yplussx80 = c(10000:55000)/1000
ypluss80 = dt.scaled(yplussx80, v1, Beta[1,1] + Beta[2, 1]*80, sigma1*sqrt(1 + (1/3646) + (1/SSx)*(80-mean(oslo$nedbor))^2))
plot(yplussx80, ypluss80, main = "t-fordeling for Y+(80)", xlab = "", ylab = "", col = "blue", type = "l")

#linjene for kredibilitetsintervall for y(x)
#qt.scaled(0.05, 3644, 0, -1) = 1.645272
y1 = Beta[1, 1] + Beta[2, 1]*x + 1.645272*sigma1*sqrt((1/3646) + (1/SSx)*(x - mean(oslo$nedbor))^2)
y2 = Beta[1, 1] + Beta[2, 1]*x - 1.645272*sigma1*sqrt((1/3646) + (1/SSx)*(x - mean(oslo$nedbor))^2)

#linjene for kredibilitetsingervall for Y+(x)
ypluss1 = Beta[1, 1] + Beta[2, 1]*x + 1.645272*sigma1*sqrt(1 + (1/3646) + (1/SSx)*(x - mean(oslo$nedbor))^2)
ypluss2 = Beta[1, 1] + Beta[2, 1]*x - 1.645272*sigma1*sqrt(1 + (1/3646) + (1/SSx)*(x - mean(oslo$nedbor))^2)

plot(x, Beta[1, 1] + Beta[2, 1]*x, type = "l", main = "90% kredibilitetsintervall for 3646 observasjoner \n med kredibilitetsintervall for neste observasjon", xlab = "Oslo", ylab = "Stavanger", col = "blue")
lines(x, y1, col = "red")
lines(x, y2, col = "red")
lines(x, ypluss1, col = "green")
lines(x, ypluss2, col = "green")


