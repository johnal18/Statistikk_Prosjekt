
data = read.csv("data/table-2.csv", header = TRUE)

oslo100 = data[3553:3653,]
stavanger100 = data[7199:7299,]

clean_row_and_id(oslo100, stavanger100)

levels(oslo100$nedbor) = gsub(",", ".", levels(oslo100$nedbor))
levels(stavanger100$nedbor) = gsub(",", ".", levels(stavanger100$nedbor))

oslo100$nedbor = as.numeric(levels(oslo100$nedbor)[as.numeric(oslo100$nedbor)])
stavanger100$nedbor = as.numeric(levels(stavanger100$nedbor)[as.numeric(stavanger100$nedbor)])

korrelasjon100 = find_correlation(oslo100$nedbor, stavanger100$nedbor, 101)

X100 = as.matrix(data.frame(1, oslo100$nedbor))
y100 = as.matrix(data.frame(stavanger100$nedbor))
X.t100 = as.matrix(t(data.frame(1, oslo100$nedbor)))
XtX100 = X.t100 %*% X100
Xty100 = X.t100 %*% y100
library(matlib)
XtXI100 = inv(XtX100)
Beta100 = XtXI100 %*% Xty100

y.t100 = as.matrix(t(as.data.frame(y100)))
Beta.t100 = as.matrix(t(as.data.frame(Beta100)))
SSe100 = (y.t100 %*% y100) - Beta.t100 %*% X.t100 %*% X100 %*% Beta100
SSe100 = SSe100[1,1]

v1100 = 99
s1100 = SSe100
sigma1100 = sqrt(s1100/99)
SSx100 = sum((oslo100$nedbor - mean(oslo100$nedbor))^2)

tx100 = c(100:290)/10000
t100 = dgamma(tx100,v1100/2, SSe100/2)
plot(tx100, t100, main = "Gammafordeling for støyen", xlab = "", ylab = "", col = "blue", type = "l")

library(metRology)

bx100 = c(-10:80)/100
b100 = dt.scaled(bx100, v1100, Beta100[2, 1], sigma1100*sqrt(1/SSx100))
plot(bx100, b100, main = "t-fordeling for stigningstallet b", xlab = "", ylab = "", col = "blue", type = "l")

y1001 = c(100:600)/100
yx100 = dt.scaled(y1001, v1100, Beta100[1,1] + Beta100[2, 1]*1, sigma1100*sqrt((1/101) + (1/SSx100)*(1-mean(oslo100$nedbor))^2))
plot(y1001, yx100, main = "t-fordeling for y(1) etter 100 observasjoner", xlab = "", ylab = "", col = "blue", type = "l")

#erstatt A med de x2 verdiene som viser fordelingen best
ypluss100 = dt.scaled(A, v1100, Beta100[1,1] + Beta100[2, 1]*x, sigma1100*sqrt(1 + (1/101) + (1/SSx100)*(x-mean(oslo$nedbor))^2))

#qt.scaled(0.05, 99, 0, -1) = 1.660391156
y1100 = Beta100[1, 1] + Beta100[2, 1]*x + 1.660391156*sigma1100*sqrt((1/101) + (1/SSx100)*(x - mean(oslo100$nedbor))^2)
y2100 = Beta100[1, 1] + Beta100[2, 1]*x - 1.660391156*sigma1100*sqrt((1/101) + (1/SSx100)*(x - mean(oslo100$nedbor))^2)
ypluss1100 = Beta100[1, 1] + Beta100[2, 1]*x + 1.660391156*sigma1100*sqrt(1 + (1/101) + (1/SSx100)*(x - mean(oslo100$nedbor))^2)
ypluss2100 = Beta100[1, 1] + Beta100[2, 1]*x - 1.660391156*sigma1100*sqrt(1 + (1/101) + (1/SSx100)*(x - mean(oslo100$nedbor))^2)

#plot(x = oslo100$nedbor, y = stavanger100$nedbor, type = "p", main = "Punkter for lineær regresjon",
#     xlab = "Oslo", ylab = "Stavanger", col = "blue")

plot(x, y1100, type = "l", main = "Kredibilitetsintervall med 101 og 3646 observasjoner",
     xlab = "Oslo", ylab = "Stavanger", col = "blue")
#lines(x, Beta100[1, 1] + Beta100[2, 1]*x, col = "green")
lines(x, y1100, col = "red")
lines(x, y2100, col = "red")
lines(x, y1, col = "purple")
lines(x, y2, col = "purple")
lines(x, ypluss1100, col = "green")
lines(x, ypluss2100, col = "green")


