
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

X = as.matrix(data.frame(1, oslo$nedbor))
y = as.matrix(data.frame(stavanger$nedbor))
X.t = as.matrix(t(data.frame(1, oslo$nedbor)))
XtX = X.t %*% X
Xty = X.t %*% y
library(matlib)
XtXI = inv(XtX)
Beta = XtXI %*% Xty

y.t = as.matrix(t(as.data.frame(y)))
Beta.t = as.matrix(t(as.data.frame(Beta)))
SSe = (y.t %*% y) - Beta.t %*% X.t %*% X %*% Beta
SSe = SSe[1,1]

v1 = 3644
s1 = SSe
sigma1 = sqrt(s1/3644)
SSx = sum((oslo$nedbor - mean(oslo$nedbor))^2)

t = dgamma(A, 1822, SSe/2)

x = c(0:90)

library(metRology)

b = dt.scaled(bx, v1, Beta[2, 1], sigma1*sqrt(1/SSx))
plot(bx, b, main = "t-fordeling for stigningstallet b", xlab = "", ylab = "", col = "blue", type = "h")

yx = dt.scaled(A, v1, Beta[1,1] + Beta[2, 1]*x, sigma1*sqrt((1/3646) + (1/SSx)*(x-mean(x))^2))

ypluss = dt.scaled(A, v1, Beta[1,1] + Beta[2, 1]*x, sigma1*sqrt(1 + (1/3646) + (1/SSx)*(x-mean(x))^2))

#qt.scaled(0.05, 3644, 0, -1)
y1 = Beta[1, 1] + Beta[2, 1]*x + 1.645272*sigma1*sqrt((1/3646) + (1/SSx)*(x - mean(x))^2)
y2 = Beta[1, 1] + Beta[2, 1]*x - 1.645272*sigma1*sqrt((1/3646) + (1/SSx)*(x - mean(x))^2)

plot(x, Beta[1, 1] + Beta[2, 1]*x, type = "l", main = "90% intervallestimat for 3646 observasjoner", xlab = "Oslo", ylab = "Stavanger", col = "blue")
lines(x, y1, col = "red")
lines(x, y2, col = "red")


