
#Korelasjon for nedbørsmengde i Oslo dagen etter nedbørsmengden i Stavanger
oslo_dagen_etter = oslo[2:3646,]
stavanger_dagen_for = stavanger[1:3645,]
clean_row_and_id(oslo_dagen_etter, stavanger_dagen_for)
korrelasjon2 = find_correlation(oslo_dagen_etter$nedbor, stavanger_dagen_for$nedbor, 3644)
#remove(oslo_dagen_etter, stavanger_dagen_for, korrelasjon2)
plot(oslo_dagen_etter$nedbor ~ stavanger_dagen_for$nedbor, main = "Punkter for nedbør i Oslo og Stavanger", xlab = "Stavanger", ylab = "Oslo", col="blue", log="xy")


#Korrelasjon for nedbørsmengde i Stavanger dagen etter Oslo
oslo_dagen_for = oslo[1:3645,]
stavanger_dagen_etter = stavanger[2:3646,]
clean_row_and_id(oslo_dagen_for, stavanger_dagen_etter)
korrelasjon3 = find_correlation(oslo_dagen_for$nedbor, stavanger_dagen_etter$nedbor, 3644)
#remove(oslo_dagen_for, stavanger_dagen_etter, korrelasjon3)
plot(oslo_dagen_for$nedbor ~ stavanger_dagen_etter$nedbor, main = "Punkter for nedbør i Oslo og Stavanger", xlab = "Stavanger", ylab = "Oslo", col="blue", log="xy")


#Plotting y ~ x, with logarithmic scale
plot(stavanger$nedbor ~ oslo$nedbor, main = "Punkter for nedbør i Oslo og Stavanger", xlab = "Oslo", ylab = "Stavanger", col="blue", log="xy")

mean(stavanger$nedbor)
mean(oslo$nedbor)


