## Segmented regression for assessing lockdown strategies for Chilean data using
## function 'segmented19' by Vito Muggeo (vito.muggeo@unipa.it)
## Reference: Muggeo VMR, Sottile G, Porcu M. (2020) Modelling COVID-19 outbreak:
## segmented regression to assess lockdown effectiveness (on ResearchGate)
## doi: 10.13140/RG.2.2.32798.28485

## downloaded from https://github.com/MinCiencia/Datos-COVID19/tree/master/output/producto3
chile <- read.csv(url("https://raw.githubusercontent.com/MinCiencia/Datos-COVID19/master/output/producto3/CasosTotalesCumulativo_T.csv"))

dbnames <- c("Fecha", "Arica.y.Parinacota", "Tarapaca", "Antofagasta", "Atacama", "Coquimbo", "Valparaiso", "Metropolitana",
  "O.Higgins", "Maule", "Nuble", "Biobio", "Araucania", "Los.Rios", "Los.Lagos", "Aysen", "Magallanes", "Total")
names(chile) <- dbnames
npsi <- 1:15 # number of 'periods'
hundred <- ">=100" # more than 100 cases
zero <- ">=0" # more than 0 cases

## loading required package
library(segmented)
## reading sources
source("segmented19.R")

now <- proc.time()

## fitting models for each 'region'
y <- chile$Total
attr(y, "first.date") <- as.character(chile$Fecha[1])
oChile <- segmented19(y, npsi = npsi, rule = hundred)

y <- chile$Arica.y.Parinacota
attr(y, "first.date") <- as.character(chile$Fecha[1])
oArica <- segmented19(y, npsi = npsi, rule = zero) # >= 0

y <- chile$Tarapaca
attr(y, "first.date") <- as.character(chile$Fecha[1])
oTarapaca <- segmented19(y, npsi = npsi, rule = hundred)

y <- chile$Antofagasta
attr(y, "first.date") <- as.character(chile$Fecha[1])
oAntofagasta <- segmented19(y, npsi = npsi, rule = hundred)

y <- chile$Atacama
attr(y, "first.date") <- as.character(chile$Fecha[1])
oAtacama <- segmented19(y, npsi = npsi, rule = zero) # >= 0

y <- chile$Coquimbo
attr(y, "first.date") <- as.character(chile$Fecha[1])
oCoquimbo <- segmented19(y, npsi = npsi, rule = zero) # >= 0

y <- chile$Valparaiso
attr(y, "first.date") <- as.character(chile$Fecha[1])
oValpo <- segmented19(y, npsi = npsi, rule = hundred)

y <- chile$Metropolitana
attr(y, "first.date") <- as.character(chile$Fecha[1])
oStgo <- segmented19(y, npsi = npsi, rule = hundred)

y <- chile$O.Higgins
attr(y, "first.date") <- as.character(chile$Fecha[1])
oOHiggins <- segmented19(y, npsi = npsi, rule = zero) # >= 0

y <- chile$Maule
attr(y, "first.date") <- as.character(chile$Fecha[1])
oMaule <- segmented19(y, npsi = npsi, rule = hundred)

y <- chile$Nuble
attr(y, "first.date") <- as.character(chile$Fecha[1])
oNuble <- segmented19(y, npsi = npsi, rule = hundred)

y <- chile$Biobio
attr(y, "first.date") <- as.character(chile$Fecha[1])
oBiobio <- segmented19(y, npsi = npsi, rule = hundred)

y <- chile$Araucania
attr(y, "first.date") <- as.character(chile$Fecha[1])
oAraucania <- segmented19(y, npsi = npsi, rule = hundred)

y <- chile$Los.Rios
attr(y, "first.date") <- as.character(chile$Fecha[1])
oLosRios <- segmented19(y, npsi = npsi, rule = zero) # >= 0

y <- chile$Los.Lagos
attr(y, "first.date") <- as.character(chile$Fecha[1])
oLosLagos <- segmented19(y, npsi = npsi, rule = hundred)

y <- chile$Aysen
attr(y, "first.date") <- as.character(chile$Fecha[1])
oAysen <- segmented19(y, npsi = npsi, rule = zero) # >= 0

y <- chile$Magallanes
attr(y, "first.date") <- as.character(chile$Fecha[1])
oMagallanes <- segmented19(y, npsi = npsi, rule = hundred)

speed <- proc.time() - now
rm(now, y) # cleaning

## plots (saved in PDF format)
pdf(file = "plot_Chile.pdf")
par(pty = "s")
plot(oChile, main = "Chile", col = c(2,4))
dev.off()

pdf(file = "plot_Arica.pdf")
par(pty = "s")
plot(oArica, main = "Arica y Parinacota", col = c(2,4))
dev.off()

pdf(file = "plot_Tarapaca.pdf")
par(pty = "s")
plot(oTarapaca, main = "Tarapacá", col = c(2,4))
dev.off()

pdf(file = "plot_Antofagasta.pdf")
par(pty = "s")
plot(oAntofagasta, main = "Antofagasta", col = c(2,4))
dev.off()

pdf(file = "plot_Atacama.pdf")
par(pty = "s")
plot(oAtacama, main = "Atacama", col = c(2,4))
dev.off()

pdf(file = "plot_Coquimbo.pdf")
par(pty = "s")
plot(oCoquimbo, main = "Coquimbo", col = c(2,4))
dev.off()

pdf(file = "plot_Valpo.pdf")
par(pty = "s")
plot(oValpo, main = "Valparaíso", col = c(2,4))
dev.off()

pdf(file = "plot_Stgo.pdf")
par(pty = "s")
plot(oStgo, main = "Santiago", col = c(2,4))
dev.off()

pdf(file = "plot_OHiggins.pdf")
par(pty = "s")
plot(oOHiggins, main = "O'Higgins", col = c(2,4))
dev.off()

pdf(file = "plot_Maule.pdf")
par(pty = "s")
plot(oMaule, main = "Maule", col = c(2,4))
dev.off()

pdf(file = "plot_Nuble.pdf")
par(pty = "s")
plot(oNuble, main = "Ñuble", col = c(2,4))
dev.off()

pdf(file = "plot_Biobio.pdf")
par(pty = "s")
plot(oBiobio, main = "Biobío", col = c(2,4))
dev.off()

pdf(file = "plot_Araucania.pdf")
par(pty = "s")
plot(oAraucania, main = "Araucanía", col = c(2,4))
dev.off()

pdf(file = "plot_LosRios.pdf")
par(pty = "s")
plot(oLosRios, main = "Los Ríos", col = c(2,4))
dev.off()

pdf(file = "plot_LosLagos.pdf")
par(pty = "s")
plot(oLosLagos, main = "Los Lagos", col = c(2,4))
dev.off()

pdf(file = "plot_Aysen.pdf")
par(pty = "s")
plot(oAysen, main = "Aysén", col = c(2,4))
dev.off()

pdf(file = "plot_Magallanes.pdf")
par(pty = "s")
plot(oMagallanes, main = "Magallanes", col = c(2,4))
dev.off()
