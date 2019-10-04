
require(maps)
require(RColorBrewer)

data <- read.csv2("./Delits_inegalites/Delits_inegalites.csv", stringsAsFactors=FALSE)
data <- data[1:96,]
nb_couleurs <- 4

###  Rapport interdécile : le rapport entre le neuvieme decile (niveau de vie qui separe les 90 % qui touchent le moins des 10 % qui touchent le plus) et le premier decile (niveau de vie qui separe les 10 % les plus pauvres des 90 % restants)
round(as.numeric(quantile(data$Rapport, prob = seq(0, 1, length = nb_couleurs+1), type = 5)), digits=2)
# On le divise selon les quantiles
data$Rapport_quantile <- cut(data$Rapport, 
	round(as.numeric(quantile(data$Rapport, prob = seq(0, 1, length = nb_couleurs+1), type = 5)), digits=2), 
	include.lowest=TRUE, right=TRUE)
# On annote la position dans les quantiles
data$Rapport_quantile_n <- as.integer(data$Rapport_quantile)


### Configuration de la carte
couleurs <- brewer.pal(nb_couleurs,"YlOrRd")

france <- map(database="france")
match_fce <- match.map(france, iconv(data$Département,to="ASCII//TRANSLIT"), exact=TRUE)

### Carte des inégalités

coord <- map(database="france", fill=TRUE, col=couleurs[data$Rapport_quantile_n],
                xlim=c(-7,10), ylim=c(41,51.5))
legend(-6, 45, legend=levels(data$Rapport_quantile),
  col=couleurs,
  bty="n",
  pch=15,
  cex=0.6,
  xjust=0,
  title="Indice inégalités")
  