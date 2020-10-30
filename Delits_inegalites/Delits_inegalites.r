
require(maps)
require(RColorBrewer)

data <- read.csv2("./Delits_inegalites/Delits_inegalites.csv", stringsAsFactors=FALSE)
data <- data[1:95,]
nb_couleurs <- 4

### Rapport interdécile 
### Le rapport entre le neuvieme decile (niveau de vie qui separe les 90 % qui touchent le moins des 10 % qui touchent le plus) et le premier decile (niveau de vie qui separe les 10 % les plus pauvres des 90 % restants)
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

par(mfrow=c(1,2))
par(mar=c(1, 1, 1, 1))


### Carte des inégalités
coord <- map(database="france", fill=TRUE, col=couleurs[data$Rapport_quantile_n],
                xlim=c(-5.5,10), ylim=c(35,55))
legend(-6, 42, legend=levels(data$Rapport_quantile),
  col=couleurs,
  bty="n",
  pch=15,
  cex=0.6,
  xjust=0,
  title="Indice inégalités")


### Carte du facteur criminel choisi
col_choix <- colnames(data)[which(colnames(data) == "HOMICIDES.ET.COUPS.ET.VIOLENCES.par.100k.hab")]
# On le divise selon les quantiles
data[,col_choix] <- round(data[,col_choix], digits=2)
data$Crimes_quantile <- cut(data[,col_choix], 
                             as.numeric(quantile(data[,col_choix], prob = seq(0, 1, length = nb_couleurs+1), type = 5)), 
                             include.lowest=TRUE, right=TRUE, left=TRUE)
data$Crimes_quantile_n <- as.integer(data$Crimes_quantile)
coord <- map(database="france", fill=TRUE, col=couleurs[data$Crimes_quantile_n],
             xlim=c(-5.5,10), ylim=c(35,55))
legend(-6, 42, legend=levels(data$Crimes_quantile),
       col=couleurs,
       bty="n",
       pch=15,
       cex=0.6,
       xjust=0,
       title="Criminalité")  

# Pas de grande corrélations à l'oeil nu. Test stat à réaliser