
require(maps)
require(RColorBrewer)

data <- read.csv2("Delits_inegalites.csv", stringAsFactors=FALSE)
data <- data[1:96,]
nb_couleurs <- 6

###  rapport interd?cile est le rapport entre le neuvi?me d?cile (niveau de vie qui s?pare les 90 % qui touchent le moins des 10 % qui touchent le plus) et le premier d?cile (niveau de vie qui s?pare les 10 % les plus pauvres des 90 % restants)
round(as.numeric(quantile(data$Rapport, prob = seq(0, 1, length = nb_couleurs+1), type = 5)), digits=2)

data$Rapport_quantile <- cut(data$Rapport, 
	round(as.numeric(quantile(data$Rapport, prob = seq(0, 1, length = nb_couleurs+1), type = 5)), digits=2), 
	include.lowest=TRUE, right=TRUE)


# Configuration de la carte
couleurs <- brewer.pal(6,"PuOr")

france <- map(database="france")
match_fce <- match.map(france, iconv(data$D?partement,to="ASCII//TRANSLIT"), exact=TRUE)
