# Rapport d'enquête

# Récupéraion des données
dat <- read.csv2("ethique.csv", sep=";", na.strings = "NA")

# Statistique bivariée




#2 variables quantitatives
# Hypothèse 1 : L'importance de l'éthique augmente avec l'âge

#On crée des classes d'âges pour faciliter la lecture et on renomme les levels  
dat$age <- cut(dat$age, c(18,25,30,35,45,55,60,65,69))     
levels(dat$age) <- c("18-25 ans", "25-30 ans","30-35 ans", "35-45 ans",
                     "45-55 ans", "55-60 ans","60-65 ans","65-69 ans")   
#On crée un tableau regroupant les 2 varibles
matable <- table(dat$age,dat$ImportanceEthique)
matable

#On réecrit le tableau de pourcentage
pourcentage=cbind(addmargins(round(prop.table(addmargins(matable,1),1),2),2), c(margin.table(matable,1),sum(matable))) 
colnames(pourcentage)<-c(colnames(matable),"TOTAL","EFFECTIF") 
pourcentage

#On crée un nuage de point
plot(dat$age,dat$ImportanceEthique,ylab = "Importance",xlab = "Âge"
     ,main = "Importance à l'éthique en fonction de l'âge")           #On crée le nuage

#On crée un diagramme en baton
barplot(matable, beside=T,xlab = "Importance accordé à l'éthique", ylab = "Effectif",
        main = "Importance accordé à l'éthique en fonction de l'âge",
        col=c("darkblue", "red","green","yellow","brown","pink","purple","black"))
legend("left", legend = c("18-25 ans", "25-30 ans","30-35 ans", "35-45 ans",
                          "45-55 ans", "55-60 ans","60-65 ans","65-69 ans"),
       fill = c("darkblue", "red","green","yellow","brown","pink","purple","black"), bty = "n")












# 2 variable qualitatif
#Hypotèse 2 : L'existence d'un code éthique est répondu dans le secteur privé

#On change le nom des levels des 2 variables
dat$secteur <- as.factor(dat$secteur)
dat$ExistenceCodeEthique <- as.factor(dat$ExistenceCodeEthique)
levels(dat$ExistenceCodeEthique) <- c("Oui", "Non", "Ne sait pas")
levels(dat$secteur) <- c("Public","Privé","Ne sait pas")

#On crée un tableau croisé
matable2 <- table(dat$ExistenceCodeEthique,dat$secteur)
matable2

#On crée un tableau de pourcentage en colonne
p_table2 <- round(addmargins(prop.table(matable2,margin = 2),1),3)
p_table2


#On crée des diagrammes en camembert, 1 pour le privé, l'autre pour le public
priv <- c(82,38,16)
pie_priv <- pie(priv, col = c("darkblue","red","white"),labels = c("Oui","Non","Ne sait pas"),
                main = "Existance d'un code éthique dans le privé")
legend(x="bottomright", legend=c("60 %","28 %","12 %"), cex=1.2,fill=c("darkblue","red","white"))

pub <- c(49,18,9)
pie_pub <- pie(pub, col = c("darkblue","red","white"),labels = c("Oui","Non","Ne sait pas"),
                main = "Existance d'un code éthique dans le public")
legend(x="bottomright", legend=c("64,5 %","23,5 %","12 %"), cex=1.2,fill=c("darkblue","red","white"))




hist(dat$sexe,dat$ImportanceEthique)






#1 variable qualitative et une quantitative
# Hypotèse 3 : L’importance de l’éthique dans le secteur privé est élevé

#On crée un tableau
dat <- read.csv2("ethique.csv", sep=";", na.strings = "NA")   #On initialise les données
imp_priv <- subset(x= dat, subset= secteur==2)                #On sélectionne que les entreprises privées
imp_priv$secteur <- as.factor(imp_priv$secteur)
levels(imp_priv$secteur) <- "Privé"                         #On renomme le level
matable3 <- table(imp_priv$secteur,imp_priv$ImportanceEthique)
matable3

t <- table(dat$secteur,dat$ImportanceEthique)
t
#On crée une boite à moustache
boxplot(imp_priv$secteur, imp_priv$ImportanceEthique, xlab="Secteur", ylab="Importance")

#On calcule la moyenne de l'importance dans le secteur privé
mean(imp_priv$ImportanceEthique)

#On crée un histogramme 
hist(imp_priv$ImportanceEthique, xlab = "Importance", ylab = "Fréquence",main = "Histogramme de l'importance
     éthique dans le secteur privé")






