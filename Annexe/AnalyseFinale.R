#Activation des librairies R qui seront utiles pour l'analyse des données
library(readxl)

#Importation des données
donneesfinales = read_excel("C:/Users/erick/Documents/Studies/Université Grenoble Alpes/M1 MIASHS WIC/Projet tuteuré/DataFinalPourAnalyseR.xlsx")
attach(donneesfinales)

#Factorisation des variables pour des analyses statistiques
donneesfinales$Genre = as.factor(donneesfinales$Genre)
donneesfinales$Niveau = as.factor(donneesfinales$Niveau)
donneesfinales$Domaine = as.factor(donneesfinales$Domaine)
donneesfinales$GelHydro = as.factor(donneesfinales$GelHydro)
donneesfinales$Masque = as.factor(donneesfinales$Masque)
donneesfinales$Type_Tissu = as.factor(donneesfinales$Type_Tissu)
donneesfinales$Type_Jet = as.factor(donneesfinales$Type_Jet)
donneesfinales$Type_FFP2 = as.factor(donneesfinales$Type_FFP2)
donneesfinales$Saluer_Bise = as.factor(donneesfinales$Saluer_Bise)
donneesfinales$Saluer_Main = as.factor(donneesfinales$Saluer_Main)
donneesfinales$Saluer_Accolade = as.factor(donneesfinales$Saluer_Accolade)
donneesfinales$Saluer_Oral = as.factor(donneesfinales$Saluer_Oral)
donneesfinales$Saluer_Check = as.factor(donneesfinales$Saluer_Check)
donneesfinales$MT_Bus = as.factor(donneesfinales$MT_Bus)
donneesfinales$MT_Tramway = as.factor(donneesfinales$MT_Tramway)
donneesfinales$MT_Vélo = as.factor(donneesfinales$MT_Vélo)
donneesfinales$MT_Train = as.factor(donneesfinales$MT_Train)
donneesfinales$MT_VhcPerso = as.factor(donneesfinales$MT_VhcPerso)
donneesfinales$MT_Covoit = as.factor(donneesfinales$MT_Covoit)
donneesfinales$MT_Pied = as.factor(donneesfinales$MT_Pied)
donneesfinales$`MT_Pas concerné` = as.factor(donneesfinales$`MT_Pas concerné`)
donneesfinales$TCSem = as.factor(donneesfinales$TCSem)
donneesfinales$EvictTC = as.factor(donneesfinales$EvictTC)
donneesfinales$SortiePersExt = as.factor(donneesfinales$SortiePersExt)
donneesfinales$GestionCourses = as.factor(donneesfinales$GestionCourses)
donneesfinales$HoraireCourses = as.factor(donneesfinales$HoraireCourses)
donneesfinales$Livraison = as.factor(donneesfinales$Livraison)
donneesfinales$PersoRisqueIndiv = as.factor(donneesfinales$PersoRisqueIndiv)
donneesfinales$PersoRisqueCot = as.factor(donneesfinales$PersoRisqueCot)
donneesfinales$ToucheVisage = as.factor(donneesfinales$ToucheVisage)
donneesfinales$RongeOngle = as.factor(donneesfinales$RongeOngle)
donneesfinales$SuiviPsy = as.factor(donneesfinales$SuiviPsy)
donneesfinales$GroupeSoutienPsy = as.factor(donneesfinales$GroupeSoutienPsy)
donneesfinales$Info_RS = as.factor(donneesfinales$Info_RS)
donneesfinales$Info_MedGen = as.factor(donneesfinales$Info_MedGen)
donneesfinales$Info_TV = as.factor(donneesfinales$Info_TV)
donneesfinales$Info_Radio = as.factor(donneesfinales$Info_Radio)
donneesfinales$Info_MedInd = as.factor(donneesfinales$Info_MedInd)
donneesfinales$Info_Cercle = as.factor(donneesfinales$Info_Cercle)
donneesfinales$Info_Journaux = as.factor(donneesfinales$Info_Journaux)
donneesfinales$Info_SiteGov = as.factor(donneesfinales$Info_SiteGov)
donneesfinales$Info_Facs = as.factor(donneesfinales$Info_Facs)
donneesfinales$Info_No = as.factor(donneesfinales$Info_No)
donneesfinales$ConfInfo = as.factor(donneesfinales$ConfInfo)
donneesfinales$Conf_Respect = as.factor(donneesfinales$Conf_Respect)
donneesfinales$CF = as.factor(donneesfinales$CF)
donneesfinales$`6AdultesTable` = as.factor(donneesfinales$`6AdultesTable`)
donneesfinales$LavageMain = as.factor(donneesfinales$LavageMain)
donneesfinales$RaisonLavageMain = as.factor(donneesfinales$RaisonLavageMain)
donneesfinales$MaskSystm = as.factor(donneesfinales$MaskSystm)
donneesfinales$RaisonMaskSystm = as.factor(donneesfinales$RaisonMaskSystm)
donneesfinales$GelHydro2 = as.factor(donneesfinales$GelHydro2)
donneesfinales$RaisonGelHydro2 = as.factor(donneesfinales$RaisonGelHydro2)
donneesfinales$EvictRgpMasse = as.factor(donneesfinales$EvictRgpMasse)
donneesfinales$RaisonEvictRgpMasse = as.factor(donneesfinales$RaisonEvictRgpMasse)
donneesfinales$Infl_Mask = as.factor(donneesfinales$Infl_Mask)
donneesfinales$Infl_DistSoc = as.factor(donneesfinales$Infl_DistSoc)
donneesfinales$RaisonInflDistSoc = as.factor(donneesfinales$RaisonInflDistSoc)
donneesfinales$AgirFct = as.factor(donneesfinales$AgirFct)
donneesfinales$RaisonAgirFct = as.factor(donneesfinales$RaisonAgirFct)
donneesfinales$ToucheSurface = as.factor(donneesfinales$ToucheSurface)
donneesfinales$RaisonToucheSurface = as.factor(donneesfinales$RaisonToucheSurface)
donneesfinales$Stress = as.factor(donneesfinales$Stress)

#Résumé des données
summary(donneesfinales)
head(donneesfinales)

#Tableaux de contingence descriptifs des variables d'identification
tabcont1 = table(donneesfinales$Genre, donneesfinales$Niveau)
tabcont1

tabcont2 = table(donneesfinales$Genre, donneesfinales$Domaine)
tabcont2

tabcont3 = table(donneesfinales$Niveau, donneesfinales$Domaine)
tabcont3

#Les raisons qui influencent les comportements des étudiants en période de pandémie
#Lavage régulier des mains
summary(donneesfinales$RaisonLavageMain)
#Usage masque
summary(donneesfinales$RaisonMaskSystm)
#Usage Gel hydroalcoolique
summary(donneesfinales$RaisonGelHydro2)
#Eviter regroupement de masse
summary(donneesfinales$RaisonEvictRgpMasse)
#Distanciation sociale
summary(donneesfinales$RaisonInflDistSoc)
#Agir en fonction des autres
summary(donneesfinales$RaisonAgirFct)
#Toucher les surfaces
summary(donneesfinales$RaisonToucheSurface)


#Différenciation par niveau
##La gestion des courses selon les niveaux
tabcont4 = table(donneesfinales$Niveau, donneesfinales$GestionCourses)
tabcont4
#Test d'indépendance 
chisq.test(tabcont4)

##Les gestes barrières selon les niveaux
tabcont51 = table(donneesfinales$Niveau, donneesfinales$Masque)
tabcont52 = table(donneesfinales$Niveau, donneesfinales$GelHydro)
tabcont53 = table(donneesfinales$Niveau, donneesfinales$EvictTC)
tabcont54 = table(donneesfinales$Niveau, donneesfinales$EvictRgpMasse)
print(tabcont51)
print(tabcont52)
print(tabcont53)
print(tabcont54)
#Test d'indépendance
chisq.test(tabcont51)
chisq.test(tabcont52)
chisq.test(tabcont53)
chisq.test(tabcont54)

#Respect des recommandations du gouvernement
tabcont61 = table(donneesfinales$Niveau, donneesfinales$Conf_Respect)
print(tabcont61)
tabcont62 = table(donneesfinales$Niveau, donneesfinales$CF)
print(tabcont62)
tabcont63 = table(donneesfinales$Niveau, donneesfinales$`6AdultesTable`)
print(tabcont63)
#Test d'indépendance
chisq.test(tabcont61)
chisq.test(tabcont62)
chisq.test(tabcont63)

#Les raisons qui poussent les étudiants à agir en période de pandémie
#Lavage des mains
tabcont71 = table(donneesfinales$Niveau, donneesfinales$RaisonLavageMain)
print(tabcont71)
#Masques
tabcont72 = table(donneesfinales$Niveau, donneesfinales$RaisonMaskSystm)
print(tabcont72)
#Gel hydroalcoolique
tabcont73 = table(donneesfinales$Niveau, donneesfinales$RaisonGelHydro2)
print(tabcont73)
#Regroupement de masse 
tabcont74 = table(donneesfinales$Niveau, donneesfinales$RaisonEvictRgpMasse)
print(tabcont74)

#Différenciation par genre
##La gestion des courses selon le genre
tabcont8 = table(donneesfinales$Genre, donneesfinales$GestionCourses)
tabcont8
#Test d'indépendance 
chisq.test(tabcont4)

##Les gestes barrières selon le genre
tabcont91 = table(donneesfinales$Genre, donneesfinales$Masque)
tabcont92 = table(donneesfinales$Genre, donneesfinales$GelHydro)
tabcont93 = table(donneesfinales$Genre, donneesfinales$EvictTC)
tabcont94 = table(donneesfinales$Genre, donneesfinales$EvictRgpMasse)
print(tabcont91)
print(tabcont92)
print(tabcont93)
print(tabcont94)
#Test d'indépendance
chisq.test(tabcont91)
chisq.test(tabcont92)
chisq.test(tabcont93)
chisq.test(tabcont94)

#Respect des recommandations du gouvernement par genre
tabcont101 = table(donneesfinales$Genre, donneesfinales$Conf_Respect)
print(tabcont101)
tabcont102 = table(donneesfinales$Genre, donneesfinales$CF)
print(tabcont102)
tabcont103 = table(donneesfinales$Genre, donneesfinales$`6AdultesTable`)
print(tabcont103)
#Test d'indépendance
chisq.test(tabcont101)
chisq.test(tabcont102)
chisq.test(tabcont103)

#Les raisons qui poussent les étudiants à agir en période de pandémie
#Lavage des mains
tabcont111 = table(donneesfinales$Genre, donneesfinales$RaisonLavageMain)
print(tabcont111)
#Masques
tabcont112 = table(donneesfinales$Genre, donneesfinales$RaisonMaskSystm)
print(tabcont112)
#Gel hydroalcoolique
tabcont113 = table(donneesfinales$Genre, donneesfinales$RaisonGelHydro2)
print(tabcont113)
#Regroupement de masse 
tabcont114 = table(donneesfinales$Genre, donneesfinales$RaisonEvictRgpMasse)
print(tabcont114)

#Liens entre les variables