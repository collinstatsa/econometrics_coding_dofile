//////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////        Collins Rostant Tatsa            ////////////////////
/////////////////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////////////
clear all
capture log close
log using "C:\Users\mirei\OneDrive\Bureau\Travail_pratique_UdeM\results_tp.log",replace
set more off
use "C:\Users\mirei\OneDrive\Bureau\Travail_pratique_UdeM\crime.dta"
set memory 20m

////La base de données d'un É.A. de 92 villes américaines

**(a) Rapportez et discutez brièvement quelques statistiques descriptives

sum crimes pop officers
tabstat crimes pop officers, statistics(n mean min max sd)

**(b) Estimation MCO du modèle (1)
regress crimes pop officers
* Les estimés B_ch
* B_ch= (B1_ch B2_ch B3_ch)'= (0.11141539  0.00006637  0.01441498)'

* Leurs écarts-types
* SigmaB1_ch= 2.464613      SigmaB2_ch= 0.0000105     SigmaB3_ch= 0.0034477

**(c) Les valeurs des sommes des carrés; totale, expliquée et des résidus

*SST= 80227.4984         SSE= 66297.7568        SSR= 13929.7416
*Calcul du R2 = SSE/SST= 66297.7568/80227.4984= 0.82637
// Le R2 calculé correspond bien a celui rapporté par la commande regress.

**(d) Construction du vecteur y et la matrice X
mkmat crimes, matrix(y)
matrix list y

gen cste=1
mkmat cste pop officers, matrix(X)
matrix list X

* Calcul de (X'X) inverse et X'y
matrix A=syminv(X'*X)
matrix list A

matrix B=X'*y
matrix list B

* Calcul de (X'X)-¹X'y
matrix B_hat=A*B
matrix list B_hat

// Les valeurs estimées B_hat correspondent bien aux estimés rapportés par la commande regress.

**(e) Valeur de sigma2_hat et matrice de Var-cov
*Sigma2_hat= SSR/(N-K)= 13929.7416/89 = 156.5139506

*Matrice de Variance-Covariance de B_hat
matrix VB_hat=e(V)
matrix list VB_hat

* Vérification
scalar VB1_hat=(2.464613)^2
scalar list VB1_hat

scalar VB2_hat=(0.000010473)^2
scalar list VB2_hat

scalar VB3_hat=(0.0034477)^2
scalar list VB3_hat

*Nous remarquons que le carré des écarts-types correspondent bien aux valeurs de la diagonales
*de la matrice de var-cov VB_hat.

**(f) Estimation du modèle avec transformation linéaire officersi*=officersi/100

gen officers2=officers/100

*Les estimés B_hat. Construisons la matrice Z=(1  pop  officers2)
mkmat cste pop officers2, matrix(Z)
matrix list Z

matrix B_tilda=syminv(Z'*Z)*Z'*y
matrix list B_tilda

*Leurs écarts-types
regress crimes pop officers2

*sigmaB1_tilda= 2.464613    sigmaB2_tilda= 0.0000105   sigmaB3_tilda= 0.3447671

*Les valeurs ajustées avant et après changement
matrix yhat=X*syminv(X'*X)*X'*y    // avant changement
matrix list yhat

matrix ytilda=Z*syminv(Z'*Z)*Z'*y  // après changement
matrix list ytilda

*Les résidus estimés avant et après changement
matrix ehat= y-(X*syminv(X'*X)*X'*y)    // avant changement
matrix list ehat

matrix etilda=y-(Z*syminv(Z'*Z)*Z'*y)   // après changement
matrix list etilda

*Nuages de points comparant les valeurs ajustées et les résidus
predict yhat
predict ytilda
scatter yhat ytilda

predict ehat
predict etilda
scatter ehat etilda

*Les valeurs ajustées et les résidus estimés sont invariants aux transformations linéaires.
*Par contre, les transformations linéaires modifient les estimés B_hat et leurs écarts-types.
*Les nuages de points se confondent et sont parfaitement alignés sur une droite pour les
*valeurs ajustées et les résidus estimés.

**(g) Estimation de B2_hat par l'approche en deux étapes de FWL
*Première étape: on estime popi= alpha1+alpha2officers+ui ou ui est le terme d'erreur
regress pop officers

predict popres,residuals
*Estimés et leurs écarts-types
*alpha1_hat= 131392.3                     alpha2_hat= 286.0615
*sigmaalpha1_hat= 20576.88                sigmaalpha2_hat= 17.16586

*Deuxième étape: on regress crimes sur les résidus de la première étape; sans constante
*puisqu'elle a été incluse dans la première étape. crimesi=phipopres+mui ou mui est le terme
*d'erreur.
regress crimes popres,nocons
*Estimés et leurs écarts-types
*phi_hat= 0.0000664                     sigmaphi_hat=  0.000041 

//Notre estimé de la deuxième étape phi_hat= 0.0000664 correspond bien au B2_ch= 0.00006637 rapporté
//a la sous-question (b).


//// Exercice 2 Considérons le modèle avec différentes zones géographiques

**(a) Estimation MCO du modèle(2)

regress crimes pop officers zone1 zone2 zone3

*Estimés et leurs écarts-types

*B_hat=(B1_hat B2_hat B3_hat)'=(-5.887572  0.0000608  0.0037286)'

*sigmaB1_hat= 3.760789     sigmaB2_hat= 0.0000112    sigmaB3_hat= 0.0037286

*delta_hat=(delta1_hat  delta2_hat  delta3_hat)'=(4.785748  2.815055  11.46616)' 

*sigmadelta1_hat= 3.891417    sigmadelta2_hat= 4.381596   sigmadelta3_hat= 3.664555


**(b) Interprétation de l'estimé delta1

**(c) Calcul statistique t pour le test bilatéral Ho:delta1=0 contre H1:delta=/=0
matrix bhat=e(b)
matrix list bhat

matrix Vbhat=e(V)
matrix list Vbhat

scalar tstat=(bhat[1,3]-0)/sqrt(Vbhat[3,3])
scalar list tstat

*La valeur (tstat= 1.2298215)calculée est identique a celle rapportée par la commande regress.
*t lu dans la table de student a alpha/2=0.025 et a N-K=92-6=86 ddl est
*Pvalue sup alpha** 

**(d) Estimation MCO du modéle (3)

regress crimes pop officers zone2 zone3 zone4

**Les estimés et leurs écarts-types

*alpa1_hat= -1.101824    alpha2_hat= 0.0000608    alpha3_hat= 0.0172234

*phi1_hat= -1.970693     phi2_hat= 6.680416       phi3_hat= -4.785748 

*sigmaalpa1_hat= 3.395167   sigmaalpha2_hat= 0.0000112   sigmaalpha3_hat= 0.0037286

*sigmaphi1_hat= 4.292075    sigmaphi2_hat= 3.208686      sigmaphi3_hat= 3.891417 

//Montrer comment ces nouveaux estimés peuvent être écrits en fonction des anciens.
//Revenir ICI


**(e) Estimation du modèle (1) séparément pour chacune des 4 zones
*zone1
regress crimes pop officers if zone1==1
matrix bzone1_hat= e(b)
matrix list bzone1_hat

*zone2
regress crimes pop officers if zone2==1
matrix bzone2_hat= e(b)
matrix list bzone2_hat

*zone3
regress crimes pop officers if zone3==1
matrix bzone3_hat= e(b)
matrix list bzone3_hat

*zone4
regress crimes pop officers if zone4==1
matrix bzone4_hat= e(b)
matrix list bzone4_hat

// Exercice 3 Estimation du modèle (4) avec erreurs hétéroscédastiques

**(a) Deux nuages de points illustrant la relation entre crimes et les variables exogènes
scatter crimes pop
scatter crimes officers

//Chacun des nuages montre une relation non linéaire entre la variable dépendante(crimes)
//et les variables exogènes pop, officers.

**(b) Calcul de Aw= X'Ew_hatX et de la matrice de var-cov robuste à l'hétéroscédasticité
mkmat ehat
gen ehat_2=ehat^2        // nous générons le carré des erreurs.
mkmat ehat_2

matrix Ew_hat=diag(ehat_2)
matrix list Ew_hat

matrix Aw=X'*Ew_hat*X
matrix list Aw

matrix Varcov=syminv(X'*X)*Aw*syminv(X'*X)      //// matrice var-cov robuste
matrix list Varcov        //Pb commande

**(c) Test d'hypothèse Ho: B_2=0 contre  H1: B_2 sup 0
*statistique t avec les écarts-types robustes
regress crimes pop officers, robust

*statistique t ignorant le problème d'hétéroscédasticité
*regress tout simplement


**(d) Estimation du modèle(4)par MCQG
*La procédure à suivre est:
*Le modèle a été estimé par les MCO et nous avons récupéré les résidus
gen lnehat2=log(ehat_2)  //log appliqué au carré des résidus
*Régression du log des erreurs au carré sur les variables exogènes
regress lnehat2 pop officers
predict lnehat2it
*Transformation des données
gen ehat_2it=exp(lnehat2it)     //Élimination du log

mkmat ehat_2it
matrix sigmahat=diag(ehat_2it)
* Calcul de Bêta MCQG par application de l'estimateur MCO sur données transformées
matrix bhat_mcqg=syminv(X'*syminv(sigmahat)*X)*X'*syminv(sigmahat)*y
matrix list bhat_mcqg


 


