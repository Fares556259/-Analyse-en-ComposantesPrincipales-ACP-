TP1 : Analyse en Composantes Principales (ACP)
================
Fares Selmi
22 April 2026

## Introduction

Ce rapport présente une Analyse en Composantes Principales (ACP)
effectuée sur les données de satisfaction relatives à la formation.
L’objectif est de résumer l’information contenue dans les variables de
satisfaction et d’identifier les principaux axes de variabilité parmi
les répondants.

## Exploration du jeu de données

``` r
# Chargement des données
data <- read.csv("Votre avis sur la formation (réponses) - Réponses au formulaire 1.csv", 
                 header = TRUE, sep = ",")

# Aperçu des premières lignes
head(data[, 1:6]) # Affichage des variables démographiques
```

    ##                    Q1              Q2    Q3        Q4           Q5
    ## 1 30/03/2026 01:27:00       18–22 ans Femme Ingénieur     Rarement
    ## 2 30/03/2026 01:31:52 Moins de 18 ans Femme     Autre     Rarement
    ## 3 30/03/2026 01:33:56       18–22 ans Femme Ingénieur      Parfois
    ## 4 30/03/2026 01:45:42       18–22 ans Homme   Licence Très souvent
    ## 5 30/03/2026 03:16:18       18–22 ans Femme Ingénieur      Parfois
    ## 6 30/03/2026 05:15:36       18–22 ans Femme Ingénieur      Parfois
    ##                                         Q6
    ## 1                       Instagram / TikTok
    ## 2                       Instagram / TikTok
    ## 3                       Instagram / TikTok
    ## 4 Amazon / Jumia, Netflix / Spotify, Autre
    ## 5                           Amazon / Jumia
    ## 6                    Amazon / Jumia, Autre

``` r
# Structure des données
str(data[, 7:15]) # Aperçu des premières variables de satisfaction
```

    ## 'data.frame':    45 obs. of  9 variables:
    ##  $ Q7 : int  2 2 3 4 3 5 3 3 3 3 ...
    ##  $ Q8 : int  2 4 3 4 2 5 3 2 3 3 ...
    ##  $ Q9 : int  3 4 NA 4 4 3 4 3 4 2 ...
    ##  $ Q10: int  4 4 4 3 3 5 5 5 3 4 ...
    ##  $ Q11: int  3 2 3 4 3 3 4 3 2 3 ...
    ##  $ Q12: int  3 3 3 2 3 1 4 4 4 2 ...
    ##  $ Q13: int  2 2 2 3 3 1 3 2 3 1 ...
    ##  $ Q14: int  4 2 2 3 4 1 4 4 3 2 ...
    ##  $ Q15: int  2 3 3 3 3 4 3 4 3 4 ...

## ACP normée pas à pas

Conformément à la méthodologie du cours, nous commençons par préparer la
matrice des données actives (variables de satisfaction Q7 à Q30).

``` r
# Sélection des variables actives (Likert 1-5)
X <- as.matrix(data[, 7:30])

# 1. Calcul de la matrice centrée
g <- colMeans(X, na.rm = TRUE)
Y <- sweep(X, 2, g, FUN = '-')

# 2. Calcul des écarts-types et réduction
n <- nrow(X)
et <- apply(Y, 2, function(x) sqrt(sum(x^2, na.rm = TRUE) / n))
Z <- sweep(Y, 2, et, FUN = '/')

# Vérification : Les variances doivent être égales à 1
round(colSums(Z^2, na.rm = TRUE) / n, 2)
```

    ##  Q7  Q8  Q9 Q10 Q11 Q12 Q13 Q14 Q15 Q16 Q17 Q18 Q19 Q20 Q21 Q22 Q23 Q24 Q25 Q26 
    ##   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1 
    ## Q27 Q28 Q29 Q30 
    ##   1   1   1   1

``` r
# 3. Calcul de la matrice des corrélations R
R <- cor(Z, use = "pairwise.complete.obs")

# 4. Calcul des valeurs propres (Eigenvalues)
vp <- eigen(R)
lambda <- vp$values
round(lambda[1:5], 3)
```

    ## [1] 8.247 3.321 1.983 1.528 1.207

## ACP avec le package FactoMineR

Nous utilisons maintenant la fonction `PCA` pour une analyse complète,
en incluant les variables démographiques et les questions de confiance
comme variables illustratives.

``` r
# Exécution de l'ACP
# Variables actives : Q7 à Q30
# Variables illustratives qualitatives : Q1 à Q6 et Q31 à Q35
res.pca <- PCA(data, 
               quanti.sup = NULL, 
               quali.sup = c(1, 2, 3, 4, 5, 6, 31, 32, 33, 34, 35), 
               graph = FALSE)
```

### 1. Choix du nombre d’axes (Éboulis des valeurs propres)

``` r
fviz_screeplot(res.pca, addlabels = TRUE, ylim = c(0, 50)) +
  theme_minimal() +
  labs(title = "Éboulis des valeurs propres",
       x = "Dimensions", y = "% de variance expliquée")
```

![](report_files/figure-gfm/scree_plot-1.png)<!-- -->

D’après le critère de Kaiser, nous retenons les axes dont la valeur
propre est supérieure à 1. Les deux premiers axes expliquent environ 48%
de l’inertie totale.

### 2. Interprétation de la carte des variables (Cercle des Corrélations)

``` r
fviz_pca_var(res.pca, col.var = "cos2") +
  scale_color_gradient2(low = "white", mid = "blue", high = "red", midpoint = 0.6) +
  theme_minimal() +
  labs(title = "Cercle des corrélations",
       subtitle = "Coloré par la qualité de représentation (cos2)")
```

![](report_files/figure-gfm/var_circle-1.png)<!-- -->

- **Axe 1** : Est fortement lié aux variables d’impact et de dépendance
  aux recommandations.
- **Axe 2** : Est lié à la qualité perçue de la personnalisation.

### 3. Interprétation de la carte des individus

``` r
fviz_pca_ind(res.pca, geom = c("point", "text"), col.ind = "cos2") +
  scale_color_gradient2(low = "white", mid = "blue", high = "red", midpoint = 0.6) +
  theme_minimal() +
  labs(title = "Carte des individus",
       subtitle = "Coloré par le cos2")
```

![](report_files/figure-gfm/ind_map-1.png)<!-- -->

### 4. Analyse par groupes (Habillage)

``` r
# Coloration par Sexe (Q3)
fviz_pca_ind(res.pca, 
             label = "none",
             habillage = 3, 
             addEllipses = TRUE, 
             ellipse.level = 0.95) +
  theme_minimal() +
  labs(title = "Projection des individus par Sexe")
```

![](report_files/figure-gfm/ind_habillage-1.png)<!-- -->

## Conclusion

L’ACP a permis de mettre en évidence deux dimensions majeures dans la
perception des recommandations algorithmiques. Les résultats montrent
une certaine homogénéité globale avec des disparités liées au niveau
d’adhésion aux suggestions personnalisées.
