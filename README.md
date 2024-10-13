# iut_sd2_rshiny_enedis
Le projet r shiny qui nous a été proposée de faire est de créer une interface rshiny avec les données d'une API sur les DPE de logements existant extrait du site: data.ademe.fr . 

Les données qui nous on été fournis été basée sur une étude des logements en France, nous avons choisi de nous concentrée sur les logementes du département du Rhône.
Nous avions la liberté de choisir les données que nous voulons extraire ou non de l'API, nous avons choisi comme donnée quantitative : Emission GES 5 usages par mois,
Coût total 5 usages, Emission GES 5 usages et le X score (score donnée pas l'API), Surface habitable logement et comme variable qualitative: Type énergie n1,Nom  commune BAN,N département BAN,Qualité isolation enveloppe,Date réception DPE,NDPE,Code postal BAN,Etiquette GES,Type bâtiment,Type énergie principale chauffage,Etiquette DPE puis Typologielogement. 

Pour réaliser ce projet nous avons dans un premiers temps extrait les données de L'API à l'aide d'une boucle R puis construit l'interface avec R_shiny.

Notre interface est composé de 3 pages, l'une destinée à la visualisation des données sans créer de croisements entre elle, la seul modalité que vous pouvées changer est un filtre sur les différents code postaux.
La deuxiéme page est une carte où vous pouvez voir ou ce situe les logements observée dans le départements du Rhône et la derniére page vous permettra de voir les différentes analyse que nous avons pus faire avec les croisements de données
