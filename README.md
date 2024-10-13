# iut_sd2_rshiny_enedis
Le projet rshiny qui nous a été proposé de faire est de créer une interface rshiny avec les données d'une API sur les DPE de logements existants extrait du site : data.ademe.fr .

Les données qui nous ont été fournies ont été basées sur une étude des logements en France, nous avons choisi de nous concentrer sur les logements du département du Rhône. Nous avions la liberté de choisir les données que nous voulons extraire ou non de l'API, nous avons choisi comme donnée quantitative : Émission GES 5 usages par mois, Coût total 5 usages, Émission GES 5 usages et le score X (score donné pas l'API), Surface habitable logement et comme variable qualitative : Type énergie n1, Nom commune BAN, N département BAN, Qualité isolation enveloppe, Date réception DPE, N°DPE, Code postal BAN, Étiquette GES, Type bâtiment, Type énergie principale chauffage, Étiquette DPE puis Typologie logement.

Pour réaliser ce projet, nous avons dans un premier temps extrait les données de L'API( Final_logement.R) à l'aide d'une boucle R puis construit l'interface avec R_shiny(Rendu Rshiny.R), pour pouvoir exporter les données de logements_final, vous devet d'abord telecharger les données :data/adresses-69.csv.

Notre interface est composée de 3 pages, l'une destinée à la visualisation des données sans créer de croisements entre elle, la seule modalité que vous pouvez changer est un filtre sur les différents codes postaux. La deuxième page est une carte où vous pouvez voir où se situer les logements mentionnés dans les départements du Rhône et la dernière page vous permettra de voir les différentes analyses que nous avons pus faire avec les croisements de données.

Pour accedé au site:  - identifiant:user
                      - mot de passe: user 
