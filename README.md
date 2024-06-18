# Gallicagram

Gallicagram est un outil de lexicométrie programmé en R et développé par Benjamin Azoulay et Benoît de Courson pour la recherche en sciences humaines et sociales. Il permet de mesurer et de visualiser la fréquence d’apparition de mots ou de syntagmes dans de nombreux corpus numérisés, y compris ceux de Gallica et d'autres bibliothèques. Gallicagram explore une grande variété de genres : livres, presse, sous-titres de films, paroles de chanson, littérature scientifique, etc. Le logiciel est librement accessible en ligne à l'adresse [https://shiny.ens-paris-saclay.fr/app/gallicagram](https://shiny.ens-paris-saclay.fr/app/gallicagram), mais elle peut aussi être lancée en local.

## Table des matières

- [Fonctionnalités](#fonctionnalités)
- [Installation](#installation)
- [Utilisation](#utilisation)
- [Documentation](#documentation)
- [Contributions](#contributions)
- [Licence](#licence)
- [Contact](#contact)

## Fonctionnalités

Gallicagram offre plusieurs fonctionnalités clés :

- **Analyse de la fréquence des mots** : mesurez la fréquence d'apparition de mots ou de syntagmes dans divers corpus.
- **Comparaison de tendances** : comparez l'évolution de plusieurs termes concurrents.
- **Recherche de cooccurrences** : trouvez et analysez les termes apparaissant ensemble dans le texte avec l'opérateur `*`.
- **Restriction du corpus** : explorez des sous-corpus spécifiques (tels que presse, livres, sélection de titres de presse, etc.).
- **Visualisation avancée** : options de lissage de courbes, normalisation des valeurs, affichage des distributions chronologiques.
- **Analyse factorielle** : représentez et interprétez les relations entre les mots et les périodes de manière synthétique.
- **Accès direct aux documents sources** : possibilité de consulter directement les documents des corpus numériques correspondants aux occurrences retournées par le logiciel.

## Installation

### Prérequis

- [R](https://www.r-project.org/)
- [RStudio](https://rstudio.com/)

### Instructions

1. Clonez le dépôt :
    ```bash
    git clone https://github.com/regicid/docker_gallicagram.git
    cd docker_gallicagram/gallicagram
    ```

2. Installez les dépendances R :
    ```R
    install.packages(c('shiny', 'rmarkdown', 'ggplot2', 'plotly', 'stringr', 'Hmisc', 'xml2', 'shinythemes', 'htmlwidgets', 'httr', 'ngramr', 'dplyr', 'htmltools', 'shinyWidgets', 'purrr', 'RSelenium', 'rvest', 'rclipboard', 'RSQLite', 'tidytext', 'DBI', 'shinybusy', 'lubridate', 'ggthemes', 'RColorBrewer', 'cowplot', 'raster', 'leaflet', 'sf', 'scales', 'cartogram', 'shinyjs', 'gtrendsR', 'timetk', 'jsonlite', 'ggwordcloud', 'FactoMineR', 'chron', 'tidyr', 'shinyalert', 'factoextra', 'bezier', 'doParallel', 'crul'))

    ```

3. Lancez l'application Shiny :
    ```R
    shiny::runApp('path_to_app/docker_gallicagram/gallicagram')
    ```

## Utilisation

Pour utiliser Gallicagram, suivez ces instructions :

1. Ouvrez l'application Shiny dans votre navigateur.
2. Choisissez le corpus parmi les options disponibles (Gallica, bibliothèques étrangères, Le Monde, etc.).
3. Sélectionnez la période et la résolution temporelle (mensuelle ou annuelle).
4. Saisissez les termes de votre recherche avec les opérateurs souhaités (`&`, `+`, `*`).
5. Explorez les résultats et utilisez les différentes options de visualisation pour affiner votre analyse.
6. Téléchargez les graphiques et les données pour une analyse ultérieure.

Pour plus d'exemples et de détails sur les options de recherche, veuillez consulter la [Documentation](#documentation).

## Documentation

Vous trouverez dans les documents suivants rédigés par les concepteurs de Gallicagram des informations détaillées sur les corpus, les méthodes de recherche, les syntaxes de recherche, les options de visualisation et les précautions d'usage concernant le logiciel.

- [Article de recherche initial](https://doi.org/10.31235/osf.io/84bf3)
- [Article de recherche complémentaire](https://journals.openedition.org/corpus/7944)
- [Notice d'utilisation](https://shiny.ens-paris-saclay.fr/app/gallicagram)

## Contributions

Les contributions sont les bienvenues ! Si vous souhaitez améliorer l'application ou ajouter de nouvelles fonctionnalités, veuillez suivre ces étapes :

1. Forkez le dépôt.
2. Créez une nouvelle branche (`git checkout -b my-fork`).
3. Effectuez vos modifications.
4. Envoyez une pull request avec une description détaillée de vos modifications.

Assurez-vous de suivre les normes de codage et de bien tester vos modifications avant de soumettre une pull request.

## Licence

Ce projet est sous licence [GPL-3.0](https://github.com/regicid/docker_gallicagram/tree/master?tab=GPL-3.0-1-ov-file#readme).

## Contact

Pour toute question ou suggestion, n'hésitez pas à nous contacter :

- Benjamin Azoulay : [benjamin.azoulay@ens-paris-saclay.fr](mailto:benjamin.azoulay@ens-paris-saclay.fr)
- Benoît de Courson : [benoit.de.courson@mpicc.de](mailto:b.decourson@csl.mpg.de)

Nous espérons que vous trouverez Gallicagram utile pour vos recherches en sciences humaines et sociales !

### Modes de Recherche

Gallicagram propose quatre modes de recherche principaux :

1. **Par document** : Compte le nombre de documents (volumes) présentant au moins une occurrence du syntagme par sous-période.
2. **Par page** : Compte le nombre de pages présentant au moins une occurrence du syntagme par sous-période.
3. **Par article** : Compte le nombre d'articles de presse présentant au moins une occurrence du syntagme par sous-période.
4. **Par n-gramme** : Compte le nombre total d'occurrences du syntagme par sous-période.

### Syntaxe de Recherche

- **Syntagme unique** : Exemple : `Clemenceau`
- **Syntagmes concurrents** : Séparés par `&`. Exemple : `Georges Clemenceau&Aristide Briand`
- **Recherche conditionnelle OU** : Utilisez `+`. Exemple : `communiste+communistes`
- **Recherche de cooccurrences** : Utilisez `*` avec une distance maximale définie. Exemple : `universel*nation`

Ces options peuvent se cumuler pour des recherches plus complexes.

## Options de Visualisation

Gallicagram offre plusieurs types de visualisation pour aider à l'interprétation des données :

1. **Courbes** : Courbes d'évolution avec marges d'erreur.
2. **Sommes** : Graphiques en barres horizontales.
3. **Histogrammes** : Graphiques en barres verticales.
4. **Bulles** : Graphiques en bulles.
5. **Aires** : Graphiques en aires.
6. **AFC (Analyse Factorielle des Correspondances)** : Représentation des relations entre termes et périodes.
7. **Nuage de mots** : Nuage de mots en fonction de leurs fréquences.
8. **Polaires** : Visualisation des tendances saisonnières.

## Corpus Disponibles

La liste des corpus interrogeables dans Gallicagram est disponible dans l'onglet notice du site.

## Précautions d'Usage

Pour une utilisation optimale de Gallicagram, les utilisateurs doivent tenir compte des spécificités et limitations des différents corpus et modes de recherche. Il est conseillé de consulter l'article de recherche associé pour une compréhension approfondie des méthodes de traitement et d'analyse des données.

Ce projet est en constante évolution. Nous vous remercions pour vos retours et espérons que Gallicagram vous sera d'une grande aide dans vos recherches.
