# Notice de Gallicagram

- Gallicagram est un programme représentant graphiquement l’évolution au cours du temps de la fréquence d’apparition d’un ou plusieurs termes dans le corpus de presse ou de livres de [Gallica](https://gallica.bnf.fr/) ou parmi des titres de presse sélectionnés par l’utilisateur.
- Développé par [Benjamin Azoulay](mailto:benjamin.azoulay@ens-paris-saclay.fr) et [Benoît de Courson](https://regicid.github.io/), il est intégralement rédigé en langage [R](https://www.r-project.org/) et présente une interface graphique interactive [Shiny](https://shiny.rstudio.com/).
- Les données produites sont téléchargeables par l’utilisateur. Le [code source](https://github.com/regicid/gallicagram_app) de Gallicagram est libre d'accès et de droits.
- L'analyse de la structure des recherches dans le corpus de presse peut être réalisée dans [Gallicapresse](http://gallicagram.hopto.org:3838/gallicapresse/).


## Extraction

- Gallicagram procède à l’extraction du nombre de résultats de recherche renvoyé par l’[API de recherche de Gallica](https://api.bnf.fr/fr/api-gallica-de-recherche). Il est paramétré pour des recherches à l’intérieur de bornes chronologiques définies par l’utilisateur. Les corpus sont restreints aux textes en français océrisés (mode texte disponible).
- Gallicagram extrait des variables annuelles ou mensuelles selon le critère choisi par l’utilisateur. Son exécution procède à l’extraction de deux types de variables. La variable « base_temp » correspond au nombre total de documents présents dans la base de données de Gallica pour une année donnée (resp. un mois). La variable « nb_temp » correspond au nombre de documents de la base dans lequel le terme recherché apparaît au cours d’une année donnée (resp. un mois). 
- Gallicagram extrait ces deux variables pour chaque année (resp. mois) comprise entre les deux bornes chronologiques définies par l’utilisateur et les stocke dans un tableau. Gallicagram présente donc un délai d’extraction qui correspond au temps nécessaire au téléchargement des données. Ce délai est d’autant plus long que les bornes chronologiques sont espacées ou que le séquençage est fin (séquençage au mois par exemple). La variable « base_temp » étant constante quelque soit la requête, elle est stockée hors ligne afin de gagner du temps d'extraction. Ses valeurs sont mises à jour régulièrement pour tenir compte des progrès de la numérisation.

## Options de recherche
- L'utilisateur peut chercher un syntagme unique (ex. Clemenceau).
- Il peut aussi comparer les évolutions respectives de deux syntagmes concurrents en les séparant par une esperluette "&" (ex. Georges Clemenceau&Aristide Briand).
- Il peut enfin effectuer une recherche conditionnelle de forme OU en utilisant le signe "+" (ex. juif+juive). Il s'agit d'un "ou" inclusif qui renverra tous les numéros contenant les termes séparés par un "+". La recherche exacte de Gallica dénombre des syntagmes exacts et isolés. Ainsi, entrer le mot "juif" ne permettra pas d'obtenir les résultats correspondant à son pluriel : "juifs". La recherche conditionnelle OU avec "+" permet d'intégrer ces résultats.
- Ces deux options de recherche sont cumulables (ex. juif+juive+judéo&calviniste+huguenot+parpaillot).
- La recherche n'est pas sensible à la casse (case insensitive).
- L'utilisateur peut choisir parmi deux corpus de recherche : la presse de Gallica (journaux et revues) et les livres de Gallica (monographies).
- Le corpus de presse peut être analysé à l'échelle mensuelle ou à l'échelle annuelle/le corpus de livres ne peut être analysé qu'à l'échelle annuelle.
- L'utilisateur peut effectuer sa recherche dans un nombre restreint de titres de presse, maîtrisant ainsi lui-même le corpus (Recherche par titre de presse). Il peut sélectionner un ou plusieurs titres. La liste des titres est triée dans l'ordre décroissant des numéros de presse disponibles dans Gallica.
- Une fonction permet d'afficher, dans la fenêtre de visualisation, le résultat de la recherche avec Google Ngram Viewer afin de le comparer à celui de Gallicagram_livres. Il faut noter que le mode de calcul est très différent (n-gramme avec Ngram Viewer vs. nombre de livres présentant le syntagme recherché avec Gallicagram), mais les évolutions et les tendances peuvent être similaires. Ngram Viewer restreint son analyse au corpus de livres de Google Books.

## Traitements

 - Le traitement des données extraites consiste au calcul pour chaque sous-séquence temporelle de la fréquence d’apparition du terme défini par l’utilisateur. Cette fréquence nommée « ratio_temp » est le rapport des deux variables extraites soit pour une sous-séquence temporelle  : 
<script type="text/javascript"
        src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-AMS_CHTML"></script>

\\[ratio_i=\frac{count_i}{base_i} =\frac{x_i}{N_i}\\]


- Le graphique présente cette fréquence en ordonnées et le temps en abscisses selon l’échelle sélectionnée. La courbe qu’il figure relie les points calculés par l’ordinateur. Des étiquettes interactives indiquent les valeurs absolues ainsi que le dénominateur N lorsque l’utilisateur passe le curseur sur les points du graphique.
- Une fonctionnalité de lissage de type loess dont l’intensité est échelonnée de 0 à 10 permet d’estomper graphiquement les fluctuations.
- Une matrice de corrélation peut être affichée en cochant la case correspondante dans le panneau latéral.
- Une option du panneau latéral permet la représentation graphique de la différence entre les fréquences correspondant aux deux premiers termes (ou groupes de termes) recherchés. Elle permet d'observer finement la coévolution de deux termes corrélés.
- En cliquant sur un point du graphique, l'utilisateur lance automatiquement la recherche correspondante dans Gallica (pour le terme, la sous-période et le corpus indiqués).

## Corpus de presse

- Le corpus de presse de Gallica ne représente qu’une petite portion du corpus de presse de la Bibliothèque nationale de France. Ce corpus est toujours en cours de numérisation et d’océrisation et seule une partie du corpus numérisé est océrisé. C’est pourquoi le dénominateur extrait par Gallicagram peut varier d’un jour à l’autre. À ce jour, 17 430 titres sont numérisés dont 12 731 océrisés.
- Le corpus de presse de Gallica n’est pas homogène en volume au cours du temps. Certaines périodes contiennent plus de titres que d’autres, c’est le cas notamment de l’entre-deux-guerres. Le calcul de la fréquence permet donc de contourner cette difficulté en rapportant le nombre de résultats au volume du corpus. Cependant, lorsque le corpus est trop mince, ces résultats peuvent perdre de leur significativité. Il revient ainsi à l’utilisateur de prendre en compte ce paramètre dans l’usage qu’il fait de Gallicagram et dans son interprétation des résultats.
- Le corpus de presse de Gallica n’est pas non plus homogène selon les thématiques ni le type de fascicule. S’y trouvent tant des revues mensuelles que des hebdomadaires, mais aussi des quotidiens, dans des domaines aussi divers que la politique, la littérature ou la philosophie. Cette répartition thématique varie aussi au cours du temps au gré de l’apparition et de la disparition de certains titres de presse. Il faut noter que Gallicagram ne donne aucune indication sur le lectorat, et donc sur la popularité réelle des termes ou des personnages recherchés.
- Le corpus de presse numérisé contient bon nombre de revues annuelles qui créent un pic en valeur absolue en janvier.
- Le corpus de presse de Gallica n’est pas homogène géographiquement. Les journaux tirés à Paris, dont certains sont nationaux, sont plus nombreux que les titres de la presse régionale.
- Gallicagram ne compte pas le nombre d’articles, mais bien le nombre de numéros présentant au moins une fois le terme recherché. Ainsi, un numéro de presse mentionnant cent fois le terme recherché ne pèsera pas plus dans le calcul de l’indicateur qu’un numéro de presse où ce terme n’apparaît qu’une seule fois.
- L'application [Gallicapresse](http://gallicagram.hopto.org:3838/gallicapresse/), la recherche par titre(s) de presse et le lancement automatique de la recherche correspondant à un point du graphique dans Gallica sont autant d'outils qui permettent d'analyser et de maîtriser le corpus.

## Corpus de livres

- Gallicagram ne compte pas le nombre de citations du terme de recherche dans la masse des livres, mais le nombre de livres figurant le terme recherché. Il diffère en cela de [Google Ngram Viewer] (https://books.google.com/ngrams/graph?content=Abel+Bonnard%2CMarcel+Proust&year_start=1890&year_end=2019&corpus=30&smoothing=3)
- Les interprétations du résultat de recherche dans le corpus de livres doivent être élaborées avec précaution. Ce corpus rassemblant des textes volumineux, les termes courants de la langue française y apparaissent très fréquemment faisant apparaître de fortes corrélations dans les évolutions au fil du temps qui ne correspondent pas à des résultats significatifs. Il est donc préférable d'y rechercher des termes peu courants (fréquence d'occurrence<5%) ou des syntagmes contenant plusieurs mots, présentant eux aussi de faibles fréquences d'occurrence.
- Les ouvrages comprenant plusieurs tomes sont comptés pour un.



