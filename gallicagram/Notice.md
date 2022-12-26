# Notice de Gallicagram

- Gallicagram est un programme représentant graphiquement l’évolution au cours du temps de la fréquence d’apparition d’un ou plusieurs syntagmes dans les corpus numérisés de <a href="https://gallica.bnf.fr/" target="_blank">Gallica</a> et de beaucoup d'autres bibliothèques.
- Développé par [Benjamin Azoulay](mailto:benjamin.azoulay@ens-paris-saclay.fr) et <a href="https://regicid.github.io/" target="_blank">Benoît de Courson</a>, il est intégralement rédigé en langage <a href="https://www.r-project.org/" target="_blank">R</a> et présente une interface graphique interactive <a href="https://shiny.rstudio.com/" target="_blank">Shiny</a>.
- Les données produites sont téléchargeables par l’utilisateur. Le <a href="https://github.com/regicid/docker_gallicagram" target="_blank">code source</a> de Gallicagram est libre d'accès et de droits.
- L'analyse de la structure des recherches dans le corpus de presse de Gallica peut être réalisée dans <a href="https://shiny.ens-paris-saclay.fr/app/gallicapresse" target="_blank">Gallicapresse</a>.

### Corpus
- Gallicagram a accès à de nombreuses bibliothèques. Quelle que soit la bibliothèque choisie, le corpus est circonscrit aux documents numérisés et océrisés et rédigés dans la langue choisie par l'utilisateur.
- Gallicagram permet aussi d'explorer des sous-corpus de Gallica totalement accessibles et bien documentés. De nombreux graphiques renseignant sur la structure des corpus de presse et de livres de Gallica sont présentés dans les onglets « Corpus de presse » et « Corpus de livres » de Gallicagram.

### Options de recherche
- Gallicagram extrait des valeurs distinctes selon le mode de recherche sélectionné par l'utilisateur. En raison de l'architecture des moteurs de recherche propre à chaque bibliothèque, il est rare que plusieurs modes de recherche soient disponibles pour un même corpus. Les corpus de Gallica sont ceux qui présentent la plus grande diversité de modes de recherche.
- 4 modes de recherche sont proposés dans Gallicagram : 
	- la recherche par document (volume) compte, pour chaque sous-période, le nombre de documents du corpus présentant au moins une occurrence du syntagme recherché ;
	- la recherche par page (page) compte, pour chaque sous-période, le nombre de pages du corpus présentant au moins une occurrence du syntagme recherché ;
	- la recherche par article (article) compte, pour chaque sous-période, le nombre d'articles du corpus de presse présentant au moins une occurrence du syntagme recherché ;
	- la recherche par n-gramme(match) compte, pour chaque sous-période, le nombre total d'occurrences du syntagme recherché dans le corpus.
- L'utilisateur peut choisir le corpus qu'il souhaite explorer.
- Il peut régler les bornes chronologiques de sa recherche.
- Pour certains corpus, il peut choisir la résolution (mensuelle ou annuelle) avec laquelle les résultats seront affichés.
- Pour chaque mode de recherche et chaque corpus, Gallicagram extrait aussi le volume de la base de données correspondante (nombre total de documents, de pages, d'articles, ou de n-grammes pour chaque sous-période).

### Syntaxe de recherche
- L'utilisateur peut chercher un syntagme unique (ex. Clemenceau).
- Il peut aussi comparer les évolutions respectives de deux syntagmes concurrents en les séparant par une esperluette "&" (ex. Georges Clemenceau&Aristide Briand).
- Il peut effectuer une recherche conditionnelle de forme OU en utilisant le signe "+" (ex. juif+juive). Il s'agit d'un "ou" inclusif qui renverra tous les numéros contenant les termes séparés par un "+". La recherche dénombre des syntagmes exacts et isolés. Ainsi, entrer le mot "juif" ne permettra pas d'obtenir les résultats correspondant à son pluriel : "juifs". La recherche conditionnelle OU avec "+" permet d'intégrer ces résultats.
- Dans le mode de recherche "Par document" (et non "Par N-gramme", comme par défaut), il peut chercher des cooccurrences dans les corpus de Gallica à l'aide de l'opérateur "\*" (ex. universel\*nation). Une case apparait alors à côté du champ de la requête pour définir la distance maximale entre les termes recherchés (en nombre de mots).  
- Ces trois options de recherche sont cumulables (ex. juif+juive+judéo&calviniste+huguenot+parpaillot ; ex. universel\*nation+universel\*patrie&étranger\*ennemi). "&" est prioritaire sur "*" qui est prioritaire sur "+". Ainsi a*b+c*d&e = [(a*b)+(c*d)]&e
- La recherche n'est pas sensible à la casse (case insensitive).

### Options de visualisation
- L'utilisateur peut : 
	- isoler certaines recherches dans le visualiseur en cliquant sur la légende des courbes qu'il souhaite faire disparaître ;
	- effectuer des zooms sur le graphique et afficher la valeur précise de chaque point de la courbe en y positionnant la souris ;
	- afficher la distribution chronologique des documents composant la base de données sur la période qu'il a choisie ;
	- comparer toutes les recherches effectuées au cours de sa session à l'intérieur d'un seul graphique ;
	- accéder à la recherche correspondante (syntagme, corpus, sous-période) sur le site de la bibliothèque explorée afin d'accéder au corpus sous-jacent à la recherche ;
	- normaliser les valeurs ;
	- lisser les courbes affichées (type loess) ;
	- comparer l'évolution de deux syntagmes par soustraction ;
	- observer les corrélations entre les syntagmes recherchés ;
	- observer les corrélations pour un même terme entre les différents modes de recherche ;
	- télécharger les graphes et les données du visualiseur ainsi que les données de la totalité de la session.

### 8 Types de visualisation différents
- Courbes : courbes d'évolution entourées de leur marge d'erreur calculée en fonction du volume de la base et de la fréquence d'occurrences du terme recherché. Présentation par défaut de Gallicagram.
- Sommes : graphique en barres horizontales figurant la somme des occurrences des différents mots sur l'ensemble de la période étudiée. Tri par ordre décroissant d'occurrences.
- Histogrammes : graphique en barres verticales figurant le nombre d'occurrences de chacun des termes recherchés au cours du temps.
- Bulles : graphique en bulles figurant la fréquence d'occurrence de chacun des termes recherchés au cours du temps. Tri des termes par ordre décroissant en regard de la somme des occurrences sur l'ensemble de la période. La fonction rescale permet de faire apparaître les termes les moins fréquents.
- Aires : graphique en aires figurant la fréquence d'occurrence de chacun des termes recherchés au cours du temps. Tri des termes par ordre décroissant en regard de la somme des occurrences sur l'ensemble de la période.
- AFC : graphique figurant le résultat de l'analyse factorielle des correspondances pour les termes recherchés sur la période étudiée. Le graphique représente à la fois les termes et les dates selon les deux axes principaux calculés par l'AFC.
- Nuage de mots : graphique en nuage de mots. Chaque bulle représente un terme de recherche sur l'ensemble de la période analysée. Son diamètre est proportionnel à la fréquence d'apparition du terme sur l'ensemble de la période.
- Polaires : graphique en coordonnées polaires. Ce mode permet d'étudier la saisonnalité des termes recherchés. Ici la fonction rescale calcule la différence par rapport à la moyenne mobile sur douze mois. Une option avancée permet d'étudier les moyennes mensuelles sur l'ensemble de la période (un cercle par mot).

### Traitements
 - Le traitement des données extraites consiste au calcul pour chaque sous-séquence temporelle de la fréquence d’apparition du terme défini par l’utilisateur. Cette fréquence est le rapport des deux variables extraites (le nombre de résultats et le volume de la base) soit pour une sous-séquence temporelle  : 
<script type="text/javascript"
        src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-AMS_CHTML"></script>

$$ ratio_i=\frac{count_i}{base_i} =\frac{x_i}{N_i} $$


- Le graphique présente cette fréquence en ordonnées et le temps en abscisses selon l’échelle sélectionnée. La courbe qu’il affiche relie les points calculés par l’ordinateur.

### Conception et précautions d'usage
- Toutes les informations nécessaires à la bonne utilisation de Gallicagram sont indiquées dans <a href="https://osf.io/preprints/socarxiv/84bf3/" target="_blank">l'article de recherche associé à ce logiciel</a>.


### Corpus disponibles / langue / mode de recherche

