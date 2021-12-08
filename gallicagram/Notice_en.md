# Gallicagram’s notice

- Gallicagram is an application which plots the evolution of use one or several syntagms use throughout time in the numerised corpus of <a href="https://gallica.bnf.fr/" target="_blank">Gallica</a>, and others libraries.
- Developed by [Benjamin Azoulay](mailto:benjamin.azoulay@ens-paris-saclay.fr) and <a href="https://regicid.github.io/" target="_blank">Benoît de Courson</a>, Gallicagram is fully written in <a href="https://www.r-project.org/" target="_blank">R</a>, in the interactive framework of <a href="https://shiny.rstudio.com/" target="_blank">Shiny</a>.
- The produced data are downloadable by the user. The <a href="https://github.com/regicid/docker_gallicagram" target="_blank">source code</a> de Gallicagram is open access and reusable.
- You can further analyse the results structure in the press corpus of Gallica can in <a href="https://shiny.ens-paris-saclay.fr/app/gallicapresse" target="_blank">Gallicapresse</a>.

### Corpus
- Gallicagram can access numerous libraries. Whatever the choice, the corpus is limited to the numerised and ocerized documents, written in the chosen language.
- You can limit Gallicagram to the subcorpuses of Gallica (e.g. specific thematic press, location, or newspaper). You can explore the structure of this corpus in the tabs “Press corpus” and “Books corpus”.
### Search options
- In certain libraries (in particular Gallica), you can use Gallicagram in several search modes. 
- there are 4 different search modes:
	- by document (volume). For each period, we count how many documents feature at least one occurrence.
	- by page. For each period, we count how many pages feature at least one occurrence
	- by article (for contemporary press). For each period, we count how many articles feature at least one occurrence. 
	- by n-gram (match). For each period, we count the exact number of occurrences in the corpus (only available for the press and books full corpuses).
- You can select a corpus, and chronological boundaries
- For some corpuses (in particular for the press corpuses), you can choose to display the results per month, instead of year
- For each search mode and corpus, Gallicagram also measures the total of documents among which the search is made. 
 
### Syntax
- You can search a unique syntagm (ex. “Clemenceau” or “Georges Clemenceau”).
- The operator “&” allows to compare the evolution of two different syntagms (ex. Georges Clemenceau&Aristide Briand will display the two curves).
- The operator “+” allows to search for a OR b. It is an inclusive “or”. It is especially useful when you also want to include the plural and the feminine of a word. For instance, “strike+strikes” will search for all occurrences of strikes, whether plural or singular.
- In the “By document” search mode, the operator “*” allows to search for co-occurrences. To do so, you have to tick the box “Use co-occurrences” below the search field, a “Distance” field will appear. Then, the algorithm search for every document where a syntagm is close to another syntagm, up tothe specified distance.
- You can combine these options (e.g. Clemenceau+Briand&Blum) The priority order is the following: & > * > +. In other words, a*b+c*d&e = [(a*b)+(c*d)]&e
- The search are case insensitive.

### Visualization options
- The plots are interactive, you can:
	
	- isolate some search by clicking on the legends on the color that you can to make disappear (or double clicking one a curve if you only want to display this one)
	- zoom over parts of the graphic by selectioning a frame
	- see the precise values by passing your mouse over a point of the plot
	- display the distribution of documents over time (advanced options)
	- display all the search of your session (advanced options). This is especially useful if you want to compare the series coming from different libraries or different languages.
	- display in the library website the documents where the algorithm found occurrences for a precise period, by clicking on the corresponding plot point.
	- perform a z-score transformation to compare series which are on different orders of magnitude
	- smooth the series (moving average by default, but loess is available in the advanced options)
	- display the difference or the ratio of the two first series
	- display a correlation matrix of the different series
	- download the graph and the data (csv). The graph is available in two forms: an interactive format (.html), identical to the one displayed in the website, and a prettier format (“Scientific graph”), in a static fomat (.png).

### Computations
 - For each period, the algorithm measures the frequency of occurrence of the syntagms. To do so, we make a ratio between the number of occurrences and the volume of data for this period (the number of words in “By “ngram” mode, the number of documents in “By document” mode, etc.). In other words:
<script type="text/javascript"
        src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-AMS_CHTML"></script>

$$ ratio_i=\frac{count_i}{base_i} =\frac{x_i}{N_i} $$


- Finally, Gallicagram plots these values with the frequency on the y-axis and the dates on the x-axis. It also draws a spline linking these points.

### Use precautions
- You can find more informations in the <a href="https://osf.io/preprints/socarxiv/84bf3/" target="_blank">associated research article</a>.