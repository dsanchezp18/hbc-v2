# Honesty By Convenience: Corruption Tolerance in Ecuador (v2)

This is a second version of the project used to compile my undergraduate thesis: *Honesty by Convenience: Corruption Tolerance in Ecuador*. Between 2014-2016, corruption tolerance greatly increased in Ecuador. Why?

In memory of the late Jorge Pazmiño (1941-2021).

I've taken great care in trying to ensure maximum reproducibility of my research "paper". Thus, one of the things I have done is upload all the code required to compile a revised version of the document. This second version includes a major reworking of the code using tidyverse syntax and most of all fixing a number of errors that pop up when trying to compile the previous repository, [hbc-ctol](https://github.com/dsanchezp18/hbc-ctol). Among many other changes, I have fixed the error which difficulted the estimation of average partial effects of the previous version, enhanced the file structure to avoid knitting errors and included a compiled PDF version of the graph that had copyrighted LAPOP data. The main file that this code knits which is found in this repository will also be found in my [personal website](https://sites.google.com/view/dsanchezp98) along with the old one that was there before.

This new version of the repository can also be conceived as a merging of the two previous repositories, [hbc-ctol](https://github.com/dsanchezp18/hbc-ctol) and [hbc-prelim](https://github.com/dsanchezp18/hbc-prelim). This is since I have also included in my scripts directory all of the code required to produce the databases needed to run the empirical models for the papers. The [hbc-prelim](https://github.com/dsanchezp18/hbc-prelim) repository worked with the raw survey data downloaded from the [LAPOP website](https://www.vanderbilt.edu/lapop/raw-data.php) to produce the manipulated database that I used for the paper.

So here you will be able to find my paper and reproduce it completely. I assume you are already knowledgeable of R, RStudio and LaTeX. However, if you're not and you still want to use my code, I recommend learning LaTeX from the Overleaf tutorials and perhaps from [here](https://www.michellekrummel.com/) too, R from the [R For Data Science](https://r4ds.had.co.nz/) marvelous textbook and *sweave/knitr* (the integration of LaTeX and R) from this [other textbook](https://www.routledge.com/Dynamic-Documents-with-R-and-knitr/Xie/p/book/9781498716963). I've compiled a list of resources which I've found helpful to learn software, which can be found on my website. There you'll find a guide (in spanish) on how to use AmericasBarometer data with R.

Some other things need to be considered before trying to work with the code: 

- Obviously you'll first need to have an R distribution installed, as well as Rstudio.

- You'll need to download a LaTeX distribution. TeX Live is the one that I use. Some instructions can be found [here](https://tug.org/texlive/acquire-netinstall.html). 

- You'll have to set up your global options in RStudio so that `.Rnw` files are weaved with *knitr*, and that the compiler is XeLaTeX. Otherwise, the Times New Roman font mandated by my university won't work for you (and who knows what other sorts of errors will appear). 
- The execution of the `main.rnw` file will compile the complete document.

These are the only things you'll need to do, to my own knowledge, to easily compile the paper into your own computer. Please, please let me know if you run into any issues, I'll do my best trying to help you: dsanchezp998@gmail.com. I am also open to suggestions, criticisms, comments and anything else you might need.

I hope that in the future a spanish version of the document will be made available.

This repository and all those created in relation to it are a tribute to my late grandfather, Jorge Enrique Pazmiño Rodríguez. In his desire to see his grandsons foster a freer, fairer, and worthier society, I have tried to contribute by posting my research for the world to use. I hope it makes a difference. 

Thank you for reading, and happy analysis!