[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/reyzaguirre/st4gi?branch=master)](https://ci.appveyor.com/project/reyzaguirre/st4gi)
[![Travis-CI Build Status](https://travis-ci.org/reyzaguirre/st4gi.png?branch=master)](https://travis-ci.org/reyzaguirre/st4gi)
[![Coverage Status](https://img.shields.io/coveralls/reyzaguirre/st4gi.svg)](https://coveralls.io/r/reyzaguirre/st4gi?branch=master)

pepa
====
Package for the Execution of Pre Cooked Analysis.

Installation
------------
To install this package you need package `devtools`:

```{r eval=F}
install.packages("devtools")
```

Then type:

```{r eval=F}
devtools::install_github(c("reyzaguirre/st4gi", "reyzaguirre/pepa"))
```
because `pepa` depends on `st4gi`.

Usage
-----
To load the package type:

```{r eval=F}
library(pepa)
```

and have some fun typing something like:

```{r eval=F}
y <- rnorm(30)
d <- data.frame(a = gl(10, 3), b = rep(c(1,2,3), 10), y = y)
pty(y)
pty(d)
pty(aov(y ~ a + b, data = d))
```

To see the documentation type:

```{r eval=F}
help(package = pepa)
```

