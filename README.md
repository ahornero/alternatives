![R Package alternatives](https://img.shields.io/badge/R%20Package-alternatives-blue.svg)
[![Licence](https://img.shields.io/badge/license-GPLv3-orange.svg)](http://www.gnu.org/licenses/gpl-3.0.html)

# alternatives
Alternative functions to make R scripting even easier and more user-friendly. For instance, it includes the *library0()* function which is a library function replacement to install (if needed) and load libraries in one step; and also including GitHub packages installation compatibility. There are other alternatives, such as *cat0*, *catn* or *stop0* in other to modify the behaviour of both *cat* and *stop* functions respectively.

## Included alternative functions

### library0
This is an alternative function to [library](https://stat.ethz.ch/R-manual/R-devel/library/base/html/library.html), which pretends to be an easier way to attach and install a package in the same step and it also installs the package if needed. It is also compatible with GitHub packages without using additional libraries.

You can indicate the package as a string or directly by the name. It also works with GitHub packages through the install-github.me service, which is based on [remotes](https://github.com/r-lib/remotes)

```R
# Load and install (if so) CRAN and GitHub R packages, and automatically as character strings or not
library0("ggplot2")
library0(ggplot2)
library0(hadley/devtools)
```

### cat0
`cat0` is an alternative to [cat](https://stat.ethz.ch/R-manual/R-devel/library/base/html/cat.html), as `paste0` for `paste`, without spaces.
```R
cat0('string', 'withoutspaces')
```

### catn
`cat0` is an alternative to [cat](https://stat.ethz.ch/R-manual/R-devel/library/base/html/cat.html) but including a newline by default.
```R
catn('string', 'withoutspaces', 'andanewline')
```

### stop0
This is indeed the same as [stop](https://stat.ethz.ch/R-manual/R-devel/library/base/html/stop.html) but quietly, very useful when included in the middle of a source file and you want to stop it without a warning message.
```R
stop0()
```

### last
It returns the last element of an array or dataframe.
```R
last(c(1,2,3))
```

## How to install it?
There are several options, my favourite one is just as follows:
```R
source('https://install-github.me/ahornero/alternatives')
```
Another, and also more popular way to install it:
```R
library(devtools)
install_github('ahornero/alternatives')
```
One more!
```R
library(githubinstall)
githubinstall('alternatives')
```
