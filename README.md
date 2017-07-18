![R Package alternatives](https://img.shields.io/badge/R%20Package-alternatives-blue.svg)
[![Licence](https://img.shields.io/badge/license-GPLv3-orange.svg)](http://www.gnu.org/licenses/gpl-3.0.html)

# alternatives
Alternative functions to make R even easier

# Included alternative functions

### library0
This is an alternative function to [library](https://stat.ethz.ch/R-manual/R-devel/library/base/html/library.html), which pretends to be an easier way to attach and install a package in the same step.
```R
library0(rgdal)
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
