
## <i class="fa fa-map" aria-hidden="true"></i> PHPpipe package

**PHPpipe** is a R package for **Pipeline of Precision Health Project in CMUH** .....

## <i class="fa fa-rocket" aria-hidden="true"></i> Installation

The current github version of **PHPpipe** is: 0.1.0 and can be installed via

``` r
library(devtools)
install_github("Jimmybbww/PHPpipe")
```

---

## <i class="fa fa-rocket" aria-hidden="true"></i> Example

Create Sample Sheet

``` r
path= system.file('extdata', 'Array_50_example_toSampleSheet.txt', package = 'PHPpipe')
PHPpipe::SampleSheet(txt = path, GT = 5, plt = 50, barcode = 12345678, outPath = './')
```

---