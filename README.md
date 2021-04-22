
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

DNA extract QC

``` r
# create folder
dir.create(file.path('./','Pass')); dir.create(file.path('./','Fail'))

# get example file path
path.std= system.file('extdata','OpticsSampleData_example.csv', package = 'PHPpipe')
path.re= system.file('extdata','DNA re-test_example.csv', package = 'PHPpipe')

# standard format
PHPpipe::DNAextractQC(path = path.std, outPath = './', type = 1)
# re-test format
PHPpipe::DNAextractQC(path = path.re, outPath = './', type = 2)
```

---

Create Sample Sheet

``` r
path= system.file('extdata', 'Array_50_example_toSampleSheet.txt', package = 'PHPpipe')
PHPpipe::SampleSheet(txt = path, GT = 5, plt = 50, barcode = 12345678, outPath = './')
```

---

