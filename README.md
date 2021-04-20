
## <i class="fa fa-map" aria-hidden="true"></i> PHPpipe package

**PHPpipe** is a R package for **Pipeline of Precision Health Project in CMUH** .....

## <i class="fa fa-rocket" aria-hidden="true"></i> Installation

The current github version of **PHPpipe** is: 0.1.0 and can be installed via

``` r
library(devtools)
install_github("Jimmybbww/PHPpipe")
```

## <i class="fa fa-rocket" aria-hidden="true"></i> Functions

### <u>f.DNAextractQC<u>
**Usage:**  
`f.DNAextractQC(path = choose.files, LowerOD = 1.65, UpperOD = 2.20, DNAcon = 10, outPath, type, db = NULL)`  


**Arguments:**  
`path`  the name of the file which the data are to be read from.  (default: `choose.files`).  
`LowerOD` lower limit of OD260/280 ratio (default: 1.65).  
`UpperOD` upper limit of OD260/280 ratio (default: 2.20).  
`DNAcon` DNA concentration (default: 10 µg/ml).  
`outPath` a character string naming a output path.  
`type` *1* : standard format/ *2* : retest format.  
`db` connection handle returned by `RODBC::odbcConnect`. (default: *NULL*).  

---
### <u>f.QCpassList<u>
**Usage:**  
`f.QCpassList(outPath, db, myFirst, myLast)`  


**Arguments:**  
`outPath` a character string naming a output path.
`db` connection handle returned by `RODBC::odbcConnect`.   
`myFirst` scanning the first workid barcode on write box.   
`myLast` scanning the last workid barcode on write box.  

---

### <u>f.Dilut2Tecan<u>
**Usage:**  
`f.Dilut2Tecan(outPath, db, array, start, appendix = F, exclude = F, n.out = 95)`  


**Arguments:**  
`outPath` a character string naming a output path.  
`db` connection handle returned by `RODBC::odbcConnect`.  
`array` array number.  
`start` the first workID to be diluting.  
`appendix`  a file containing re-diluting, manual diluting and exclusion samples.  
`exclude` a *logical* value indicating whether or not to be exclude samples.  
`n.out` maximum number of samples.  

---
### <u>f.SampleSheet<u>
**Usage:**  
`f.SampleSheet(txt= file.choose(), outPath, db = NULL, GT, plt, barcode)`  


  **Arguments:**  
  `txt` the name of the file which the data are to be read from.  (default: `choose.files`).  
  `outPath` a character string naming a output path.  
  `db` connection handle returned by `RODBC::odbcConnect`. (default: *NULL*).  
  `GT` machine number.  
  `plt` array number.  
  `barcode` scanning the barcode on the array.  

---