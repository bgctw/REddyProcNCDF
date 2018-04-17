## Test environments

* local Mint 18, R 3.4.3
* R-devel-san docker image
* Travis ci: Ubuntu 14.04.5 LTS 
* win_builder: x86_64-w64-mingw32

## R CMD check results
Note that last update was less than one day.
No further notes, warnings, nor errors.

## Reply to CRAN comments
With REddyProc 1.1.2 on CRAN there is an additional issue reported for setup noLD
https://www.stats.ox.ac.uk/pub/bdr/noLD/REddyProc.out

I added a numeric tolerance to test of .binUstar instead of testing equality of
the two numeric matrices to hopefully prevent failing the test with noLD.


