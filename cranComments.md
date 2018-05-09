## Test environments

* local Mint 18, R 3.4.4
* R-devel-san docker image
* Travis ci: Ubuntu 14.04.5 LTS 
* win_builder: x86_64-w64-mingw32

## R CMD check results
1 Note:
The Possibly mis-spelled words in DESCRIPTION are correct.

## Response to CRAN review

RC1: Possibly mis-spelled words in DESCRIPTION:
  REddyProc (4:43, 9:27)
Please single quote software names in Title and Description field.

AC: We put REddyProc in single quotes in the Title and Description field.

RC2: Unknown, possibly mis-spelled, fields in DESCRIPTION:
  'Remotes'
AC: We removed the Remotes field before submission to CRAN
  

