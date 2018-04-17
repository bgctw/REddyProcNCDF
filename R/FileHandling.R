# copied from REddyProc because not exported from there

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ File handling
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fInitFilesDir <- function(
  ##description<<
  ## Get all available files with specific file extension in directory
  Dir.s           ##<< Directory (string) to be searched for files
  , lFileExt.s     ##<< File extension (string) specification
  , fixed = TRUE	  ##<< set to FALSE, if using regular expressions
)
  ##author<<
  ## AMM
{
  # List files in path and grep files with specified file extension as character string
  list.files(path = Dir.s)[grep(lFileExt.s, list.files(path = Dir.s), fixed = fixed)]
  ##value<<
  ## Character vector with names of all available site files.
}

#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fStripFileExtension <- function(
  ##description<<
  ## Strip file extension
  lFiles.V.s     ##<< String vector with names of all available site files
)
  ##author<<
  ## AMM
{
  # RegExp: Search for first dot and replace rest of string with nothing
  sub('[.]. * ', '', lFiles.V.s)
  ##value<<
  ## Character vector containing the first part of file names (before first dot in file name).
}


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fSetFile <- function(
  ##description<<
  ## Set file name with path and check if directory and / or file exists
  FileName.s            ##<< File name as a string
  , Dir.s                ##<< Directory as a string
  , IO.b                 ##<< Input / output flag, TRUE for input, FALSE for output
  , CallFunction.s = ''    ##<< Name (string) of the caller function for warnings
)
  ##author<<
  ## AMM
  # TEST: Dir.s <- 'inst / examples'; FileName.s <- 'Example_DETha98.txt'; IO.b <- T; CallFunction.s <- 'test'
{
  # Check if string for directory provided
  Dir.b <- fCheckValString(Dir.s)

  # Check if directory exists
  if (IO.b && Dir.b && (file.access(Dir.s, mode = 4) != 0))
    stop(CallFunction.s, ':::fSetFile::: Directory does not exist: ', Dir.s)

  # Make directory if mode is output
  if ( !IO.b && Dir.b && (file.access(Dir.s, mode = 0) != 0) ) {
    dir.create(Dir.s)
    message(CallFunction.s, ':::fSetFile::: Directory created: ', Dir.s)
    if (file.access(Dir.s, mode = 2) != 0)
      stop(CallFunction.s, ':::fSetFile::: Directory could not be created: ', Dir.s)
  }

  # Set file name accordingly
  File.s <- if (Dir.b) file.path( Dir.s, FileName.s ) else FileName.s

  # If input file, check if file exists
  if (IO.b && (file.access(File.s, mode = 4) != 0) )
    stop(CallFunction.s, ':::fSetFile::: File does not exist or has no read permission: ', File.s)

  File.s
  ##value<<
  ## Returns name of file with complete path.
}


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+++ Variable check functions
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

fCheckValString <- function(
  ##description<<
  ## Check if variable is a non-empty character string
  Value.s               ##<< Value to be checked if string
)
  ##author<<
  ## AMM
  ##details<<
  ## See test_CheckValue.R for more details.
{
  if ( length(Value.s) == 0) {
    FALSE
  } else if (!is.na(Value.s) && (!is.character(Value.s) || !nzchar(Value.s)) ) {
    FALSE
  } else {
    TRUE
  }
  ##value<<
  ## Boolean value if true of false.
}

