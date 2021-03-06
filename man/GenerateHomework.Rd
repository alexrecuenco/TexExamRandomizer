% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Wrappers.R
\name{GenerateHomework}
\alias{GenerateHomework}
\title{Generate Homework}
\usage{
GenerateHomework(
  x,
  Table,
  CommandNames,
  ColumnNames,
  outputDirectory,
  outputBaseName
)
}
\arguments{
\item{x}{A character vector, each element represents one line of the latex document}

\item{Table}{Data frame from which to extract the information}

\item{CommandNames}{Character vector with the same length as \code{columnNames}}

\item{ColumnNames}{Character vector with the names of the columns to be used}

\item{outputDirectory}{The directory in which the output will be placed}

\item{outputBaseName}{The starting name for the output files

The files will look like

\code{<outputDirectory>/<outputBaseName>_00<number>.tex}

Where the number of zeros is the minimum number of zeros required to have a different version number for each file. (i.e., if there is only 45 files, it is 01-45; but with 132 files, it would be 001-132)}
}
\value{
Character vector with the file names of the output.
}
\description{
This function personalizes a 'LaTeX' document with data from a table,
generating a new file for each row which is saved on the \code{outputDirectory}.
}
\details{
The command names should be 'LaTeX' commands that are being defined through

 \code{\\newcommand\{\\<CommandNames[i]>\}\{<previous definition>\}}

 The definition of these commands will be changed to be

 \code{\\newcommand\{\\<CommandNames[i]>\}\{<Table[ColumnNames[i]][file #]>\}}

 And it will output one file for each command.

 The intent of this function was to populate information into a generic homework to personalize it for every student using 'LaTeX'.
 (It actually generalizes to maybe other problems).
}
\seealso{
\code{\link{ReplaceFromTable}} to get a better idea of how the replacement is made. To see examples of how to use it, look at the code in \code{\link{jsonhwparser}}
}
