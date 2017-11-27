# Author Alejandro Gonzalez Recuenco
# e-mail <alejandrogonzalezrecuenco@gmail.com>
# (C) 2017
#### LateX Folder Compilation Codes ####
####
####

#' @title Compiling function
#'
#' @description This function calls latexmk, which must be part of the system commands, a directory where tex files are found and outputs their pdf and other things in the pdf.dir.out
#' The functions \code{\link{CompileLatexDirEXAM}} and \code{\link{CompileLatexDirHW}} are identical wrappers of the same function, \code{\link{CompileLatexDir}}. Do not use them, they are just kept for "backwards" compatibility
#'
#' @details Write the tex files relative paths to other files as to be read from the directory in which latex.dir.in is found
#' This function is intended to be use to compile a bunch of files which are stemmed from an original one. That is why the directory
#'
#' @param pdf.dir.out Directory where the pdf output will be sent to
#' @param latex.dir.in Directory where all the tex files are found.
#' @param engine: If xelatex is not this option, pdfmaker is called.
#
#' @param compile.dir: Directory from which compilation is invoked, if not specified, the directory we are compiling will be from where we do it. (This is specially usefull since we want to mantain the same relative paths from the main file).
#' @return None
#' @author Alejandro Recuenco \email{alejandrogonzalezrecuenco@@gmail.com}
#' @keywords internal
#'
#'
#' @family Compilation functions

CompileLatexDir <- function(pdf.dir.out, latex.dir.in, engine = "xelatex", compile.dir = NULL, extracmdoptions = NULL){
  # TODO: add other commands, not only XeLaTex



  # print(normalizePath(pdf.dir.out))
  FullPdfDir <- normalizePath(pdf.dir.out)
  FullTexDir <- normalizePath(latex.dir.in)


  if (engine == "xelatex") {
    cmd_option <- "-xelatex"
  } else {
    cmd_option <- "-pdf"
  }

  cmdarg_outputdir <- sprintf("-outdir='%s'", FullPdfDir)


  cmdarg_input_tex <-
    sprintf(
      "'%s'*.tex",
      sub(
        pattern = "a$",
        x = file.path(FullTexDir, "a"),
        replacement = ""
      )
    )
  #Adding the a at the end with file.path and then replacing it with "" is a trick to have the file.path decide how to add the / or \ depending on the operating system, without us doing the checking.
  # 'Directory/'*.tex will be sent to the command line... probably we need to be more careful with quotations and so on

  extracmdoptions <- c(cmd_option, extracmdoptions)
  othercmdoption <- "-quiet"


  cmdArgs <- c(cmdarg_outputdir,
               cmdarg_input_tex,
               extracmdoptions,
               othercmdoption)
  if (is.null(compile.dir)) {
    compile.dir <- latex.dir.in
  }

  owd <- getwd() #Original Working Directory
  if (!is.null(owd)) {
    on.exit(setwd(owd), add = TRUE)
    setwd(compile.dir)
    #After the on.exit, if it causes an error setwd(owd) is still called, do not put the set before the on.exit....
  } else {
    warning("Current working directory is unknown, can't change the compilation directory and return to the same directory afterwards...")
  }




  system2(
    command = "latexmk",
    args = cmdArgs,
    wait = TRUE
  );
}



#' @rdname  CompileLatexDir
#' @keywords internal
#' @family Compilation functions
CompileLatexDirEXAM <- function(pdf.dir.out, latex.dir.in, engine = "xelatex", compile.dir = NULL, extracmdoptions = NULL){
  CompileLatexDir(pdf.dir.out, latex.dir.in, engine = engine, compile.dir = compile.dir, extracmdoptions = extracmdoptions)
}

#' @rdname CompileLatexDir
#' @keywords internal
#' @family Compilation functions
CompileLatexDirHW <- function(pdf.dir.out, latex.dir.in, engine = "xelatex", compile.dir = NULL, extracmdoptions = NULL){
    CompileLatexDir(pdf.dir.out, latex.dir.in, engine = engine, compile.dir = compile.dir, extracmdoptions = extracmdoptions)
}
