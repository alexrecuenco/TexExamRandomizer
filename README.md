# TexExamRandomizer 
[![CRAN](https://www.r-pkg.org/badges/version/TexExamRandomizer)](https://cran.r-project.org/package=TexExamRandomizer)
[![Build Status](https://travis-ci.org/alexrecuenco/TexExamRandomizer.svg?branch=master)](https://travis-ci.org/alexrecuenco/TexExamRandomizer)
[![Downloads](http://cranlogs.r-pkg.org/badges/TexExamRandomizer)](http://www.r-pkg.org/pkg/TexExamRandomizer)

[comment]:?color=brightgreen 

_R package used to randomize and grade `LaTeX` exams and homework automatically._

It randomizes `LaTeX` exams with a flexible set of options that can be provided directly in the document, using a simple JSON-format.
An example of options could be:

- Write personal information from students gathered previously on a table automatically.
- Randomize the order of sections, questions, choices, etc. Choosing which of those are fixed and which ones are not.
- Select the number of output questions


The package provides some vignettes that go through the options that are available for the users. Goign through them might give a nice overview of the available options.

There are also examples of different exam formats inside `inst/extdata`, that might be good to look at before deciding whether to use it or not.

## Installation

### From github

To install the latest development version from github, 

```r
devtools::install_github("alexrecuenco/TexExamRandomizer")
```

### From CRAN

Otherwise, to download the latest stable version uploaded on CRAN

```r
install.packages("TexExamRandomizer")
```

## Documentation

The basic documentation can be found on the vignettes provided with this class. Once downloaded, write on your R terminal the command

```r
vignette(package = "TexExamRandomizer")
```
to see all the documentation available.

  
## Using the package with TexShop (In MAC OS)


This package includes in the `exec/` folder a couple of `.engine` files that are intended to be used with TexShop. 
Once installed, you won't need to move outside of your TexShop environment

To install it:

1. You first have to copy the files `exec/examrandomizer` and `exec/gradeexamrandomizer` in some folder that can be found by your `$PATH` variable. 

    I don't recommend adding the `exec/` folder to the `$PATH` variable, because when you update your R distribution, you would need to update again everything. On the other hand, when this software is updated, those files can stay where they are. You won't need to replace those files again.
    
2. After you have done that, you need to add both `.engines` files in your Engines folder from your TexShop distribution.

    In my case, that folder is `~/Library/TexShop/Engines`. (You will see a bunch of the other `.engines` files already in there)
    
3. To use the software, now that it is installed, you simply write your exams in `LaTeX`. 

   You will have to specify in JSON format at the start of your exam what is your exam format (unless you are using the default exam class format). The possible options are described in
   
   ```r
   vignette("ExamOptions", package = "TexExamRandomizer")
   ``` 

4. When your exam is ready for compilation, and you have tested it compiles with your `LaTeX` engine. Switch the engine in the engine window in TexShop to `examrandomizer` and compile using that engine. It will automatically generate a folder with all the exam versions, as well as an answer sheet and a file with the options that it used.

## Using the package in Windows

I managed to make it work with TexMaker once, although it wasn't pretty.

1. I first had to make sure that I added a folder, such as `C:\\TexRandomizer` with the `examrandomizer` and `gradeexamrandomizer` files copied inside them.

2. Then, I added a script in TexMaker that calls those scripts directly. You first need to find where Rscript is located on your installation path.

   Then, the script will look something similar to
    
    ```bash
    C:\\<Rscript location> C:\\TexRandomizer\examrandomizer --file "$1" --compile
    ```

3. You could then use that script to call the engine directly from the TexMaker menu, wihtout having to open R at any moment.
