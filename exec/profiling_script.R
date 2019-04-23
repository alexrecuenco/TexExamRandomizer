# Profiling compilation
#
#
# library(profvis)


profile_results <- function() {

    compile.dir <- system.file(
        "extdata",
        "ExampleTexDocuments",
        package = "TexExamRandomizer"
    )

    temporal_directory <- tempdir()
    file.copy(compile.dir, to = temporal_directory, recursive = TRUE)


    owd <- getwd()
    if (!is.null(owd)) {
        on.exit(setwd(owd), add = TRUE)
        setwd(compile.dir)
    } else {
        stop("What? We couldn't detect what is the current working directory")
    }

    files <- dir(full.names = TRUE)
    files <- files[grepl(files, pattern = "\\.tex$")]


    create_exams <- function(times = 3)  {
        for (i in rep(1, times = times)) {
            for (file in files) {
                opt <- list(options = list(file = file))
                TexExamRandomizer::compilation_options(file = file, compile = FALSE)

                TexExamRandomizer::jsonexamparser(opt = opt)


            }


        }
    }

    profvis::profvis({create_exams()},interval = 0.007)



}


profvis_result <- profile_results()
print(profvis_result)

