



document_test_default <-
    list(
        testype = "Default options",
        doc = c(
            "line1",
            "line2",
            "\\begin{document}",
            "text \\hi %\\begin{questions} text not dragged",
            "this should go \\begin{questions}",
            "\\question hi",
            "\\begin{choices}",
            "\\choice",
            "\\CorrectChoice",
            "% This shoudlbe ignored \\choice \\CorrectChoice",
            "\\end{choices}",
            "\\end{questions}",
            "text dragged by second question",
            "this should go \\begin{questions}",
            "\\question hi",
            "\\begin{choices}",
            "\\choice",
            "\\CorrectChoice",
            "% This should be with the previous choicebe ignored \\choice \\CorrectChoice",
            "\\end{choices}",
            "\\begin{choices}",
            "\\choice -_-",
            "\\CorrectChoice",
            "\\end{choices}",
            "\\begin{choices}",
            "\\choice -_-",
            "\\CorrectChoice",
            "line should go in CorrectChoice",
            "\\end{choices}",
            "\\begin{choices}",
            "\\choice -_-",
            "line should go in choice",
            "\\CorrectChoice",
            "\\end{choices}",
            "\\questions line should not be considered a question",
            "line should stay with the question",
            "\\begin{choices}",
            "line should go in pre",
            "\\choice -_-",
            "\\CorrectChoice",
            "\\end{choices}",
            "\\end{questions}",
            "this should not happen go \\begin{questionss}",
            "\\end{questionss}",
            "\\end{document}"

        ),
        layername = c("questions", "choices"),
        layercmd = c("question", "(choice|CorrectChoice)"),
        v1resultfile = "document_test_default.rds"
    )

document_test_4 <-
    list(
        testtype = "Testing document with four layers",
        doc =
            c(
                "line1",
                "line2",
                "\\begin{document}",
                "\\begin{layerone}",
                "\\layeronecmd hi",
                "\\begin{layertwo}",
                "\\layertwocmd",
                "\\begin{layerthree}",
                "\\layerthreecmd",
                "\\layerthreecmd",
                "\\begin{layerfour}",
                "\\layerfourcmd",
                "\\layerfourcmd",
                "\\end{layerfour}",
                "\\end{layerthree}",
                "\\layertwocmd",
                "% This shoudlbe ignored \\choice \\CorrectChoice",
                "\\end{layertwo}",
                "\\layertwocmd should be ignored",
                "\\layertwocmd should be ignored",
                "\\layeronecmd should be ignored",
                "% This shoudlbe ignored \\choice \\CorrectChoice",
                "\\end{layerone}",
                "\\end{document}"
            ),
        layername = c("layerone", "layertwo", "layerthree", "layerfour"),
        layercmd = c("layeronecmd", "layertwocmd", "layerthreecmd", "layerfourcmd"),
        v1resultfile = "document_test_4.rds"

    )

documents_to_test <- list(document_test_default, document_test_4)
