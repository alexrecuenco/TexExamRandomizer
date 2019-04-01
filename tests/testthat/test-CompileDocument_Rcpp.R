context("test-compiledocument_rcpp")


# As a temporary solution to allow for faster restructuring, we just check whether the files are the same as the loaded ones

test_that("Backwards compatibility works", {

    compiled_structures <- list()

    for (document in documents_to_test) {
        doc <- document$doc
        layername <- document$layername
        layercmd <- document$layercmd

        compiled_structure <- TexExamRandomizer::StructureDocument(doc, layersNames = layername, layersCmd = layercmd)

        v1_result <- readRDS(document$v1resultfile)

        expect_equal(compiled_structure, v1_result)



        compiled_structures <- append(compiled_structures, list(compiled_structure))
    }
})
