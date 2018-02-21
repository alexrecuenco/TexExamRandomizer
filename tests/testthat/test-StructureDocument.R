


testthat::test_that("Testing documents can be reconstructed", {
    testthat::expect_equal(
        unlist(
            TexExamRandomizer::StructureDocument(
                document_test_4$doc,
                layersNames = document_test_4$layername,
                layersCmd = document_test_4$layercmd),
            use.names = FALSE
            ),
        document_test_4$doc
        )
}
)