

testthat::test_that("Documents can be recombined", {
    testthat::expect_equal(
        unlist(
            TexExamRandomizer::StructureDocument(
                document_test_4,
                layersNames = document_test_4_options$layername,
                layersCmd = document_test_4_options$layercmd),
            use.names = FALSE
            ),
        document_test_4
        )
}
)