test_that("test if we can create and train scvi model using CreateSCVI", {
    data("object", package = "scvitools")
    pbmc <- object
    model <- CreateSCVI(pbmc, max_epochs = 1)
    testthat::expect_true(model$is_trained)
})

test_that("test if we can create scvi model using CreateSCVI", {
    data("object", package = "scvitools")
    pbmc <- object
    model <- CreateSCVI(pbmc, max_epochs = 1, train = FALSE)
    testthat::expect_false(model$is_trained)
})

test_that("test if we can run RunSCVI", {
    data("object", package = "scvitools")
    pbmc <- object
    model <- CreateSCVI(pbmc, max_epochs = 1)
    pbmc <- RunSCVI(pbmc, scvi_model = model)
    testthat::expect_equal(Seurat::Reductions(object = pbmc), "scvi")
})
