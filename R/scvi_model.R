#' Create and train SCVI model
#'
#' This function create and train a scvi model.
#' @param max_epochs Number of passes through the dataset.
#'       If NULL, defaults to min(round(c(round((20000 / n_cells) * 400), 400)))
#' @param early_stopping Perform early stopping.
#' @param use_gpu Use default GPU if available (if NULL or TRUE),
#'                or index of GPU to use (if int),
#'                or name of GPU (if string), or use CPU (if FALSE).
#' @param train If TRUE the model will be created and trained.
#' @concept Create scvi-tools model
#' @importFrom reticulate r_to_py
#' @rdname CreateSCVI
#' @export
CreateSCVI <- function(object, ...) {
    UseMethod("CreateSCVI")
}

#' @rdname CreateSCVI
#' @return Return a trained SCVI model.
#' @export
CreateSCVI.Seurat <- function(
        object,
        main_layer = "counts",
        max_epochs = NULL,
        use_gpu = NULL,
        train = TRUE,
        early_stopping = FALSE
) {
    # if (!requireNamespace('Seurat')) {
    #     stop("Running SCVI on a Seurat object requires Seurat")
    # }

    # creating the scanpy data
    adata <- sceasy::convertFormat(object, from = "seurat",
                                   to = "anndata",
                                   main_layer = main_layer,
                                   drop_single_values = FALSE)

    # setting up the scvi model
    scvi()$model$SCVI$setup_anndata(adata)

    model <- scvi()$model$SCVI(adata)

    if (train == TRUE) {
        model$train(max_epochs = is.integer(max_epochs),
                    use_gpu = reticulate::r_to_py(use_gpu),
                    early_stopping = reticulate::r_to_py(early_stopping))
    }

    return(model)
}

#' scvi-tools single cell
#'
#' Run dimentionality reduction usinc SCVI model
#' @param scvi_model A scvi_model
#' @concept Run scvi-tools model
#' @param ... other parameters
#' @rdname RunSCVI
#' @export
RunSCVI <- function(object, ...) {
    UseMethod("RunSCVI")
}

#' @rdname RunSCVI
#' @param object A seurat object
#' @return Return a Seurat object with the computed embedding
#'         with all the measure computed. For downstream Seurat analyses,
#'         use reduction='scvi'.
#' @export
RunSCVI.Seurat <- function(
        object,
        scvi_model = NULL,
        reduction.name = "scvi",
        reduction_key = "scvi_"
) {
    if (is.null(scvi_model)) {
        stop(paste0(
            "You have to pass a scvi model. The model can be"),
            "traine or not.")
    }

    scvi_model <- reticulate::r_to_py(scvi_model)
    if (!reticulate::py_to_r(scvi_model$is_trained)) {
        message("The model is not trained. Il will be trained now")
        scvi_model$train(max_epochs = max_epochs,
                    use_gpu = r_to_py(use_gpu),
                    early_stopping = r_to_py(early_stopping))
    }

    latent <- scvi_model$get_latent_representation()
    latent <- as.matrix(latent)
    rownames(latent) <- colnames(object)
    object[[reduction.name]] <-
    CreateDimReducObject(embeddings = latent,
                         key = reduction_key,
                         assay = DefaultAssay(object))
return(object)
}

