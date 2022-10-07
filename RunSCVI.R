#' @title Run che scvi function
#' @description
#'              \code{\link{}}
#'
#'
#' @param object The ScreenR object obtained using the
#'                       \code{\link{create_screenr_object}}


#' @concept Run
#' @export
#' @rdname RunSCVI
#' @examples
#' object <- get0("object", envir = asNamespace("ScreenR"))
#' metrics <- compute_metrics(object,
#'     control = "TRT",
#'     treatment = "Time3", day = "Time3"
#' )
#' head(metrics)
RunSCVI <- function(object, ...) {
    UseMethod("RunSCVI")
}

#' @rdname RunSCVI
#' @param max_epochs Number of passes through the dataset.
#'       If NULL, defaults to min(round(c(round((20000 / n_cells) * 400), 400)))
#' @param early_stopping Perform early stopping.
#' @param use_gpu Use default GPU if available (if NULL or TRUE),
#'                or index of GPU to use (if int),
#'                or name of GPU (if string), or use CPU (if FALSE).
#' @return Return a Seurat object with the computed embedding
#'         with all the measure computed. For downstream Seurat analyses,
#'         use reduction='scvi'.
#' @export
RunSCVI.Seurat <- function(
        object,
        main_layer = "counts",
        max_epochs = NULL,
        use_gpu = NULL,
        early_stopping = FALSE

) {
    # if (!requireNamespace('Seurat')) {
    #     stop("Running SCVI on a Seurat object requires Seurat")
    # }

    assay.use <- Seurat::DefaultAssay(object)

    # creating the scanpy data
    adata <- sceasy::convertFormat(object, from = "seurat",
                                   to = "anndata",
                                   main_layer = main_layer,
                                   drop_single_values = FALSE)

    # setting up the scvi model
    scvi$model$SCVI$setup_anndata(adata)

    model <- scvi$model$SCVI(adata)

    # Tansform the R inpti into python input
    if (is.null(max_epochs)) {
        max_epochs <-  r_to_py(max_epochs)
    } else if(is.numeric(max_epochs)) {
        max_epochs <-  as.integer(max_epochs)
    }
    model$train(max_epochs = max_epochs,
                use_gpu = r_to_py(use_gpu),
                early_stopping = r_to_py(early_stopping))

    latent <- model$get_latent_representation()


    latent <- as.matrix(latent)
    rownames(latent) <- colnames(object)
    object[["scvi"]] <-
        CreateDimReducObject(embeddings = latent,
                             key = "scvi_",
                             assay = DefaultAssay(object))

    return(object)
}
