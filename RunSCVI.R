#' SCVI single cell integration
#'
#' Run SCVI algorithm with Seurat.
#'

RunSCVI <- function(object, ...) {
    UseMethod("RunSCVI")
}


RunSCVI.Seurat <- function(
        object,
        conda_env = NULL,
        python = NULL, 
        main_layer = "counts",
) {
    if (!requireNamespace('Seurat', quietly = TRUE)) {
        stop("Running SCVI on a Seurat object requires Seurat")
    }
    assay.use <- assay.use %||% Seurat::DefaultAssay(object)
    
    if (!requireNamespace('reticulate', quietly = TRUE)) {
        stop("Running SCVI requires reticulate")
    }
    
    if (!is.null(conda_env)) {
        reticulate::use_condaenv(conda_env)
    }
    
    if (!is.null(python)) {
        reticulate::use_python(python)
    }
    
    # Importing scanpy
    sc <- reticulate::import("scanpy", convert = FALSE)
    scvi <- reticulate::import("scvi", convert = FALSE)
    
    
    adata <- sceasy::convertFormat(object, from = "seurat", 
                                   to = "anndata", 
                                   main_layer = main_layer, 
                                   drop_single_values = FALSE)
    
    
    scvi$model$SCVI$setup_anndata(adata)
    
    model <- scvi$model$SCVI(adata)
    
    model$train(max_epochs = )
    
    latent <- model$get_latent_representation()
    
    
    latent <- as.matrix(latent)
    rownames(latent) <- colnames(object)
    object[["scvi"]] <- 
        CreateDimReducObject(embeddings = latent, 
                             key = "scvi_", 
                             assay = DefaultAssay(object))
    
    return(object)
}
