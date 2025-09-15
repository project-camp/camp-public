#' Perform Quick Discriminant Analysis
#' 
#' @description
#' This function performs either Linear Discriminant Analysis (LDA) or Quadratic 
#' Discriminant Analysis (QDA) based on multivariate normality and homoscedasticity tests.
#' 
#' @param dataset A numeric matrix or data frame containing the predictor variables
#' @param group_data A factor vector containing the group assignments
#' @param compositional_center Logical, whether to return compositional means (default: FALSE)
#' @param orig Original compositional data for inverse transformation (default: NULL)
#' @param predict Logical, whether to return predictions (default: FALSE)
#' 
#' @return A list containing:
#' \itemize{
#'   \item prior: Prior probabilities of group membership
#'   \item means: Group means (either in ILR or original space)
#'   \item predictions: If predict=TRUE, predicted classifications
#' }
#' 
#' @examples
#' \dontrun{
#' data(iris)
#' result <- quick_discriminant_analysis(iris[,1:4], iris$Species)
#' }
#' 
#' @importFrom dplyr %>% group_by do
#' @importFrom MVN mvn
#' @importFrom biotools boxM
#' @importFrom MASS lda
#' @importFrom rrcov ldaPP qdaClass
#' 
#' @export
quick_discriminant_analysis <- function(dataset, group_data, show_test_results = TRUE,
                                      compositional_center = FALSE, orig = NULL,
                                      predict = FALSE) {
    # Early validation returns
    if (!is.data.frame(dataset) && !is.matrix(dataset)) {
        stop("dataset must be a data frame or matrix")
    }
    if (!is.factor(group_data)) {
        group_data <- as.factor(group_data)
    }
    if (nrow(dataset) != length(group_data)) {
        stop("Number of rows in dataset must match length of group_data")
    }
    if (compositional_center && is.null(orig)) {
        stop("orig must be provided when compositional_center = TRUE")
    }

    # Pre-allocate results list with known components
    results <- list(
        mvn = NULL,
        boxm = NULL,
        method = NULL,
        model = NULL,
        priors = NULL,
        means = NULL,
        predict = NULL
    )
    
    # More efficient group-wise normality testing
    dataset_matrix <- as.matrix(dataset)
    unique_groups <- levels(group_data)
    mvn_results <- vector("list", length(unique_groups))
    
    for (i in seq_along(unique_groups)) {
        group_idx <- group_data == unique_groups[i]
        mvn_results[[i]] <- MVN::mvn(dataset_matrix[group_idx, ], mvnTest = "energy")$multivariateNormality
    }
    
    results$mvn <- do.call(rbind, lapply(seq_along(unique_groups), function(i) {
        res <- mvn_results[[i]]
        res$Group <- unique_groups[i]
        res
    }))
    rownames(results$mvn) <- NULL

    # Test homoscedasticity with error handling
    results$boxm <- tryCatch(
        biotools::boxM(dataset_matrix, group_data),
        error = function(e) {
            warning("Box's M-test failed. Defaulting to QDA.")
            list(p.value = 0)
        }
    )

    # Print test results if requested
    if (show_test_results) {
        cat("\n=== Multivariate Normality Test Results ===\n")
        print(results$mvn[, c("Group", "Statistic", "p value", "MVN")])
        cat("\n=== Homoscedasticity Test Results (Box's M-test) ===\n")
        print(results$boxm)
    }

    # Determine and perform appropriate analysis
    use_lda <- all(results$mvn$MVN == "YES") && results$boxm$p.value > 0.05
    use_ldaRob <- any(results$mvn$MVN == "NO") && results$boxm$p.value > 0.05
    use_qda <- all(results$mvn$MVN == "YES") && results$boxm$p.value < 0.05
    use_qdaRob <- any(results$mvn$MVN == "NO") && results$boxm$p.value < 0.05
    
    if (use_lda) {
        message("All assumptions met: Performing standard LDA")
        model <- lda(group_data ~ dataset_matrix)
        results$method <- "LDA"
        center_data <- model$means
    } else if (use_ldaRob) {
        message("Non-normal data detected: Performing robust LDA (PP)")
        model <- ldaPP(dataset_matrix, group_data)
        results$method <- "LDA-PP"
        center_data <- model@center
    } else if (use_qda) {
        message("Heteroscedastic normal data: Performing QDA")
        model <- qdaClass(dataset_matrix, group_data)
        results$method <- "QDA"
        center_data <- model@center
    } else {
        message("Non-normal heteroscedastic data: Performing robust QDA")
        model <- QdaCov(dataset_matrix, group_data, scale = TRUE)
        results$method <- "QDA-Robust"
        center_data <- model@center
    }

    # Store results
    results$model <- model
    results$priors <- switch(results$method,
        "LDA" = model$prior,
        "LDA-PP" = model@prior,
        "QDA" = model@prior,
        "QDA-Robust" = model@prior
    )
    
    results$means <- if (compositional_center) {
        ilrInv(center_data, orig = orig)
    } else {
        center_data
    }
    
    if (predict) {
        results$predict <- predict(model)
    }

    # Print results
    if (show_test_results) {
        cat(sprintf("=== %s ===\n", if(use_lda) "Linear Discriminant Analysis" else "Quadratic Discriminant Analysis"))
        cat("\nPrior probabilities of group membership:\n")
        print(results$priors)
        cat(sprintf("\nGroup %s:\n", 
                   if(compositional_center) "compositional centers" else "means in LR space"))
        print(results$means)
        if (predict) {
            cat("\nPredicted classifications:\n")
            print(results$predict)
        }
    }
    
    invisible(results)
}