#' Draw Venn Diagram of two or three character vectors
#'
#' @param x A list of character vectors.
#' @param cat.name A character vector of category names
#' @param f.name A string. output file name.
#' @return A png file. Venn Diagram of two or three sets
#' @examples
#' x1 = as.character(c(1:5))
#' x2 = as.character(c(3:10))
#' draw_venn(list(x1, x2), cat.name=c("x1", "x2"), f.name="test.venn.png")
#' @export
#' @description
#' utility function to draw Venn diagram of two or three character vectors.
#' User need to specify category names and file name to store png file.
#' @author Seongyong Park (2020-08-18)

draw_venn <- function(x, cat.name, f.name){
  if(length(x) == 2){
    cols = c("blue", "red")
  } else if(length(x) == 3){
    cols = c("blue", "green", "red")
  } else if(length(x) == 4){
    cols = c("blue", "green", "red", "cyan")
  } else {
    print("input vecter should be <= 4")
  }

  venn.diagram(
    x = x,
    category.names = cat.name,
    col = "transparent",
    fill = cols,
    alpha = 0.30,
    print.mode=c("raw","percent"),
    filename = f.name,
    imagetype="png",
    output=TRUE,
  )
}

#' Differential Gene Expression Analysis by Limma
#'
#' @param gex A gene x sample data frame. rownames are gene or probe ids and colnames are sample ids.
#' @param label A factor. Indicates class label of samples. It should be ordered as sample ids in gex data frame.
#' @return DEG data frame which contains logFC, AveExpr, t, P.Value, adj.P.Val (by Benjamin-Hochberg correction)
#' @examples
#' gex = do.call(rbind, lapply(1:100, function(i) rnorm(100, sample(1:10), 2)))
#' rownames(gex) = paste0("gid", 1:100)
#' colnames(gex) = paste0("sid", 1:100)
#' label = factor(c(rep("ALL", 50), rep("AML", 50)), levels=c("ALL", "AML"))
#' limma_deg(gex, label)
#' @export
#' @description
#' utility function to get differential expression table from limma function in limma package.
#' @author Seongyong Park (2020-08-18)
limma_deg <- function(gex, label){
  design = model.matrix(~0+label)
  colnames(design) = levels(label)
  fit <- lmFit(gex, design)
  contrast <- paste(levels(label)[2], levels(label)[1], sep = "-")
  contrast.matrix <- makeContrasts(contrasts = contrast, levels = design)
  fitE <- contrasts.fit(fit, contrast.matrix)
  fitE <- eBayes(fitE)
  tT <- topTable(fitE, adjust = "BH", sort.by = "B", number = Inf)

  return(tT)
}
