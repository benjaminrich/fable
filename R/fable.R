namesOrLabels <- function(x) {
    sapply(seq_along(x), function(i) {
        if (!is.null(attr(x[[i]], "label"))) {
            attr(x[[i]], "label")
        } else {
            names(x)[i]
        }
    })
}

render.value <- function(x, .default="") {
    suppressWarnings({
        if (is.null(x) || length(x) == 0) {
            .default
        } else {
            as.character(x)
        }
    })
}

render.count <- function(x, .default="") {
    suppressWarnings({
        if (is.null(x) || length(x) == 0) {
            .default
        } else {
            as.character(length(x))
        }
    })
}

render.npct <- function(x, pct, .default="") {
    suppressWarnings({
        if (is.null(x) || length(x) == 0) {
            .default
        } else {
            sprintf("%d (%0.1f%%)", x, pct)
        }
    })
}


#' The mythical formatted table generator.
#'
#' Creates formatted HTML tables of in a flexible, convenient way.
#'
#' @param x An object.
#' @param data A data.frame.
#' @param value A column name or position.
#' @param facets A two-sided formula of the form \code{r1 + r2 ~ c1 + c2} where
#' \code{r1, r2} specify row variables and \code{c1, c2} column variables for
#' splitting the data.
#' @param rowvars A list of row variables for splitting the data.
#' @param colvars A list of column variables for splitting the data.
#' @param render A function to render the contents of each cell to character data.
#' @param expand.along Specify the direction to expand the table when render
#' returns a (named) vector.
#' @param text A character matrix containing the textual content of each table cell.
#' @param drop If \code{TRUE} (the default), rows and columns with zero counts
#' will be dropped.
#' @param collapse.cells If \code{TRUE} (the default), row/column header cells
#' will be collapsed (merged) where appropriate.
#' @param lab Specify the contents of an extra table cell spanning
#' over all column labels.
#' @param standalone If true, a standalone document containing the table output
#' is generated displayed in the default viewer/browser. If false, generated
#' HTML is output to the console.
#' @param ... Additional arguments passed to \code{render}.
#' @return None (invisible \code{NULL}). Called for its side effects.
#'
#' @examples
#' fable(mtcars)
#' fable(mtcars, mpg, facets=(gear ~ cyl), lab="Cylinders")
#' fable(mpg ~ gear | cyl, data=mtcars, lab="Cylinders")
#' fable(rownames(mtcars) ~ gear | cyl, data=mtcars,
#'   render=paste, collapse="<br/>", lab="Cylinders")
#'
#' fable(treatment ~ rowpos | colpos, data=OrchardSprays, lab="colpos")
#' fable(paste(treatment, decrease, sep="<br/>") ~ rowpos | colpos, data=OrchardSprays, lab="colpos")
#' r <- function(x) formatC(c(Mean=mean(x), SD=sd(x)), digits=3)
#' fable(decrease ~ treatment, data=OrchardSprays, render=r, expand.along="rows")
#' fable(decrease ~ treatment, data=OrchardSprays, render=r, expand.along="columns")
#'
#' r <- function(x) sprintf("%0.3g (%0.3g)", mean(x), sd(x))
#' fable(len ~ dose | supp, data=ToothGrowth, lab="Mean (SD)",
#'   render=r)
#'
#' r <- function(x) formatC(c(Mean=mean(x), SD=sd(x)), digits=3)
#' fable(len ~ dose | supp, data=ToothGrowth, lab="Supplement Type",
#'   render=r)
#'
#' fable(len ~ dose | supp, data=ToothGrowth, lab="Supplement Type",
#'   render=r, expand.along="columns")
#'
#' @keywords utilities
#' @export
fable <- function(x, ...) {
    UseMethod("fable")
}

#' @describeIn fable The \code{data.frame} method.
#' @export
#' @importFrom stats formula model.frame na.pass
#' @importFrom Formula Formula model.part
fable.data.frame <- function(x, value, facets, ..., render, expand.along=c("rows", "columns"), drop=c("both", "rows", "columns", "none"), collapse.cells=TRUE, lab, standalone=!isTRUE(getOption("knitr.in.progress"))) {
    if (missing(value) && missing(facets)) {
        value <- unlist(as.list(format(x)))
        eg <- expand.grid(rownames(x), colnames(x))
        rowvars <- eg[, 1, drop=F]
        colvars <- eg[, 2, drop=F]
        if (missing(lab) || is.null(lab)) {
            names(rowvars) <- " " # Avoid displaying anything in the row label header
        } else {
            names(rowvars) <- lab # In this case use lab for row label header instead
        }
        lab <- NULL
    } else if (missing(value)) {
        stop("Cannot specify facets without value")
    } else if (missing(facets)) {
        stop("Cannot specify values without facets")
        f <- . ~ .
    } else {
        value <- eval(substitute(value), x)
        f <- Formula(facets)
        m <- model.frame(f, data=x, na.action=na.pass)
        rowvars <- model.part(f, data=m, lhs=1, drop=F)
        colvars <- model.part(f, data=m, rhs=1, drop=F)
    }

    fable.numeric(value, rowvars, colvars, render=render, expand.along=expand.along, drop=drop, collapse.cells=collapse.cells, lab=lab, standalone=standalone, ...)
}

#' @describeIn fable The \code{formula} method.
#' @export
#' @importFrom stats formula model.frame na.pass
#' @importFrom Formula Formula model.part
fable.formula <- function(x, data, ..., render, expand.along=c("rows", "columns"), drop=c("both", "rows", "columns", "none"), collapse.cells=TRUE, lab, standalone=!isTRUE(getOption("knitr.in.progress"))) {
    f <- Formula(x)
    m <- model.frame(f, data=data, na.action=na.pass)
    x <- model.part(f, data=m, lhs=1, drop=T)
    rowvars <- model.part(f, data=m, rhs=1, drop=F)
    if (length(f)[2] > 1) {
        colvars <- model.part(f, data=m, rhs=2, drop=F)
    } else {
        colvars <- data.frame(rep(as.character(f[[2]]), nrow(m)))
        names(colvars) <- as.character(f[[2]])
        if (!is.null(xlabel <- attr(x, "label"))) {
            colvars[,1] <- xlabel
        }
        if (missing(lab)) {
            lab <- NULL
        }
    }

    fable.numeric(x, rowvars, colvars, render=render, expand.along=expand.along, drop=drop, collapse.cells=collapse.cells, lab=lab, standalone=standalone, ...)
}

#' @describeIn fable The \code{numeric} method.
#' @export
#' @importFrom stats setNames ftable
fable.numeric <- function(x, rowvars, colvars, ..., render, expand.along=c("rows", "columns"), drop=c("both", "rows", "columns", "none"), collapse.cells=TRUE, lab, standalone=!isTRUE(getOption("knitr.in.progress"))) {

    expand.along <- match.arg(expand.along)

    tab <- table(c(rev(rowvars), rev(colvars)), dnn=c(rev(names(rowvars)), rev(names(colvars))))
    if (is.null(rowvars) || length(rowvars) == 0) {
        counts <- tab
        class(counts) <- "ftable"
        dim(counts) <- c(1, length(counts))
        attr(counts, "row.vars") <- list()
        attr(counts, "col.vars") <- setNames(list(names(tab)), names(colvars))
    } else {
        counts <- ftable(table(c(rev(rowvars), rev(colvars))), row.vars=names(rowvars), col.vars=names(colvars))
    }
    if (missing(lab)) {
        #lab <- names(colvars)[1]
        lab <- NULL
    }
    if (missing(render)) {
        if (all(counts <= 1)) {
            render <- render.value
        } else {
            render <- render.count
        }
    }
    if (expand.along == "rows") {
        text <- lapply(split(x, c(rev(rowvars), rev(colvars))), render, ...)
    } else {
        text <- lapply(split(x, c(rev(colvars), rev(rowvars))), render, ...)
    }
    stats <- names(text[[1]])
    nstats <- length(stats)
    text <- unlist(text)
    if (expand.along != "rows") {
        text <- matrix(text, nrow=nrow(counts), byrow=T)
    }

    a <- attributes(counts)
    names(a$row.vars) <- namesOrLabels(rowvars)
    names(a$col.vars) <- namesOrLabels(colvars)
    if (nstats > 0) {
        if (expand.along == "rows") {
            counts <- counts[rep(seq_len(nrow(counts)), each=nstats),, drop=F]
            a$row.vars <- c(a$row.vars, setNames(list(stats), "Statistic"))
        } else {
            counts <- counts[,rep(seq_len(ncol(counts)), each=nstats), drop=F]
            a$col.vars <- c(a$col.vars, setNames(list(stats), "Statistic"))
        }
    }
    a$dim <- dim(counts)
    attributes(counts) <- a
    attributes(text) <- a

    fable.ftable(counts, text=text, drop=drop, collapse.cells=collapse.cells, lab=lab, standalone=standalone)
}

#' @describeIn fable The \code{ftable} method.
#' @export
#' @importFrom stats ftable
fable.ftable <- function(x, text=matrix(as.character(x), nrow(x)), drop=c("both", "rows", "columns", "none"), collapse.cells=TRUE, lab, standalone=!isTRUE(getOption("knitr.in.progress"))) {
    if (missing(lab)) {
        lab <- NULL
    }
    if (standalone) {
        viewer <- getOption("viewer")
        if (is.null(viewer)) {
            viewer <- utils::browseURL
        }
        dir <- tempfile()
        dir.create(dir)
        html.file <- file.path(dir, "fable.html")
        cat(file=html.file, append=TRUE, html.standalone.head.ez)
        utils::capture.output(file=html.file, append=TRUE,
            .fable.ftable.internal(
                x              = x,
                text           = text,
                drop           = drop,
                collapse.cells = collapse.cells,
                lab            = lab))
        cat(file=html.file, append=TRUE, html.standalone.foot.ez)
        viewer(html.file)
    } else {
        .fable.ftable.internal(
            x              = x,
            text           = text,
            drop           = drop,
            collapse.cells = collapse.cells,
            lab            = lab)
    }
}

html.standalone.head.ez <- '
<!DOCTYPE html>
<html xmlns="http://www.w3.org/1999/xhtml">
<head>
<meta charset="utf-8">
<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
<meta name="generator" content="fable" />
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>fable Output</title>
<style type="text/css">
table {
    font-family: "Times New Roman", Times, serif;
    font-size: 9pt;
    border-collapse: collapse;
    padding: 0px;
    margin: 0px;
}
th {
    background-color: #ccc;
}
th,  td {
    border: 1pt solid black;
    padding: 0.1ex 2ex;
    text-align: center;
    white-space:nowrap;
}
.rowlabel {
    text-align: left;
}
.tabletitle {
    font-family: "Times New Roman", Times, serif;
    font-size: 12pt;
    font-weight: bold;
}

.tablefooter {
    font-family: "Times New Roman", Times, serif;
    font-size: 9pt;
}
</style>
</head>
<body>
'

html.standalone.foot.ez <- '
<!-- dynamically load mathjax for compatibility with self-contained -->
<script>
  (function () {
    var script = document.createElement("script");
    script.type = "text/javascript";
    script.src  = "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML";
    document.getElementsByTagName("head")[0].appendChild(script);
  })();
</script>
</body>
</html>
'


.fable.ftable.internal <- function(x, text=matrix(as.character(x), nrow(x)), drop=c("both", "rows", "columns", "none"), collapse.cells=TRUE, lab) {

    if (!inherits(x, "ftable")) stop("'x' must be an \"ftable\" object")
    if (!all.equal(dim(x), dim(text))) stop("'x' and 'text' must be have the same dimensions")

    drop <- match.arg(drop)

    xrv <- attr(x, "row.vars")
    xcv <- attr(x, "col.vars")

    rlab <- rev(expand.grid(rev(xrv), stringsAsFactors=F))
    clab <- rev(expand.grid(rev(xcv), stringsAsFactors=F))

    zr <- apply(x, 1, sum) == 0
    zc <- apply(x, 2, sum) == 0

    if (drop == "both") {
        text <- text[!zr, !zc, drop=F]
        rlab <- rlab[!zr, , drop=F]
        clab <- clab[!zc, , drop=F]
    } else if (drop == "rows") {
        text <- text[!zr, ]
        rlab <- rlab[!zr, ]
    } else if (drop == "columns") {
        text <- text[, !zc]
        clab <- clab[, !zc]
    }

    collapseLabels <- function(lab) {
        res <- lapply(seq_along(lab), function(i) {
            z <- lab[,i]
            z2 <- apply(lab[,1:i, drop=F], 1, paste0, collapse=".")
            n <- length(z)
            z[c(FALSE, z2[-1] == z2[-n])] <- ""
            z
        })
        attributes(res) <- attributes(lab)
        res
    }

    if (collapse.cells) {
        rlab <- collapseLabels(rlab)
        clab <- collapseLabels(clab)
    }

    makeRowLabelTags <- function(rlab) {
        lapply(seq_along(rlab), function(i) {
            z <- rlab[,i]
            ind <- z != ""
            span <- table(cumsum(ind))
            sp <- ifelse(span > 1, sprintf(" rowspan=\"%d\"", span), "")
            cl <- sprintf(" class=\"rowlabel l%drl\"", length(rlab) - i + 1)
            td <- "td"
            tags <- paste0("<", td, sp, cl, ">", z[ind], "</", td, ">\n")
            z[ind] <- tags
            z
        })
    }

    makeColLabelTags <- function(clab) {
        lapply(seq_along(clab), function(i) {
            z <- clab[,i]
            ind <- z != ""
            span <- table(cumsum(ind))
            sp <- ifelse(span > 1, sprintf(" colspan=\"%d\"", span), "")
            cl <- sprintf(" class=\"collabel l%dcl\"", length(clab) - i + 1)
            td <- "th"
            tags <- paste0("<", td, sp, cl, ">", z[ind], "</", td, ">\n")
            z[ind] <- tags
            z
        })
    }

    rltags <- makeRowLabelTags(rlab)
    cltags <- makeColLabelTags(clab)

    makeRowLabelHeadTags <- function(rhlab, span) {
        sp <- if (span > 1) sprintf(" rowspan=\"%d\"", span) else ""
        cl <- " class=\"rowlabelhead\""
        td <- "th"
        tags <- paste0("<", td, sp, cl, ">", rhlab, "</", td, ">\n")
        tags
    }


    if (!missing(lab) & !is.null(lab)) {
        span <- ncol(text)
        sp <- if (span > 1) sprintf(" colspan=\"%d\"", span) else ""
        cl <- " class=\"lab\""
        td <- "th"
        tags <- paste0("<", td, sp, cl, ">", lab, "</", td, ">\n")
        cltags <- c(tags, cltags)
    }

    rlhtags <- makeRowLabelHeadTags(names(xrv), length(cltags))

    cat("<table>\n")
    for (i in seq_along(cltags)) {
        tags <- cltags[[i]]
        if (i == 1) {
            for (j in rev(seq_along(rlhtags))) {
                tags <- c(rlhtags[j], tags)
            }
        }
        cat(paste0("<tr>\n", paste0(tags, collapse=""), "</tr>\n", collapse=""))
    }

    dat <- as.matrix(text)

    for (i in 1:nrow(dat)) {
        td <- "td"
        tags <- paste0("<", td, ">", dat[i,], "</", td, ">\n")
        for (j in rev(seq_along(rltags))) {
            tags <- c(rltags[[j]][i], tags)
        }
        cat(paste0("<tr>\n", paste0(tags, collapse=""), "</tr>\n", collapse=""))
    }
    cat("</table>\n")
}


