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


#' Formatted tables the easy way.
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
#' @param lab Specify the contents of an extra table cell spanning
#' over all column labels.
#' @param footnote A character string to be added as a footnote to the table.
#' The default is to omit the footnote.
#' @param expand.along Specify the direction to expand the table when render
#' returns a (named) vector.
#' @param text A character matrix containing the textual content of each table cell.
#' @param drop If \code{TRUE} (the default), rows and columns with zero counts
#' will be dropped.
#' @param collapse.cells If \code{TRUE} (the default), row/column header cells
#' will be collapsed (merged) where appropriate.
#' @param row.names If \code{TRUE} (the default), row names will be shown in the
#' first column of the table. Set to \code{FALSE} to suppress row names.
#' Only applies when displaying whole \code{data.frame}.
#' @param ... Additional arguments passed to \code{render}.
#'
#' @return A \code{character} which contains an HTML table fragment. It has
#' additional class attributes that cause it to be displayed in a browser in an
#' interactive context, and rendered as HTML in a \code{knitr} context.
#'
#' @examples
#' # mtcars examples
#' fable(mtcars)
#' fable(mtcars, mpg, facets=(gear ~ cyl), lab="Cylinders")
#' fable(mpg ~ gear | cyl, data=mtcars, lab="Cylinders")
#' fable(rownames(mtcars) ~ gear | cyl, data=mtcars,
#'   render=paste, collapse="<br/>", lab="Cylinders")
#'
#' # OrchardSprays examples
#' fable(head(OrchardSprays, 12))
#' fable(head(OrchardSprays, 12), row.names=FALSE)
#' fable(treatment ~ rowpos | colpos, data=OrchardSprays, lab="colpos")
#' fable(paste(treatment, decrease, sep="<br/>") ~ rowpos | colpos, data=OrchardSprays, lab="colpos")
#'
#' rndr.meansd <- function(x) formatC(c(Mean=mean(x), SD=sd(x)), digits=3)
#' fable(decrease ~ treatment, data=OrchardSprays, render=rndr.meansd, expand.along="rows")
#' fable(decrease ~ treatment, data=OrchardSprays, render=rndr.meansd, expand.along="columns")
#'
#' # ToothGrowth examples
#' fable(len ~ dose | supp, data=ToothGrowth, lab="Mean (SD)",
#'   render=function(x) sprintf("%0.3g (%0.3g)", mean(x), sd(x)))
#'
#' fable(len ~ dose | supp, data=ToothGrowth, lab="Supplement Type",
#'   render=rndr.meansd)
#'
#' fable(len ~ dose | supp, data=ToothGrowth, lab="Supplement Type",
#'   render=rndr.meansd, expand.along="columns")
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
fable.data.frame <- function(x, value, facets, ..., render, lab, footnote,
    expand.along=c("rows", "columns"), drop=c("both", "rows", "columns", "none"),
    collapse.cells=TRUE, row.names=T) {

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
        lab <- 0 # Special value
        attr(lab, ".suppressrowlabels") <- !row.names
    } else if (missing(value)) {
        stop("Cannot specify facets without value")
    } else if (missing(facets)) {
        stop("Cannot specify values without facets")
        f <- . ~ .
    } else {
        value <- eval(substitute(value), x, enclos=parent.frame())
        f <- Formula(facets)
        m <- model.frame(f, data=x, na.action=na.pass)
        rowvars <- model.part(f, data=m, lhs=1, drop=F)
        colvars <- model.part(f, data=m, rhs=1, drop=F)
    }

    fable.numeric(value, rowvars, colvars, render=render, lab=lab, footnote=footnote,
        expand.along=expand.along, drop=drop, collapse.cells=collapse.cells, ...)
}

#' @describeIn fable The \code{formula} method.
#' @export
#' @importFrom stats formula model.frame na.pass
#' @importFrom Formula Formula model.part
fable.formula <- function(x, data, ..., render, lab, footnote,
    expand.along=c("rows", "columns"), drop=c("both", "rows", "columns", "none"),
    collapse.cells=TRUE) {

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

    fable.numeric(x, rowvars, colvars, render=render, lab=lab, footnote=footnote,
        expand.along=expand.along, drop=drop, collapse.cells=collapse.cells, ...)
}

#' @describeIn fable The \code{numeric} method.
#' @export
#' @importFrom stats setNames ftable
fable.numeric <- function(x, rowvars, colvars, ..., render, lab, footnote,
    expand.along=c("rows", "columns"), drop=c("both", "rows", "columns", "none"),
    collapse.cells=TRUE) {

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
        text <- lapply(split(unname(x), c(rev(rowvars), rev(colvars))), render, ...)
    } else {
        text <- lapply(split(unname(x), c(rev(colvars), rev(rowvars))), render, ...)
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

    fable.ftable(counts, text=text, lab=lab, footnote=footnote, drop=drop, collapse.cells=collapse.cells)
}

#' @describeIn fable The \code{ftable} method.
#' @export
#' @importFrom stats ftable
fable.ftable <- function(x, text=matrix(as.character(x), nrow(x)), ..., lab, footnote,
    drop=c("both", "rows", "columns", "none"), collapse.cells=TRUE) {

    .fable.ftable.internal(
        x              = x,
        text           = text,
        lab            = lab,
        footnote       = footnote,
        drop           = drop,
        collapse.cells = collapse.cells)
}

.fable.ftable.internal <- function(x, text=matrix(as.character(x), nrow(x)), lab, footnote,
    drop=c("both", "rows", "columns", "none"), collapse.cells=TRUE, .suppressrowlabels=F) {

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


    if (!missing(lab) && !is.null(lab)) {
        .suppressrowlabels <- attr(lab, ".suppressrowlabels")
        if (is.null(.suppressrowlabels)) {
            .suppressrowlabels <- FALSE
            span <- ncol(text)
            sp <- if (span > 1) sprintf(" colspan=\"%d\"", span) else ""
            cl <- " class=\"lab\""
            td <- "th"
            tags <- paste0("<", td, sp, cl, ">", lab, "</", td, ">\n")
            cltags <- c(tags, cltags)
        }
    }

    rlhtags <- makeRowLabelHeadTags(names(xrv), length(cltags))

    thead <- lapply(seq_along(cltags), function(i) {
        tags <- cltags[[i]]
        if (i == 1 && !.suppressrowlabels) {
            for (j in rev(seq_along(rlhtags))) {
                tags <- c(rlhtags[j], tags)
            }
        }
        paste0("<tr>\n", paste0(tags, collapse=""), "</tr>\n", collapse="")
    })

    dat <- as.matrix(text)

    tbody <- lapply(seq_len(nrow(dat)), function(i) {
        td <- "td"
        tags <- paste0("<", td, ">", dat[i,], "</", td, ">\n")
        if (!.suppressrowlabels) {
            for (j in rev(seq_along(rltags))) {
                tags <- c(rltags[[j]][i], tags)
            }
        }
        paste0("<tr>\n", paste0(tags, collapse=""), "</tr>\n", collapse="")
    })

    x <- paste0(
        "<table>\n",
        paste0(thead, collapse=""),
        paste0(tbody, collapse=""),
        "</table>\n")

    if (!missing(footnote) && !is.null(footnote)) {
        footnote <- sprintf('<p class="tablefooter">%s</p>\n', footnote)
        x <- paste0(x, footnote)
    }

    structure(x, class=c("fable", "html", "character"), html=TRUE)
}

#' Print \code{fable} object.
#' @param x An object returned by \code{\link{fable}}.
#' @param ... Further arguments passed on to other \code{print} methods.
#' @return Returns \code{x} invisibly.
#' @details In an interactive context, the rendered table will be displayed in
#' a web browser. Otherwise, the HTML code will be printed as text.
#' @export
print.fable <- function(x, ...) {
    if (interactive()) {
        z <- htmltools::HTML(x)
        default.style <- htmltools::htmlDependency("fable", "1.0",
            src=system.file(package="fable", "fable_defaults_1.0"),
            stylesheet="fable_defaults.css")
        z <- htmltools::div(class="Rfable", default.style, z)
        z <- htmltools::browsable(z)
        print(z, ...) # Calls htmltools:::print.html(z, ...)
    } else {
        cat(x)
    }
    invisible(x)
}


#' Method for printing in a \code{knitr} context.
#' @param x An object returned by \code{\link{fable}}.
#' @param ... Further arguments passed on to \code{knitr::knit_print}.
#' @importFrom knitr knit_print
#' @export
knit_print.fable <- function(x, ...) {
    knit_to_html <-
        !is.null(knitr::opts_knit$get("rmarkdown.pandoc.to")) &&
        grepl("^html", knitr::opts_knit$get("rmarkdown.pandoc.to"))

    if (knit_to_html) {
        z <- htmltools::HTML(x)
        default.style <- htmltools::htmlDependency("fable", "1.0",
            src=system.file(package="fable", "fable_defaults_1.0"),
            stylesheet="fable_defaults.css")
        z <- htmltools::div(class="Rfable", default.style, z)
        knitr::knit_print(z, ...)
    } else {
        knitr::knit_print(as.character(x), ...)
    }
}

