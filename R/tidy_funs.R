pluck_index<- function(x, field) {
  lapply(x, "[[", field)
}

add_commas <- function(num) {
  format(num, big.mark=",", scientific=FALSE, trim=TRUE)
}

align_vectors <- function(x, y, expand=TRUE) {
    nx <- names(x)
    ny <- names(y)
    if(is.null(nx) || is.null(ny))
      stop("Need names attributes for both x and y")

    if(length(unique(nx)) != length(nx))
      stop("names(x) not all distinct")
    if(length(unique(ny)) != length(ny))
      stop("names(y) not all distinct")

    if(expand) {
      nfull <- unique(c(nx, ny))
      if(any(!(nfull %in% nx)))
        x <- c(x, setNames(rep(NA, length(nfull)-length(nx)),
                           nfull[!(nfull %in% nx)]))
      if(any(!(nfull %in% ny)))
        y <- c(y, setNames(rep(NA, length(nfull)-length(ny)),
                           nfull[!(nfull %in% ny)]))
    }

    nx <- names(x)
    ny <- names(y)
    common <- nx[nx %in% ny]
    return(list(x=x[common], y=y[common]))
}

format_table <- function(xtab, percent = NULL, decimals = 1) {
  if(!is.table(xtab)) stop("Must pass a table object.")

  rowsums <- rowSums(xtab)
  colsums <- colSums(xtab)
  total <- sum(xtab)
  out <- xtab
  class(out) <- "character"

  if(!is.null(percent)){
    if(percent == "row") {
      for(row in 1:(nrow(out))){
        out[row,] = paste0(add_commas(xtab[row,]), " (", 100*round(xtab[row,]/rowsums[row], digits = decimals), "%)")
      }
    } else if (percent == "col") {
      for(col in 1:(ncol(out))){
        out[,col] = paste0(add_commas(xtab[,col]), " (", 100*round(xtab[,col]/colsums[col], digits = decimals), "%)")
      }
    } else if (percent == "total") {
      for(col in 1:(ncol(out))){
        out[,col] = paste0(add_commas(xtab[,col]), " (", 100*round(xtab[,col]/total, digits = decimals), "%)")
      }
    }
  }
  out <- cbind(out, Total = add_commas(colsums))
  out <- rbind(out, Total = add_commas(c(rowsums, total)))
  return(out)
}

align_vectors <- function(x, y, expand=TRUE) {
    nx <- names(x)
    ny <- names(y)
    if(is.null(nx) || is.null(ny))
      stop("Need names attributes for both x and y")
    if(length(unique(nx)) != length(nx))
      stop("names(x) not all distinct")
    if(length(unique(ny)) != length(ny))
      stop("names(y) not all distinct")
    if(expand) {
      nfull <- unique(c(nx, ny))
      if(any(!(nfull %in% nx)))
        x <- c(x, setNames(rep(NA, length(nfull)-length(nx)),
                           nfull[!(nfull %in% nx)]))
      if(any(!(nfull %in% ny)))
        y <- c(y, setNames(rep(NA, length(nfull)-length(ny)),
                           nfull[!(nfull %in% ny)]))
    }
    nx <- names(x)
    ny <- names(y)
    common <- nx[nx %in% ny]
    list(x=x[common], y=y[common])
  }

n_unique <- function(x, na.rm=TRUE)  {
    if(na.rm && !is.null(x)) x <- x[!is.na(x)]
    length(unique(vec))
}

ci_plot <- function(est, se=NULL, lo=NULL, hi=NULL, SEmult=2, labels=NULL, rotate=FALSE, ...) {
    if(is.null(se) && is.null(lo) && is.null(hi)) {
      se <- rep(0, length(est))
    }
    if(!is.null(se) && (!is.null(lo) || !is.null(hi))) {
      warning("Provide either se or both lo and hi; se is being used")
    }
    if(!is.null(se)) {
      stopifnot(length(se) == length(est))

      lo <- est - se*SEmult
      hi <- est + se*SEmult
    } else {
      stopifnot(length(lo) == length(est), length(hi) == length(est))
    }

    if(is.null(labels)) {
      labels <- names(est)
    }
    if(is.null(labels)) {
      labels <- rep(LETTERS, length(est))[seq_along(est)]
    }
    stopifnot(length(labels) == length(est))

    hide_ciplot <-
      function(est, lo, hi, rotate=FALSE,
               vlines=NULL, vlines.col="white", vlines.lwd=1,
               hlines=NULL, hlines.col="white", hlines.lwd=1,
               xat=NULL, xlim=NULL, xaxs="r", xlab=NULL,
               yat=NULL, ylim=NULL, yaxs="r", ylab=NULL,
               las=1, pch=21, bg="slateblue", ci_col="black",
               ci_lwd=2, ci_endseg=0.05, labels=NULL, main="",
               mgp.x=NULL, mgp.y=NULL, mgp=NULL, ...) {
        n_group <- length(est)
        group <- seq_len(n_group)

        if(!rotate) {
          xlim <- c(0.5, n_group+0.5)
          vlines <- 1:n_group
          vlines.col <- "gray70"
          vlines.lwd <- 4
          xat <- NA
          if(is.null(xlab)) xlab <- "Group"

          if(is.null(ylim)) ylim <- range(c(lo, hi, est))

          # deal with vlines/lines ** FIX ME **

          grayplot(group, est,
                   vlines=vlines, vlines.col=vlines.col, vlines.lwd=vlines.lwd,
                   hlines=hlines, hlines.col=hlines.col, hlines.lwd=hlines.lwd,
                   xat=xat, xlim=xlim, xaxs=xaxs, xlab=xlab,
                   yat=yat, ylim=ylim, yaxs=yaxs, ylab=ylab, las=las, type="n",
                   main=main, mgp=mgp, mgp.x=mgp.x, mgp.y=mgp.y, ...)
          axis(side=1, at=vlines, labels, las=las, tick=FALSE, mgp=c(0,0.2,0))

          segments(group, lo, group, hi, col=ci_col, lwd=ci_lwd)
          segments(group-ci_endseg, lo, group+ci_endseg, lo, col=ci_col, lwd=ci_lwd)
          segments(group-ci_endseg, hi, group+ci_endseg, hi, col=ci_col, lwd=ci_lwd)

          points(group, est, pch=pch, bg=bg, ...)

        }
        else {
          ylim <- c(0.5, n_group+0.5)
          hlines <- 1:n_group
          hlines.col <- "gray70"
          hlines.lwd <- 4
          yat <- NA
          if(is.null(ylab)) ylab <- "Group"

          if(is.null(xlim)) xlim <- range(c(lo, hi, est))

          grayplot(est, group,
                   vlines=vlines, vlines.col=vlines.col, vlines.lwd=vlines.lwd,
                   hlines=hlines, hlines.col=hlines.col, hlines.lwd=hlines.lwd,
                   xat=xat, xlim=xlim, xaxs=xaxs, xlab=xlab,
                   yat=yat, ylim=ylim, yaxs=yaxs, ylab=ylab,
                   v_over_h=TRUE, las=las, type="n", main=main,
                   mgp=mgp, mgp.x=mgp.x, mgp.y=mgp.y, ...)
          axis(side=2, at=hlines, labels, las=las, tick=FALSE, mgp=c(0,0.3,0))

          segments(lo, group, hi, group, col=ci_col, lwd=ci_lwd)
          segments(lo, group-ci_endseg, lo, group+ci_endseg, col=ci_col, lwd=ci_lwd)
          segments(hi, group-ci_endseg, hi, group+ci_endseg, col=ci_col, lwd=ci_lwd)

          points(est, group, pch=pch, bg=bg, ...)
        }

      }

    hide_ciplot(est=est, lo=lo, hi=hi, rotate=rotate, labels=labels, ...)
    invisible()
}

get_crayon <- function(colors=NULL)  {
    if(is.null(colors)) return(crayons)

    colors <- unlist(colors)
    allnames <- names(crayons)
    m <- match(colors, allnames)
    notfound <- colors[is.na(m)]
    g <- vapply(notfound, function(a) {
      z <- grep(a, allnames, ignore.case=TRUE)
      if(length(z) < 1) return(-1)
      if(length(z) > 1) return(-2)
      z }, 1)

    if(any(g < 0)) {
      if(any(g == -1)) warning("Some colors not found")
      if(any(g == -2)) warning("Some colors with multiple matches")
    }
    g[g < 0] <- NA
    m[is.na(m)] <- g

    result <- crayons[g]
    names(result)[is.na(g)] <- colors[is.na(g)]

    return(result)
}

spell_out <- function(number, capitalize=FALSE, max_value=9) {
  origin = number

  ones = list(zero=0, one=1, two=2, three=3, four=4, five=5,
              six=6, seven=7, eight=8, nine=9)
  teens = list(eleven=11, twelve=12, thirteen=13, fourteen=14, fifteen=15,
               sixteen=16, seventeen=17, eighteen=18, nineteen=19)
  tens = list(ten=10, twenty=20, thirty=30, forty=40, fifty=50,
              sixty=60, seventy=70, eighty=80, ninety=90)
  large_digits = list(hundred=100, thousand=1000, million=1e6, billion=1e9, trillion=1e12)
  double_digits = c(teens,tens)

  #Split the string into words
  string=gsub("-"," ",gsub(" & ", " and ",string,ignore.case=T))
  string=numberTypes(string)
  wrdsplit=strsplit(tolower(string)," ")[[1]]
  wrdsplit=wrdsplit[wrdsplit!=""]
  isNumber=apply(data.frame(wrdsplit),1,isNumericWord)
}

forest_plot <- function(estimate, lci, uci, labels = NULL, rotate = FALSE) {
  assertthat::assert_that(is.numeric(estimate) && length(estimate) > 0)
  assertthat::assert_that((is.numeric(lci) && length(lci) > 0) && (is.numeric(uci) && length(uci) > 0))
  assertthat::assert_that(length(lci) == length(uci) && length(lci) == length(estimate))
  if(is.null(labels)) labels <- seq_along(estimate)

  data <- cbind(estimate, lci, uci, labels)

  plot <- ggplot2::ggplot(data, ggplot2::aes(x = labels, y = estimate, ymin = lci, ymax = uci)) +
    ggplot2::geom_point() +
    ggplot2::geom_errorbar()
}



