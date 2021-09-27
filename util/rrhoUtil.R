# functions from the RRHO2 package to calculate the hyper matrix
library(ggplot2)

defaultStepSize <- function(list1, list2) {
  n1 <- dim(list1)[1]
  n2 <- dim(list2)[1]
  result <- ceiling(min(sqrt(c(n1, n2))))
  return(result)
}

numericListOverlap <-
  function(sample1,
           sample2,
           stepsize,
           method = "hyper",
           alternative,
           tol = 0.5,
           maximum) {
    
    n <- length(sample1)
    
    overlap_hyper <- function(a, b) {
      count <-
        as.integer(sum(as.numeric(sample1[1:a] %in% sample2[1:b])))
      signs <- 1L
      switch(
        alternative,
        enrichment = {
          log.pval <-
            -phyper(
              q = count - 1,
              m = a,
              n = n - a + 1,
              k = b,
              lower.tail = FALSE,
              log.p = TRUE
            )
          signs <- 1L
        },
        two.sided = {
          the.mean <- a * b / n
          signs <- sign(count - the.mean)
          if (signs < 0) {
            lower <- count
            upper <- 2 * the.mean - count
          } else{
            lower <- 2 * the.mean - count
            upper <- count
          }
          log.pval <-
            -log(
              phyper(
                q = lower + tol,
                m = a,
                n = n - a + 1,
                k = b,
                lower.tail = TRUE
              ) +
                phyper(
                  q = upper - tol,
                  m = a,
                  n = n - a + 1,
                  k = b,
                  lower.tail = FALSE
                )
            )
          #max<-log.pval[is.finite(log.pval)==TRUE]
          log.pval[!is.finite(log.pval)] <- maximum
        },
        split = {
          log.pval <-
            -phyper(
              q = count - 1,
              m = a,
              n = n - a + 1,
              k = b,
              lower.tail = FALSE,
              log.p = TRUE
            )
          log.pval[is.na(log.pval)] <- 0
          signs <- 1L
        }
      )
      
      return(c(
        counts = count,
        log.pval = as.numeric(log.pval),
        signs = as.integer(signs)
      ))
    }
    
    overlap_fisher <- function(a, b) {
      s1 <- sample1[1:a]
      s2 <- sample2[1:b]
      lenA <- as.integer(sum(as.numeric(s1 %in% s2)))
      lenB <- length(s1)
      lenC <- length(s2)
      
      #Odds <- lenA/(lenB-lenA)/(lenC-lenA)*(n - lenB - lenC + lenA)
      Odds <-
        ((lenA) * (n - lenB - lenC + lenA)) / ((lenC - lenA) * (lenB -
                                                                  lenA))
      if (Odds == 0) {
        Odds <- 1
      }
      if (is.na(Odds) == TRUE) {
        Odds <- 1
      }
      if (Odds == Inf) {
        Odds <- maximum
      }
      if (Odds == -Inf) {
        Odds <- 0
      }
      logOdds <- log(abs(Odds)) * sign(Odds)
      #logOdds[Odds == 0]<- maximum
      #logOdds[logOdds<0]<- -maximum
      #logOdds[!is.finite(logOdds)] <- sign(logOdds[!is.finite(logOdds)]) * 100
      signs <- 1L
      
      return(c(
        counts = lenA,
        log.pval = as.numeric(logOdds),
        signs = as.integer(signs)
      ))
    }
    
    indexes <-
      expand.grid(i = seq(1, n, by = stepsize),
                  j = seq(1, n, by = stepsize))
    if (method == "hyper") {
      overlaps <-
        apply(indexes, 1, function(x)
          overlap_hyper(x['i'], x['j']))
      
    } else if (method == "fisher") {
      overlaps <-
        apply(indexes, 1, function(x)
          overlap_fisher(x['i'], x['j']))
    }
    
    nrows <- sqrt(ncol(overlaps))
    matrix.counts <- matrix(overlaps['counts',], ncol = nrows)
    matrix.log.pvals <- matrix(overlaps['log.pval',], ncol = nrows)
    matrix.signs <- matrix(overlaps['signs',], ncol = nrows)
    
    return(list(
      counts = matrix.counts,
      log.pval = matrix.log.pvals,
      signs = matrix.signs
    ))
  }

# The RRHO2 function. Relevant parts taken from their package
RRHO2 <-
  function (list1,
            list2,
            stepsize = defaultStepSize(list1, list2),
            BY = FALSE,
            log10.ind = FALSE,
            maximum = 50,
            boundary = 0.1,
            method = "fisher",
            alternative = "split") {
    if (length(list1[, 1]) != length(unique(list1[, 1])))
      stop("Non-unique gene identifier found in list1")
    if (length(list2[, 1]) != length(unique(list2[, 1])))
      stop("Non-unique gene identifier found in list2")
    
    # ensure that they are data frames
    list1 <- data.frame(list1)
    list2 <- data.frame(list2)
    
    # order the lists
    list1 <- list1[order(list1[, 2], decreasing = TRUE), ]
    list2 <- list2[order(list2[, 2], decreasing = TRUE), ]
    
    nlist1 <- length(list1[, 1])
    nlist2 <- length(list2[, 1])
    
    N <- max(nlist1, nlist2)
    
    # begin the RRHO calculations
    .hypermat_normal <-
      numericListOverlap(
        list1[, 1],
        list2[, 1],
        stepsize,
        method = method,
        alternative = alternative,
        maximum = maximum
      )
    hypermat_normal <- .hypermat_normal$log.pval
    
    .hypermat_flipX <-
      numericListOverlap(
        rev(list1[, 1]),
        list2[, 1],
        stepsize,
        method = method,
        alternative = alternative,
        maximum = maximum
      )
    hypermat_flipX <- .hypermat_flipX$log.pval
    hypermat_flipX2 <- hypermat_flipX[nrow(hypermat_flipX):1,]
    
    stepList1 <- seq(1, nlist1, stepsize)
    stepList2 <- seq(1, nlist2, stepsize)
    
    len1 <- length(stepList1)
    len2 <- length(stepList2)
    
    lenStrip1 <- round(len1 * boundary)
    lenStrip2 <- round(len2 * boundary)
    
    
    boundary1 <- sum(list1[stepList1, 2] > 0)
    boundary2 <- sum(list2[stepList2, 2] > 0)
    
    hypermat <-
      matrix(
        NA,
        nrow = nrow(hypermat_normal) + lenStrip1,
        ncol = ncol(hypermat_normal) + lenStrip2
      )
    hypermat[1:boundary1, 1:boundary2] <-
      hypermat_normal[1:boundary1, 1:boundary2] ## u1u2, quadrant III
    
    hypermat[lenStrip1 + (boundary1 + 1):len1, lenStrip2 + (boundary2 + 1):len2] <-
      hypermat_normal[(boundary1 + 1):len1, (boundary2 + 1):len2] ## d1d2, quadrant I
    
    hypermat[1:boundary1, lenStrip2 + (boundary2 + 1):len2] <-
      hypermat_flipX[len1:(len1 - boundary1 + 1), (boundary2 + 1):len2] ## u1d2, quadrant II
    
    hypermat[lenStrip1 + (boundary1 + 1):len1, 1:boundary2] <-
      hypermat_flipX[(len1 - boundary1):1, 1:boundary2] ## u1d2, quadrant IV
    
    
    if (log10.ind) {
      hypermat <- hypermat * log10(exp(1))
    }
    
    if (BY) {
      hypermatvec <- matrix(hypermat,
                            nrow = nrow(hypermat) *
                              ncol(hypermat),
                            ncol = 1)
      hypermat.byvec <- p.adjust(exp(-hypermatvec), method = "BY")
      hypermat.by <-
        matrix(-log(hypermat.byvec),
               nrow = nrow(hypermat),
               ncol = ncol(hypermat))
      if (log10.ind)
        hypermat.by <- hypermat.by * log10(exp(1))
      result$hypermat.by <- hypermat.by
    }
    
    maxind.dd <-
      which(max(hypermat[lenStrip1 + (boundary1 + 1):len1, lenStrip2 + (boundary2 +
                                                                          1):len2],
                na.rm = TRUE) == hypermat, arr.ind = TRUE)
    #
    maxind.dd <-
      maxind.dd[maxind.dd[, 1] >= lenStrip1 + (boundary1 + 1) &
                  maxind.dd[, 1] <= lenStrip1 + len1 &
                  maxind.dd[, 2] >= lenStrip2 + (boundary2 + 1) &
                  maxind.dd[, 2] <= lenStrip2 + len2,]
    
    indlist1.dd <-
      seq(1, nlist1, stepsize)[maxind.dd[1] - lenStrip1]
    indlist2.dd <-
      seq(1, nlist2, stepsize)[maxind.dd[2] - lenStrip2]
    
    genelist.dd <- intersect(list1[indlist1.dd:nlist1,
                                   1], list2[indlist2.dd:nlist2, 1])
    maxind.uu <- which(max(hypermat[1:boundary1, 1:boundary2],
                           na.rm = TRUE) == hypermat, arr.ind = TRUE)
    #
    maxind.uu <-
      maxind.uu[maxind.uu[, 1] >= 1 &
                  maxind.uu[, 1] <= boundary1 &
                  maxind.uu[, 2] >= 1 &
                  maxind.uu[, 2] <= boundary2,]
    
    indlist1.uu <- seq(1, nlist1, stepsize)[maxind.uu[1]]
    indlist2.uu <- seq(1, nlist2, stepsize)[maxind.uu[2]]
    
    genelist.uu <- intersect(list1[1:indlist1.uu, 1],
                             list2[1:indlist2.uu, 1])
    #
    maxind.ud <-
      which(max(hypermat[1:boundary1, lenStrip2 + (boundary2 + 1):len2],
                na.rm = TRUE) == hypermat, arr.ind = TRUE)
    #
    maxind.ud <-
      maxind.ud[maxind.ud[, 1] >= 1 &
                  maxind.ud[, 1] <= boundary1 &
                  maxind.ud[, 2] >= lenStrip2 + (boundary2 + 1) &
                  maxind.ud[, 2] <= lenStrip2 + len2,]
    
    indlist1.ud <- seq(1, nlist1, stepsize)[maxind.ud[1]]
    indlist2.ud <-
      seq(1, nlist2, stepsize)[maxind.ud[2] - lenStrip2]
    
    genelist.ud <- intersect(list1[1:indlist1.ud,
                                   1], list2[indlist2.ud:nlist2, 1])
    maxind.du <-
      which(max(hypermat[lenStrip1 + (boundary1 + 1):len1, 1:boundary2],
                na.rm = TRUE) == hypermat, arr.ind = TRUE)
    #
    maxind.du <-
      maxind.du[maxind.du[, 1] >= lenStrip1 + (boundary1 + 1) &
                  maxind.du[, 1] <= lenStrip1 + len1 &
                  maxind.du[, 2] >= 1 &
                  maxind.du[, 2] <= boundary2,]
    
    indlist1.du <-
      seq(1, nlist1, stepsize)[maxind.du[1] - lenStrip1]
    indlist2.du <- seq(1, nlist2, stepsize)[maxind.du[2]]
    
    if (is.na(indlist2.du) == TRUE) {
      indlist2.du <- max(seq(1, nlist2, stepsize))
    }
    
    genelist.du <- intersect(list1[indlist1.du:nlist1, 1],
                             list2[1:indlist2.du, 1])
    
    # now, return the results
    # this include a few things:
    # 1. The hyper matrix
    # 2. The gene lists which overlap in the four quadrants: UU, FF, UD and DU
    # 3. The information needed to draw the Venn diagrams, namely the number of genes Up, down, and overlapping
    
    return(
      list(
        hypermat = hypermat,
        geneLists = list(
          UpUp = genelist.uu,
          DownDown = genelist.dd,
          UpDown = genelist.ud,
          DownUp = genelist.du
        ),
        ConcordantVenn = list(
          UpUp = list(
            area1 = length(1:indlist1.uu),
            area2 = length(1:indlist2.uu),
            cross.area = length(genelist.uu)
          ),
          DownDown = list(
            area1 = length(indlist1.dd:nlist1),
            area2 = length(indlist2.dd:nlist2),
            cross.area = length(genelist.dd)
          )
        ),
        DiscordantVenn = list(
          UpDown = list(
            area1 = length(1:indlist1.ud),
            area2 = length(indlist2.ud:nlist2),
            cross.area = length(genelist.ud)
          ),
          DownUp = list(
            area1 = length(indlist1.du:nlist1),
            area2 = length(1:indlist2.du),
            cross.area = length(genelist.du)
          )
        )
      )
    )
    
    
  }
