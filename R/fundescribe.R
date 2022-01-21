#' @title A Convenient Way of Descriptive Statistics
#'
#' @description This function can perform descriptive statistics according to different data types.
#'
#' @details This function can perform descriptive statistics according to different data types. If the data is a continuous variable, the mean and standard deviation or median and quartile are automatically output; if the data is a categorical variable, the number and percentage are automatically output. In addition, if you enter two variables, the first variable will be described hierarchically based on the second variable and the statistical differences between different groups will be compared using appropriate statistical methods. And for groups more than two, the post hoc test will be applied.
#'
#' @param x The variable to be statistically described.
#'
#' @param y An optional variable need to be a factor, or a character, or a logical vector.
#'
#' @param data An optional parameter, the name of the data containing x and y.
#'
#' @param na.rm An optional parameter, if FALSE, the information of NA will be given.
#' 
#' @param norm.t An optional parameter, there are seven normal test methods available: c("ks.test", "shapiro.test", "cvm.test", "lillie.test", "pearson.test", "sf.test", "ad.test").
#'
#' @export
#'
#' @return No return value, called for side effects.
#'
#' @importFrom "graphics" "hist"
#' @importFrom "stats" "TukeyHSD" "aov" "kruskal.test" "na.omit" "qqline" "qqnorm" "t.test" "wilcox.test" "xtabs" "shapiro.test" "ks.test" "var"
#' @importFrom "gmodels" "CrossTable"
#' @importFrom "psych" "describe" "describeBy"
#' @importFrom "fitdistrplus" "descdist"
#' @importFrom "nortest" "cvm.test" "ad.test" "lillie.test" "pearson.test" "sf.test"
#' @importFrom "rcompanion" "pairwiseNominalIndependence"
#' @importFrom "CATT" "CATT"
#' @importFrom "multiCA" "multiCA.test"
#' @importFrom "FSA" "dunnTest"
#'
#' @author Xiuquan Nie, niexiuquan1995@foxmail.com
#'
#' @references
#' Libiseller, C. and Grimvall, A. (2002) Performance of partial Mann-Kendall tests for trend detection in the presence of covariates.
#' \emph{Environmetrics}, 13, 71-84. \doi{doi:10.1002/env.507}.
#'
#' Patefield, W. M. (1981) An efficient method of generating r x c tables with given row and column totals.
#' \emph{Applied Statistics}, 30, 91-97. \doi{doi:10.2307/2346669}
#'
#' Hope, A. C. A. (1968) A simplified Monte Carlo significance test procedure.
#' \emph{Journal of the Royal Statistical Society Series B}, 30, 582-598. \doi{doi:10.1111/J.2517-6161.1968.TB00759.X}
#'
#' Mehta, C. R. and Patel, N. R. (1983) A network algorithm for performing Fisher's exact test in r x c contingency tables.
#' \emph{Journal of the American Statistical Association}, 78, 427-434. \doi{doi:10.1080/01621459.1983.10477989}
#'
#' Mehta, C. R. and Patel, N. R. (1986) Algorithm 643: FEXACT, a FORTRAN subroutine for Fisher's exact test on unordered r x c contingency tables.
#' \emph{ACM Transactions on Mathematical Software}, 12, 154-161. \doi{doi:10.1145/6497.214326}
#'
#' Clarkson, D. B., Fan, Y. and Joe, H. (1993) A Remark on Algorithm 643: FEXACT: An Algorithm for Performing Fisher's Exact Test in r x c Contingency Tables.
#' \emph{ACM Transactions on Mathematical Software}, 19, 484-488. \doi{doi:10.1145/168173.168412}
#'
#' Cochran, W. G. (1954) Some methods for strengthening the common chi-squared tests.
#' \emph{International Biometric Society}, 10 (4), 417-451. \doi{doi:10.2307/3001616}
#'
#' Armitage, P. (1955) Tests for Linear Trends in Proportions and Frequencies.
#' \emph{International Biometric Society}, 11 (3), 375-386. \doi{doi:10.2307/3001775}
#'
#' Szabo, A. (2016) Test for trend with a multinomial outcome.
#' \emph{American Statistician}, 73 (4), 313-320. \doi{doi:10.1080/00031305.2017.1407823}
#'
#' David, F. B. (1972) Constructing confidence sets using rank statistics.
#' \emph{Journal of the American Statistical Association}, 67, 687-690. \doi{doi:10.1080/01621459.1972.10481279}
#'
#' Joanes, D. N. and Gill, C. A. (1998) Comparing measures of sample skewness and kurtosis.
#' \emph{The Statistician}, 47, 183-189. \doi{doi:10.1111/1467-9884.00122}
#'
#' Dunn, O. J. (1964) Multiple comparisons using rank sums.
#' \emph{Technometrics}, 6, 241-252. \doi{doi:10.1080/00401706.1964.10490181}
#'
#' Copenhaver, M. D. and Holland, B. S. (1988) Computation of the distribution of the maximum studentized range statistic with application to multiple significance testing of simple effects.
#' \emph{Journal of Statistical Computation and Simulation}, 30, 1-15. \doi{doi:10.1080/00949658808811082}
#'
#' Chambers, J. M., Freeny, A. and Heiberger, R. M. (1992) \emph{Statistical Models in S.}
#' 49-52. \doi{doi:10.1201/9780203738535-5}
#'
#' Shaffer, J. P. (1995) Multiple hypothesis testing.
#' \emph{Annual Review of Psychology}, 46, 561-584. \doi{doi:10.1146/annurev.ps.46.020195.003021}
#'
#' Myles, H. and Douglas, A. W. (1973) \emph{Nonparametric Statistical Methods.}
#' 115-120. \doi{doi:10.2307/2063815}
#'
#' Rahman, M. and Tiwari, R. (2012) Pairwise comparisons in the analysis of carcinogenicity data.
#' \emph{Health}, 4, 910-918. \doi{doi:10.4236/health.2012.410139}
#' 
#' Thode, H. J. (2002) \emph{Testing for Normality.} \doi{10.1201/9780203910894}
#'
#' @examples
#' data(T2D)
#' fundescribe(T2D$age, norm.t = c("lillie.test"))
#' fundescribe(gender, data = T2D)
#' fundescribe(education, diabetes, data = T2D)
#' fundescribe(T2D$glucose, T2D$diabetes)
fundescribe <- function(x, y, data = NULL, na.rm = TRUE, norm.t = NULL){
  {
    if(missing(y)){
      {
        if(is.null(data)){
          if(!is.vector(x) & !is.factor(x)){
            stop("x must be a vector or a factor!")
          }
          data <- data.frame(x)
          colnames(data) <- c("x")
          datana <- data
        }
        else{
          xname <- deparse(substitute(x))
          coln <- colnames(data)
          if(!(xname %in% coln)){
            stop("No x in data!")
          }
          xwz <- as.numeric(which(coln == xname))
          data <- data.frame(data[xwz])
          colnames(data) <- c("x")
          datana <- data
        }
        {#quality control
          if(length(data$x)==1){
            stop("x has only one element.")
          }
          else{
            if((NA %in% data$x)){
              if(!(F %in% (data$x %in% NA))){
                stop("There is only NA in x!")
              }
              write("Warning: There is NA in x! NA has been omitted when doing subsequent statistical analysis.", stdout())
            }
            if((NaN %in% data$x)){
              if(!(F %in% (data$x %in% NaN))){
                stop("There is only NaN in x!")
              }
              data$x[is.nan(data$x)] <- NA
              write("Warning: There is NaN in x! NaN has been omitted when doing subsequent statistical analysis.", stdout())
            }
            typex <- class(data$x)[1]
            if(typex == "integer" | typex == "numeric"){
              infdata <- abs(data$x)
              if((Inf %in% infdata)){
                if(!(F %in% (infdata %in% Inf))){
                  stop("There is only Inf in x!")
                }
                data$x[is.infinite(data$x)] <- NA
                write("Warning: There is Inf in x! Inf has been omitted when doing subsequent statistical analysis.", stdout())
              }
            }
          }
        }
        data <- na.omit(data)
      }
      {
        typex <- class(data$x)[1]
        if(typex == "factor" |
           typex == "logical" |
           typex == "ordered" |
           typex == "character"){
          xx <- as.factor(data$x)
          if(nlevels(xx) == 1){
            xnum <- length(data$x)
            prox <- round(xnum/(dim(datana)[1]), digits = 5)*100
            shux <- paste("Only one element is included in x, and the number of it is ", xnum, " (", prox, "%)", sep = "")
            write(shux, stdout())
          }
          if(nlevels(xx) > 1){
            CrossTable(data$x, digits = 5)
          }
        }
        if(typex == "numeric" | typex == "integer"){
          hist(data$x)
          qqnorm(data$x)
          qqline(data$x)
          write("The histogram and QQ plot of variable x have been drawn.", stdout())
          print(describe(data$x, quant = c(.05, .1, .25, .5, .75, .90, .95)))
          if(!is.null(norm.t)){
            descdist(data$x)
            for (i in norm.t) {
              if(i == "ks.test"){
                print(ks.test(data$x,"pnorm",mean=mean(data$x),sd=sqrt(var(data$x))))
              }
              if(i == "shapiro.test"){
                print(shapiro.test(data$x))
              }
              if(i == "cvm.test"){
                print(cvm.test(data$x))
              }
              if(i == "ad.test"){
                print(ad.test(data$x))
              }
              if(i == "lillie.test"){
                print(lillie.test(data$x))
              }
              if(i == "pearson.test"){
                print(pearson.test(data$x))
              }
              if(i == "sf.test"){
                print(sf.test(data$x))
              }
              if(i != "ks.test" & i != "shapiro.test" & i != "cvm.test"
                 & i != "ad.test" & i != "lillie.test" & i != "pearson.test"
                 & i != "sf.test"){
                stop("There are only seven normal test methods available: c('ks.test', 'shapiro.test', 'cvm.test', 'lillie.test', 'pearson.test', 'sf.test', 'ad.test').")
              }
            }
          }
        }
      }
      {
        if(na.rm == FALSE){
          nanum <- sum(is.na(datana$x))
          prona <- round(nanum/(dim(datana)[1]), digits = 5)*100
          shu <- paste("The number of NA in x is ", nanum, " (", prona, "%).", sep = "")
          write(c("-------------------------------------------------------------------------------"), stdout())
          write(shu, stdout())
        }
      }
    }
  }
  {
    if(!missing(y)){
      {
        if(is.null(data)){
          if(!is.vector(x) & !is.factor(x)){
            stop("x must be a vector or a factor!")
          }
          if(!is.vector(y) & !is.factor(y)){
            stop("y must be a vector or a factor!")
          }
          data <- data.frame(x, y)
          colnames(data) <- c("x", "y")
          datana <- data
        }
        else{
          xname <- deparse(substitute(x))
          yname <- deparse(substitute(y))
          coln <- colnames(data)
          if(!(xname %in% coln)){
            stop("No x in data!")
          }
          if(!(yname %in% coln)){
            stop("No y in data!")
          }
          xwz <- as.numeric(which(coln == xname))
          ywz <- as.numeric(which(coln == yname))
          data <- data.frame(data[xwz], data[ywz])
          colnames(data) <- c("x", "y")
          datana <- data
        }
        {#quality control x
          if(length(data$x)==1){
            stop("x has only one element.")
          }
          else{
            if((NA %in% data$x)){
              if(!(F %in% (data$x %in% NA))){
                stop("There is only NA in x!")
              }
              write("Warning: There is NA in x! NA has been omitted when doing subsequent statistical analysis.", stdout())
            }
            if((NaN %in% data$x)){
              if(!(F %in% (data$x %in% NaN))){
                stop("There is only NaN in x!")
              }
              data$x[is.nan(data$x)] <- NA
              write("Warning: There is NaN in x! NaN has been omitted when doing subsequent statistical analysis.", stdout())
            }
            typex <- class(data$x)[1]
            if(typex == "integer" | typex == "numeric"){
              infdatax <- abs(data$x)
              if((Inf %in% infdatax)){
                if(!(F %in% (infdatax %in% Inf))){
                  stop("There is only Inf in x!")
                }
                data$x[is.infinite(data$x)] <- NA
                write("Warning: There is Inf in x! Inf has been omitted when doing subsequent statistical analysis.", stdout())
              }
            }
          }
        }
        {#quality control y
          if(length(data$y)==1){
            stop("y has only one element.")
          }
          else{
            if((NA %in% data$y)){
              if(!(F %in% (data$y %in% NA))){
                stop("There is only NA in y!")
              }
              write("Warning: There is NA in y! NA has been omitted when doing subsequent statistical analysis.", stdout())
            }
            if((NaN %in% data$y)){
              if(!(F %in% (data$y %in% NaN))){
                stop("There is only NaN in y!")
              }
              data$y[is.nan(data$y)] <- NA
              write("Warning: There is NaN in y! NaN has been omitted when doing subsequent statistical analysis.", stdout())
            }
            typey <- class(data$y)[1]
            if(typey == "integer" | typey == "numeric"){
              infdatay <- abs(data$y)
              if((Inf %in% infdatay)){
                if(!(F %in% (infdatay %in% Inf))){
                  stop("There is only Inf in y!")
                }
                data$y[is.infinite(data$y)] <- NA
                write("Warning: There is Inf in y! Inf has been omitted when doing subsequent statistical analysis.", stdout())
              }
            }
          }
        }
        data <- na.omit(data)
      }
      {
        typey <- class(data$y)[1]
        lth <- length(data$y)
        if(typey == "factor"){
          typex <- class(data$x)[1]
          yl <- nlevels(as.factor(as.character(data$y)))
          if(typex == "factor"){
            xl <- nlevels(as.factor(as.character(data$x)))
            if(yl == 1){
              warning("y has only one element.")
              if(xl == 1){
                stop("x must at least have 2 elements.")
              }
              if(xl > 1){
                CrossTable(data$x, data$y, digits = 5)
              }
            }
            if(yl == 2){
              if(xl == 1){
                CrossTable(data$x, data$y, digits = 5)
                warning("x has only one element.")
              }
              if(xl == 2){
                CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T, fisher = T)
              }
              if(xl > 2){
                if(lth < 40){
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T, fisher = T)
                }else{
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T)
                }
                kfeduqh <- xtabs(~ x + y, data = data)
                write(c("-------------------------------------------------------------------------------",
                        "Post hoc multiple comparisons between different groups of x:"), stdout())
                print(pairwiseNominalIndependence(kfeduqh, method = "fdr"))
              }
            }
            if(yl > 2){
              if(xl == 1){
                CrossTable(data$x, data$y, digits = 5)
                warning("x has only one element.")
              }
              if(xl == 2){
                if(lth < 40){
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T, fisher = T)
                }else{
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T)
                }
                kfeduhq <- xtabs(~ y + x, data = data)
                write(c("-------------------------------------------------------------------------------",
                        "Post hoc multiple comparisons between different groups of y:"), stdout())
                print(pairwiseNominalIndependence(kfeduhq))
              }
              if(xl > 2){
                if(lth < 40){
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T, fisher = T)
                }else{
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T)
                }
                kfeduqh <- xtabs(~ x + y, data = data)
                kfeduhq <- xtabs(~ y + x, data = data)
                write(c("-------------------------------------------------------------------------------",
                        "Post hoc multiple comparisons between different groups:"), stdout())
                write(c("--------------------", "Post hoc multiple comparisons between different groups of x:"), stdout())
                print(pairwiseNominalIndependence(kfeduqh))
                write(c("--------------------", "Post hoc multiple comparisons between different groups of y:"), stdout())
                print(pairwiseNominalIndependence(kfeduhq))
              }
            }
          }
          if(typex == "logical" | typex == "character"){
            data$x <- as.character(data$x)
            data$x <- as.factor(data$x)
            xl <- nlevels(data$x)
            if(yl == 1){
              warning("y has only one element.")
              if(xl == 1){
                stop("x must at least have 2 elements.")
              }
              if(xl > 1){
                CrossTable(data$x, data$y, digits = 5)
              }
            }
            if(yl == 2){
              if(xl == 1){
                CrossTable(data$x, data$y, digits = 5)
                warning("x has only one element.")
              }
              if(xl == 2){
                CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T, fisher = T)
              }
              if(xl > 2){
                if(lth < 40){
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T, fisher = T)
                }else{
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T)
                }
                kfeduqh <- xtabs(~ x + y, data = data)
                write(c("-------------------------------------------------------------------------------",
                        "Post hoc multiple comparisons between different groups of x:"), stdout())
                print(pairwiseNominalIndependence(kfeduqh, method = "fdr"))
              }
            }
            if(yl > 2){
              if(xl == 1){
                CrossTable(data$x, data$y, digits = 5)
                warning("x has only one element.")
              }
              if(xl == 2){
                if(lth < 40){
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T, fisher = T)
                }else{
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T)
                }
                kfeduhq <- xtabs(~ y + x, data = data)
                write(c("-------------------------------------------------------------------------------",
                        "Post hoc multiple comparisons between different groups of y:"), stdout())
                print(pairwiseNominalIndependence(kfeduhq))
              }
              if(xl > 2){
                if(lth < 40){
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T, fisher = T)
                }else{
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T)
                }
                kfeduqh <- xtabs(~ x + y, data = data)
                kfeduhq <- xtabs(~ y + x, data = data)
                write(c("-------------------------------------------------------------------------------",
                        "Post hoc multiple comparisons between different groups:"), stdout())
                write(c("--------------------", "Post hoc multiple comparisons between different groups of x:"), stdout())
                print(pairwiseNominalIndependence(kfeduqh))
                write(c("--------------------", "Post hoc multiple comparisons between different groups of y:"), stdout())
                print(pairwiseNominalIndependence(kfeduhq))
              }
            }
          }
          if(typex == "ordered"){
            xl <- nlevels(as.factor(as.character(data$x)))
            if(yl == 1){
              warning("y has only one element.")
              if(xl == 1){
                stop("x must at least have 2 elements.")
              }
              if(xl > 1){
                CrossTable(data$x, data$y, digits = 5)
              }
            }
            if(yl == 2){
              if(xl == 1){
                CrossTable(data$x, data$y, digits = 5)
                warning("x has only one element.")
              }
              if(xl == 2){
                CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T, fisher = T)
              }
              if(xl > 2){
                if(lth < 40){
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T, fisher = T)
                }else{
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T)
                }
                kfeduqh <- xtabs(~ x + y, data = data)
                data$xn <- as.numeric(data$x)
                write(c("-------------------------------------------------------------------------------",
                        "Wilcoxon rank sum test:", "Mann-Whitney U test = Wilcoxon rank sum test"), stdout())
                print(wilcox.test(xn ~ y, data = data))
                write(c("-------------------------------------------------------------------------------",
                        "Post hoc multiple comparisons between different groups of x:"), stdout())
                print(pairwiseNominalIndependence(kfeduqh, method = "fdr"))
                write(c("-------------------------------------------------------------------------------",
                        "The Cochran-Armitage trend test for x:"), stdout())
                data$xx <- factor(as.factor(as.numeric(data$x)), ordered = T)
                print(CATT(data$y, data$xx))
              }
            }
            if(yl > 2){
              if(xl == 1){
                CrossTable(data$x, data$y, digits = 5)
                warning("x has only one element.")
              }
              if(xl == 2){
                if(lth < 40){
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T, fisher = T)
                }else{
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T)
                }
                kfeduhq <- xtabs(~ y + x, data = data)
                write(c("-------------------------------------------------------------------------------",
                        "Post hoc multiple comparisons between different groups of y:"), stdout())
                print(pairwiseNominalIndependence(kfeduhq))
              }
              if(xl > 2){
                if(lth < 40){
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T, fisher = T)
                }else{
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T)
                }
                data$xn <- as.numeric(data$x)
                write(c("-------------------------------------------------------------------------------",
                        "Kruskal-Wallis rank sum test:"), stdout())
                print(kruskal.test(xn ~ y, data = data))
                kfeduqh <- xtabs(~ x + y, data = data)
                kfeduhq <- xtabs(~ y + x, data = data)
                write(c("-------------------------------------------------------------------------------",
                        "Post hoc multiple comparisons between different groups:"), stdout())
                write(c("--------------------", "Dunn's post hoc tests between different groups of y:"), stdout())
                print(dunnTest(xn ~ y, data = data, method = "bh"))
                write(c("--------------------", "Post hoc multiple comparisons between different groups of x:"), stdout())
                print(pairwiseNominalIndependence(kfeduqh))
                write(c("--------------------", "Post hoc multiple comparisons between different groups of y:"), stdout())
                print(pairwiseNominalIndependence(kfeduhq))
                write(c("-------------------------------------------------------------------------------",
                        "The Multinomial Cochran-Armitage trend test for x:"), stdout())
                data$xx <- factor(as.factor(as.numeric(data$x)), ordered = T)
                tab <- xtabs(~ y + xx, data = data)
                print(multiCA.test(tab)$overall)
              }
            }
          }
          if(typex == "numeric" | typex == "integer"){
            hist(data$x)
            qqnorm(data$x)
            qqline(data$x)
            write("The histogram and QQ plot of variable x have been drawn.", stdout())
            if(!is.null(norm.t)){
              descdist(data$x)
              for (i in norm.t) {
                if(i == "ks.test"){
                  print(ks.test(data$x,"pnorm",mean=mean(data$x),sd=sqrt(var(data$x))))
                }
                if(i == "shapiro.test"){
                  print(shapiro.test(data$x))
                }
                if(i == "cvm.test"){
                  print(cvm.test(data$x))
                }
                if(i == "ad.test"){
                  print(ad.test(data$x))
                }
                if(i == "lillie.test"){
                  print(lillie.test(data$x))
                }
                if(i == "pearson.test"){
                  print(pearson.test(data$x))
                }
                if(i == "sf.test"){
                  print(sf.test(data$x))
                }
                if(i != "ks.test" & i != "shapiro.test" & i != "cvm.test"
                   & i != "ad.test" & i != "lillie.test" & i != "pearson.test"
                   & i != "sf.test"){
                  stop("There are only seven normal test methods available: c('ks.test', 'shapiro.test', 'cvm.test', 'lillie.test', 'pearson.test', 'sf.test', 'ad.test').")
                }
              }
            }
            if(yl == 1){
              warning(c("y has only one element."))
              print(describe(data$x, quant = c(.05, .1, .25, .5, .75, .90, .95)))
            }
            if(yl == 2){
              write(c("-------------------------------------------------------------------------------",
                      "Two sample t-test:"), stdout())
              print(t.test(x ~ y, data = data))
              write(c("-------------------------------------------------------------------------------",
                      "Wilcoxon rank sum test:", "Mann-Whitney U test = Wilcoxon rank sum test"), stdout())
              print(wilcox.test(x ~ y, data = data))
              write(c("-------------------------------------------------------------------------------",
                      "Descriptive statistical results:"), stdout())
              print(describe(data$x, quant = c(.05, .1, .25, .5, .75, .90, .95)))
              write(c("-------------------------------------------------------------------------------",
                      "Descriptive statistical results stratified by y:"), stdout())
              print(describeBy(data$x, data$y, quant = c(.05, .1, .25, .5, .75, .90, .95)))
            }
            if(yl > 2){
              fit <- aov(x ~ y, data = data)
              kfit <- kruskal.test(x ~ y, data = data)
              llfitb <- dunnTest(x ~ y, data = data, method = "bh")
              write(c("-------------------------------------------------------------------------------",
                      "Variance analysis (one-way ANOVA):"), stdout())
              print(summary(fit))
              write(c("-------------------------------------------------------------------------------",
                      "Kruskal-Wallis rank sum test:"), stdout())
              print(kfit)
              write(c("-------------------------------------------------------------------------------",
                      "Tukey's HSD post hoc tests for normal x between different groups of y:"), stdout())
              print(TukeyHSD(fit))
              write(c("-------------------------------------------------------------------------------",
                      "Dunn's post hoc tests for non-normal x between different groups of y:"), stdout())
              print(llfitb)
              write(c("-------------------------------------------------------------------------------",
                      "Descriptive statistical results:"), stdout())
              print(describe(data$x, quant = c(.05, .1, .25, .5, .75, .90, .95)))
              write(c("-------------------------------------------------------------------------------",
                      "Descriptive statistical results stratified by y:"), stdout())
              print(describeBy(data$x, data$y, quant = c(.05, .1, .25, .5, .75, .90, .95)))
            }
          }
        }
        if(typey == "character" | typey == "logical"){
          data$y <- as.factor(data$y)
          typex <- class(data$x)[1]
          yl <- nlevels(data$y)
          if(typex == "factor"){
            xl <- nlevels(as.factor(as.character(data$x)))
            if(yl == 1){
              warning("y has only one element.")
              if(xl == 1){
                stop("x must at least have 2 elements.")
              }
              if(xl > 1){
                CrossTable(data$x, data$y, digits = 5)
              }
            }
            if(yl == 2){
              if(xl == 1){
                CrossTable(data$x, data$y, digits = 5)
                warning("x has only one element.")
              }
              if(xl == 2){
                CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T, fisher = T)
              }
              if(xl > 2){
                if(lth < 40){
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T, fisher = T)
                }else{
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T)
                }
                kfeduqh <- xtabs(~ x + y, data = data)
                write(c("-------------------------------------------------------------------------------",
                        "Post hoc multiple comparisons between different groups of x:"), stdout())
                print(pairwiseNominalIndependence(kfeduqh, method = "fdr"))
              }
            }
            if(yl > 2){
              if(xl == 1){
                CrossTable(data$x, data$y, digits = 5)
                warning("x has only one element.")
              }
              if(xl == 2){
                if(lth < 40){
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T, fisher = T)
                }else{
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T)
                }
                kfeduhq <- xtabs(~ y + x, data = data)
                write(c("-------------------------------------------------------------------------------",
                        "Post hoc multiple comparisons between different groups of y:"), stdout())
                print(pairwiseNominalIndependence(kfeduhq))
              }
              if(xl > 2){
                if(lth < 40){
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T, fisher = T)
                }else{
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T)
                }
                kfeduqh <- xtabs(~ x + y, data = data)
                kfeduhq <- xtabs(~ y + x, data = data)
                write(c("-------------------------------------------------------------------------------",
                        "Post hoc multiple comparisons between different groups:"), stdout())
                write(c("--------------------", "Post hoc multiple comparisons between different groups of x:"), stdout())
                print(pairwiseNominalIndependence(kfeduqh))
                write(c("--------------------", "Post hoc multiple comparisons between different groups of y:"), stdout())
                print(pairwiseNominalIndependence(kfeduhq))
              }
            }
          }
          if(typex == "logical" | typex == "character"){
            data$x <- as.character(data$x)
            data$x <- as.factor(data$x)
            xl <- nlevels(data$x)
            if(yl == 1){
              warning("y has only one element.")
              if(xl == 1){
                stop("x must at least have 2 elements.")
              }
              if(xl > 1){
                CrossTable(data$x, data$y, digits = 5)
              }
            }
            if(yl == 2){
              if(xl == 1){
                CrossTable(data$x, data$y, digits = 5)
                warning("x has only one element.")
              }
              if(xl == 2){
                CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T, fisher = T)
              }
              if(xl > 2){
                if(lth < 40){
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T, fisher = T)
                }else{
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T)
                }
                kfeduqh <- xtabs(~ x + y, data = data)
                write(c("-------------------------------------------------------------------------------",
                        "Post hoc multiple comparisons between different groups of x:"), stdout())
                print(pairwiseNominalIndependence(kfeduqh, method = "fdr"))
              }
            }
            if(yl > 2){
              if(xl == 1){
                CrossTable(data$x, data$y, digits = 5)
                warning("x has only one element.")
              }
              if(xl == 2){
                if(lth < 40){
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T, fisher = T)
                }else{
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T)
                }
                kfeduhq <- xtabs(~ y + x, data = data)
                write(c("-------------------------------------------------------------------------------",
                        "Post hoc multiple comparisons between different groups of y:"), stdout())
                print(pairwiseNominalIndependence(kfeduhq))
              }
              if(xl > 2){
                if(lth < 40){
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T, fisher = T)
                }else{
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T)
                }
                kfeduqh <- xtabs(~ x + y, data = data)
                kfeduhq <- xtabs(~ y + x, data = data)
                write(c("-------------------------------------------------------------------------------",
                        "Post hoc multiple comparisons between different groups:"), stdout())
                write(c("--------------------", "Post hoc multiple comparisons between different groups of x:"), stdout())
                print(pairwiseNominalIndependence(kfeduqh))
                write(c("--------------------", "Post hoc multiple comparisons between different groups of y:"), stdout())
                print(pairwiseNominalIndependence(kfeduhq))
              }
            }
          }
          if(typex == "ordered"){
            xl <- nlevels(as.factor(as.character(data$x)))
            if(yl == 1){
              warning("y has only one element.")
              if(xl == 1){
                stop("x must at least have 2 elements.")
              }
              if(xl > 1){
                CrossTable(data$x, data$y, digits = 5)
              }
            }
            if(yl == 2){
              if(xl == 1){
                CrossTable(data$x, data$y, digits = 5)
                warning("x has only one element.")
              }
              if(xl == 2){
                CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T, fisher = T)
              }
              if(xl > 2){
                if(lth < 40){
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T, fisher = T)
                }else{
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T)
                }
                kfeduqh <- xtabs(~ x + y, data = data)
                data$xn <- as.numeric(data$x)
                write(c("-------------------------------------------------------------------------------",
                        "Wilcoxon rank sum test:", "Mann-Whitney U test = Wilcoxon rank sum test"), stdout())
                print(wilcox.test(xn ~ y, data = data))
                write(c("-------------------------------------------------------------------------------",
                        "Post hoc multiple comparisons between different groups of x:"), stdout())
                print(pairwiseNominalIndependence(kfeduqh, method = "fdr"))
                write(c("-------------------------------------------------------------------------------",
                        "The Cochran-Armitage trend test for x:"), stdout())
                data$xx <- factor(as.factor(as.numeric(data$x)), ordered = T)
                print(CATT(data$y, data$xx))
              }
            }
            if(yl > 2){
              if(xl == 1){
                CrossTable(data$x, data$y, digits = 5)
                warning("x has only one element.")
              }
              if(xl == 2){
                if(lth < 40){
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T, fisher = T)
                }else{
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T)
                }
                kfeduhq <- xtabs(~ y + x, data = data)
                write(c("-------------------------------------------------------------------------------",
                        "Post hoc multiple comparisons between different groups of y:"), stdout())
                print(pairwiseNominalIndependence(kfeduhq))
              }
              if(xl > 2){
                if(lth < 40){
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T, fisher = T)
                }else{
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T)
                }
                data$xn <- as.numeric(data$x)
                write(c("-------------------------------------------------------------------------------",
                        "Kruskal-Wallis rank sum test:"), stdout())
                print(kruskal.test(xn ~ y, data = data))
                kfeduqh <- xtabs(~ x + y, data = data)
                kfeduhq <- xtabs(~ y + x, data = data)
                write(c("-------------------------------------------------------------------------------",
                        "Post hoc multiple comparisons between different groups:"), stdout())
                write(c("--------------------", "Dunn's post hoc tests between different groups of y:"), stdout())
                print(dunnTest(xn ~ y, data = data, method = "bh"))
                write(c("--------------------", "Post hoc multiple comparisons between different groups of x:"), stdout())
                print(pairwiseNominalIndependence(kfeduqh))
                write(c("--------------------", "Post hoc multiple comparisons between different groups of y:"), stdout())
                print(pairwiseNominalIndependence(kfeduhq))
                write(c("-------------------------------------------------------------------------------",
                        "The Multinomial Cochran-Armitage trend test for x:"), stdout())
                data$xx <- factor(as.factor(as.numeric(data$x)), ordered = T)
                tab <- xtabs(~ y + xx, data = data)
                print(multiCA.test(tab)$overall)
              }
            }
          }
          if(typex == "numeric" | typex == "integer"){
            hist(data$x)
            qqnorm(data$x)
            qqline(data$x)
            write("The histogram and QQ plot of variable x have been drawn.", stdout())
            if(!is.null(norm.t)){
              descdist(data$x)
              for (i in norm.t) {
                if(i == "ks.test"){
                  print(ks.test(data$x,"pnorm",mean=mean(data$x),sd=sqrt(var(data$x))))
                }
                if(i == "shapiro.test"){
                  print(shapiro.test(data$x))
                }
                if(i == "cvm.test"){
                  print(cvm.test(data$x))
                }
                if(i == "ad.test"){
                  print(ad.test(data$x))
                }
                if(i == "lillie.test"){
                  print(lillie.test(data$x))
                }
                if(i == "pearson.test"){
                  print(pearson.test(data$x))
                }
                if(i == "sf.test"){
                  print(sf.test(data$x))
                }
                if(i != "ks.test" & i != "shapiro.test" & i != "cvm.test"
                   & i != "ad.test" & i != "lillie.test" & i != "pearson.test"
                   & i != "sf.test"){
                  stop("There are only seven normal test methods available: c('ks.test', 'shapiro.test', 'cvm.test', 'lillie.test', 'pearson.test', 'sf.test', 'ad.test').")
                }
              }
            }
            if(yl == 1){
              warning(c("y has only one element."))
              print(describe(data$x, quant = c(.05, .1, .25, .5, .75, .90, .95)))
            }
            if(yl == 2){
              write(c("-------------------------------------------------------------------------------",
                      "Two sample t-test:"), stdout())
              print(t.test(x ~ y, data = data))
              write(c("-------------------------------------------------------------------------------",
                      "Wilcoxon rank sum test:", "Mann-Whitney U test = Wilcoxon rank sum test"), stdout())
              print(wilcox.test(x ~ y, data = data))
              write(c("-------------------------------------------------------------------------------",
                      "Descriptive statistical results:"), stdout())
              print(describe(data$x, quant = c(.05, .1, .25, .5, .75, .90, .95)))
              write(c("-------------------------------------------------------------------------------",
                      "Descriptive statistical results stratified by y:"), stdout())
              print(describeBy(data$x, data$y, quant = c(.05, .1, .25, .5, .75, .90, .95)))
            }
            if(yl > 2){
              fit <- aov(x ~ y, data = data)
              kfit <- kruskal.test(x ~ y, data = data)
              llfitb <- dunnTest(x ~ y, data = data, method = "bh")
              write(c("-------------------------------------------------------------------------------",
                      "Variance analysis (one-way ANOVA):"), stdout())
              print(summary(fit))
              write(c("-------------------------------------------------------------------------------",
                      "Kruskal-Wallis rank sum test:"), stdout())
              print(kfit)
              write(c("-------------------------------------------------------------------------------",
                      "Tukey's HSD post hoc tests for normal x between different groups of y:"), stdout())
              print(TukeyHSD(fit))
              write(c("-------------------------------------------------------------------------------",
                      "Dunn's post hoc tests for non-normal x between different groups of y:"), stdout())
              print(llfitb)
              write(c("-------------------------------------------------------------------------------",
                      "Descriptive statistical results:"), stdout())
              print(describe(data$x, quant = c(.05, .1, .25, .5, .75, .90, .95)))
              write(c("-------------------------------------------------------------------------------",
                      "Descriptive statistical results stratified by y:"), stdout())
              print(describeBy(data$x, data$y, quant = c(.05, .1, .25, .5, .75, .90, .95)))
            }
          }
        }
        if(typey == "ordered"){
          typex <- class(data$x)[1]
          yl <- nlevels(as.factor(as.character(data$y)))
          if(typex == "factor"){
            xl <- nlevels(as.factor(as.character(data$x)))
            if(yl == 1){
              warning("y has only one element.")
              if(xl == 1){
                stop("x must at least have 2 elements.")
              }
              if(xl > 1){
                CrossTable(data$x, data$y, digits = 5)
              }
            }
            if(yl == 2){
              if(xl == 1){
                CrossTable(data$x, data$y, digits = 5)
                warning("x has only one element.")
              }
              if(xl == 2){
                CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T, fisher = T)
              }
              if(xl > 2){
                if(lth < 40){
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T, fisher = T)
                }else{
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T)
                }
                kfeduqh <- xtabs(~ x + y, data = data)
                write(c("-------------------------------------------------------------------------------",
                        "Post hoc multiple comparisons between different groups of x:"), stdout())
                print(pairwiseNominalIndependence(kfeduqh, method = "fdr"))
              }
            }
            if(yl > 2){
              if(xl == 1){
                CrossTable(data$x, data$y, digits = 5)
                warning("x has only one element.")
              }
              if(xl == 2){
                if(lth < 40){
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T, fisher = T)
                }else{
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T)
                }
                kfeduhq <- xtabs(~ y + x, data = data)
                data$yn <- as.numeric(data$y)
                write(c("-------------------------------------------------------------------------------",
                        "Wilcoxon rank sum test:", "Mann-Whitney U test = Wilcoxon rank sum test"), stdout())
                print(wilcox.test(yn ~ x, data = data))
                write(c("-------------------------------------------------------------------------------",
                        "Post hoc multiple comparisons between different groups of y:"), stdout())
                print(pairwiseNominalIndependence(kfeduhq))
                write(c("-------------------------------------------------------------------------------",
                        "The Cochran-Armitage trend test for y:"), stdout())
                data$yy <- factor(as.factor(as.numeric(data$y)), ordered = T)
                print(CATT(data$x, data$yy))
              }
              if(xl > 2){
                if(lth < 40){
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T, fisher = T)
                }else{
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T)
                }
                data$yn <- as.numeric(data$y)
                write(c("-------------------------------------------------------------------------------",
                        "Kruskal-Wallis rank sum test:"), stdout())
                print(kruskal.test(yn ~ x, data = data))
                kfeduqh <- xtabs(~ x + y, data = data)
                kfeduhq <- xtabs(~ y + x, data = data)
                write(c("-------------------------------------------------------------------------------",
                        "Post hoc multiple comparisons between different groups:"), stdout())
                write(c("--------------------", "Dunn's post hoc tests between different groups of x:"), stdout())
                print(dunnTest(yn ~ x, data = data, method = "bh"))
                write(c("--------------------", "Post hoc multiple comparisons between different groups of x:"), stdout())
                print(pairwiseNominalIndependence(kfeduqh))
                write(c("--------------------", "Post hoc multiple comparisons between different groups of y:"), stdout())
                print(pairwiseNominalIndependence(kfeduhq))
                write(c("-------------------------------------------------------------------------------",
                        "The Multinomial Cochran-Armitage trend test for y:"), stdout())
                data$yy <- factor(as.factor(as.numeric(data$y)), ordered = T)
                tab <- xtabs(~ x + yy, data = data)
                print(multiCA.test(tab)$overall)
              }
            }
          }
          if(typex == "logical" | typex == "character"){
            data$x <- as.character(data$x)
            data$x <- as.factor(data$x)
            xl <- nlevels(data$x)
            if(yl == 1){
              warning("y has only one element.")
              if(xl == 1){
                stop("x must at least have 2 elements.")
              }
              if(xl > 1){
                CrossTable(data$x, data$y, digits = 5)
              }
            }
            if(yl == 2){
              if(xl == 1){
                CrossTable(data$x, data$y, digits = 5)
                warning("x has only one element.")
              }
              if(xl == 2){
                CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T, fisher = T)
              }
              if(xl > 2){
                if(lth < 40){
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T, fisher = T)
                }else{
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T)
                }
                kfeduqh <- xtabs(~ x + y, data = data)
                write(c("-------------------------------------------------------------------------------",
                        "Post hoc multiple comparisons between different groups of x:"), stdout())
                print(pairwiseNominalIndependence(kfeduqh, method = "fdr"))
              }
            }
            if(yl > 2){
              if(xl == 1){
                CrossTable(data$x, data$y, digits = 5)
                warning("x has only one element.")
              }
              if(xl == 2){
                if(lth < 40){
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T, fisher = T)
                }else{
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T)
                }
                kfeduhq <- xtabs(~ y + x, data = data)
                data$yn <- as.numeric(data$y)
                write(c("-------------------------------------------------------------------------------",
                        "Wilcoxon rank sum test:", "Mann-Whitney U test = Wilcoxon rank sum test"), stdout())
                print(wilcox.test(yn ~ x, data = data))
                write(c("-------------------------------------------------------------------------------",
                        "Post hoc multiple comparisons between different groups of y:"), stdout())
                print(pairwiseNominalIndependence(kfeduhq))
                write(c("-------------------------------------------------------------------------------",
                        "The Cochran-Armitage trend test for y:"), stdout())
                data$yy <- factor(as.factor(as.numeric(data$y)), ordered = T)
                print(CATT(data$x, data$yy))
              }
              if(xl > 2){
                if(lth < 40){
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T, fisher = T)
                }else{
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T)
                }
                data$yn <- as.numeric(data$y)
                write(c("-------------------------------------------------------------------------------",
                        "Kruskal-Wallis rank sum test:"), stdout())
                print(kruskal.test(yn ~ x, data = data))
                kfeduqh <- xtabs(~ x + y, data = data)
                kfeduhq <- xtabs(~ y + x, data = data)
                write(c("-------------------------------------------------------------------------------",
                        "Post hoc multiple comparisons between different groups:"), stdout())
                write(c("--------------------", "Dunn's post hoc tests between different groups of x:"), stdout())
                print(dunnTest(yn ~ x, data = data, method = "bh"))
                write(c("--------------------", "Post hoc multiple comparisons between different groups of x:"), stdout())
                print(pairwiseNominalIndependence(kfeduqh))
                write(c("--------------------", "Post hoc multiple comparisons between different groups of y:"), stdout())
                print(pairwiseNominalIndependence(kfeduhq))
                write(c("-------------------------------------------------------------------------------",
                        "The Multinomial Cochran-Armitage trend test for y:"), stdout())
                data$yy <- factor(as.factor(as.numeric(data$y)), ordered = T)
                tab <- xtabs(~ x + yy, data = data)
                print(multiCA.test(tab)$overall)
              }
            }
          }
          if(typex == "ordered"){
            xl <- nlevels(as.factor(as.character(data$x)))
            if(yl == 1){
              warning("y has only one element.")
              if(xl == 1){
                stop("x must at least have 2 elements.")
              }
              if(xl > 1){
                CrossTable(data$x, data$y, digits = 5)
              }
            }
            if(yl == 2){
              if(xl == 1){
                CrossTable(data$x, data$y, digits = 5)
                warning("x has only one element.")
              }
              if(xl == 2){
                CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T, fisher = T)
              }
              if(xl > 2){
                if(lth < 40){
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T, fisher = T)
                }else{
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T)
                }
                kfeduqh <- xtabs(~ x + y, data = data)
                data$xn <- as.numeric(data$x)
                write(c("-------------------------------------------------------------------------------",
                        "Kruskal-Wallis rank sum test:"), stdout())
                print(kruskal.test(xn ~ y, data = data))
                write(c("-------------------------------------------------------------------------------",
                        "Post hoc multiple comparisons between different groups of x:"), stdout())
                print(pairwiseNominalIndependence(kfeduqh, method = "fdr"))
                write(c("-------------------------------------------------------------------------------",
                        "The Cochran-Armitage trend test for x:"), stdout())
                data$xx <- factor(as.factor(as.numeric(data$x)), ordered = T)
                print(CATT(data$y, data$xx))
              }
            }
            if(yl > 2){
              if(xl == 1){
                CrossTable(data$x, data$y, digits = 5)
                warning("x has only one element.")
              }
              if(xl == 2){
                if(lth < 40){
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T, fisher = T)
                }else{
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T)
                }
                kfeduhq <- xtabs(~ y + x, data = data)
                data$yn <- as.numeric(data$y)
                write(c("-------------------------------------------------------------------------------",
                        "Kruskal-Wallis rank sum test:"), stdout())
                print(kruskal.test(yn ~ x, data = data))
                write(c("-------------------------------------------------------------------------------",
                        "Post hoc multiple comparisons between different groups of y:"), stdout())
                print(pairwiseNominalIndependence(kfeduhq))
                write(c("-------------------------------------------------------------------------------",
                        "The Cochran-Armitage trend test for y:"), stdout())
                data$yy <- factor(as.factor(as.numeric(data$y)), ordered = T)
                print(CATT(data$x, data$yy))
              }
              if(xl > 2){
                if(lth < 40){
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T, fisher = T)
                }else{
                  CrossTable(data$x, data$y, digits = 5, expected = T, chisq = T)
                }
                data$xn <- as.numeric(data$x)
                write(c("-------------------------------------------------------------------------------",
                        "Kruskal-Wallis rank sum test for x between different groups of y:"), stdout())
                print(kruskal.test(xn ~ y, data = data))
                data$yn <- as.numeric(data$y)
                write(c("-------------------------------------------------------------------------------",
                        "Kruskal-Wallis rank sum test for y between different groups of x:"), stdout())
                print(kruskal.test(yn ~ x, data = data))
                kfeduqh <- xtabs(~ x + y, data = data)
                kfeduhq <- xtabs(~ y + x, data = data)
                write(c("-------------------------------------------------------------------------------",
                        "Post hoc multiple comparisons between different groups:"), stdout())
                write(c("--------------------", "Dunn's post hoc tests between different groups of x:"), stdout())
                print(dunnTest(yn ~ x, data = data, method = "bh"))
                write(c("--------------------", "Dunn's post hoc tests between different groups of y:"), stdout())
                print(dunnTest(xn ~ y, data = data, method = "bh"))
                write(c("--------------------", "Post hoc multiple comparisons between different groups of x:"), stdout())
                print(pairwiseNominalIndependence(kfeduqh))
                write(c("--------------------", "Post hoc multiple comparisons between different groups of y:"), stdout())
                print(pairwiseNominalIndependence(kfeduhq))
                write(c("-------------------------------------------------------------------------------",
                        "The Multinomial Cochran-Armitage trend test for x:"), stdout())
                data$xx <- factor(as.factor(as.numeric(data$x)), ordered = T)
                tab <- xtabs(~ y + xx, data = data)
                print(multiCA.test(tab)$overall)
                write(c("-------------------------------------------------------------------------------",
                        "The Multinomial Cochran-Armitage trend test for y:"), stdout())
                data$yy <- factor(as.factor(as.numeric(data$y)), ordered = T)
                tab <- xtabs(~ x + yy, data = data)
                print(multiCA.test(tab)$overall)
              }
            }
          }
          if(typex == "numeric" | typex == "integer"){
            hist(data$x)
            qqnorm(data$x)
            qqline(data$x)
            write("The histogram and QQ plot of variable x have been drawn.", stdout())
            if(!is.null(norm.t)){
              descdist(data$x)
              for (i in norm.t) {
                if(i == "ks.test"){
                  print(ks.test(data$x,"pnorm",mean=mean(data$x),sd=sqrt(var(data$x))))
                }
                if(i == "shapiro.test"){
                  print(shapiro.test(data$x))
                }
                if(i == "cvm.test"){
                  print(cvm.test(data$x))
                }
                if(i == "ad.test"){
                  print(ad.test(data$x))
                }
                if(i == "lillie.test"){
                  print(lillie.test(data$x))
                }
                if(i == "pearson.test"){
                  print(pearson.test(data$x))
                }
                if(i == "sf.test"){
                  print(sf.test(data$x))
                }
                if(i != "ks.test" & i != "shapiro.test" & i != "cvm.test"
                   & i != "ad.test" & i != "lillie.test" & i != "pearson.test"
                   & i != "sf.test"){
                  stop("There are only seven normal test methods available: c('ks.test', 'shapiro.test', 'cvm.test', 'lillie.test', 'pearson.test', 'sf.test', 'ad.test').")
                }
              }
            }
            if(yl == 1){
              warning(c("y has only one element."))
              print(describe(data$x, quant = c(.05, .1, .25, .5, .75, .90, .95)))
            }
            if(yl == 2){
              write(c("-------------------------------------------------------------------------------",
                      "Two sample t-test:"), stdout())
              print(t.test(x ~ y, data = data))
              write(c("-------------------------------------------------------------------------------",
                      "Wilcoxon rank sum test:", "Mann-Whitney U test = Wilcoxon rank sum test"), stdout())
              print(wilcox.test(x ~ y, data = data))
              write(c("-------------------------------------------------------------------------------",
                      "Descriptive statistical results:"), stdout())
              print(describe(data$x, quant = c(.05, .1, .25, .5, .75, .90, .95)))
              write(c("-------------------------------------------------------------------------------",
                      "Descriptive statistical results stratified by y:"), stdout())
              print(describeBy(data$x, data$y, quant = c(.05, .1, .25, .5, .75, .90, .95)))
            }
            if(yl > 2){
              fit <- aov(x ~ y, data = data)
              kfit <- kruskal.test(x ~ y, data = data)
              llfitb <- dunnTest(x ~ y, data = data, method = "bh")
              write(c("-------------------------------------------------------------------------------",
                      "Variance analysis (one-way ANOVA):"), stdout())
              print(summary(fit))
              write(c("-------------------------------------------------------------------------------",
                      "Kruskal-Wallis rank sum test:"), stdout())
              print(kfit)
              write(c("-------------------------------------------------------------------------------",
                      "Tukey's HSD post hoc tests for normal x between different groups of y:"), stdout())
              print(TukeyHSD(fit))
              write(c("-------------------------------------------------------------------------------",
                      "Dunn's post hoc tests for non-normal x between different groups of y:"), stdout())
              print(llfitb)
              write(c("-------------------------------------------------------------------------------",
                      "The Variance Analysis Trend Test for y:"), stdout())
              data$yn <- as.numeric(data$y)
              jjj <- summary(aov(x ~ yn, data = data))
              jj <- data.frame(jjj[[1]])
              fv2 <- jj$F.value
              fv <- fv2[1]
              pv2 <- jj$Pr..F.
              pv <- pv2[1]
              xname <- "x"
              fenname <- "y"
              dname <- sprintf("%s and %s", xname, fenname)
              print(structure(list(method = "The Variance Analysis Trend Test",
                                   statistic = c(F.value = fv),
                                   p.value = pv, data.name = dname), class = "htest"))
              write(c("-------------------------------------------------------------------------------",
                      "Descriptive statistical results:"), stdout())
              print(describe(data$x, quant = c(.05, .1, .25, .5, .75, .90, .95)))
              write(c("-------------------------------------------------------------------------------",
                      "Descriptive statistical results stratified by y:"), stdout())
              print(describeBy(data$x, data$y, quant = c(.05, .1, .25, .5, .75, .90, .95)))
            }
          }
        }
        if(typey != "factor" &
           typey != "ordered" &
           typey != "character" &
           typey != "logical"){
          stop("y must be a factor, a character, or a logical value.")
        }
      }
      {
        if(na.rm == FALSE){
          nanumx <- sum(is.na(datana$x))
          pronax <- round(nanumx/(dim(datana)[1]), digits = 5)*100
          nanumy <- sum(is.na(datana$y))
          pronay <- round(nanumy/(dim(datana)[1]), digits = 5)*100
          nanumxy <- sum((is.na(datana$x) & is.na(datana$y)))
          pronaxy <- round(nanumxy/(dim(datana)[1]), digits = 5)*100
          shu <- paste("The number of NA in x is ", nanumx, " (", pronax, "%), ",
                       "The number of NA in y is ", nanumy, " (", pronay, "%), ",
                       "The number of NA in both x and y is ", nanumxy, " (", pronaxy, "%), ", sep = "")
          write(c("-------------------------------------------------------------------------------"), stdout())
          write(shu, stdout())
        }
      }
    }
  }
}
