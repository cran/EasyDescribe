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
#' @export
#'
#' @return No return value, called for side effects.
#'
#' @importFrom "graphics" "hist"
#' @importFrom "stats" "TukeyHSD" "aov" "kruskal.test" "na.omit" "qqline" "qqnorm" "t.test" "wilcox.test" "xtabs"
#' @importFrom "gmodels" "CrossTable"
#' @importFrom "psych" "describe" "describeBy"
#' @importFrom "trend" "mk.test"
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
#' @examples
#' data(T2D)
#' fundescribe(T2D$age)
#' fundescribe(gender, data = T2D)
#' fundescribe(education, diabetes, data = T2D)
#' fundescribe(T2D$glucose, T2D$diabetes)
fundescribe <- function(x, y, data = NULL, na.rm = TRUE){
  {
    if(missing(y)){
      {
        if(is.null(data)){
          data <- data.frame(x)
          colnames(data) <- c("x")
          datana <- data
          data <- na.omit(data)
        }
        else{
          xname <- deparse(substitute(x))
          coln <- colnames(data)
          xwz <- as.numeric(which(coln == xname))
          data <- data.frame(data[xwz])
          colnames(data) <- c("x")
          datana <- data
          data <- na.omit(data)
        }
        if(length(data$x) == 0){
          shuna <- "Only NA is included in x."
          write(shuna, stdout())
        }
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
          warning("Mann-Kendall trend test is a single sample trend test, and it should be sorted according to time variables in advance.")
          print(mk.test(data$x))
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
          data <- data.frame(x, y)
          colnames(data) <- c("x", "y")
          datana <- data
          data <- na.omit(data)
        }
        else{
          xname <- deparse(substitute(x))
          yname <- deparse(substitute(y))
          coln <- colnames(data)
          xwz <- as.numeric(which(coln == xname))
          ywz <- as.numeric(which(coln == yname))
          data <- data.frame(data[xwz], data[ywz])
          colnames(data) <- c("x", "y")
          datana <- data
          data <- na.omit(data)
        }
        if(length(data$x) == 0){
          shuna <- "Only NA is included in x or y."
          write(shuna, stdout())
        }
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
                        "Post hoc multiple comparisons between different groups:"), stdout())
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
                        "Post hoc multiple comparisons between different groups:"), stdout())
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
                        "Post hoc multiple comparisons between different groups:"), stdout())
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
                        "Post hoc multiple comparisons between different groups:"), stdout())
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
                write(c("-------------------------------------------------------------------------------",
                        "Post hoc multiple comparisons between different groups:"), stdout())
                print(pairwiseNominalIndependence(kfeduqh, method = "fdr"))
                write(c("-------------------------------------------------------------------------------",
                        "The Cochran-Armitage trend test:"), stdout())
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
                        "Post hoc multiple comparisons between different groups:"), stdout())
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
                write(c("-------------------------------------------------------------------------------",
                        "The Multinomial Cochran-Armitage trend test:"), stdout())
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
                      "Tukey's HSD post hoc tests for normal data:"), stdout())
              print(TukeyHSD(fit))
              write(c("-------------------------------------------------------------------------------",
                      "Dunn's post hoc tests for non-normal data:"), stdout())
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
                        "Post hoc multiple comparisons between different groups:"), stdout())
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
                        "Post hoc multiple comparisons between different groups:"), stdout())
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
                        "Post hoc multiple comparisons between different groups:"), stdout())
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
                        "Post hoc multiple comparisons between different groups:"), stdout())
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
                write(c("-------------------------------------------------------------------------------",
                        "Post hoc multiple comparisons between different groups:"), stdout())
                print(pairwiseNominalIndependence(kfeduqh, method = "fdr"))
                write(c("-------------------------------------------------------------------------------",
                        "The Cochran-Armitage trend test:"), stdout())
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
                        "Post hoc multiple comparisons between different groups:"), stdout())
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
                write(c("-------------------------------------------------------------------------------",
                        "The Multinomial Cochran-Armitage trend test:"), stdout())
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
                      "Tukey's HSD post hoc tests for normal data:"), stdout())
              print(TukeyHSD(fit))
              write(c("-------------------------------------------------------------------------------",
                      "Dunn's post hoc tests for non-normal data:"), stdout())
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
                        "Post hoc multiple comparisons between different groups:"), stdout())
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
                        "Post hoc multiple comparisons between different groups:"), stdout())
                print(pairwiseNominalIndependence(kfeduhq))
                write(c("-------------------------------------------------------------------------------",
                        "The Cochran-Armitage trend test:"), stdout())
                data$yy <- factor(as.factor(as.numeric(data$y)), ordered = T)
                print(CATT(data$x, data$yy))
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
                write(c("-------------------------------------------------------------------------------",
                        "The Multinomial Cochran-Armitage trend test:"), stdout())
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
                        "Post hoc multiple comparisons between different groups:"), stdout())
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
                        "Post hoc multiple comparisons between different groups:"), stdout())
                print(pairwiseNominalIndependence(kfeduhq))
                write(c("-------------------------------------------------------------------------------",
                        "The Cochran-Armitage trend test:"), stdout())
                data$yy <- factor(as.factor(as.numeric(data$y)), ordered = T)
                print(CATT(data$x, data$yy))
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
                write(c("-------------------------------------------------------------------------------",
                        "The Multinomial Cochran-Armitage trend test:"), stdout())
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
                write(c("-------------------------------------------------------------------------------",
                        "Post hoc multiple comparisons between different groups:"), stdout())
                print(pairwiseNominalIndependence(kfeduqh, method = "fdr"))
                write(c("-------------------------------------------------------------------------------",
                        "The Cochran-Armitage trend test:"), stdout())
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
                        "Post hoc multiple comparisons between different groups:"), stdout())
                print(pairwiseNominalIndependence(kfeduhq))
                write(c("-------------------------------------------------------------------------------",
                        "The Cochran-Armitage trend test:"), stdout())
                data$yy <- factor(as.factor(as.numeric(data$y)), ordered = T)
                print(CATT(data$x, data$yy))
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
                write(c("-------------------------------------------------------------------------------",
                        "The Multinomial Cochran-Armitage trend test of x:"), stdout())
                data$xx <- factor(as.factor(as.numeric(data$x)), ordered = T)
                tab <- xtabs(~ y + xx, data = data)
                print(multiCA.test(tab)$overall)
                write(c("-------------------------------------------------------------------------------",
                        "The Multinomial Cochran-Armitage trend test of y:"), stdout())
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
                      "Tukey's HSD post hoc tests for normal data:"), stdout())
              print(TukeyHSD(fit))
              write(c("-------------------------------------------------------------------------------",
                      "Dunn's post hoc tests for non-normal data:"), stdout())
              print(llfitb)
              write(c("-------------------------------------------------------------------------------",
                      "The Variance Analysis Trend Test:"), stdout())
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
