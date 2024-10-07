# --------------------------------------
# FUNCTION q_hist
# required packages: none
# description:
# inputs:
# outputs:
########################################
#' Creates a Histogram of a Continuous Variable
#'
#' This function plots a histogram of a continuous variable,
#' along with its mean, 95% confidence interval, and a smoothed
#' density curve. A one-sample t-test compares the observed mean to
#' an expectation from the null hypothesis (default h_0 = 0.0)
#'
#' @param x a vector of numeric values (integer or real)
#' @param h_0 a constant numeric for the expectation of the
#' null hypothesis. The default value is h_0 = 0.0.
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' q_hist(x=runif(1000),h_0 = 0.50)
#' }
#'
q_hist <- function (x = rnorm(100),  h_0 = 0) {

# function body
    df <- data.frame(x = x)
    mean <- paste("mean =", round(mean(x), digits = 3))
    var <- paste("; var =", round(var(x), digits = 3))
    null <- paste("; H_0 =", round(h_0, digits = 3))
    z <- t.test(x, mu = h_0)$p.value
    p_val <- paste(": p =", signif(z, digits = 3))

    z <- ggplot2::ggplot(df) +
         aes(x = x) +
         geom_vline(xintercept = c(mean(df$x),
         quantile(df$x, prob = c(0.025, 0.975)),
         h_0),
         linetype = c("solid", "dashed", "dashed", "solid"),
         col = c("black", "black", "black", "red")) +
         geom_density(col="grey80",fill="grey80") +
         geom_histogram(aes(y = after_stat(density)),
                        col = "black",
                        fill = "wheat") +
         theme_classic(base_size = 20,
                       base_family="serif") +
         theme(plot.subtitle = element_text(size = 15)) +
         labs(subtitle = paste0(mean, var, null, p_val)) +
         xlab("X Variable") +
         ylab("Density")
    plot(z)



} # end of function q_hist
# --------------------------------------
# q_hist()
