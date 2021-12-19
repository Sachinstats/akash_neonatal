library(finalfit)
library(dplyr)
library(ggplot2)

or_plot_code <- function (.data, dependent, explanatory, random_effect = NULL,
                          factorlist = NULL, glmfit = NULL, confint_type = NULL, remove_ref = FALSE,
                          breaks = NULL, column_space = c(-0.5, 0, 0.5), dependent_label = NULL,
                          prefix = "", suffix = ": OR (95% CI, p-value)",
                          table_text_size = 5, title_text_size = 18, plot_opts = NULL,
                          table_opts = NULL, ...)
{
  requireNamespace("ggplot2")
  if (!is.null(factorlist)) {
    if (is.null(factorlist$Total))
      stop("summary_factorlist function must include total_col=TRUE")
    if (is.null(factorlist$fit_id))
      stop("summary_factorlist function must include fit_id=TRUE")
  }
  if (is.null(factorlist)) {
    factorlist = summary_factorlist(.data, dependent, explanatory,
                                    total_col = TRUE, fit_id = TRUE)
  }
  if (remove_ref) {
    factorlist = factorlist %>% dplyr::mutate(label = ifelse(label ==
                                                               "", NA, label)) %>% tidyr::fill(label) %>%
      dplyr::group_by(label) %>% dplyr::filter(dplyr::row_number() !=
                                                 1 | dplyr::n() > 2 | levels %in% c("Mean (SD)",
                                                                                    "Median (IQR)")) %>% rm_duplicate_labels()
  }
  if (is.null(breaks)) {
    breaks = scales::pretty_breaks()
  }
  if (is.null(confint_type) && is.null(random_effect)) {
    confint_type = "profile"
  }
  else if (is.null(confint_type) && (!is.null(random_effect) |
                                     class(glmfit) == "glmerMod")) {
    confint_type = "default"
  }
  if (is.null(glmfit) && is.null(random_effect)) {
    glmfit = glmuni(.data, dependent, explanatory)
    glmfit_df_c = fit2df(glmfit, condense = TRUE, estimate_suffix = " (univariable)",
                         confint_type = confint_type, ...)
  }
  else if (is.null(glmfit) && !is.null(random_effect)) {
    glmfit = glmmixed(.data, dependent, explanatory, random_effect)
    glmfit_df_c = fit2df(glmfit, condense = TRUE, estimate_suffix = " (multilevel)",
                         confint_type = confint_type, ...)
  }
  if (!is.null(glmfit) && is.null(random_effect)) {
    glmfit_df_c = fit2df(glmfit, condense = TRUE, estimate_suffix = " (multivariable)",
                         confint_type = confint_type, estimate_name = "OR",
                         exp = TRUE, ...)
  }
  else if (!is.null(glmfit) && !is.null(random_effect)) {
    glmfit_df_c = fit2df(glmfit, condense = TRUE, estimate_suffix = " (multilevel)",
                         confint_type = confint_type, estimate_name = "OR",
                         exp = TRUE, ...)
  }
  glmfit_df = fit2df(glmfit, condense = FALSE, confint_type = confint_type,
                     estimate_name = "OR", exp = TRUE, ...)
  df.out = finalfit_merge(factorlist, glmfit_df_c)
  df.out = finalfit_merge(df.out, glmfit_df, ref_symbol = "1.0")
  df.out$Total = stringr::str_remove(df.out$Total, " \\(.*\\)") %>%
    as.numeric()
  df.out$Total[which(df.out$levels %in% c("Mean (SD)",
                                          "Median (IQR)"))] = dim(.data)[1]
  df.out$levels[which(df.out$levels %in% c("Mean (SD)",
                                           "Median (IQR)"))] = "-"
  if (any(is.na(df.out$label))) {
    remove_rows = which(is.na(df.out$label))
    df.out = df.out[-remove_rows, ]
  }
  else {
    df.out
  }
  df.out$levels = as.character(df.out$levels)
  df.out$fit_id = factor(df.out$fit_id, levels = df.out$fit_id[order(-df.out$index)])
  g1 = ggplot(df.out, aes(x = as.numeric(OR), xmin = as.numeric(L95),
                          xmax = as.numeric(U95), y = fit_id)) + geom_point(aes(size = Total),
                                                                            shape = 22, fill = "darkblue") + geom_errorbarh(height = 0.2) +
    geom_vline(xintercept = 1, linetype = "longdash",
               colour = "black") + scale_x_continuous(trans = "log10",
                                                      breaks = breaks) + xlab("Odds ratio (95% CI, log scale)") +
    theme_classic(14) + theme(axis.title.x = element_text(),
                              axis.title.y = element_blank(), axis.text.y = element_blank(),
                              axis.line.y = element_blank(), axis.ticks.y = element_blank(),
                              legend.position = "none")
  t1 = ggplot(df.out, aes(x = as.numeric(OR), y = fit_id)) +
    annotate("text", x = column_space[1], y = df.out$fit_id,
             label = df.out[, 2], hjust = 0, size = table_text_size) +
    annotate("text", x = column_space[2], y = df.out$fit_id,
             label = df.out[, 3], hjust = 1, size = table_text_size) +
    annotate("text", x = column_space[3], y = df.out$fit_id,
             label = df.out[, 8], hjust = 1, size = table_text_size) +
    theme_classic(14) + theme(axis.title.x = element_text(colour = "white"),
                              axis.text.x = element_text(colour = "white"), axis.title.y = element_blank(),
                              axis.text.y = element_blank(), axis.ticks.y = element_blank(),
                              line = element_blank())
  g1 = g1 + plot_opts
  t1 = t1 + table_opts
  title = plot_title(.data, dependent, dependent_label = dependent_label,
                     prefix = prefix, suffix = suffix)
  gridExtra::grid.arrange(t1, g1, ncol = 2, widths = c(3, 2),
                          top = grid::textGrob(title, x = 0.02, y = 0.2, gp = grid::gpar(fontsize = title_text_size),
                                               just = "left"))
}

# OR plotdata(colon_s)
explanatory = c("age.factor", "sex.factor", "obstruct.factor", "perfor.factor")
dependent = "mort_5yr"
colon_s %>%finalfit(dependent, explanatory )
colon_s %>%or_plot(dependent, explanatory, table_text_size=4, title_text_size=14,
                   plot_opts=list(xlab("OR, 95% CI"), theme(axis.title = element_text(size=12))))

colon_s %>%or_plot_code(dependent, explanatory, table_text_size=4, title_text_size=14,
                        plot_opts=list(xlab("OR, 95% CI"), theme(axis.title = element_text(size=12))))
