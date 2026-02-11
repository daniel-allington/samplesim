library(dplyr)
library(purrr)
library(reshape2)
library(ggplot2)

complement <- function(x, rho) {
  y <- rnorm(length(x))
  x.perp <- residuals(lm(y ~ x))
  rho * sd(x.perp) * x + x.perp * sd(y) * sqrt(1 - rho^2)
}

take.sample <- function(x, n, data.defect.correlation) {
  proportion.to.sample <- n / length(x)
  y <- complement(x, data.defect.correlation * 2)
  sampling.p <- ecdf(y)(y) * proportion.to.sample * 2
  sample(x, size = n, replace = FALSE, prob = sampling.p)
}

prop.ci <- function(p, n, conf.level) {
  conf.level <- conf.level %>% as.character
  critical.values <- c('0.9' = 1.645, '0.95' = 1.96, '0.99' = 2.575)
  if(!conf.level %in% names(critical.values)) {
    stop(paste('Acceptable confidence levels:', paste(names(critical.values), collapse = ', ')))
  }
  moe <- critical.values[conf.level] * sqrt(p * (1 - p) / n)
  tibble(Low = p - moe, High = p + moe)
}

justmid <- function(d) {
  d %>%
    summarise(
      Mid = mean(p)
    )
}

minmax <- function(d) {
  d %>%
    summarise(
      Low = min(p),
      High = max(p)
    )
}

sdrange <- function(d) {
  d %>%
    summarise(
      Low = mean(p) - sd(p),
      High = mean(p) + sd(p)
    )
}

visualise.samples <- function(
    d, 
    include.mean = FALSE, 
    population.line = FALSE,
    range.function = NULL,
    confint = NULL) {
  
  d <- d %>%
    mutate(
      Sample = factor(
        Sample,
        levels = c(
          'Pop.', d$Sample[d$Sample!='Pop.']) %>% rev
      ),
      Colour = ifelse(
        Sample == 'Pop.',
        'Pop.', 'Sample'
      )
    )
  
  if(include.mean) {
    d.sample.summary <- d %>%
      filter(
        Sample != 'Pop.'
      ) %>%
      summarise(Mid = mean(p)) %>%
      melt(id = NULL, variable.name = 'Statistic', value.name = 'Value')
  } else {
    d.sample.summary <- tibble(
      Statistics = character(), Value = numeric()
    )
  }
  
  if(!is.null(range.function)) {
    d.sample.summary <- d %>%
      filter(
        Sample != 'Pop.'
      ) %>%
      range.function %>%
      melt(id = NULL, variable.name = 'Statistic', value.name = 'Value') %>%
      rbind(
        d.sample.summary
      )
  }
  
  p <- d %>%
    ggplot(
      aes(x = Sample, y = p, colour = Colour, fill = Colour)
    ) +
    geom_col()
  
  if(population.line) {
    p <- p +
      geom_hline(
        yintercept = d$p[d$Sample=='Pop.'],
        linetype = 'dashed',
        linewidth = 1,
        colour = 'darkred'
      )
  }
  
  if(!is.null(confint)) {
    p <- p +
      geom_errorbar(
        aes(
          ymin = Low,
          ymax = High
        ),
        data = tibble(d, prop.ci(d$p, d$n, confint)),
        width = 0.25
      )
  }
  
  if(nrow(d.sample.summary) > 0) {
    p <- p +
      geom_hline(
        aes(yintercept = Value, linetype = Statistic),
        linewidth = 1,
        data = d.sample.summary
      ) +
      scale_linetype_manual(
        values = c(
          Low = 'dotted', Mid = 'dotdash', High = 'dotted'), guide = 'none')
  }
  
  p +
    scale_x_discrete(
      '') +
    scale_y_continuous(
      '', limits = c(0,1), labels = scales::percent) +
    scale_colour_manual(
      values = c(
        'Pop.' = 'darkred', 'Sample' = 'darkblue'), guide = 'none') +
    scale_fill_manual(
      values = c(
        'Pop.' = 'pink', 'Sample' = 'lightblue'), guide = 'none') +
    coord_flip() +
    theme_bw()
}
