#---------------------------------------#
#            load libraries             #
#---------------------------------------#
library(tidyverse)
library(cowplot)
library(modelr)
library(ez)
library(rootSolve)
library(nleqslv)
library(ggsignif)
library(ggpubr)
library(reshape2)
library(KernSmooth)


#---------------------------------------#
#         global configuration          #
#---------------------------------------#
# theme setting
theme_set(theme_classic())
# flag for save figures
saveFigure = TRUE

#--------------------------------------#
#  load data: 3 experimental datasets  #
#--------------------------------------#
# see also readme.pdf in experiments folder
# Exp. 1. Replication of Horowitz & Wolfe (1998) Exp. 1
# Exp. 2. reward manipulation: session wise (two sessions/exps)
# Exp. 3. task difficulty increased,  the search difficulty was varied to manipulate decision sensitivity
exp_names = c('exp1', 'exp2', 'exp3')
exp_files = file.path('data', paste0(exp_names, '.csv'))
exps = map(exp_files, read.csv)

#---------------------------------------#
#    define some analysis functions     #
# such as getMeanData, sdt_dprime,      #
#  getSlope, basicEstimates,...         #
#---------------------------------------#

#' Get Mean Data
#' a function to get the mean RT, Error, etc.
#' @param df experimental raw data
#'
getMeanData <- function(df) {
  # prepare group variables
  if ('reward' %in% colnames(df)) {
    # second experiment with reward
    gp_var = c("sub", "setsize", "target", "dyn", "reward")
  } else {
    gp_var = c("sub", "setsize", "target", "dyn")
  }
  gp_vars = syms(gp_var) # prepare the group variables
  
  # 1. error analysis: mean errors for each subject
  error_sub = df %>% group_by(!!!gp_vars) %>%
    summarise(err = 1 - mean(correct)) %>%
    group_by(!!!gp_vars[2:length(gp_vars)]) %>%
    nest() %>% rename(err_data = data)
  # grand mean errors
  error = error_sub %>% unnest() %>%
    group_by(!!!gp_vars[2:length(gp_vars)]) %>%
    summarise(merr = mean(err),
              n = n(),
              se = sd(err) / sqrt(n - 1)) %>%
    left_join(., error_sub)
  
  # 2. rt analysis: mean RTs with valid data
  mrt_sub <- df %>% group_by(sub) %>%
    filter(correct == 1 &
             rt < mean(rt) + 2.5 * sd(rt) & rt > 0.1) %>% #outlier
    group_by(!!!gp_vars) %>% #subject-wise
    summarise(mrt = mean(rt)) %>%
    group_by(!!!gp_vars[2:length(gp_vars)]) %>%
    nest() %>% rename(rt_data = data)
  
  # grand mean RTs
  mrt <- mrt_sub %>% unnest() %>%
    group_by(!!!gp_vars[2:length(gp_vars)]) %>%
    summarise(mmrt = mean(mrt),
              n = n(),
              sert = sd(mrt) / sqrt(n - 1)) %>%
    left_join(., mrt_sub)
  
  # return joined table.
  tbl = left_join(mrt, error)
  if ('reward' %in% colnames(df)) {
    # second experiment with reward
    # change labels, such that it differs from 'target' column
    tbl$reward = factor(tbl$reward, labels = c("RA", "RP"))
  }
  return(tbl)
}



#' Estimate d-prime and C
#' calculate response sensitivity and bias using signal detection theory (sdt)
#' revision: correction for extreme values according to Macmillan and Kaplan (1985), 
#' correction factor: 0.5/n.
#' @param df experimental raw data
#' @return dprime and bias C
sdt_dprime <- function(df) {
  # prepare group variables
  if ('reward' %in% colnames(df)) {
    # second experiment with reward
    gp_var = c("sub", "target", "dyn", "setsize", "reward")
  } else {
    gp_var = c("sub", "target", "dyn", "setsize")
  }
  gp_vars = syms(gp_var) # prepare the group variables
  
  # subject-wise
  sdt_sub <- df %>%  #filter(resp > 0) %>% # filter valid trials
    group_by(!!!gp_vars) %>%
    summarise(mresp = mean(correct), cnt = n()) %>% # calculate positive response subject-wise
    spread(target, mresp) %>% # rearrange hit (Prsent) and FA (Absent) in one row
    mutate(dprime = qnorm(pmin(Present, 1-0.5/cnt)) - qnorm(pmax(1-Absent, 0.5/cnt)),
           bias = -(qnorm(pmin(Present, 1-0.5/cnt)) + qnorm(pmax(1-Absent, 0.5/cnt))) /
             2) %>%
    group_by(!!!gp_vars[3:length(gp_vars)]) %>% # no 'sub' and 'target'
    nest()
  
  # grand average
  sdt_avg <-
    sdt_sub %>% unnest() %>% group_by(!!!gp_vars[3:length(gp_vars)]) %>%
    summarise(
      n = n(),
      mdprime = mean(dprime),
      dse = sd(dprime) / sqrt(n - 1),
      mbias = mean(bias),
      cse = sd(bias) / sqrt(n - 1)
    )
  
  # return joined table
  sen = left_join(sdt_avg, sdt_sub)
  if ('reward' %in% colnames(df)) {
    # second experiment with reward
    # change labels, such that it differs from 'target' column
    sen$reward = factor(sen$reward, labels = c("RA", "RP"))
  }
  return(sen)
}

#' Linear regression of RT data
#' @param df individual sub data
mrt_model <- function(df) {
  lm(rt ~ setsize, data = df)
}


# definition of fuction getSlope
getSlope <- function(df) {
  # prepare group variables
  if ('reward' %in% colnames(df)) {
    # second experiment with reward
    gp_var = c("sub", "target", "dyn", "reward")
  } else {
    gp_var = c("sub", "target", "dyn")
  }
  gp_vars = syms(gp_var) # prepare the group variables
  
  slopes <- df %>% group_by(sub) %>%
    filter(correct == 1 &
             rt < mean(rt) + 2.5 * sd(rt) &
             rt > 0.1) %>% #remove incorrect and outliers
    group_by(!!!gp_vars) %>% nest()  %>%  # nested data
    mutate(model = map(data, mrt_model)) %>%  # linear regression
    mutate(slope = map(model, broom::tidy)) %>%  # get estimates out
    unnest(slope, .drop = TRUE) %>% # remove raw data
    select(-std.error, -statistic,-p.value) %>%  # remove unnessary clumns
    spread(term, estimate) %>%   # spread stimates
    rename(minRT = `(Intercept)`, slope = setsize)  # rename columns
  
  if ('reward' %in% colnames(df)) {
    # second experiment with reward
    # change labels, such that it differs from 'target' column
    slopes$reward = factor(slopes$reward, labels = c("RA", "RP"))
  }
  return(slopes)
}



#' Basic analysis
#' wrap up all basic average data, mean rt, error, sensitivity
#' @param df experimental raw data
#'
basicEstimates <- function(df) {
  mrt = getMeanData(df)  # mean RT and errors
  slope = getSlope(df)  # slopes
  sensitivity = sdt_dprime(df) # sensitivity and bias
  return(list(
    'mrt' = mrt,
    'slope' = slope,
    'sensitivity' = sensitivity
  ))
}



dat = map(exps, basicEstimates)
names(dat) <- exp_names


#------------------------------------------#
#    define some plot functions            #
# such as plotRT, plotSlopeErrbar,plotSDT  #
#------------------------------------------#

# definition of the function plotRT
plotRT <- function(df) {
  if ("reward" %in% colnames(df)) {
    fig_aes = aes(
      x = setsize,
      y = mmrt,
      color = dyn,
      shape = reward,
      fill = interaction(reward, target)
    )
    line_aes = aes(linetype = target)
    lt = quo(reward)
  } else {
    fig_aes = aes(
      x = setsize,
      y = mmrt,
      color = dyn,
      fill = target
    )
    line_aes = aes(linetype = target)
    lt = quo(target)
  }
  
  fig.rt <- ggplot(df, fig_aes) +
    geom_point(size = 2) + geom_line(line_aes) +
    geom_errorbar(aes(ymin = mmrt - sert, ymax = mmrt + sert), width = 0.5) +
    labs(y = 'RT (Secs)', color = 'Search', shape = 'Reward', linetype = 'Target') +
    scale_x_continuous(breaks = c(8, 12, 16)) +
    scale_color_manual(values = c('#fdae61', '#2c7bb6')) +
    scale_linetype_manual(values = c(2, 1)) +
    scale_fill_manual(values = c('white', 'white','white','white')) +
    scale_shape_manual(values = c(21,22)) +
    facet_wrap( ~ dyn) +
    guides(fill = 'none') + 
    theme(legend.position = 'right', strip.background = element_blank()) 
  
  # change y axis
  fig_aes$y = quo(merr)
  
  fig.err <- ggplot(df, fig_aes) +
    geom_bar(position = position_dodge(), stat = 'identity', color = 'black') +
    geom_errorbar(aes(ymin = merr, ymax = merr + se),color = 'black',
                  position = position_dodge()) +
    labs(x = 'Set Size', y = 'Error rate', fill = '') +
    scale_x_continuous(breaks = c(8, 12, 16)) +
    scale_color_manual(values = c('#fdae61', '#2c7bb6')) +
    scale_fill_manual(values = c('black', 'gray50','gray80','white')) +
    facet_wrap( ~ dyn) +
    theme(
      legend.position = 'right',
      strip.background = element_blank(),
      strip.text.x = element_blank()
    ) 
  
  plot_grid(fig.rt,
            fig.err,
            nrow = 2,
            rel_heights = c(3, 2), 
            rel_widths = c(3,2))
}


# ---- generation of results in figures  ----
# plotting results of experiments in figures and save them to the folder 'figures'
# Figure 2 -- exp1_rt.png
# Figure 4 -- exp2_rt.png
# Figure 6 -- exp3_rt.png
figRTs = list()
for (exname in exp_names) {
  figRTs[[exname]] = plotRT(dat[[exname]]$mrt)
  if (saveFigure == TRUE)
    ggsave(file.path('figures', paste0(exname, '_rt.png')),
           figRTs[[exname]] ,
           width = 7,
           height = 5)
}


#' Plot slope bars
#' plotting out bars and errorbar of mean slopes
#' @param df a table of slope
plotSlopeErrbar <- function(df) {
  if ('reward' %in% colnames(df)) {
    # second experiment with reward
    gp_var = c("target", "dyn", "reward")
    #    fig_aes = aes(x = dyn, y = m_slope, ymin = m_slope, color = interaction(target,reward), ymax = m_slope + sd_slope,
    #                  group = interaction(target,reward), fill = interaction(target,reward))
    fig_aes = aes(
      x = dyn,
      y = m_slope,
      ymin = m_slope,
      color = dyn,
      fill = interaction(target, reward),
      ymax = m_slope + sd_slope
    )
  } else {
    gp_var = c("target", "dyn")
    fig_aes = aes(
      x = dyn,
      y = m_slope,
      ymin = m_slope,
      color = dyn,
      fill = target,
      ymax = m_slope + sd_slope
    )
  }
  gp_vars = syms(gp_var) # prepare the group variables
  
  mslope <- df %>% group_by(!!!gp_vars) %>%
    summarise(
      m_slope = mean(slope) * 1000,
      n = n(),
      sd_slope = sd(slope) / sqrt(n - 1) * 1000
    )
  
  fig.mSlope.bar <- ggplot(mslope, fig_aes) +
    geom_bar(stat = "identity",
             position = position_dodge()) +
    geom_errorbar(width = 0.2,
                  position = position_dodge(width = 0.9)) +
    labs(x = "Search Type", y = "Slope (ms/item)") +
    scale_color_manual(values = c('#fdae61', '#2c7bb6')) +
    scale_fill_manual(values = c('black', 'gray80','gray50','white')) +
    guides(color = 'none') +
    theme(legend.position = c(0.26,0.85), legend.title = element_blank())
  
  return(fig.mSlope.bar)
}


#' plot dprime and bias
#' @param df table of sensitivity
#'
plotSDT <- function(df) {
  if ("reward" %in% colnames(df)) {
    fig_aes = aes(
      x = setsize,
      y = mdprime,
      color = dyn,
      shape = reward
    )
    line_aes = aes(linetype = reward)
  } else {
    fig_aes = aes(
      x = setsize,
      y = mdprime,
      color = dyn,
      shape = dyn
    )
    line_aes = aes(linetype = dyn)
  }
  
  err_aes = aes(ymin = mdprime - dse, ymax = mdprime + dse)
  
  fig.dprime <- ggplot(df, fig_aes) +
    geom_point(size = 2) + geom_line(line_aes) +
    geom_errorbar(err_aes, width = 0.5) +
    xlab('Set Size') + ylab("d'") +
    scale_x_continuous(breaks = c(8, 12, 16)) +
    scale_color_manual(values = c('#fdae61', '#2c7bb6')) +
    #    scale_fill_manual(values = c('gray90', 'gray40')) +
    scale_shape_manual(values = c(21,22)) +
    theme(legend.position = 'none', legend.title = element_blank())
  
  # plot biase, by changing y axis
  fig_aes$y = quo(mbias)
  err_aes = aes(ymin = mbias - cse, ymax = mbias + cse)
  
  fig.bias <- ggplot(df, fig_aes) +
    geom_point(size = 2) + geom_line(line_aes) +
    geom_errorbar(err_aes, width = 0.5) +
    scale_color_manual(values = c('#fdae61', '#2c7bb6')) +
    #    scale_fill_manual(values = c('gray90', 'gray40')) +
    scale_shape_manual(values = c(21,22)) +
    scale_x_continuous(breaks = c(8, 12, 16)) +
    theme(legend.position = 'right')
  
  if ("reward" %in% colnames(df)) {
    fig.bias <- fig.bias + guides(linetype = 'none') +
      labs(x='Set Size', y = "C", fill = 'Display', shape = 'Reward', color = 'Display')
  } else {
    fig.bias <- fig.bias + 
      labs(x='Set Size', y = "C", fill = 'Display', shape = 'Display', color = 'Display', linetype = 'Display')
  }
  return(list('dprime' = fig.dprime, 'bias' = fig.bias))
}

# ---- slope_sdt_figures ---
figSlopes = list()
fig_ds = list()
fig_cs = list()
for (exname in exp_names) {
  figSlopes[[exname]] = plotSlopeErrbar(dat[[exname]]$slope)
  fig = plotSDT(dat[[exname]]$sensitivity)
  fig_ds[[exname]] = fig$dprime
  fig_cs[[exname]] = fig$bias
}

# ---- combined_plot ---
# combine the subfigures of mean search slopes, target discrimination sensitivity and response criterion
# figure 3 -- 'exp1_combine.png'
# Figure 5 -- 'exp2_combine.png'
# Figure 7 -- 'exp3_combine.png'

fig_combine = list()
for (exname in exp_names) {
  fig = plot_grid(figSlopes[[exname]],
                  fig_ds[[exname]],
                  fig_cs[[exname]],
                  nrow = 1,rel_widths = c(2,2,3),
                  labels = c("A", "B", "C"))
  if (saveFigure == TRUE) {
    ggsave(file.path('figures', paste0(exname, '_combine.png')),
           fig ,
           width = 9,
           height = 3)
  }
  fig_combine[[exname]] = fig
}

# ---- ANOVA for RT ----
rt_anova = list()
for (exname in exp_names) {
  rt_anova[[exname]] = dat[[exname]]$mrt %>% unnest() %>% ungroup() %>%
    mutate_at(c('sub', 'setsize'), funs(factor(.))) %>% # convert to factors
    {
      if (exname == 'exp2')
        ezANOVA(
          data = .,
          dv = mrt,
          wid = sub,
          within = .(setsize, target, dyn, reward)
        )
      else
        ezANOVA(
          data = .,
          dv = mrt,
          wid = sub,
          within = .(setsize, target, dyn)
        )
    }
}

# ---- slope_anova ----
slope_anova = list()
for (exname in exp_names) {
  slope_anova[[exname]] = dat[[exname]]$slope %>% ungroup() %>%
    mutate_at(c('sub'), funs(factor(.))) %>% # convert to factors
    {
      if (exname == 'exp2')
        ezANOVA(
          data = .,
          dv = slope,
          wid = sub,
          within = .(target, dyn, reward)
        )
      else
        ezANOVA(
          data = .,
          dv = slope,
          wid = sub,
          within = .(target, dyn)
        )
    }
}

# ---- slope_anova ----
d_anova = list() # anova for d'
c_anova = list() # anova for c
for (exname in exp_names) {
  d_anova[[exname]] = dat[[exname]]$sensitivity %>% unnest() %>% ungroup() %>%
    mutate_at(c('sub', 'setsize'), funs(factor(.))) %>% # convert to factors
    {
      if (exname == 'exp2')
        ezANOVA(
          data = .,
          dv = dprime,
          wid = sub,
          within = .(dyn, setsize, reward)
        )
      else
        ezANOVA(
          data = .,
          dv = dprime,
          wid = sub,
          within = .(dyn, setsize)
        )
    }
  c_anova[[exname]] = dat[[exname]]$sensitivity %>% unnest() %>% ungroup() %>%
    mutate_at(c('sub', 'setsize'), funs(factor(.))) %>% # convert to factors
    {
      if (exname == 'exp2')
        ezANOVA(
          data = .,
          dv = bias,
          wid = sub,
          within = .(dyn, setsize, reward)
        )
      else
        ezANOVA(
          data = .,
          dv = bias,
          wid = sub,
          within = .(dyn, setsize)
        )
    }
}



