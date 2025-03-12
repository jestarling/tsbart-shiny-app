### Function to draw stillbirth risk plot over gestational age.
make_stillbirth_plot <- function(ggdf, pars){
   
   # Set up title with risk info for that patient/ga combo.
   subtitle_rr = round(as.numeric(ggdf$phat_adj[which(ggdf$gest_age==as.numeric(pars$gestage))]), 1)
   subtitle_lb = round(as.numeric(ggdf$phat_lb_adj[which(ggdf$gest_age==as.numeric(pars$gestage))]), 1)
   subtitle_ub = round(as.numeric(ggdf$phat_ub_adj[which(ggdf$gest_age==as.numeric(pars$gestage))]), 1)
   mysub = paste0('At ', pars$gestage, ' weeks, estimated stillbirth risk is ', subtitle_rr,
                  " per 10,000 ongoing pregnancies, with 95% CI (",
                  subtitle_lb, " - ", subtitle_ub, ")")

   # Create plot.
   plt = ggplot(ggdf, aes(x=gest_age, y=phat_adj)) +
         geom_ribbon(aes(x=gest_age, ymin=phat_lb_adj, ymax=phat_ub_adj), alpha=0.3, fill='grey20') +
         geom_line(linewidth=1.2, colour='dodgerblue4') +
         coord_cartesian(xlim = c(34,42), expand=F) +
         labs(x = 'Gestational age (weeks)',
              y = 'Risk at gestational age',
              subtitle = mysub) +
         geom_vline(mapping = NULL, data = NULL, xintercept = as.numeric(pars$gestage), linetype=2, size=.8, colour='grey30') +
      theme_classic(base_size=14, base_family='Helvetica') +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.y = element_line(colour='grey95'),
            panel.grid.major.y = element_line(colour='grey95'))
}

### Function to draw stillbirth w/w percent change plot.
make_change_plot <- function(ggdf, pars){
   
   # Create fill varaible.
   ggdf$fill = ifelse(ggdf$gest_age==as.numeric(pars$gestage), TRUE, FALSE)
   
   # Max y axis limit.
   ymax = max(ggdf$phat_wk_pctchg, na.rm=TRUE) + 10
   
   # Tidy labels.
   ggdf$labs = ifelse(ggdf$gest_age==as.numeric(pars$gestage), 
                      paste0(
                         ifelse(round(ggdf$phat_wk_pctchg,1)>0, "+",
                                ifelse(round(ggdf$phat_wk_pctchg,1)<0, "-", "")),
                         round(ggdf$phat_wk_pctchg,1), '%'), 
                      '')
   
   # Create plot.
   plt = ggplot(ggdf, aes(x=gest_age, y=phat_wk_pctchg, fill = fill)) +
      geom_bar(stat='identity') +
      scale_fill_manual(name='', values=c('grey70', 'dodgerblue4')) +
      geom_text(aes(label = labs), size=4, vjust=-1) +
      coord_cartesian(ylim=c(0,ymax)) +
      scale_x_continuous(breaks=34:42, labels=34:42) +
      guides(fill='none') +
      labs(x = 'Gestational age (wks)',
           y = 'Percent change') +
      theme_classic(base_size=14, base_family='Helvetica') +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.y = element_line(colour='grey95'),
            panel.grid.major.y = element_line(colour='grey95'))
}


### Function to compare stillbirth risk.
make_compare_plot <- function(ggdf, ggdf_similar, pars){
   
   ggdf_similar = ggdf_similar %>% 
      mutate(is_patient = ifelse(id %in% unique(ggdf$id), TRUE, FALSE))
   
   ggdf_similar = ggdf_similar %>% 
      mutate(birthwtQ_lab = factor(case_when(
         birthwtQ == '[0,0.1)' ~ paste0('Very low fetal wt ', birthwtQ),
         birthwtQ == '[0.1,0.25)' ~ paste0('Low fetal wt ', birthwtQ),
         birthwtQ == '[0.25,0.75)' ~ paste0('Moderate fetal wt ', birthwtQ),
         birthwtQ == '[0.75,0.9)' ~ paste0('High fetal wt ', birthwtQ),
         birthwtQ == '[0.9,1]' ~ paste0('Very high fetal wt ', birthwtQ)),
         levels = c('Very low fetal wt [0,0.1)',
                    'Low fetal wt [0.1,0.25)',
                    'Moderate fetal wt [0.25,0.75)',
                    'High fetal wt [0.75,0.9)',
                    'Very high fetal wt [0.9,1]')))
   
   ggdf_similar = ggdf_similar %>% 
      mutate(wtgainQ_lab = factor(case_when(
         wtgainQ == '[0,0.1)' ~ paste0('Very low ', wtgainQ),
         wtgainQ == '[0.1,0.25)' ~ paste0('Low ', wtgainQ),
         wtgainQ == '[0.25,0.75)' ~ paste0('Moderate ', wtgainQ),
         wtgainQ == '[0.75,0.9)' ~ paste0('High ', wtgainQ),
         wtgainQ == '[0.9,1]' ~ paste0('Very high ', wtgainQ)),
         levels = c('Very low [0,0.1)',
                    'Low [0.1,0.25)',
                    'Moderate [0.25,0.75)',
                    'High [0.75,0.9)',
                    'Very high [0.9,1]')))
   
   ggdf_braces = data.frame('x' = 34:42,
                            'xend' = as.numeric(pars$gestage),
                            'y' = -2,
                            'yend' = -4)
   
   # Create plot to compare induced.
   plt = ggplot(ggdf_similar, aes(x=gest_age, y=phat_adj, 
                                  linetype=factor(id), linewidth=birthwtQ_lab, color=wtgainQ_lab,
                                  alpha = is_patient)) + 
      geom_line() +
      scale_alpha_manual(name='', values = c(.4,1)) +
      coord_cartesian(xlim = c(34,42),  ylim=c(0,NA), expand=F, clip='off') +
      geom_vline(mapping = NULL, data = NULL, xintercept = as.numeric(pars$gestage), linetype=2, size=.8, colour='grey30') +
      scale_colour_manual(name='Weight gain quantile',
                          values = c('firebrick', 'orange2', 'forestgreen', 'purple', 'dodgerblue4')) +
      guides(linetype='none', linewidth='none', alpha='none', colour='none') +
      scale_linewidth_manual(name='', values=rep(1, times=nrow(ggdf_similar)/9)) +
      scale_linetype_manual(name='', values=rep(1, times=nrow(ggdf_similar)/9)) +
      labs(x = '',
           y = 'Risk per 10,000 ongoing pregnancies',
           subtitle = 'Gestational age (wks)') +
      facet_wrap(~birthwtQ_lab, ncol=5) +
      theme_classic(base_size=14, base_family='Helvetica') +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.y = element_line(colour='grey95'),
            panel.grid.major.y = element_line(colour='grey95'),
            legend.position='bottom',
            legend.direction='horizontal') +
      geom_segment(aes(x = as.numeric(pars$gestage), xend = as.numeric(pars$gestage),y = -5, yend = -8),
                   colour = 'grey30') +
      # Segments for bracket.
      geom_segment(aes(x = 34, xend = 42, y = -8, yend = -8), colour = 'grey30') +
      geom_segment(aes(x = 34, xend = 34, y = -8, yend = -12), colour = 'grey30') +
      geom_segment(aes(x = 42, xend = 42, y = -8, yend = -12), colour = 'grey30')  +
      theme(plot.margin = margin(b=25)) +
      theme(panel.spacing = unit(2, "lines"),
            plot.subtitle = element_text(hjust=0.5),
            strip.background = element_rect(colour='white')) 
   
   plt2 = ggplot(ggdf_similar %>% filter(gest_age == as.numeric(pars$gestage)), 
                 aes(x=wtgainQ_lab, y=phat_adj, fill=wtgainQ_lab,
                     alpha = is_patient)) + 
      geom_bar(stat='identity', position=position_dodge()) +
      scale_alpha_manual(name='', values = c(.5,1)) +
      guides(alpha='none', colour='none') +
      geom_text(aes(label=round(phat_adj,1)), size=4, vjust=-.1) +
      geom_text(aes(label='*', colour=is_patient, y=ifelse(is_patient, phat_adj, NA)), size=15, vjust=0) +
      scale_colour_manual(name='', values=c('grey30', 'grey30')) +
      scale_fill_manual(name='Maternal weight gain',
                        values = c('firebrick', 'orange2', 'forestgreen', 'purple', 'dodgerblue4')) +
      scale_x_discrete(labels = c('Very\nlow', 'Low', 'Mod.','High', 'Very\nhigh')) +
      labs(x = '',
           y = paste0('Risk at ', pars$gestage, ' wks')) +
      facet_wrap(~birthwtQ_lab, ncol=5, scales='free_x') +
      theme_classic(base_size=14, base_family='Helvetica') +
      theme(panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.y = element_line(colour='grey95'),
            panel.grid.major.y = element_line(colour='grey95'),
            legend.position='bottom',
            legend.direction='horizontal') +
      theme(panel.spacing = unit(2, "lines")) +
      scale_y_continuous(expand = expansion(mult = 0.1)) +
      theme(strip.text = element_blank(),
            strip.background = element_blank())
   
   panel = plot_grid(plt, plt2, nrow=2, ncol=1, rel_heights = c(1,1), align='hv')
   
   return(panel)
}