### Function to make risk table.
make_risk_table <- function(dat){
   
   dat = dat[order(dat$gest_age),c('gest_age','phat_adj','phat_lb_adj','phat_ub_adj')]
   
   # Create percent increase for each gestational age from previous.
   dat$pct_incr = noquote(format(round((round(dat$phat_adj,1)  - round(lag(dat$phat_adj,1),1))/ 
                                             round(lag(dat$phat_adj,1),1) * 100, 1), nsmall=1))
   dat$pct_incr[1] = ""
   
   # Create absolute week-week increase.
   dat$abs_incr = noquote(format(round(dat$phat_adj,1)  - round(lag(dat$phat_adj,1),1), nsmall=2))
   
   # Format a column for output.
   dat$phat = ""
   for(i in 1:nrow(dat)){
      dat$phat[i] = paste0(format(round(dat$phat_adj[i],1),nsmall=1),
                              " (",
                              format(round(dat$phat_lb_adj[i],1),nsmall=1),
                              " - ",
                              format(round(dat$phat_ub_adj[i],1),nsmall=1),
                              ")"
      )
   }
   
   dat$pct_incr = format(dat$pct_incr,nsmall=1)
   dat$pct_incr[1] = ""
   
   dat$abs_incr = format(dat$abs_incr,nsmall=1)
   dat$abs_incr[1] = ""
   
   dat$abs_pct_incr = paste0(dat$abs_incr, ' (', dat$pct_incr, '%)')
   dat$abs_pct_incr[1] = "--"
   
   # Create table for output.
   out = dat[,c('gest_age','phat','abs_pct_incr')]
   colnames(out) = c('Week', 
                          'Risk (95% CI)', 
                          'Wk/wk Change')
   
   rownames(out) = NULL
   
   return(out)
   
}