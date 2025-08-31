pie_plot <- function(x) {
  dt2 <- dt[, .(views = sum(`Number of views last 7 days`),views_prop = sum(view_pct)),by=.(x)][order(-views)][1:10]
  
  vals <- paste(round(dt2$views_prop,1),sep="")
  
  dt[, .(views = sum(`Number of views last 7 days`),views_prop = sum(view_pct)),by=.(x)][order(-views)][1:10] %>% 
    plot_ly(labels=~x,values=~vals,type="pie",textinfo="text",text=paste0(vals,"%",sep="")) %>% 
    layout(title = '% share of last 7 days views (top 10)') %>% 
    style(hoverinfo="none")
}

avg_ex <- function(x){
  historical_exchange_rates(x, to = "CAD",start_date = "2021-01-01", end_date = "2021-11-16") %>%
    `colnames<-`(c('date','conv')) %>% 
    summarise(mean(conv)) %>% 
    as.numeric
}
