  library(ggplot2)
  library(ggrepel)
  library(scales)
  library(cowplot)
  
# A function to create comparative LC/MS histograms 

# Description of plot, required input files breakdown, output of `metaboPlotter` function;
  
# Description  
# x axis: retention time of LC run
# y axis: number of metabolites eluted)

# Required arguments for the function:
# XCMS_old: XCMS output dataframe; the `rtmed` columns only is needed/used
# XCMS_new: XCMS output dataframe; the `rtmed` columns only is needed/used
  
# to_plot_old: 
# to_plot_new: 
  
metaboPlotter <- function(   XCMS_old,    
                             XCMS_new, 
                             
                             to_plot_old,  
                             to_plot_new,
                             
                             LCMS_cohort_id){
    
  XCMS_old$rtmed <- XCMS_old$rtmed / 60
  XCMS_new$rtmed <- XCMS_new$rtmed / 60

  library(ggplot2)
  
  # https://stackoverflow.com/questions/11766856/normalizing-y-axis-in-histograms-in-r-ggplot-to-proportion
  # for % of features as y axis lim
  library(scales)
  # p for Palia 
  
  
  # WHAT: Pseudo create a histogram to extract x, y axis maximum values 
  # WHY:  To use for creating a (soft coded) seq of axis break points.
  P_hist <-   ggplot() + 
              geom_histogram(data = XCMS_old,
                 aes(x = rtmed),
                 bins = 64, 
                 color = 'darkgray',
                 fill  = 'orange'); 
  
  
  # https://stackoverflow.com/questions/7705345/how-can-i-extract-plot-axes-ranges-for-a-ggplot2-object
  y_MAX = layer_scales(P_hist)$y$range$range[2]
  x_MAX = layer_scales(P_hist)$x$range$range[2]
  
  Y_temp <- seq(0, y_MAX, step)
  XCMS_old$soft_coded_y <- (rep(Y_temp, 10000))[1:dim(XCMS_old)[1]]
  to_plot_old$soft_coded_y <- XCMS_old$soft_coded_y [1:dim(to_plot_old)[1]]
  step  = (y_MAX * 0.80) /  dim(to_plot_old)[1]
  soft_coded_y <- to_plot_old$soft_coded_y 
  
  p <- ggplot() +
    
    labs(title = "Distribution of features by retention time in minutes",
         x = "Retention time (minutes)",
         y = "number of features",
         #size = "horsepower"    , 
         col  =   "Metabolite" , 
         
         #shape= "# of gears"   , 
         caption = paste0("min(rtmed)    = ", round(min(XCMS_old$rtmed)   , 1) , "\n",
                          "median(rtmed) = ", round(median(XCMS_old$rtmed), 1) , "\n",
                          "mean(rtmed) = "  , round(mean(XCMS_old$rtmed), 1) , "\n",
                          "max(rtmed)    = ", round(max(XCMS_old$rtmed)   , 1)),
         
         subtitle = paste0("LC/MS Run for"  , LCMS_cohort_id, " (previous cohort) \n", 
                           "N features = ",
                           dim(XCMS_old)[1])) + 
    
    scale_x_continuous(breaks = seq(0, x_MAX + 4, by = 1)) +
    
    
    #scale_y_continuous(labels = percent_format()) + # % of total counts in y axis
    
    # start of histogram
    geom_histogram(data = XCMS_old,
                   aes(x = rtmed),
                   bins = 64, 
                   color = 'darkgray',
                   fill  = 'orange')  +
  
    # 2 of 2 - Start of 2nd dataframe's plot:
    geom_point( data  = to_plot_old,
                aes(x = rtmed, 
                    y = soft_coded_y,
                    colour = name_ordered),
                size = 2.5)+
  
    geom_label_repel(data = to_plot_old, 
                     aes(label = factor(name_ordered), 
                         x = rtmed, 
                         y = soft_coded_y, 
                         colour = name_ordered),
                     arrow = arrow(length = unit(0.03, "npc"),
                                   type = "closed", 
                                   ends = "first"),
                     box.padding = 2,
                     label.padding = 0.25,
                     point.padding = 1e-06, 
                     label.r = 0.25,
                     
                     
                     force = 2, 
                     nudge_x = 2,
                     nudge_y = 2,
                     direction    = "y",
                     hjust        = 0.7,
                     vjust        = 0.7,
                     segment.size = 0.2) +
  
      theme_gray() + 
      theme(axis.line = element_line(size = 3, colour = "grey80"))+
      theme(axis.text = element_text(colour = "black"));
  
  
  
  
  #q for kainourio
  # 1 of 2 - Start of first dataframe's plot:
  Q_hist <-   ggplot() + 
    
    #scale_y_continuous(labels = percent_format()) + # % of total counts in y axis
    
    geom_histogram(data = XCMS_new,
                   aes(x = rtmed),
                   bins = 64, 
                   color = 'darkgray',
                   fill  = 'cadetblue');
  
  #y_MAX = layer_scales(Q_hist)$y$range$range[2]
  #x_MAX = layer_scales(Q_hist)$x$range$range[2]
  Y_temp <- seq(0, y_MAX, step)
  XCMS_new$soft_coded_y <- (rep(Y_temp, 10000))[1:dim(XCMS_new)[1]]
  to_plot_new$soft_coded_y <- XCMS_new$soft_coded_y [1:dim(to_plot_new)[1]]
  step  = (y_MAX * 0.80) /  dim(to_plot_new)[1]
  
  
  q <- ggplot() +
    
    
    
    labs(title = "Distribution of features by retention time in minutes",
         x = "Retention time (minutes)",
         y = "number of features",
         col  =   "Metabolite" ,  
         
         caption = paste0("min(rtmed)    = ", round(min(XCMS_new$rtmed)   , 1) , "\n",
                          "median(rtmed) = ", round(median(XCMS_new$rtmed), 1) , "\n",
                          "mean(rtmed) = "  , round(mean(XCMS_new$rtmed), 1) , "\n",
                          "max(rtmed)    = ", round(max(XCMS_new$rtmed)   , 1)),
         subtitle = paste0("LC/MS Run for"  , LCMS_cohort_id, " (current cohort) \n", 
                           "N features = ",
                           dim(XCMS_new)[1])) + 
    
    scale_x_continuous(breaks = seq(0, x_MAX + 4 , by = 1)) +
    
    #scale_y_continuous(labels = percent_format()) + # % of total counts in y axis
    
    # start of histogram
    geom_histogram(data = XCMS_new,
                   aes(x = rtmed),
                   bins = 64, 
                   color = 'darkgray',
                   fill  = 'cadetblue')  +
    
    # 2 of 2 - Start of 2nd dataframe's plot:
    geom_point( data  = to_plot_new,
                aes(x = rtmed, 
                    y = soft_coded_y,
                    colour = name_ordered),
                size = 2.5)+
    
    geom_label_repel(data = to_plot_new, 
                     aes(label = factor(name_ordered), 
                         x = rtmed, 
                         y = soft_coded_y, 
                         colour = name_ordered),
                     arrow = arrow(length = unit(0.03, "npc"),
                                   type = "closed", 
                                   ends = "first"),
                     box.padding = 2,
                     label.padding = 0.25,
                     point.padding = 1e-06, 
                     label.r = 0.25,
                     
                     
                     force = 2, 
                     nudge_x = 2,
                     nudge_y = 2,
                     direction    = "y",
                     hjust        = 0.7,
                     vjust        = 0.7,
                     segment.size = 0.2) +
    theme_gray() + 
    theme(axis.line = element_line(size = 3, colour = "grey80"))+
    theme(axis.text = element_text(colour = "black"));
  
  
  
  
  
  
  library(cowplot)
  
  comparativePlot <- plot_grid(p, q, 
                     hjust = -0.8,
                     ncol = 2, nrow = 1)
  
  return(comparativePlot)
  }
   
  # png(filename =  paste0(dropbox_dir,
  #                         '2_dataNov2016/Validation/',
  #                        'plots_indicators_mapped_on_histograms/',
  #                         LCMS_cohort_id,
  #                        '_2col1row__xy_soft.png'),
  #      width = 1800, height = 738,res = 300)
  # plot(gBoth)
  # dev.off()
