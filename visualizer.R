c1 = c("#ca5e4a","#5ba965","#c55a9f","#ad963d","#777acd")

col_vol = c("#ca5e4a","#00418f", "#756c6b", "#a60401")

col_enrich = c("#ca5e4a", "#636EFA", "#93e9be")


plot_line = function(df, pal = palette("default"), 
                     ll=c("Line Plot", "x",
                          "y", "line"), marker = FALSE) {
  #General plot line function for plotting both x vs y and x vs y1 vs y2 vs y345...
  
  #Melted with ref to the column 1
  df_m = reshape2::melt(df, id.vars = 1) 
  colnames(df_m) = c("x", "variable", "y")
  df_m$y = as.numeric(df_m$y)
  p = ggplot2::ggplot(df_m, ggplot2::aes(x, y)) +
    ggplot2::geom_line(linewidth = 1)+
    ggplot2::labs(title = ll[1], x = ll[2], y = ll[3])+
    theme_classic()
  
  if (ncol(df) > 2){
    p = p + ggplot2::geom_line(ggplot2::aes(colour = factor(variable)), linewidth = 1) +
      ggplot2::scale_color_manual(values = pal) +
      #theme_Publication() +
      ggplot2::labs(color = ll[4])
  }
  if (marker == TRUE){
    p = p + ggplot2::geom_point()+
      ggrepel::geom_text_repel(aes(label = y), size = 3, force = 1.5, force_pull = 0.5)
      # ggrepel::geom_label_repel(aes(label = y), box.padding = 0.35,
      #                           segment.color = 'grey50', size = 3, 
      #                           force = 1.5, force_pull = 0.5)
  }
  return(p)
}

plot_volcano <- function(df, pal = palette("default"),
                         ll = c("Volcano Plot", "x", "y", "dot"), log2FC_name = 'log2FoldChange',
                         neglog10padj_name = "neglog10padj", diffExpressed_name = "diffExpressed",
                         logFC_threshold = 2, padj_threshold = 0.05){
  p <- ggplot(data = df,
               aes(x = !!sym(log2FC_name), y = !!sym(neglog10padj_name), color=!!sym(diffExpressed_name))) +
    geom_point() +
    geom_vline(xintercept= c(-logFC_threshold, logFC_threshold), color=pal[[1]], linetype = 'dashed') +
    geom_hline(yintercept= -log10(padj_threshold), color=pal[[1]], linetype = 'dashed') +
    scale_color_manual(values=c(DOWN = pal[[2]], NO = pal[[3]], UP = pal[[4]]), drop = FALSE) +
    labs(title = ll[[1]], x = ll[[2]], y = ll[[3]], color = ll[[4]]) +
    theme_classic()
  return(p)
}

plot_enrich_std = function(enrich_res_df, pal = palette("default"),
                           ll = c("Plot", "x", "y", "col"),
                           pos_neg = "pos"){
  if (pos_neg == "pos"){
    fill = pal[[1]]
  } else if (pos_neg == "neg") {
    fill = pal[[2]]
  } else {
    fill = pal[[3]]
  }
  enrich_plotdf = enrich_res_df %>% mutate(neglog10adjp = log(p.adjust, 10)*(-1))
  enrich_plotdf = enrich_plotdf %>% filter(p.adjust < 0.05) %>% head(10)
  print(enrich_plotdf)
  
  fig <- ggplot(enrich_plotdf, aes(x = reorder(Description, neglog10adjp, sum),
                                   y=neglog10adjp)) +
    geom_bar(stat = "identity",fill = fill) +
    ggplot2::geom_label(aes(label = ID), size = 3)+
    coord_flip() +
    labs(title = ll[[1]], x = ll[[2]], y = ll[[3]], color = ll[[4]])+
    scale_x_discrete(labels = scales::label_wrap(50)) 
  return(fig)
}