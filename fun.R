library(dplyr)
library(reshape2)
library(ggplot2)
library(cowplot)
library(grid)
library(gridExtra)


which_class <- function(predicted,
                        true) {
  if (predicted == 0) {
    if (true == 0) {
      return("TN")
    } else if (true == 1) {
      return("FN") 
    } else {
      return(NA)
    }
  } else if (predicted == 1) {
    if (true == 0) {
      return("FP")
    } else if (true == 1) {
      return("TN")
    } else {
      return(NA)
    }
  } else {
    return(NA)
  }
}

two_digits <- function(x){
  if(x>=10)
    return(x)
    else
      return(paste0("0",x))
}

category <- function(predicted,
                     true,
                     threshold){
  prediction <- as.numeric(as.numeric(predicted<threshold))
  table(prediction,true)
  label <- mapply(which_class,
                  predicted = prediction,
                  true = true)
  return(as.factor(label))
}

generate_plots <- function(n_graphs,
                           predicted_0,
                           predicted_1,
                           x_coord,
                           identifier){

  
  predicted <- ifelse(c(predicted_0,predicted_1)>1,
                      1,
                      ifelse(c(predicted_0,predicted_1)<0,
                             0,
                             c(predicted_0,predicted_1)))
  
  true <- c(rep(0,length(predicted_0)),
            rep(1,length(predicted_1)))
  
  dt_aux <- cbind(predicted,true) %>%
    as.data.frame() %>%
    mutate(true=factor(true,levels=c(0,1)))

  roc_aux <- cbind(seq(min(dt_aux$predicted),
                       max(dt_aux$predicted),length.out = n_graphs),NA,NA) %>%
    as.data.frame()
  colnames(roc_aux) <- c("idx","t_p_r","f_p_r")
  
  aux_counter_roc <- 1
  
  get_plot <- function(i){
    threshold <- roc_aux$idx[i]
    #### first plot ####
    dat_1 <- with(density(dt_aux[dt_aux$true==1,'predicted']), data.frame(x, y)) %>% 
      filter(between(x,0,1))
    dat_1$category <- ifelse(dat_1$x<threshold,"FN","TP")
    dat_0 <- with(density(dt_aux[dt_aux$true==0,'predicted']), data.frame(x, y)) %>% 
      filter(between(x,0,1))
    dat_0$category <- ifelse(dat_0$x<threshold,"TN","FP")
    dat <- rbind(dat_0,dat_1)
    dat$category <- factor(dat$category, 
                           levels = c("TP","FN","TN","FP"),
                           ordered = TRUE)
    
    p1 <- ggplot() +
      geom_line(data = dat, 
                aes(x = x,
                    y = y,
                    group = category)) +
      geom_area(data = dat,
                aes(x = x,
                    y = y,
                    group = category,
                    fill = category),
                alpha = 0.4) + 
      geom_vline(xintercept = threshold,
                 linetype = "dashed",
                 color = "red") + 
      scale_x_continuous(limits = c(0,1)) +
      labs(x = "threshold",
           y = "density") +
      theme(legend.position = c(x_coord, 0.85)) + 
      scale_fill_manual(values = c("TP" = "navy",
                                   "FP" = "orange",
                                   "TN" = "red",
                                   "FN" = "skyblue")) 
    
    if(sum(dat$category=="FP")>0){
      print("adding FP label")
      p1 <- p1 + geom_text(aes(x = sum(dat_0[dat_0$category=="FP","x"]*dat_0[dat_0$category=="FP","y"])/sum(dat_0[dat_0$category=="FP","y"]),
                               y = sum(dat_0[dat_0$category=="FP","x"]*dat_0[dat_0$category=="FP","y"])/sum(dat_0[dat_0$category=="FP","x"]),
                               label = "FP")) 
    }
    if(sum(dat$category=="FN")>0){
      print("adding FN label")
      p1 <- p1 + geom_text(aes(x = sum(dat_1[dat_1$category=="FN","x"]*dat_1[dat_1$category=="FN","y"])/sum(dat_1[dat_1$category=="FN","y"]),
                               y = sum(dat_1[dat_1$category=="FN","x"]*dat_1[dat_1$category=="FN","y"])/sum(dat_1[dat_1$category=="FN","x"]),
                               label = "FN"))
    }
    if(sum(dat$category=="TP")>0){
      print("adding TP label")
      p1 <- p1 + geom_text(aes(x = sum(dat_1[dat_1$category=="TP","x"]*dat_1[dat_1$category=="TP","y"])/sum(dat_1[dat_1$category=="TP","y"]),
                               y = sum(dat_1[dat_1$category=="TP","x"]*dat_1[dat_1$category=="TP","y"])/sum(dat_1[dat_1$category=="TP","x"]),
                               label = "TP"))
    }
    if(sum(dat$category=="TN")>0){
      print("adding TN label")
      p1 <- p1 + geom_text(aes(x = sum(dat_0[dat_0$category=="TN","x"]*dat_0[dat_0$category=="TN","y"])/sum(dat_0[dat_0$category=="TN","y"]),
                               y = sum(dat_0[dat_0$category=="TN","x"]*dat_0[dat_0$category=="TN","y"])/sum(dat_0[dat_0$category=="TN","x"]),
                               label = "TN")) 
    }
    p1
    #### second plot ####
    roc_aux$f_p_r[i] <<- sum(dat$category=="FP")/sum(dat$category%in%c("FP","TN"))
    roc_aux$t_p_r[i] <<- sum(dat$category=="TP")/sum(dat$category%in%c("FN","TP"))
    
    while(sum(roc_aux$f_p_r==0, na.rm = TRUE)>1){
      print(paste0("current aux_counter_roc is ", aux_counter_roc))
      roc_aux$f_p_r[(i-aux_counter_roc):(i-aux_counter_roc)] <<- roc_aux$f_p_r[(i-aux_counter_roc):(i-aux_counter_roc)]+0.00000001
      aux_counter_roc <<- aux_counter_roc + 1
      print(paste0("current aux_counter_roc is updated to  ", aux_counter_roc))
    }
    
    p3 <- roc_aux %>% 
      filter(!is.na(t_p_r)) %>%
      ggplot() + 
      geom_line(aes(x = f_p_r,
                    y = t_p_r)) + 
      geom_point(aes(x = f_p_r[i],
                     y = t_p_r[i],
                     size = 2)) + 
      geom_abline(slope = 1, linetype = "dashed") +
      scale_x_continuous(limits = c(0,1)) + 
      scale_y_continuous(limits = c(0,1)) +
      labs(x = "False Positive Rate",
           y = "True Positive Rate") + 
      theme(legend.position="none")
    
    p <- grid.arrange(p1,p3,ncol=2)
    # p <- grid.rect(width = 1, height = 1, gp = gpar(lwd = 3, col = "black", fill = NA))
    # 
    p
    dev.copy(png,paste0("img/plot_auc_",
                        identifier
                        ,"_",two_digits(i),".png"))
    dev.off()
    return(p)
  }
  
  for (i in 1:n_graphs){
    get_plot(i)
    print(i)
  }
  
}


