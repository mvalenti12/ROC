library(dplyr)
library(reshape2)
library(ggplot2)

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

set.seed(123)

n_sim <- 100000
predicted_0 <- rnorm(n = n_sim, 
                     mean = 0.55,
                     sd = 0.05)

predicted_1 <- rnorm(n = n_sim, 
                     mean = 0.4,
                     sd = 0.05)

predicted <- ifelse(c(predicted_0,predicted_1)>1,
                    1,
                    ifelse(c(predicted_0,predicted_1)<0,
                           0,
                           c(predicted_0,predicted_1)))

true_0 <- rnorm(n = n_sim, 
                mean = 0.35,
                sd = 0.05)

true_1 <- rnorm(n = n_sim, 
                mean = 0.65,
                sd = 0.05)

true <- ifelse(c(true_0,true_1)>0.5,
               1,0)

dt_aux <- cbind(predicted,true) %>%
  as.data.frame() %>%
  mutate(true=factor(true,levels=c(0,1)))

category <- function(predicted,
                     true,
                     threshold
){
  prediction <- as.numeric(as.numeric(predicted<threshold))
  table(prediction,true)
  label <- mapply(which_class,
                  predicted = prediction,
                  true = true)
  return(as.factor(label))
}

dt_aux$category <- category(dt_aux$predicted,
                            dt_aux$true,
                            threshold = 0.2)
roc_aux <- cbind(seq(min(dt_aux$predicted),
                     max(dt_aux$predicted),length.out = 100),NA,NA) %>%
  as.data.frame()
colnames(roc_aux) <- c("idx","t_p_r","f_p_r")


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
  
  p <- ggplot() +
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
         y = "density")
  p
  #### second plot ####
  roc_aux$t_p_r[i] <- sum(dat$category=="TP")/sum(dat$category%in%c("FN","TP"))
  print(roc_aux$t_p_r[i])
  roc_aux$f_p_r[i] <- sum(dat$category=="FP")/sum(dat$category%in%c("FP","TN"))
  print(roc_aux$f_p_r[i])
  p2 <- roc_aux %>%
    melt(id.var = "idx") %>%
    filter(!is.na(value)) %>%
    ggplot() + 
    geom_line(aes(x = idx,
                  y = value,
                  col = variable,
                  group = variable)) + 
    scale_x_continuous(limits = c(0,1)) + 
    scale_y_continuous(limits = c(0,1))
  p2
  return(p2)
}
for (i in 1:99){
  get_plot(i)
  print(i)
}



true_positive <- nrow(dt_aux[dt_aux$true==1,])
true_negative <- nrow(dt_aux[dt_aux$true!=1,])

#lets consider that 0 is negative and 1 is positive





threshold <- 0.5 
for (i in 1:length(roc_aux)){
  threshold <- roc_aux[i]
  # true positive rate or sensitivity or recall
  # how good 
  t_p_r[i] <- nrow(dt_aux[dt_aux$predicted < threshold & dt_aux$true==1,])/true_positive
  #false positive rate or specificity
  f_p_r[i] <- nrow(dt_aux[dt_aux$predicted < threshold & dt_aux$true!=1,])/true_negative
}

res <- cbind(roc_aux,
             t_p_r,
             f_p_r) %>%
  as.data.frame() %>%
  melt(id.var = roc_aux)

p +
  geom_line(data = res,
            aes(x = roc_aux,
                y = value,
                col = variable,
                group = variable))

plot(t_p_r,
     f_p_r, type = "l")
