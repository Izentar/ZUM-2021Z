

run_plots <- function(dataSet, postfix) {
  head(dataSet)
  
  
  subSetClass1 <- subset(dataSet, Class == 1)
  sum(duplicated(subSetClass1))
  
  subSetClass0 <- subset(dataSet, Class == 0)
  sum(duplicated(subSetClass0))
  
  ggplot(dataSet, aes(fill = factor(Class), x = Time)) +
    geom_histogram() + facet_grid(Class ~ ., scales = 'free_y')
  ggsave(paste("time", postfix , ".pdf", sep = ""))
  
  dataSet %>%
    count(Class) %>%
    mutate(percentage = n / nrow(dataSet)) -> dataSet_perc
  
  ggplot(dataSet_perc, aes(x = factor(Class), y = percentage)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_x_discrete(labels = c("1", "0")) +
    labs(x = 'Class', y = 'Percentage') +
    ggtitle("Dystrybucja transakcji wlasciwych oraz z falszerstwami.") +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggsave(paste("class_percentage", postfix, ".pdf", sep = ""))
  
  ggplot(dataSet) +
    geom_jitter(aes(x = factor(Class), y = Amount)) +
    labs(x = 'Class', y = 'Amount') +
    ggtitle("Wartosci dokonanych transakcji w zaleznosci od typu transkacji.") +
    theme(plot.title = element_text(hjust = 0.5))
  ggsave(paste("amount", postfix, ".pdf", sep = ""))
}