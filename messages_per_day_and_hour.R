title<-paste0("Most Messages happen at hour ",chat() %>% mutate(hour = hour(time)) %>% count(hour) %>% top_n(1) %>% pull(hour))
      chat %>%
        mutate(hour = hour(time)) %>%
        count(hour) %>%
        ggplot(aes(x = hour, y = n)) +
        geom_bar(stat = "identity",fill="steelblue") +
        ylab("") + xlab("Messages for every hour") +
        ggtitle(title)+
        scale_x_continuous(breaks = 0:23)


daysed<-c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
        
        most_active_day_of_week<-chat() %>% mutate(day = wday(as.Date(time),week_start = 2)) %>% count(day) %>% top_n(1) %>% pull(day)
        most_active_day_of_week<-daysed[most_active_day_of_week]
        title<-paste0("Most messages are sent on a ",most_active_day_of_week)
        days<-c("Mon","Tue","Wed","Thu","Fri","Sat","Sun") # for axis labels
        
        chat() %>%
          mutate(day = wday(as.Date(time),week_start = 2)) %>%
          count(day)  %>%
          ggplot(aes(x = day, y = n)) +
          geom_bar(stat = "identity", fill="steelblue") +
          ylab("") + xlab("Messages Per Day of week") +
          ggtitle(title) +       
          scale_x_continuous(breaks = 1:7,labels=days)+
          scale_x_continuous(breaks = 1:7,labels=days)
