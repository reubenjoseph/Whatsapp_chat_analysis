no<-chat() %>% mutate(day=date(time)) %>% summarise(no=length(unique(day))) %>%pull(no)
      old<-chat() %>% mutate(day=date(time))%>% arrange(day) %>% slice(1) %>% select(Oldest=day) %>%pull(Oldest)
      new<-chat() %>% mutate(day=date(time))%>% arrange(desc(day)) %>% slice(1) %>% select(Newest=day) %>%pull(Newest)
      total_no_of_days<-as.numeric(new-old)
      no_of_days_of_messgaes<- as.numeric(total_no_of_days-no) # total no of days with messages
      users<- n_distinct(chat()$author) # unique users
      
      total <- (chat() %>% count())/users #total no of messages
      smiley<-(chat() %>% unnest(emoji) %>% count())/users #no of smileys
      media <-(chat() %>% filter(text == "<Media omitted>") %>% count())/users #no of media
      links<- chat() %>% filter(str_detect(text,"www.")| str_detect(text,"http:")|str_detect(text,"https:")|str_detect(text,"youtu.be")) %>% count() #no of links
      deleted_messages<- round(((chat() %>% filter(text=="This message was deleted" | text=="You deleted this message")) %>% count())/users) # deleted messages
      avg1<- chat() %>% summarise (n = mean(words)) #no of words
      avg2<-chat()  %>% filter(text != "<Media omitted>") %>% filter(text != "This message was deleted" | text!="You deleted this message")%>% 
        summarise (n = mean(count_character)) #no of characters
      percent_days_without_messages<-round(no_of_days_of_messgaes/total_no_of_days*100,2) # % days without msg
      #colnames(percent_days_without_messages)<-c("n")
      
      
      avg_table<-total %>% rbind(smiley) %>% rbind(media) %>% rbind(links) %>% rbind(deleted_messages) %>% rbind(avg1)%>% rbind(avg2) %>% rbind(percent_days_without_messages)
      rownames(avg_table)<-c("Total messages sent","No of smileys sent","No of media sent","No of links sent",
                             "No of deleted messages","Avg words per message","Average character per message","% days without messaging")
      avg_table[] <- lapply(avg_table, as.integer)
      
      
      
      chat<-chat() %>% filter(author==person())
      no<-chat %>% mutate(day=date(time)) %>% summarise(no=length(unique(day))) %>%pull(no)
      old<-chat %>% mutate(day=date(time))%>% arrange(day) %>% slice(1) %>% select(Oldest=day) %>%pull(Oldest)
      new<-chat %>% mutate(day=date(time))%>% arrange(desc(day)) %>% slice(1) %>% select(Newest=day) %>%pull(Newest)
      no_of_days_of_messgaes<- as.numeric(no) # total no of days with messages
      total_no_of_days<-as.numeric(new-old) #total no od days
      
      
      total <- chat %>% count() #total no of messages
      smiley<-chat %>% unnest(emoji) %>% count() #no of smileys
      media <-chat %>% filter(text == "<Media omitted>") %>% count() #no of media
      links<- chat %>% filter(str_detect(text,"www.")| str_detect(text,"http:")|str_detect(text,"https:")|str_detect(text,"youtu.be")) %>% count() #no of links
      deleted_messages<- chat %>% filter(text=="This message was deleted" | text=="You deleted this message") %>% count() # deleted messages
      avg1<- chat %>% summarise (n = mean(words)) #no of words
      avg2<-chat %>% filter(text != "<Media omitted>")%>% filter(text != "This message was deleted" | text!="You deleted this message")  %>% summarise(n = mean(count_character)) #no of characters
      percent_days_without_messages<-round(no_of_days_of_messgaes/total_no_of_days*100,2) # % days without msg
      # colnames(percent_days_without_messages)<-c("n")
      
      
      author_table<-total %>% rbind(smiley) %>% rbind(media) %>% rbind(links) %>% rbind(deleted_messages) %>% rbind(avg1)%>% rbind(avg2) %>% rbind(percent_days_without_messages)
      rownames(author_table)<-c("Total messages sent","No of smileys sent","No of media sent","No of links sent",
                                "No of deleted messages","Avg words per message","Average character per message","% days without messaging")
      author_table[] <- lapply(author_table, as.integer)
      
      final_table<- author_table %>% cbind(avg_table) 
      colnames(final_table)<-c(person(),"Group Average")
      final_table<- final_table %>%  mutate(count="0")
      
      for(i in 1:8){
        if(final_table[i,1]>final_table[i,2]){
          final_table[i,3]=3}
        else{
          if(final_table[i,1]==final_table[i,2]){
            final_table[i,3]=2}
          else{final_table[i,3]=1 }
        }}
      final_table[8,3]=3
      rownames(final_table)<-c("Total messages sent","No of smileys sent","No of media sent","No of links sent",
                               "No of deleted messages","Avg words per message","Average character per message","% days without messaging")
      
      datatable(final_table[],class = 'cell-border stripe', options = list(pageLength = 15, lengthChange = FALSE, searchable=FALSE, dom = 't',
                                                                           columnDefs = list(list(targets = 3, visible = FALSE))
      )) %>% formatStyle(
        1, 3,
        backgroundColor = styleEqual(c(1,2,3), c('#EA9999','#FFE599', '#B6D7A8'))
      )
      
      
      
      
    }
