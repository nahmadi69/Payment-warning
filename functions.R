
ff=function(Paid_value,Payment_value){
  x=data.frame(Paid_value,Payment_value)
  for(i in 1:(nrow(x)-1)){
    x[i+1,"Paid_value"]=x[i,"Paid_value"]-x[i,"Payment_value"]
  }
  for(i in 1:(nrow(x))){
    if(x[i,"Paid_value"]>x[i,"Payment_value"]){
      x[i,"Paid_value"] <- x[i,"Payment_value"]
    }
  }
  return(x$Paid_value)
}


fff=function(Payment_data,Paid_data){
  Payment_data= Payment_data %>% mutate(Paid_value=NA,Paid_year=NA,Paid_month=NA,Paid_day=NA)
  c=0
  j=0
  if(nrow(Paid_data)>0){
    for(i in 1:nrow(Payment_data)){
      if(i>1 ){if( Payment_data$Paid_value[i-1]<Payment_data$Payment_value[i-1] ){
        break
      }}
      repeat {
        if(round(c,2)>=round(Payment_data$Payment_value[i],2) ){
          Payment_data$Paid_value[i]=Payment_data$Payment_value[i]
          Payment_data$Paid_year[i]=Paid_data$Paid_year[j]
          Payment_data$Paid_month[i]=Paid_data$Paid_month[j]
          Payment_data$Paid_day[i]=Paid_data$Paid_day[j]
          c=c-Payment_data$Paid_value[i]
          break
        }
        if( j == nrow(Paid_data)){
          Payment_data$Paid_value[i]=c
          break
        }
        j=j+1
        c=c+Paid_data$Paid_value[j]
        
      }
    }
  }
  
  g=Payment_data %>% mutate(Paid_date =  ymd(paste0(Paid_year, "-", Paid_month, "-", Paid_day)),
                            Paid_month= month(Paid_date, label = TRUE)) %>% 
    select(-c(Paid_year,Paid_month,Paid_day))    
  g[["Paid_value"]][is.na(g[["Paid_value"]])]=0
  return( g)
}


payment_function=function(Payment, Paid){
  
  i=sort(unique(Payment$Order_number))[1]
  
  d_Payment=Payment %>% filter(Order_number==i)
  d_Paid=Paid %>% filter(Order_number==i)
  
  g=fff(Payment_data = d_Payment,Paid_data = d_Paid)
  # g[["Paid_value"]][is.na(g[["Paid_value"]])]=0
  for(i in sort(unique(Payment$Order_number))[-1]){
    
    d_Payment=Payment %>% filter(Order_number==i)
    d_Paid=Paid %>% filter(Order_number==i)
    g=g %>%bind_rows(fff(Payment_data = d_Payment,Paid_data = d_Paid))
    # g[["Paid_value"]][is.na(g[["Paid_value"]])]=0
    
  }
  return(g)
}
