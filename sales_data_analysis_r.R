getwd()
library(tidyverse)
library(knitr)
library(ggplot2)
library(lubridate)
library(arules)
library(arulesViz)
library(dplyr)
library(chron)
library(bReeze)
library(naniar)



setwd("C:\\Users\\samni\\OneDrive\\Desktop\\analysis")
data <- read.csv("ER Excel Case Study_October 2018.csv", header =T)
head(data) 
summary(data)
str(data)
any(is.na(data))
is.na(data)
data1<-data[complete.cases(data), ]

str(data1$ModifiedTxnDate)
class(data1$ModifiedTxnTime)


#data1$ModifiedTxnDate<-as.character(data1$ModifiedTxnDate)
#data1$ModifiedTxnTime<-as.character(data1$ModifiedTxnTime)


glimpse(data1)
data1$Customer.Id <- as.factor(data1$Customer.Id)
data1$ModifiedTxnTime <- times(data1$ModifiedTxnTime)
class(data1$ModifiedTxnTime)
total_time<-aggregate(list(Total.Time = data1$ModifiedTxnTime), data1[15], sum)
mean(total_time$Total.Time)

### to check unique/distinct cutomers in data

Cust_id<- unique(data1$Customer.Id, incomparables = F)

length(Cust_id)

#total number of distinct items in data

itm <- unique(data1$UniqueItemName , incomparables = F)
length(itm)


### unique store

store <- unique(data1$ModifiedStore , incomparables = F)
length(store)

### unique store codes

storecd <- unique(data1$ModifiedStoreCode , incomparables = F)
length(storecd)


ggplot(data= data1,aes(x=data1$ModifiedTxnDate)) + geom_histogram(fill="indianred")

detach("package:plyr", unload=TRUE)
data1 %>% 
  group_by(Customer.Id) %>% 
  summarize(n_items = mean(ItemQty)) %>%
  ggplot(aes(x=n_items))+
  geom_histogram(fill="indianred", bins= 20) + 
  geom_rug()+
  coord_cartesian(xlim=c(0,8))

table(data1$ModifiedStore)
table(data1$ModifiedStoreCode)


################################################

One_basket = data1 %>%
  filter( ModifiedStoreCode=='TLHOS0093')

print(One_basket)

n_distinct(One_basket$UniqueItemName)

#total items purchase

One_basket %>% summarise(sum(ItemQty))


abc<-data1 %>% group_by(data1$ModifiedStore)%>%summarise(n=n())%>%ungroup()%>%arrange()
abc
write.table(abc, file = "foo.csv", sep = ",", col.names = NA,
            qmethod = "double")

glimpse(data1)


##################################totalsales weekly

dat <- data1 %>%
  mutate(ModifiedTxnDate = dmy(ModifiedTxnDate))
head(dat)
str(dat$ModifiedTxnDate)

Max_week_sale <- filter(dat, !is.na(Customer.Id),!is.na(UniqueItemCode))

Max_week_sale$Weekdays <- weekdays(Max_week_sale$ModifiedTxnDate)
Max_week_sale$Sales <- Max_week_sale$ItemQty*Max_week_sale$ItemPaidAmount
highsales <- Max_week_sale %>% group_by(Max_week_sale$Weekdays) %>% summarize(SalesAmount=sum(Sales)) %>% arrange(desc(SalesAmount))
head(highsales)

write.table(highsales, file = "sales.csv", sep = ",", col.names = NA,
            qmethod = "double")
##################################totalsales Monthly

Max_Month_sale <- filter(dat, !is.na(Customer.Id),!is.na(UniqueItemCode))

Max_Month_sale$Months <- months(Max_Month_sale$ModifiedTxnDate)
Max_Month_sale$Sales <- Max_Month_sale$ItemQty*Max_Month_sale$ItemPaidAmount
highsales <- Max_Month_sale %>% group_by(Max_Month_sale$Months) %>% summarize(SalesAmount=sum(Sales)) %>% arrange(desc(SalesAmount))
head(highsales)

write.table(highsales, file = "sales12.csv", sep = ",", col.names = NA,
            qmethod = "double")


##########################################by repeat customers


repeatcustomers<-dat %>%group_by((Customer.Id),(ModifiedTxnDate))%>%summarise(Count=n())%>%ungroup()%>%arrange()
repeatcustomers

write.table(repeatcustomers, file = "repeat1.csv", sep = ",", col.names = NA,
            qmethod = "double")
############################################most revenew item

revenue<-dat%>%group_by(dat$UniqueItemCode)%>%summarise(sales=sum(ItemQty*ItemPaidAmount))%>%ungroup()%>%arrange(desc(sales))
revenue
write.table(revenue, file = "Uniqueitem.csv", sep = ",", col.names = NA,
            qmethod = "double")
###############################################itemnames sales

revenue1<-dat%>%group_by(dat$UniqueItemName)%>%summarise(sales=sum(ItemQty*ItemPaidAmount))%>%ungroup()%>%arrange(desc(sales))
revenue1
write.table(revenue1, file = "itemname.csv", sep = ",", col.names = NA,
            qmethod = "double")
################################################sale amountvs item
Sales_Detail<- dat %>% mutate(Sales = ItemQty*ItemPaidAmount)
s1<-Sales_Detail%>% filter(!is.na(Sales))

################################################sale amountvs item

sales<-s1 %>%group_by(dat$ModifiedStore)%>% summarise(SalesAmount =sum(Sales)) %>%arrange(desc(SalesAmount))
sales
write.table(sales, file = "salesstore.csv", sep = ",", col.names = NA,
            qmethod = "double")

##### which hours are most crowded and, therefore, need more staff?
cust<- Max_week_sale %>% group_by(Customer.Id) %>% summarise(Spend=sum(ItemQty*ItemPaidAmount)) %>% arrange(desc(Spend)) %>%head(10)
cust
###########################################################################################

#########################new_analysis

glimpse(data)

str(data)
summary(data)

glimpse(scle)

scle <- data
scle$ItemMRP <- as.numeric(scle$ItemMRP)
##(scle$ItemPaidAmount <- sapply(scle$ItemPaidAmount, range))
view(scle1)
any(is.na(scle))
any_na(scle)       
miss_var_summary(scle)
gg_miss_case(scle)

boxplot(scle$ItemPaidAmount)
(scle_quartiles <- quantile(scle$ItemPaidAmount))
class(scle)


scle %>%
  summarise(n_records = n(),
            n_invoices = n_distinct(ModifiedBillNo),
            n_missing_inv_rec = sum(as.integer(is.na(ModifiedBillNo)),na.rm = TRUE), 
            n_customers = n_distinct(Customer.Id),
            n_missing_cust_rec = sum(as.integer(is.na(Customer.Id)),na.rm = TRUE))

scle %>%
  summarise(n_dist_stocks = n_distinct(UniqueItemCode), 
            n_missing_stocks = sum(as.integer(is.na(UniqueItemCode)),na.rm = TRUE), 
            n_dist_desc = n_distinct(UniqueItemName), 
            n_missing_desc = sum(as.integer(is.na(UniqueItemName)),na.rm = TRUE), 
            n_missing_quant = sum(as.integer(is.na(ItemQty)),na.rm = TRUE),
            n_missing_mrp = sum(as.integer(is.na(ItemMRP)),na.rm = TRUE),
            n_missing_gross = sum(as.integer(is.na(ItemGrossAmount)),na.rm = TRUE),
            n_missing_net = sum(as.integer(is.na(ItemNetAmount)),na.rm = TRUE),
            n_missing_paid = sum(as.integer(is.na(ItemPaidAmount)),na.rm = TRUE),
            n_missing_disc = sum(as.integer(is.na(ItemDiscountAmount)),na.rm = TRUE))
scle %>%
  summarise(n_rows = n(),
            n_distinct_ = n_distinct(UniqueItemCode))
            
scle %>%
  summarise(n_rows = n(),
            n_distinct_ = n_distinct(ModifiedStore))


scle %>%
  summarise(n_rows = n(),
            n_distinct_ = n_distinct(ModifiedStoreCode))
scle %>%
  summarise(n_rows = n(),
            n_distinct_ = n_distinct(ModifiedBillNo))
scle %>%
  summarise(n_rows = n(),
            n_distinct_ = n_distinct(UniqueItemName))

scle %>%
  summarise(n_rows = n(),
            n_distinct_ = n_distinct(UniqueItemCode))

scle %>%
  summarise(n_rows = n(),
            n_distinct_ = n_distinct(Barcode))

scle %>%
  summarise(n_rows = n(),
            n_distinct_ = n_distinct(Customer.Id))
#####################################################duplicates

duplicate <- scle %>%
  group_by(ModifiedBillNo) %>%
  summarise(n_rows = n()) %>%
  filter(n_rows > 1) %>%
  inner_join(scle, by = "ModifiedBillNo") %>%
  arrange(desc(n_rows), ModifiedBillNo)
write.table(duplicate, file = "duplicate.csv", sep = ",", col.names = NA,
            qmethod = "double")

###########################################sales by month
class(scle$ModifiedTxnDate)
class(scle$ModifiedTxnDate)

scle$ModifiedTxnDate = dmy(scle$ModifiedTxnDate)


scle %>%
  mutate(SalesMonth = as.character(as.integer(year(ModifiedTxnDate) * 100 + month(ModifiedTxnDate)))) %>%
  filter(!is.na(SalesMonth) & Customer.Id) %>%
  group_by(Customer.Id, SalesMonth) %>%
  summarise(SalesAmount = sum(ItemQty*ItemPaidAmount, na.rm = TRUE)) %>%
  collect %>%
  ggplot() +
  geom_line(aes(x = SalesMonth, y = SalesAmount, group = Customer.Id, color = Customer.Id)) +
  labs(x = "Sales month",
       y = "Sales amount",
       title = "Total sales by month",
       color = "Customer.Id") +
  theme(axis.text.x = element_text(angle = 45), plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::format_format(big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  scale_fill_brewer(palette = "Paired")

################################################new vs Existing customers

# #scle_new <- scle %>%
#   mutate(SalesMonth = as.character(as.integer(year(ModifiedTxnDate) * 100 + month(ModifiedTxnDate)))) %>%
#   filter(!is.na(SalesMonth) & Customer.Id ) %>%
#   group_by(Customer.Id) %>%
#   summarise(FirstPurchase = min(SalesMonth, na.rm = TRUE)) %>%
#   mutate(TenureGroup = ifelse(FirstPurchase < 20180920, "Existing", "New"))
# 
# write.table(scle_new, file = "newvsexisting12.csv", sep = ",", col.names = NA,
#             qmethod = "double")



sales_data <- scle%>% mutate(ModifiedTxn_Date = as.character(as.integer(year(ModifiedTxnDate) * 100 + month(ModifiedTxnDate),Customer_ID=as.character(Customer.Id))))

sales_data_new <- sales_data %>%
  group_by(Customer.Id)%>%
  mutate(date_of_first_engagement=min(ModifiedTxn_Date))%>%
  ungroup()

view(sales_data_new)

sales_data_final <- sales_data_new%>%
  mutate(Customer_Status = case_when(ModifiedTxn_Date>date_of_first_engagement ~ "Existing",
                                     ModifiedTxn_Date == date_of_first_engagement ~ "New",
                                     TRUE ~ "Other"))

view(sales_data_final)

New_and_Returning_Customers <-  sales_data_final%>%
  group_by(floor_date(ModifiedTxnDate,unit = 'month'))%>%
  summarise(New_Customers = n_distinct(Customer.Id[Customer_Status== "New"]),
  Existing_Customers= n_distinct(Customer.Id[Customer_Status=="Existing"]))

write.table(sales_data_final, file = "detailnewvsextable.csv", sep = ",", col.names = NA,qmethod = "double")
##############################################################################################
x <- scle  %>%summarise(SalesAmount = sum(ItemQty * ItemPaidAmount, na.rm = TRUE))
view(x)
scle %>%
  mutate(SalesMonth = as.character(as.integer(year(ModifiedTxnDate) * 100 + month(ModifiedTxnDate)))) %>%
  filter(!is.na(SalesMonth) & Customer.Id) %>%
  left_join(scle_new , by = "Customer.Id") %>%
  group_by(TenureGroup, SalesMonth) %>%
  summarise(SalesAmount = sum(ItemQty * ItemPaidAmount, na.rm = TRUE)) %>%
  collect %>%
  ggplot() +
  geom_bar(aes(x = SalesMonth, y = SalesAmount, fill = reorder(factor(TenureGroup), SalesAmount)), stat = "identity") +
  labs(x = "Sales month",
       y = "Sales amount ",
       title = "Total sales by month",
       fill = "Customer tenure group") +
  theme(axis.text.x = element_text(angle = 45), plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(labels = scales::format_format(big.mark = ".", decimal.mark = ",", scientific = FALSE)) +
  scale_fill_brewer(palette = "Paired")


#########################################################top 10 new customer

scle %>%
  mutate(SalesMonth = as.character(as.integer(year(ModifiedTxnDate) * 100 + month(ModifiedTxnDate)))) %>%
  filter(!is.na(SalesMonth) & Customer.Id) %>%
  left_join(scle_new, by = "Customer.Id") %>%
  filter(TenureGroup == "New") %>%
  group_by(Customer.Id, FirstPurchase) %>%
  summarise(SalesAmount = sum(ItemQty * ItemPaidAmount, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(rank(desc(SalesAmount)) < 50) %>%
  arrange(desc(SalesAmount)) %>%
  collect()
############################################product analysis


recent<- scle
recent1 <- recent %>% select("ModifiedTxnDate","ItemQty","ItemPaidAmount","Customer.Id")
view(recent_new)
dim(recent1)
recent_new <- recent1 %>% mutate(ModifiedTxn_Date = as.character(as.integer(year(ModifiedTxnDate) * 100 + month(ModifiedTxnDate)),Customer_ID=as.character(Customer.Id)))
str(recent_new)
recent_new$ItemQty <- as.numeric(recent_new$ItemQty )
revenue<- recent_new$ItemQty * recent_new$ItemPaidAmount
recent3 <- cbind(recent_new, revenue)                                 
view(recent3)
write.table(recent3, file = "revenue1234.csv", sep = ",", col.names = NA,qmethod = "double")


#################################################################################

repeatnew <- recent3 %>% group_by(Customer.Id)%>%
  mutate(date_of_first_engagement=min(ModifiedTxnDate))%>%
  ungroup()

write.table(repeatnew, file = "first_engagement.csv", sep = ",", col.names = NA,qmethod = "double")
str(repeatnew)


data_fart <-repeatnew%>%
  mutate(Customer_Status = case_when(ModifiedTxnDate > date_of_first_engagement ~ "Repeat",
                                     ModifiedTxn_Date == date_of_first_engagement ~ "New",
                                     TRUE ~ "others"))
write.table(data_fart, file = "newandold23.csv", sep = ",", col.names = NA,qmethod = "double")
view(data_fart)

New_and_Returning_Customers <-  data_fart%>%
  group_by(floor_date(ModifiedTxnDate,unit = 'month'))%>%
  summarise(New_Customers = n_distinct(Customer.Id[Customer_Status=="New"]), Returning_Customers= n_distinct(Customer.Id[Customer_Status=="Repeat"]))


view(New_and_Returning_Customers)

write.table(New_and_Returning_Customers, file = "finallynewvsold.csv", sep = ",", col.names = NA,qmethod = "double")
view(repeatnew)


customer1 <- repeatnew %>%
  mutate(SalesMonth = as.character(as.integer(year(ModifiedTxnDate) * 100 + month(ModifiedTxnDate)))) %>%
  filter(!is.na(SalesMonth)) %>%
  group_by(Customer.Id) %>%
  summarise(FirstPurchase = min(SalesMonth, na.rm = TRUE)) 
view(customer1)




