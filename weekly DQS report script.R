library(plyr)
library(dplyr)
library(ggplot2)
library(quantmod)
library(reshape2)
library(scales)
library(DataCombine)
library(timeDate)
library(tidyr)



##READ IN PARSED OUT WEEKLY STATS SPREADSHEETS

AllAudits<-read.csv("DQS032017.csv")


###CHANGE DATE COLUMNS TO ACTUAL DATE TYPE AND FORMAT


AllAudits$Date<-as.Date(AllAudits$Date,format = "%m/%d/%Y")

###Change Blank Assignments to 'UNASSIGNED'

AllAudits$Service.Manager<-sub("^$","Unassigned",AllAudits$Service.Manager)
AllAudits$Service.Senior.Manager<-sub("^$","Unassigned",AllAudits$Service.Senior.Manager)
AllAudits$Service.Operations.Associate<-sub("^$","Unassigned",AllAudits$Service.Operations.Associate)
AllAudits$Reconciliation.Senior.Manager<-sub("^$","Unassigned",AllAudits$Reconciliation.Senior.Manager)
AllAudits$Reconciliation.Manager.Lead<-sub("^$","Unassigned",AllAudits$Reconciliation.Manager.Lead)
AllAudits$Reconciliation.Analyst<-sub("^$","Unassigned",AllAudits$Reconciliation.Analyst)




###COLLAPSE DATE COLUMNS FROM LONG FORM AND CHANGE N/A VALUES TO ZEROS

AllAuditsTotals<-ddply(AllAudits,.(Date,Service.Operations.Associate,Service.Manager)
                       ,summarize,Total.Actionable = (sum(Total.Actionable.Audit.Results)),
                       Total.Missing =(sum(Account.Status..Accounts.Missing.Reconciliation)),
                       Total.Duplicate =(sum(Account.Status..Duplicate.Account.Numbers)),
                       Total.Closed.With.Holdings =(sum(Holdings.Recon..Closed.Accounts.with.Holdings)),
                       Total.Unreconciled =(sum(Holdings.Recon..Unreconciled.Accounts)),
                       Total.Missing.Price =(sum(Pricing..Missing.Price.Audit)),
                       Total.Missing.Factor =(sum(Pricing..Mortgage.Backed.Securities.with.Missing.or.Invalid.Factors)),
                       Total.NV =(sum(Pricing..No.Values..NV..Security.Price.Audit)),
                       Total.Biz.Diff =(sum(Pricing..Price.Differences.between.Business.Days)),
                       Total.PC.vs.Bloomberg =(sum(Pricing..Price.Differences.between.PC.and.Bloomberg)),
                       Total.CUSIP.Diff =(sum(Security.Information..Security.CUSIP.Differences.between.AV.and.Bloomberg)),
                       Total.Deb.Cred.Net =(sum(Transaction..Accounts.where.Debits.and.Credits.don.t.net.out.on.the.Same.Day)),
                       Total.Rec.Xfer =(sum(Transaction..Accounts.with.Receipts.and.Transfers.on.the.Same.Day)),
                       Total.Buy.Sell.Zero =(sum(Transaction..Buys.and.Sells.with.Net.Amount.of.0)),
                       Total.Cash.Exp.Assg =(sum(Transaction..Expenses.Assigned.to.Cash)),
                       Total.Journal.Cred.Inc =(sum(Transaction..Journals.and.Credits.on.Account.Inception.Date)),
                       Total.Rec.Xfer.NoVal =(sum(Transaction..Receipts.and.Transfers.without.Market.Value)),
                       Total.Trans.to.None =(sum(Transaction..Transactions.to.None.that.are.not.Receipts.or.Transfers)),
                       Total.CIL.Notes =(sum(Transaction..Transactions.with.Notes.Cash.in.Lieu)),
                       Total.Unproc.Xfers =(sum(Transaction..Unprocessed.Account.Transfers))
)

AllAuditsTotals[is.na(AllAuditsTotals)]<-0

###SUMMARIZE DATA TO TOTALS

#AllAudits1<-AllAudits1%>%
  #mutate(AuditCategory = ifelse(AuditType %in% AccountStatus,"Account Status",
  #ifelse(AuditType %in% SecurityPricing, "Security Pricing",
  #ifelse(AuditType%in% MissingPrices, "Missing Prices",
  #ifelse(AuditType%in% SecurityInfo, "Security Info",
  #ifelse(AuditType %in% HoldingsRecon, "Holdings and Recon",       
  #ifelse(AuditType %in% TransactionAudits,"Transaction Audits",
  #ifelse(AuditType %in% TotalActionable, "Total Actionable Audits"))))))))



AllAuditsTotals<-AllAuditsTotals%>%
  gather(AuditType,Results,4:23)


##Daily Change by Audit, by Ops Assoc.
AllAuditsTotals<-AllAuditsTotals%>%
  group_by(Service.Operations.Associate, AuditType)%>%
  mutate(TotalDiff= ave(Results,FUN=function(x) c(0, diff(x))))


      
##Subset only Actionable, Unreconciled, Missing
AllAuditsTotalsAction<-subset(AllAuditsTotals,AuditType %in% 
                                c("Total.Actionable",
                                  "Total.Unreconciled",
                                  "Total.Missing"))



##Summary Totals Table to Label team-visualizations
sum_count<-AllAuditsTotalsAction%>%
  group_by(Date,Service.Operations.Associate,Service.Manager)%>%
  summarise(Total.Results = sum(Results))
  
sum_count<-subset(sum_count,Service.Manager %in% c("Brian Harkins"))

AllAuditsTotalsAction<-AllAuditsTotalsAction%>%
  transform(Service.Operations.Associate = reorder(Service.Operations.Associate, desc(Results)))


####PLOTS OF WEEKLY AUDIT NUMBERS BY TEAM INCLUDES TOTAL AUDIT# ANNOTATION ON TOP


HawthorneViz<-ggplot(subset(AllAuditsTotalsAction,Service.Manager %in% c("Brian Harkins")), aes(x= Service.Operations.Associate))+
  geom_bar(aes(y = Results, fill = AuditType),stat = "identity", position = "stack")+
  geom_text(data = subset(sum_count,Service.Manager %in% c("Brian Harkins")), aes(y = Total.Results,label = Total.Results), vjust = -1, hjust = .5, size = 2.5, fontface = "bold")+
  scale_fill_hue(l=30,name = "Hawthorne Audits",
  labels = c("Total Actionable", "Missing Accounts", "Unreconciled Accounts"))+
  scale_y_discrete(breaks=NULL)+ 
  ggtitle("Audit Overview")+
  ylab("Totals")+xlab("Weekly Totals")+
  theme(legend.text = element_text(size = 10, face = "bold"),legend.position = "top",
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 90, hjust = 1,vjust = .5, size = 10, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        strip.text.x = element_text(size=10,face="bold"),
        strip.text.y = element_text(size=12,angle = .45, face="bold"))+
  facet_grid(~Date,scales = "free", space = "free")


SycamoreViz<-ggplot(subset(AllAuditsTotalsAction,Service.Manager %in% c("Matt King")), aes(x= Service.Operations.Associate))+
  geom_bar(aes(y = Results, fill = AuditType),stat = "identity", position = "stack")+
  geom_text(data = subset(sum_count,Service.Manager %in% c("Matt King")), aes(y = Total.Results,label = Total.Results), vjust = -1, hjust = .5, size = 2.5, fontface = "bold")+
  scale_fill_hue(l=30,name = "Hawthorne Audits",
  labels = c("Total Actionable", "Missing Accounts", "Unreconciled Accounts"))+
  scale_y_discrete(breaks=NULL)+ 
  ggtitle("Audit Overview")+
  ylab("Totals")+xlab("Weekly Totals")+
  theme(legend.text = element_text(size = 10, face = "bold"),legend.position = "top",
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 90, hjust = 1,vjust = .5, size = 10, face = "bold"),
        axis.text.y = element_text(size = 10, face = "bold"),
        strip.text.x = element_text(size=10,face="bold"),
        strip.text.y = element_text(size=12,angle = .45, face="bold"))+
  facet_grid(~Date,scales = "free", space = "free")

##multiplot(HawthorneViz, SycamoreViz, cols = 2)


write.csv(WeeklyReportVizHawThorne,file = "Hawthorne Week 2.csv",row.names = FALSE)

write.csv(WeeklyReportVizSycamore,file = "Sycamore week 2.csv",row.names = FALSE)

