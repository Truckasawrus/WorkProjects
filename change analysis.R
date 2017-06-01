

##Daily Change by Audit, by Enterprise

AllAuditsEnterpriseChange<-AllAuditsEnterprise%>%
  group_by(Enterprise.ID, AuditType)%>%
  mutate(TotalDiff= ave(Results,FUN=function(x) c(0, diff(x))))

##Daily Change by Audit, by Ops Associate

AllAuditsChangeAssoc<-AllAuditsTeamTotals%>%
  group_by(Service.Operations.Associate,EnterpriseID, AuditType)%>%
  mutate(TotalDiff= ave(Results,FUN=function(x) c(0, diff(x))))


AllAuditsEnterpriseChange$Thresh <- ifelse(AllAuditsEnterpriseChange$TotalDiff < 0, "decrease", "increase")  # above / below avg flag

AllAuditsEnterpriseChangeMains<-AllAuditsChangeAssoc%>%
  group_by(AuditType,Service.Operations.Associate)%>%
  summarise(TotalDiff = sum(TotalDiff))

# Plot
ggplot(subset(AllAuditsChangeAssoc,Service.Operations.Associate == "Matt King"& AuditType %in% c("Total.Actionable","Total.Missing","Total.Unreconciled") & TotalDiff != 0), 
       aes(x=Enterprise.ID, y=TotalDiff, label=Enterprise.ID)) + 
  geom_bar(stat='identity', position = "stack",aes(fill=AuditType), width=.5)  +
 ## scale_fill_manual(name="Change", 
   ##                 labels = c("Increased", "Decreased"), 
     ##               values = c("increase"="#00ba38", "decrease"="#f8766d")) + 
  labs(subtitle="Change", 
       title= "Diverging Bars") + 
  coord_flip()+facet_wrap(~AuditType)