function Output=SetGrandQuota(TotalEffort,IsITQ,StartPop,Time)
%% Function to optimize the Grand ITQ scenario

global Fish Turf System
TotalEffort
GrandQuotaPop=GrowPopulation(StartPop,TotalEffort,Time,IsITQ,1,'No','eh'); %Growpoulation at selected effort, with grand ITQ trading
Output=-sum(GrandQuotaPop.FinalProfits); %Change this if you don't want to run at EQ

end
