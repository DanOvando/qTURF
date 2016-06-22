function Output=SetGrandQuota(TotalEffort,IsITQ,StartPop,Time)
%% Function to optimize the Grand ITQ scenario

global Fish Turf System

 [theta]= find_theta(TotalEffort,StartPop); %Find trading that results in equal marginal profits

GrandQuotaPop=GrowPopulation(StartPop,TotalEffort,Time,IsITQ,2,'No','eh',theta); %Growpoulation at selected effort, with grand ITQ trading

Output= -sum(GrandQuotaPop.FinalProfits)%Change this if you don't want to run at EQ

end
