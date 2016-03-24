function Output=OmniPlanner(EffortVector,IsITQ,Time,StartPop)

global Fish Turf System

OmniPopulation=GrowPopulation(StartPop,EffortVector,Time,IsITQ,0,'No','eh'); %Growpoulation at selected effort

Output=-sum(OmniPopulation.FinalProfits); %Change this if you don't want to run at EQ

end
