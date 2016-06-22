function [ output_args ] = check_theta( opted, StartPop, Time )
%check_theta Function to check theta
%   Detailed explanation goes here

global Fish Turf System

opted(2)
GrandQuotaPop=GrowPopulation(StartPop,opted(1),Time,1,2,'No','eh',opted(2)); %Growpoulation at selected effort, with grand ITQ trading

output_args=-sum(GrandQuotaPop.FinalProfits); %Change this if you don't want to run at EQ
end
