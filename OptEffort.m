function Output=OptEffort(IndividualTurfEffort,WhichTurf,TurfEfforts,IsITQ,StartPop,Time,MakeFigures,FigureName)
%% Optimize the amount of effort for a TURF to exert, given conditions
%IndividualTurfEffort: The effort of the individual TURF being optimzied
% WhichTurf: Which TURF are you optimizing?
% TurfEfforts: Vector of TURF efforts for all TURFs
% IsITQ: 1 or 0 whether or not there are internal ITQs
% StartPop: The starting population
% Time: Time to run the model, 'EQ' runs to equilibrium
% MakeFigures: 1 or 0, make figures or not
% FigureName: figure names

global Fish Turf System

% if OptMode==1
TurfEfforts(WhichTurf)=IndividualTurfEffort; %Assign the appropriate effort quotas to each TURF
% end
FishingPressure=DistributeFleet(TurfEfforts,Turf.TurfNums); %Distribute the fleet among the patches

FishedPop= GrowPopulation(StartPop,FishingPressure,Time,IsITQ,0,MakeFigures,FigureName); %Fish the population

Outcomes=FishedPop; %The fished population

RawProfits=Outcomes.TurfProfits(WhichTurf,:); %Profits for the selected TURF

FinalProfits=RawProfits(end); %Final profits for the selected TURF

DiscountedProfits= RawProfits./(1+Turf.DiscountRate(WhichTurf)).^(1:length(RawProfits)); %Discounted profits

NPV= sum(DiscountedProfits); %Net present value of profits

Output=-FinalProfits; %You'll need to change this if you ever start discounting

% Output=-NPV; %Final profits for the TURF in question

end
