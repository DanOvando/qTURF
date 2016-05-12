function [ out ] = tune_msy(us)
%tune_costs method for tuning best-case ITQ costs
%   Set a desired cost to revenue ratio at a target biomass level

global Fish Turf System

MSY=GrowPopulation(Fish.K,[us(1) us(2)],'EQ',0,0,'No','blah');

catch_v_k = MSY.catches(:,end)'./Fish.K;

out = -sum(catch_v_k);

end

