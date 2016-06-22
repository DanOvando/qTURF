function [ diff ] = tune_theta( theta,U,Pop)
global Fish Turf System

        out = GrowPopulation(Pop,U,'EQ',1,2,'No','f', theta);
        
        diff = (out.FinalMarginalProfits(1) - out.FinalMarginalProfits(2));

end

