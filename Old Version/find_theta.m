
function theta = find_theta(U, StartPop) %Central population growth function
global Fish Turf System

ts = [0,1];

store_mp = NaN(length(ts),2);

for t = 1:length(ts)
   temp = GrowPopulation(StartPop,U,'EQ',1,2,'No','eh', ts(t)); %Growpoulation at selected effort, with grand ITQ trading

    store_mp(t,:) = temp.FinalMarginalProfits;
   

end

testpop = GrowPopulation(StartPop,U,'EQ',1,2,'No','eh', 0.5); %Growpoulation at selected effort, with grand ITQ trading

if testpop.collapsed == 1
    theta = 0.5;
elseif (store_mp(1,1) > store_mp(1,2) && store_mp(end,1) > store_mp(end,2))
    theta = 1;
    
elseif (store_mp(1,2) > store_mp(1,1) && store_mp(end,2) > store_mp(end,1))
    theta = 0;
else
      [theta, arg, flag]=fzero(@(theta) tune_theta(theta,U,StartPop),0.5); %Find trading that results in equal marginal profits

end

% checkpop = GrowPopulation(StartPop,U,'EQ',1,2,'No','eh', theta); %Growpoulation at selected effort, with grand ITQ trading
% 
% checkpop.FinalMarginalProfits

end

