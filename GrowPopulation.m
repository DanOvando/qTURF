function Output=GrowPopulation(StartPop,U,Time,IsITQ,IsTrading,MakeFigures,FileName, theta) %Central population growth function
%% Population model
global Fish Turf System

% U is fishing mortality
% Time is the amount of time

%              StartPop=100;
%              Time=190;
%              U=[.09 .0818];
%          MakeFigures='No'
%          IsITQ=0
%          IsTrading=0
%          FileName='wh'

cr_ratio = 0.75;

Effort=U./Turf.q; %Translate F in to effort;

if length(Effort)==1
    Effort=ones(1,System.NumPatches)* Effort; %Make effort the right size
end

TurfEffort=nan(1,Turf.NumTurfs); %Distribute TURF effort by patch
for t=1:Turf.NumTurfs
    Where=Turf.TurfLocations==t;
    Temp=Effort(Where);
    TurfEffort(t)=Temp(1);
end

if strcmp(Time,'EQ') %Set the amount of time to run the model
    TimeMode='EQ';
    Time=4000;
else
    TimeMode='Finite';
end

%Make storage

PopMatrix=NaN(System.NumPatches,Time);

t=1   ;

PopMatrix(:,1)=StartPop;

TimeTurfProfits=NaN(Turf.NumTurfs,Time);

catches=NaN(Turf.NumTurfs,Time);

revenues=NaN(Turf.NumTurfs,Time);

costs=NaN(Turf.NumTurfs,Time);


TimeTurfMarginalProfits=NaN(Turf.NumTurfs,Time);

StoreEffort=NaN(Turf.NumTurfs,Time);

RealEffort=NaN(Turf.NumTurfs,Time);


EQCount=0;

StopTime=1e10; %Maximum time for the model to run

while t<(Time+1)
    
    MovePopulation= (PopMatrix(:,t))' * Fish.Dispersal; %Move fish population
    
    %['Time is ' num2str(t)];
    
    if IsTrading==1 %Trade quota amonst TURFs if desired
        %         RealizedEffort=TradeQuota(sum(Effort),IsITQ,MovePopulation);
        
        if length(IsITQ) >1
            warning('Attempting to trade with different internal ITQs')
        end
        
        RealizedEffort=TradeQuota(U,IsITQ,MovePopulation, cr_ratio);
        

        %         RealizedEffort=RealizedEffort./Turf.q;
    elseif IsTrading == 2
        
        quota_pool = U .* sum(MovePopulation);
        
        RealizedEffort = ([theta, 1 - theta] .* quota_pool) ./ (Turf.q .* MovePopulation);
        
    else
        
        RealizedEffort=Effort;
    end
    
    StoreEffort(:,t)=RealizedEffort.*Turf.q; %Transform effort from E to U
    
    RealEffort(:,t)=RealizedEffort; %Store real effort
    
    
    Quota= RealizedEffort.*Turf.q.*MovePopulation; %Determine the quota for each level of effort
    
    Quota=max(0,min(Quota,MovePopulation)); %Make sure quota isn't higher than population, less than 0
    
    catches(:,t) = Quota;
    
    for tt=1:Turf.NumTurfs %Calculate TURF profits
        
        if length(IsITQ) >1 % In case you have partial internal ITQs
            
            itq_temp = IsITQ(tt) ;
        else
            itq_temp = IsITQ;
            
        end
        
        [temp_profits, temp_marginal_profits, temp_revenues, temp_costs] = TurfProfits(Quota,RealizedEffort,MovePopulation,tt,itq_temp,'Simple', cr_ratio);
        
        TimeTurfProfits(tt,t)= temp_profits ; %Calculate profits in each TURF
        
        TimeTurfMarginalProfits(tt,t)= temp_marginal_profits;%Calculate profits in each TURF
        
        revenues(tt,t) = temp_revenues;
        
        costs(tt,t) = temp_costs;
        
    end
     %
    
    Grow= max(0,MovePopulation+(MovePopulation.*Fish.r).*(1-MovePopulation./(Fish.K .* System.HabQuality))-Quota); %Grow the population
    
    %     Grow(Grow<1)=0;
    
    PopMatrix(:,t+1)=Grow; %Assign the new population to the next year
    
    
    PopChange=abs(sum(PopMatrix(:,t+1)-PopMatrix(:,t))); %Check to see if the population is changing
    
    
    if strcmp(TimeMode,'EQ') && PopChange<= System.PopTolerance && EQCount<2 || t > 500 %Stop population growth once it hits EQ +1 year
        
        StopTime=t; %Number of years to EQ
        
        EQCount=EQCount+1;
        
    end
    
    t=t+1;

    if EQCount==2
        t=Time+1;
    end
    
    
end %Close population loop



TimeTurfProfits = TimeTurfProfits(:, 1:StopTime);

TimeTurfMarginalProfits = TimeTurfMarginalProfits(:, 1:StopTime);

PopMatrix = PopMatrix(:, 1:StopTime); 

StoreEffort = StoreEffort(:,1:StopTime); 

revenues = revenues(:,1:StopTime); 

costs = costs(:,1:StopTime); 

catches = catches(:,1:StopTime); 



Output.Trajectory=PopMatrix; %Store time series of population by patch

Output.Final=PopMatrix(:,end); %Store final population

Output.TurfProfits=TimeTurfProfits; %Store TURF profits over time

Output.FinalProfits=TimeTurfProfits(:,end); %Store final TURF profits

Output.FinalMarginalProfits = TimeTurfMarginalProfits(:,end); %Store final TURF profits

Output.Effort=StoreEffort; %Store effort over time

Output.RealEffort = RealEffort;

Output.catches = catches;

Output.revenues = revenues;

Output.costs = costs;

% 
% 
% if strcmp(MakeFigures,'Yes')
%     
%     
% end


end
