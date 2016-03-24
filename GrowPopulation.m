function Output=GrowPopulation(StartPop,U,Time,IsITQ,IsTrading,MakeFigures,FileName) %Central population growth function
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

TimeTurfMarginalProfits=NaN(Turf.NumTurfs,Time);


StoreEffort=NaN(Turf.NumTurfs,Time);

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
        RealizedEffort=TradeQuota(U,IsITQ,MovePopulation);
        
        RealizedEffort=RealizedEffort./Turf.q;
    else
        RealizedEffort=Effort;
    end
    
    StoreEffort(:,t)=RealizedEffort.*Turf.q; %Transform effort form U to E
    
    Quota= RealizedEffort.*Turf.q.*MovePopulation; %Determine the quota for each level of effort
    
    Quota=max(0,min(Quota,MovePopulation)); %Make sure quota isn't higher than population, less than 0
    
    for tt=1:Turf.NumTurfs %Calculate TURF profits
        
        if length(IsITQ) >1 % In case you have partial internal ITQs
            
            itq_temp = IsITQ(tt) ;
        else
            itq_temp = IsITQ;
            
        end
        
        [temp_profits, temp_marginal_profits] = TurfProfits(Quota,RealizedEffort,MovePopulation,tt,itq_temp,'Simple');
        
        TimeTurfProfits(tt,t)= temp_profits ; %Calculate profits in each TURF
        
        TimeTurfMarginalProfits(tt,t)= temp_marginal_profits;%Calculate profits in each TURF
        
    end
    
    
    Grow= max(0,MovePopulation+(MovePopulation.*Fish.r).*(1-MovePopulation./(Fish.K .* System.HabQuality))-Quota); %Grow the population
    
    %     Grow(Grow<1)=0;
    
    PopMatrix(:,t+1)=Grow; %Assign the new population to the next year
    
    
    PopChange=abs(sum(PopMatrix(:,t+1)-PopMatrix(:,t))); %Check to see if the population is changing
    
    t=t+1;
    
    if strcmp(TimeMode,'EQ') && PopChange<= System.PopTolerance && EQCount<2 %Stop population growth once it hits EQ +1 year
        
        StopTime=t; %Number of years to EQ
        
        EQCount=EQCount+1;
        
    end
    
    if EQCount==2
        t=Time+1;
    end
    
    
end %Close population loop


%Clean up data
IsEmpty=StopTime<= 1:size(PopMatrix,2);

TimeTurfProfits(:,IsEmpty)=[];

TimeTurfMarginalProfits(:,IsEmpty)=[];

PopMatrix(:,IsEmpty)=[];

StoreEffort(:,IsEmpty)=[];

Output.Trajectory=PopMatrix; %Store time series of population by patch

Output.Final=PopMatrix(:,end); %Store final population

Output.TurfProfits=TimeTurfProfits; %Store TURF profits over time

Output.FinalProfits=TimeTurfProfits(:,end); %Store final TURF profits

Output.FinalMarginalProfits = TimeTurfMarginalProfits(:,end); %Store final TURF profits

Output.Effort=StoreEffort; %Store effort over time

if strcmp(MakeFigures,'Yes')
    
    %     figure
    %     plot(Output.Trajectory','LineWidth',3)
    %     ylim([0,max(Fish.K)])
    %     xlabel('Time')
    %     ylabel('Biomass')
    %     print(gcf,'-depsc',[FileName 'Population Trajectory.eps'])
    %     close
    %
    %     figure
    %     bar(Output.Final')
    %     xlabel('Patch')
    %     ylabel('Final Biomass')
    %     colormap summer
    %     print(gcf,'-depsc',[FileName 'Final Population.eps'])
    %     close
    %
    %     figure
    %     plot(Output.TurfProfits','LineWidth',3)
    %     xlabel('Time')
    %     ylabel('Profits')
    %     print(gcf,'-depsc',[FileName 'Profit Trajectory.eps'])
    %     close
    %
    %     figure
    %     bar(Output.FinalProfits')
    %     xlabel('Patch')
    %     ylabel('Final Profits')
    %     colormap summer
    %     print(gcf,'-depsc',[FileName 'Final Profits.eps'])
    %     close
    
    
    
end


end
