function fn=FunctionHolder %Function storage. New functions must be added in here
global Fish Turf System
fn.GrowPopulation=@GrowPopulation;
fn.DispersalKernel=@DispersalKernel;
fn.SetTurfQuota=@SetTurfQuota;
fn.DistributeFleet=@DistributeFleet;
fn.TurfProfits=@TurfProfits;
fn.OptEffort=@OptEffort;
fn.TurfGame=@TurfGame;
fn.SetGrandQuota=@SetGrandQuota;
fn.TradeQuota=@TradeQuota;
fn.OmniPlanner=@OmniPlanner;
end

function Output=GrowPopulation(StartPop,U,Time,IsITQ,IsTrading,MakeFigures,FileName) %Central population growth function
%% Population model
global Fish Turf System

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

StoreEffort=NaN(Turf.NumTurfs,Time);

EQCount=0;

StopTime=1e10; %Maximum time for the model to run

while t<(Time+1)
    
    MovePopulation= (PopMatrix(:,t))' * Fish.Dispersal; %Move fish population
    
    ['Time is ' num2str(t)];
    
    if IsTrading==1 %Trade quota amonst TURFs if desired
        %         RealizedEffort=TradeQuota(sum(Effort),IsITQ,MovePopulation);
        RealizedEffort=TradeQuota(U,IsITQ,MovePopulation);
        
        RealizedEffort=RealizedEffort./Turf.q;
    else
        RealizedEffort=Effort;
    end
    
    StoreEffort(:,t)=RealizedEffort.*Turf.q; %Transform effort form U to E
    
    Quota= RealizedEffort.*Turf.q.*MovePopulation; %Determine the quota for each level of effort
    
    Quota=max(0,min(Quota,MovePopulation)); %Make sure quota isn't higher than population, less than 0
    
    for tt=1:Turf.NumTurfs %Calculate TURF profits
        
        TimeTurfProfits(tt,t)= TurfProfits(Quota,RealizedEffort,MovePopulation,tt,IsITQ,'Simple');%Calculate profits in each TURF
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

PopMatrix(:,IsEmpty)=[];

StoreEffort(:,IsEmpty)=[];

Output.Trajectory=PopMatrix; %Store time series of population by patch

Output.Final=PopMatrix(:,end); %Store final population

Output.TurfProfits=TimeTurfProfits; %Store TURF profits over time

Output.FinalProfits=TimeTurfProfits(:,end); %Store final TURF profits

Output.Effort=StoreEffort; %Store effort over time

if strcmp(MakeFigures,'Yes')
    
    figure
    plot(Output.Trajectory','LineWidth',3)
    ylim([0,max(Fish.K)])
    xlabel('Time')
    ylabel('Biomass')
    print(gcf,'-depsc',[FileName 'Population Trajectory.eps'])
    close
    
    figure
    bar(Output.Final')
    xlabel('Patch')
    ylabel('Final Biomass')
    colormap summer
    print(gcf,'-depsc',[FileName 'Final Population.eps'])
    close
    
    figure
    plot(Output.TurfProfits','LineWidth',3)
    xlabel('Time')
    ylabel('Profits')
    print(gcf,'-depsc',[FileName 'Profit Trajectory.eps'])
    close
    
    figure
    bar(Output.FinalProfits')
    xlabel('Patch')
    ylabel('Final Profits')
    colormap summer
    print(gcf,'-depsc',[FileName 'Final Profits.eps'])
    close
    
    
    
end


end

function Dispersal=DispersalKernel(Mode,Dispersal) %Calculate dispersal kernel/probabilities
%% Calculate dispersal probabilities
global Fish Turf System

if strcmp(Mode,'Simple')
    %Really simple distance only works for 2 patch system. Direction is a
    %length 2 vector specifying what % of the population in each patch goes
    %to the other patch. Remainder stays in the home patch. so, .75 means
    %that 75% of individuals in patch 1 go to patch 2    
%     Dispersal=[1,1];å    
    Dii=[1-Dispersal(1), Dispersal(1) ; Dispersal(2), 1-Dispersal(2) ];
    
elseif Fish.Movement==0
    
    Dii=diag(repmat(1,1,System.NumPatches)); %sets up larval dispersal matrix with home cells filled in
else
    punit=2.5/Fish.Movement; %divide 2.5 standard deviations by the max dispersal distance, assumes that larvae will all settle with the bounds of 95% confidence intervals
    prang=-2.5:punit:2.5; %breamp.ks up norm dist into punit units
    pdisp=normpdf(prang); %assigns normal probability of larvae settling at distance punit
    
    pdisp=pdisp./sum(pdisp); %standardizes 95% distritbution to 100%
    
    Dii=diag(repmat(max(pdisp),1,System.NumPatches)); %sets up larval dispersal matrix with home cells filled in
    Dii2=diag(repmat(0,1,System.NumPatches)); %sets up larval dispersal matrix with home cells filled in
    Dii3=diag(repmat(0,1,System.NumPatches)); %sets up larval dispersal matrix with home cells filled in
    
    for i=1:System.NumPatches
        for j=1:System.NumPatches
            dif=abs(j-i);
            if dif==0
                Dii(i,j)= Dii(i,j); %ignores home cells
                
            elseif dif<=Fish.Movement
                Dii(i,j)=pdisp(find(pdisp==max(pdisp))+dif); %fills in remaining cells according to distance from home cell
            end
            if System.Wrap==1
                if  (System.NumPatches-i)<Fish.Movement && (System.NumPatches-j)<Fish.Movement
                    missingR=Fish.Movement-(System.NumPatches-i);
                    Dii2(i,1:missingR)=(pdisp(find(pdisp==max(pdisp))+(((Fish.Movement+1)-missingR):Fish.Movement)));
                end
                if (i-1)<Fish.Movement
                    missingL=(Fish.Movement-(i-1));
                    Dii3(i,((System.NumPatches+1)-missingL):end)=fliplr((pdisp(find(pdisp==max(pdisp))+(((Fish.Movement+1)-missingL):Fish.Movement))));
                end
                
            end
            
        end
    end
    Dii=Dii+Dii2+Dii3;
    
    
end
Dispersal=Dii;
end


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

function Profits=TurfProfits(Quota,Effort,Pop,WhichTurfs,IsITQ,Method) %Calculate TURF profits
%% Calculate profits in the selected TURF(s)
global Fish Turf System

% Quota: The quota caught
% Effort: the effort exerted
% WhichTurfs: TURFs you are calculating profits for
% IsITQ: 1 or 0 whether the TURFs have internal ITQs
% Method: Marks how profits are distributed, simple is the only option so far

Biomass=Pop;
if strcmp(Method,'Simple')
    
    if IsITQ==0 %Non-ITQ profits function, with racing
        Col=1;
        
        Alpha2=Turf.Alpha;
        
        Beta=Turf.Alpha(WhichTurfs,Col)./(2.*Effort);%Increase costs to dissipiate marginal profits
        
        Beta(isinf(Beta))=0;
    else %ITQ profit function
        
        Col=2;
        
        Alpha2=Turf.Alpha.*(1-System.ITQCosts);
        
        Beta= (Alpha2(:,Col)'.*Turf.q)./2; %Costs scale to catcheability
        
    end
    
    PatchProfits=nan(length(WhichTurfs),System.NumPatches); %Storage
    
    ['Profit effort is ' num2str(Effort)];
    
    ['Biomass is ' num2str(Biomass(1)) ' ' num2str(Biomass(2))];
    
    clear MarginalProfits
    
    for t=1:length(WhichTurfs) %Loop over TURFs
        
        Where=Turf.TurfLocations==WhichTurfs(t);
        
        PatchProfits(t,Where)=Alpha2(WhichTurfs(t),Col).*Quota(Where)-Beta(WhichTurfs(t)).*Effort(Where).^2; %Calculate profits for TURF t
        
        MarginalProfits(t)=Alpha2(WhichTurfs(t),Col).*Turf.q(WhichTurfs(t)).*Biomass(WhichTurfs(t))- 2.*Beta(WhichTurfs(t)).*Effort(Where);
        
    end
    ['TURF ',num2str(WhichTurfs), ' Profit marginal profits are ' num2str(MarginalProfits(1))];
    
    Profits=nansum(PatchProfits,2);
end

end

function FishingPressure=DistributeFleet(WhatRate,WhatTurf) %Distribute a fleet over a TURFs patches
%% Distribute fishing effort over patches
global Fish Turf System

% WhatRate: The fishing effort getting distributed
% WhatTurf: What TURF your're dealing with

DistributedU=zeros(1,System.NumPatches); %Storage
% WhatTurf=[1,2]
% WhatRate=TurfEfforts

for t=1:length(WhatTurf) %Loop over TURFs
    where=Turf.TurfLocations==WhatTurf(t); %Find TURF
    DistributedU(where)=WhatRate(t); %Assign effort to the right patches for TURF t
end

FishingPressure=DistributedU; %Distribtued fishing pressure

end

function Output=TurfGame(StartingGuess,Nudge,IsITQ,StartPop,Time,FLimit)
%% Game theory model for deciding TURF fishing efforts

global Fish Turf System

% StartingGuess: Starting guess of effort
% Nudge: how many times to nudge the starting guess to see if you end up somewhere different
% IsITQ: are there internal ITQs?
% StartPop: the starting population
% Time: the time to run the model

% Options=optimset('MaxFunEvals',1000,'TolFun',1e-9,'TolCon',1e-9,'MaxIter',1000);
Options=optimset('MaxFunEvals',1000,'Display','notify','Algorithm','active-set');
% StartPop=UnfishedPop
% Time='EQ'
%  StartingGuess=[Umsy,Umsy];

EffortStrategy=StartingGuess;

Tolerance=1e-3; %Tolerance for game

FinalStrategy=nan(Nudge,length(StartingGuess)); %Storage

for n=1:Nudge %Loop over nudges to check for consistent convergence
    
    if n>1
        
        EffortStrategy=StartingGuess.*lognrnd(0,.9,1,length(StartingGuess)); %Nudge starting guess
        
    end
    
    DeltaQuota=999;
    
    while DeltaQuota>Tolerance %While quotas are changing
        
        OldStrategy=EffortStrategy;
        Flip=-1;
        for t=1:Turf.NumTurfs %Pass the game back and forth between each TURF
            
            %             EffortStrategy
            %             pause
            ProfitFunction=@(Target)OptEffort(Target,t,EffortStrategy,IsITQ,StartPop,Time,'No','No'); %pass additional parameters
            
            Bottom=.0001;
            Top=1.9*Fish.Umsy;
            if FLimit>0
             
                Flip=Flip*-1;
                Bottom=.0001;
                Top=FLimit-EffortStrategy(t+Flip);
                
            end

            [OptimalU,Profits,exitflag,output]=fmincon(ProfitFunction,Fish.Umsy/2,[],[],[],[],Bottom,Top,[],Options); %optimize effort to maximize turf T profits
            ['Turf', num2str(t) ,'Profits are' ,num2str(Profits)];
            
            EffortStrategy(t)=OptimalU;
            
        end
        
        NewStrategy=EffortStrategy; % Assign new effort strategy given last step in the game
        
        DeltaQuota=sum(abs(NewStrategy-OldStrategy)); % Calculate change in quota from step to step in the game
        
    end
    
    FinalStrategy(n,:)=EffortStrategy; %Store final strategy
    
end

GameEquilibrium=mean(FinalStrategy,1); %Mean effort strategies over nudges

Flag=sum(abs(mean(FinalStrategy,1)-FinalStrategy(1,:)))>Tolerance; %Check for consistent convergence

if Flag>0
    warning('Game theoretic outcome different under different starting guesses')
end

Output.Effort=GameEquilibrium;

Output.Flag=Flag;

end

function Output=SetGrandQuota(TotalEffort,IsITQ,StartPop,Time)
%% Function to optimize the Grand ITQ scenario

global Fish Turf System

GrandQuotaPop=GrowPopulation(StartPop,TotalEffort,Time,IsITQ,1,'No','eh'); %Growpoulation at selected effort, with grand ITQ trading

Output=-sum(GrandQuotaPop.FinalProfits); %Change this if you don't want to run at EQ

end

function Output= TradeQuota(TotalEffort,IsITQ,Pop)
%% Function to trade quota in the grand ITQ
global Fish Turf System
%  Pop=UnfishedPop;
%    TotalEffort=(2*Umsy)./Turf.q;
%    IsITQ=1;

TurfPop=nan(1,Turf.NumTurfs);

if IsITQ==0 %Calculate non-ITQ profit function
    Col=1;
    
    %             Alpha2=Turf.Alpha;
    
    Beta=Turf.Alpha(WhichTurfs,Col)./(2.*Effort);%Increase costs to dissipiate marginal profits
    
    Beta(isinf(Beta))=0;
    
    %     Beta=Turf.Alpha(:,Col)./(TotalEffort); %Divide up effort equally
    %
    %      Beta(isinf(Beta))=0;
    
    
    
else %Calculate ITQ profit function
    Col=2;
    
    Beta= (Turf.Alpha(:,Col)'.*Turf.q)./2;
    
    %         Biomass=Quota./(Turf.q*Effort);
    
    %         Turf.Alpha(WhichTurfs,Col)-(2.*Beta.*Quota)./(Turf.q.*Biomass)
end


for t=1:Turf.NumTurfs %loop over TURFs
    TurfPop(t)=sum(Pop(Turf.TurfLocations==t));
    Pi(t)=Turf.Alpha(t,Col).*Turf.q(t).*TurfPop(t); %Calculate profits in turf t
    %     Pi(t)=Turf.Alpha(t,Col).*TurfPop(t);
    Marg(t)=Pi(t)-2*Beta(t); %Calculate marginal profits
    
end

% Theta=((Pi(2)-Pi(1))/(2*TotalEffort)-Beta(2))./(-Beta(1)-Beta(2));

% Theta=(2.*Beta(2).*Turf.q(1).*TotalEffort + Pi(1).*Turf.q(1)    )

Theta=fzero(@(Theta) ZeroFun(Theta,Pi,Beta,Turf,TotalEffort,Col),.5); %Find trading that results in equal marginal profits

['Theta is ' num2str(Theta)];

['Trade Effort is ' ,num2str(((TotalEffort.*Theta))./Turf.q(1)),' ', num2str((TotalEffort.*(1-Theta))./Turf.q(2))];

['Trade biomass is ' num2str(TurfPop(1)),' ',num2str(TurfPop(2))];

['Are MPs Equal ' num2str((Turf.Alpha(1,Col).*Turf.q(1).*TurfPop(1)-(2.*Beta(1).*((TotalEffort.*Theta)))./Turf.q(1))),' ',...
    num2str(Turf.Alpha(2,Col).*Turf.q(2).*TurfPop(2)-(2.*Beta(2).*(TotalEffort.*(1-Theta))./Turf.q(2)))];

%   fTheta=fzero(@(Theta) ZeroFun(Theta,Pi,Beta,Turf,TotalEffort,Col),.5);

QuotaPrice=Turf.Alpha(2,Col).*Turf.q(2).*TurfPop(2)-(2.*Beta(2).*(TotalEffort.*(1-Theta))./Turf.q(2));
Turf.QuotaPrice=QuotaPrice;
if (Theta>1) % Account for corner solutions; if one TURF isn't fishing
    
    if Marg(1)>Marg(2) %Assign all the fishing to whichever TURF is more efficient
        Theta=1;
    elseif Marg(1)<Marg(2)
        Theta=0;
    elseif Marg(1)==Marg(2)
        Theta=0.5;
    end
    
elseif (Theta<0)
    Theta=0;
end

Output=DistributeFleet([TotalEffort.*Theta,TotalEffort.*(1-Theta)],1:2);

end

function Output=OmniPlanner(EffortVector,IsITQ,Time,StartPop)

global Fish Turf System

OmniPopulation=GrowPopulation(StartPop,EffortVector,Time,IsITQ,0,'No','eh'); %Growpoulation at selected effort

Output=-sum(OmniPopulation.FinalProfits); %Change this if you don't want to run at EQ

end


