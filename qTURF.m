%% qTURF
% This model runs the 2013 ECO-A project qTURF, evaluating the
% interactions between TURFs and ITQs

%Created by Dan Ovando and Sarah Poon
%% Setup Workspace
 clear all
close all
pause on

global Fish Turf System
opt_tol = 0.01;
FunctionHolder; % Load in functions

fn=FunctionHolder(); %Allow functions to be called

RunName='Blah'; %Set name of folder to store results

mkdir('Results');

BaseFolder=strcat('Results/',RunName,'/'); %Folder for all results

FigureFolder=strcat('Results/',RunName,'/Figures/'); %Folder for figures

RawFolder=strcat('Results/',RunName,'/RawResults/'); %Folder for numeric results

mkdir(FigureFolder)

mkdir(RawFolder)

ControlFile; %Load in controlfile

save(strcat(BaseFolder,'WorkSpace.mat'),'Turf','Fish','System') %save workspace for future reference


%% Set up Initial Conditions

EQPop=GrowPopulation(10,0,'EQ',0,0,'Yes',FigureFolder); %Create Unfished Population

UnfishedPop=EQPop.Final;

Umsy=(Fish.r/2); %Calculate Umsy

% TestFishing=GrowPopulation(UnfishedPop,Umsy,'EQ',0,0,'Yes',strcat(FigureFolder,'Test'));

uSeq=linspace(0.01*Umsy,3*Umsy,100);

for u=1:length(uSeq) %Test a variety of fishing pressues to evaluate MSY/MEY
    
    Temp = GrowPopulation(UnfishedPop,[uSeq(u),uSeq(u)],'EQ',1,0,'No','eh');
    
    %     Temp=OptEffort(uSeq(u),1,[0,Umsy],0,UnfishedPop,'EQ','No',strcat(FigureFolder,'OptTest'));
    
    %Temp=Temp.FinalMarginalProfits;
    
    Profits(u,:)=Temp.FinalProfits;
    
    MargProfits(u,:)=Temp.FinalMarginalProfits;
    
end


figure %Plot MEY
plot(uSeq,Profits(:,1),'r')
hold on
plot(uSeq,MargProfits(:,1))
refline(0,0)
xlabel('Effort')
ylabel('Equilibrium $s')
hold off

legend('Profits',' Marginal Profits')

figure %Plot MEY
plot(uSeq,MargProfits)
xlabel('Effort')
ylabel('Marginal Profits')
legend('TURF 1','TURF 2')

% FishMovement=[0,System.NumPatches]; %Set vector of fish movement scales to be tested

FishMovement=[0,0 ; .5,.5 ; .25, 0.1 ; 0.1,.25]; %Set vector of fish movement scales to be tested

TurfDifference = [1,4]; %Set vector of TURF fishing skill hetergeneity to be tested

%Create result storage space
Results.Biomass=nan(2,size(FishMovement,1)*length(TurfDifference),5);
Results.Profits=nan(2,size(FishMovement,1)*length(TurfDifference),5);
Results.Effort=nan(2,size(FishMovement,1)*length(TurfDifference),5);
Results.cost=nan(2,size(FishMovement,1)*length(TurfDifference),5);

%Results.TradeValue=nan(2,size(FishMovement,1)*length(TurfDifference));

c=0; %counter
BaseK=Fish.K;
BasePop=UnfishedPop;

StoreK=nan(2,size(FishMovement,1).*length(TurfDifference));
for d=1:size(FishMovement,1) %loop over movement experiment
    d
    %     Fish.Movement=FishMovement(d); %Set fish movement to selected value
    
    Fish.Dispersal=DispersalKernel('Simple',FishMovement(d,:)); %Calculate dispersal kernel
    
    UnfishedPop=BasePop;
    
    Fish.K=BaseK;
    
    NewPop=GrowPopulation(UnfishedPop,0,'EQ',0,0,'Yes',[FigureFolder 'doh Dispersal is' num2str(d)]);
    
    UnfishedPop=NewPop.Final;
    
    if System.AdjustK==1 && d>2
        
        Fish.K=NewPop.Final';
        
        Delta=100;
        
        while Delta>0.001
            
            OldPop=NewPop.Final;
            
            UnfishedPop=NewPop.Final;
            
            NewPop=GrowPopulation(UnfishedPop,0,'EQ',0,0,'No',[FigureFolder 'doh 2 Dispersal is' num2str(d)]);
            
            Fish.K=NewPop.Final';
            
            UnfishedPop=NewPop.Final;
            
            Delta=abs(sum(UnfishedPop)-sum(OldPop));
        end
        
    end
    
    
    [umsy]=fmincon(@tune_msy,[Fish.r Fish.r] ,[],[],[],[],[.001 .001],[5*Fish.Umsy 5*Fish.Umsy]); %Find Umsy
    
    
    msy_pop = GrowPopulation(Fish.K,[umsy(1) umsy(2)],'EQ',0,0,'No','blah');
    
    %Fish.Umsy = umsy
    
    for h=1:length(TurfDifference) %Loop over heterogeniety in TURFs
        
        h
        
        c=c+1; %update counter
        
        StoreK(:,c)=Fish.K';
        
        Turf.q(1)=Turf.q(2).*TurfDifference(h); %Change TURF ability
        
        % Tune costs
        a = 1;
        
        msy_revenues = msy_pop.revenues(:,end);
        
        msy_catches = msy_pop.catches(:,end);
        
        msy_biomass = msy_pop.Trajectory(:,end);
        
        msy_effort = msy_catches ./ (Turf.q' .* msy_biomass);
        
        System.cr_ratio  = 0.75;
        
        Alpha2=Turf.Alpha.*(1-System.ITQCosts);
        
        costs = (Turf.Alpha(:,2) .*(1-System.ITQCosts) .* msy_catches .* System.cr_ratio) ./ (msy_effort.^2);
        
        
        Turf.Beta(:,2) = costs;
        
        
        %        (costs .* msy_effort.^2) ./ (Turf.Alpha(:,2) .* msy_catches)
        
        
        %Calculate results without ITQ
        
        GameResults = TurfGame([Umsy,Umsy],4,0,UnfishedPop,'EQ',0); %Optimize effort without internal ITQs
        
        DistEffort=DistributeFleet([GameResults.Effort],Turf.TurfNums); %Distribute fishing effort
        
        NonITQGameOutcome= GrowPopulation(UnfishedPop,DistEffort,'EQ',0,0,'Yes',[FigureFolder 'Dispersal is' num2str(d) 'Het is' num2str(TurfDifference(h))  'No ITQ Game Outcome  ']);
        
        Results.Biomass(:,c,1)=NonITQGameOutcome.Final;
        
        Results.Profits(:,c,1)=NonITQGameOutcome.FinalProfits;
        
        Results.Effort(:,c,1)=GameResults.Effort;
        
        Results.MarginalProfits(:,c,1) = NonITQGameOutcome.FinalMarginalProfits;
        
        Results.costs(:,c,1) = NonITQGameOutcome.costs(:,end);
        
        
        %Calculate results with internal ITQ
        
        GameResults=TurfGame([Umsy,Umsy],1,1,UnfishedPop,'EQ',0); %Optimize effort with internal ITQ
        
        DistEffort=DistributeFleet([GameResults.Effort],Turf.TurfNums); %Distribute fishing effort
        
        ITQGameOutcome= GrowPopulation(UnfishedPop,DistEffort,'EQ',1,0,'Yes',[FigureFolder 'Dispersal is' num2str((d)) 'Het is' num2str(TurfDifference(h))  'With ITQ Game Outcome  ']);
        
        
        Results.Biomass(:,c,2)=ITQGameOutcome.Final;
        
        Results.Profits(:,c,2)=ITQGameOutcome.FinalProfits;
        
        Results.Effort(:,c,2)=GameResults.Effort;
        
        Results.MarginalProfits(:,c,2) = ITQGameOutcome.FinalMarginalProfits;
        
        Results.costs(:,c,2) = ITQGameOutcome.costs(:,end);
        
        
        %Calculate results with grand ITQ
        GrandITQFunction=@(Target)SetGrandQuota(Target,1,UnfishedPop,'EQ'); %pass additional parameters
        
        Options=optimset('MaxFunEvals',1000,'Display','notify','Algorithm','active-set'); %set optimization options
        
        SetGrandQuota(0.0077, 1, UnfishedPop, 'EQ')
        
        [OptimalU,Profits,exitflag,output]=fmincon(GrandITQFunction,Fish.Umsy ,[],[],[],[],.001,5*Fish.Umsy ,[],Options); %optimize effort to maximize turf T profits
        
        old_profits = Profits;
        opt_diff = 100;
        while opt_diff > opt_tol
            'tuning, please wait'
            [OptimalU,Profits,exitflag,output]=fmincon(GrandITQFunction,lognrnd(0,.05).*OptimalU,[],[],[],[],.001,20*Fish.Umsy,[],Options); %optimize effort to maximize turf T profits
            
            opt_diff = abs((Profits ./ old_profits) - 1)
            old_profits = Profits;
            
        end
        
        GameResults=TurfGame([OptimalU,OptimalU/2],1,1,UnfishedPop,'EQ',OptimalU); %Optimize effort with internal ITQ
        
        GrandITQOutcome=GrowPopulation(UnfishedPop,OptimalU,'EQ',1,1,'Yes',[FigureFolder 'Dispersal is' num2str(FishMovement(d)) 'Het is' num2str(TurfDifference(h))  'Grand ITQ Game Outcome  ']);
        %         thetas = linspace(0,1,100);
        %
        %         test_theta=@(Target)check_theta(Target,UnfishedPop,'EQ'); %pass additional parameters
        %
        %         [opted,Profits,exitflag,output]=fmincon(test_theta,[.07*Fish.Umsy .1],[],[],[],[],[.001 0],[20*Fish.Umsy 1],[],Options); %optimize effort to maximize turf T profits
        %
        %
        %         huh=GrowPopulation(UnfishedPop,opted(1),'EQ',1,2,'No',[FigureFolder 'Dispersal is' num2str(FishMovement(d)) 'Het is' num2str(TurfDifference(h))  'Grand ITQ Game Outcome  '],opted(2));
        %
        %         for n = 1:length(thetas)
        %                 GrandITQOutcome=GrowPopulation(UnfishedPop,OptimalU,'EQ',1,2,'No',[FigureFolder 'Dispersal is' num2str(FishMovement(d)) 'Het is' num2str(TurfDifference(h))  'Grand ITQ Game Outcome  '],thetas(n));
        %
        %         test_marg(n, :) = (GrandITQOutcome.FinalProfits);
        %
        %          opted=GrowPopulation(UnfishedPop,OptimalU,'EQ',1,2,'No',[FigureFolder 'Dispersal is' num2str(FishMovement(d)) 'Het is' num2str(TurfDifference(h))  'Grand ITQ Game Outcome  '],0.3897);
        %
        %         end
        %
        %         test_marg2 = max(0,test_marg(2:end,:)) - max(0,test_marg(1:(end - 1),:))
        %
        %         figure
        %         plot(thetas,max(0,sum(test_marg,2)))
        
        Results.Biomass(:,c,3)=GrandITQOutcome.Final;
        
        Results.Profits(:,c,3)=GrandITQOutcome.FinalProfits;
        
        Results.Effort(:,c,3)=GrandITQOutcome.Effort(:,end);
        
        Results.MarginalProfits(:,c,3) = GrandITQOutcome.FinalMarginalProfits;
        
        Results.costs(:,c,3) = GrandITQOutcome.costs(:,end);
        
        
        UTraded=(GameResults.Effort'-GrandITQOutcome.Effort(:,end)); %Trade value calculated as the marginal price
        
        EffortBought=UTraded(UTraded>0)/Turf.q(UTraded>0)';
        
        if isempty(EffortBought)==1
            EffortBought=0 ;
        end
        %        Results.TradeValue(:,c)=EffortBought.*Turf.QuotaPrice.*sign(UTraded);
        
        % Calculate results with Internal ITQ in one, derby in another
        
        GameResults=TurfGame([Umsy,Umsy],1,[1,0],UnfishedPop,'EQ',0); %Optimize effort with internal ITQ
        
        DistEffort=DistributeFleet([GameResults.Effort],Turf.TurfNums); %Distribute fishing effort
        
        ITQ_Derby_GameOutcome= GrowPopulation(UnfishedPop,DistEffort,'EQ',[1,0],0,'Yes',[FigureFolder 'Dispersal is' num2str((d)) 'Het is' num2str(TurfDifference(h))  'With ITQ Game Outcome  ']);
        
        Results.Biomass(:,c,4)=ITQ_Derby_GameOutcome.Final;
        
        Results.Profits(:,c,4)=ITQ_Derby_GameOutcome.FinalProfits;
        
        Results.Effort(:,c,4)=GameResults.Effort;
        
        Results.MarginalProfits(:,c,4) = ITQ_Derby_GameOutcome.FinalMarginalProfits;
        
        Results.costs(:,c,4) = ITQ_Derby_GameOutcome.costs(:,end);
        
        
        %Calculate Social Planner Outcome
        
        OmniPlannerFunction=@(Target)OmniPlanner(Target,1,'EQ',UnfishedPop);
        
        Options=optimset('MaxFunEvals',1000,'Display','notify','Algorithm','active-set'); %set optimization options
        
        [OptimalAllocation,Profits,exitflag,output]=fmincon(OmniPlannerFunction,[Fish.Umsy,Fish.Umsy],[],[],[],[],[0,0],[10*Fish.Umsy, 10*Fish.Umsy],[],Options); %optimize effort to maximize turf T profits
        
        old_profits = Profits;
        
        opt_diff = 100;
        opt_tol = 0.01;
        while opt_diff > opt_tol
            [OptimalAllocation,Profits,exitflag,output]=fmincon(OmniPlannerFunction,lognrnd(0,.05).*OptimalAllocation,[],[],[],[],[0,0],[10*Fish.Umsy, 10*Fish.Umsy],[],Options); %optimize effort to maximize turf T profits
            'tuning, please wait'
            
            opt_diff = abs((Profits ./ old_profits) - 1);
            old_profits = Profits;
            
        end
        
        %         for k = 1:10
        %
        %         [OptimalAllocation,Profits,exitflag,output]=fmincon(OmniPlannerFunction,[Fish.Umsy,Fish.Umsy],[],[],[],[],[0,0],[10*Fish.Umsy, 10*Fish.Umsy],[],Options); %optimize effort to maximize turf T profits
        %         end
        
        %         deaths = linspace(0,0.4,10);
        %
        %         thetas = linspace(0,1,20);
        %
        %         for m  = 1:length(deaths)
        %
        %             for o = 1:length(thetas)
        %
        %                         arg=GrowPopulation(UnfishedPop,[deaths(m)],'EQ',1,2,'No','blah',thetas(o))
        % %                         arg=GrowPopulation(UnfishedPop,[thetas(m) thetas(o)],'EQ',1,0,'Yes',[FigureFolder 'Dispersal is' num2str((d)) 'Het is' num2str(TurfDifference(h))  'Omni Planner Outcome  ']);
        %
        %                 damnit(m,o) = max(0,sum(arg.FinalProfits))
        %
        %             end
        %         end
        %
        %         surfc(thetas, deaths, damnit)
        
        OmniPlannerOutcome=GrowPopulation(UnfishedPop,OptimalAllocation,'EQ',1,0,'Yes',[FigureFolder 'Dispersal is' num2str((d)) 'Het is' num2str(TurfDifference(h))  'Omni Planner Outcome  ']);
        
        Results.Biomass(:,c,5)=OmniPlannerOutcome.Final;
        
        Results.Profits(:,c,5)=OmniPlannerOutcome.FinalProfits;
        
        Results.Effort(:,c,5)=OmniPlannerOutcome.Effort(:,end);
        
        Results.MarginalProfits(:,c,5) = OmniPlannerOutcome.FinalMarginalProfits;
        
        Results.costs(:,c,5) = OmniPlannerOutcome.costs(:,end);
        
        
    end
end

GameNames = {'No ITQ','Internal ITQ','Inter-TURF ITQ','ITQ v Derby','Omni'};

Trading=Results.Effort(:,:,3)-Results.Effort(:,:,2);

Results.TotalProfits=sum(Results.Profits,1); %Calculate system wide profits

Results.TotalEffort=sum(Results.Effort,1); %Calculate system wide profits

Results.TotalBiomass=sum(Results.Biomass,1); %Calculate system wide profits

RawResults=Results;

Results.TotalProfits=100.*Results.TotalProfits(:,:,1:(length(GameNames) - 1))./repmat(Results.TotalProfits(:,:,end),[1,1,length(GameNames) - 1]);

Results.TotalEffort=100.*Results.TotalEffort(:,:,1:(length(GameNames) - 1))./repmat(Results.TotalEffort(:,:,end),[1,1,length(GameNames) - 1]);

Results.TotalBiomass=100.*Results.TotalBiomass(:,:,1:(length(GameNames) - 1))./repmat(Results.TotalBiomass(:,:,end),[1,1,length(GameNames) - 1]);


Results.Biomass(Results.Biomass<0)=0;

Results.Effort(Results.Effort<0)=0;

Results.Profits(Results.Profits<0)=0;

OmniResults.Biomass= repmat(Results.Biomass(:,:,end),[1,1,(length(GameNames) - 1)]);

OmniResults.Profits= repmat(Results.Profits(:,:,end),[1,1,(length(GameNames) - 1)]);

OmniResults.Effort= repmat(Results.Effort(:,:,end),[1,1,(length(GameNames) - 1)]);


Results.Biomass(:,:,1:(length(GameNames) - 1))= 100.*((Results.Biomass(:,:,1:(length(GameNames) - 1)))./OmniResults.Biomass);
%What is happeinng here, relative values don't look right t all
Results.Profits(:,:,1:(length(GameNames) - 1))= 100.*min(2,((Results.Profits(:,:,1:(length(GameNames) - 1)))./OmniResults.Profits));

Results.Effort(:,:,1:(length(GameNames) - 1))= 100.*min(2,((Results.Effort(:,:,1:(length(GameNames) - 1)))./OmniResults.Effort));

Results.Effort(isinf(Results.Effort))=2;
Results.Effort(isnan(Results.Effort))=0;

GameNames={'No ITQ','Internal ITQ','Grand ITQ','ITQ v Derby'};

% SceneNames={'No Movement - Identical TURFs','No Movement - Different TURFs','High Movement - Identical TURFs','High Movement - Different TURFs','SS1 - Identical TURFs','SS1 - Different TURFs','SS2 - Identical TURFs','SS2 - Different TURFs'};

SceneNames={'M0 - Identical TURFs','M0 - Different TURFs','M1 - Identical TURFs','M1 - Different TURFs','M2 - Identical TURFs','M2 - Different TURFs','M3 - Identical TURFs','M3 - Different TURFs'};

Scenes= length(GameNames);

save([RawFolder,'All Results.mat'])

Biomass = RawResults.Biomass;
Profits = RawResults.Profits;
Effort = RawResults.Effort;
MarginalProfits = RawResults.MarginalProfits;
TotalEffort = RawResults.TotalEffort;
TotalBiomass = RawResults.TotalBiomass;
TotalProfits = RawResults.TotalProfits;
costs = RawResults.costs
save([RawFolder,'raw qTURF Results.mat'],'Biomass','Profits','Effort', 'TotalProfits','MarginalProfits','costs')

Biomass = Results.Biomass;
Profits = Results.Profits;
Effort = Results.Effort;
TotalEffort = Results.TotalEffort;
TotalBiomass = Results.TotalBiomass;
TotalProfits = Results.TotalProfits;


save([RawFolder,'qTURF Results.mat'],'Biomass','Profits','Effort', 'TotalProfits')

