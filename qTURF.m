%% qTURF
% This model runs the 2013 ECO-A project qTURF, evaluating the
% interactions between TURFs and ITQs

%Created by Dan Ovando and Sarah Poon
%% Setup Workspace
clear all
close all

global Fish Turf System

FunctionHolder; % Load in functions

fn=FunctionHolder(); %Allow functions to be called

RunName='WTF'; %Set name of folder to store results

mkdir('Results');

BaseFolder=strcat('Results/',RunName,'/'); %Folder for all results

FigureFolder=strcat('Results/',RunName,'/Figures/'); %Folder for figures

RawFolder=strcat('Results/',RunName,'/RawResults/'); %Folder for numeric results

mkdir(FigureFolder)

mkdir(RawFolder)

ControlFile; %Load in controlfile

save(strcat(BaseFolder,'WorkSpace.mat'),'Turf','Fish','System') %save workspace for future reference


%% Set up Initial Conditions

EQPop=fn.GrowPopulation(10,0,'EQ',0,0,'Yes',FigureFolder); %Create Unfished Population

UnfishedPop=EQPop.Final;

Umsy=(Fish.r/2); %Calculate Umsy

% TestFishing=fn.GrowPopulation(UnfishedPop,Umsy,'EQ',0,0,'Yes',strcat(FigureFolder,'Test'));

uSeq=linspace(0,2*Umsy,100);

for u=1:length(uSeq) %Test a variety of fishing pressues to evaluate MSY/MEY
    
    Temp=fn.GrowPopulation(UnfishedPop,[uSeq(u),uSeq(u)],'EQ',0,0,'No','eh');
    
    %     Temp=fn.OptEffort(uSeq(u),1,[0,Umsy],0,UnfishedPop,'EQ','No',strcat(FigureFolder,'OptTest'));
    
    Temp=Temp.FinalProfits;
    
    Profits(u,:)=Temp;
end


figure %Plot MEY
plot(uSeq,Profits)
legend('Turf 1','Turf 2')


% FishMovement=[0,System.NumPatches]; %Set vector of fish movement scales to be tested

FishMovement=[0,0 ; .5,.5 ; .25, 0.1 ; 0.1,.25]; %Set vector of fish movement scales to be tested


TurfDifference=[1,4]; %Set vector of TURF fishing skill hetergeneity to be tested

%Create result storage space
Results.Biomass=nan(2,size(FishMovement,1)*length(TurfDifference),4);
Results.Profits=nan(2,size(FishMovement,1)*length(TurfDifference),4);
Results.Effort=nan(2,size(FishMovement,1)*length(TurfDifference),4);
Results.TradeValue=nan(2,size(FishMovement,1)*length(TurfDifference));

c=0; %counter
BaseK=Fish.K;
BasePop=UnfishedPop;

StoreK=nan(2,size(FishMovement,1).*length(TurfDifference));
for d=1:size(FishMovement,1) %loop over movement experiment
    
    %     Fish.Movement=FishMovement(d); %Set fish movement to selected value
    
    Fish.Dispersal=fn.DispersalKernel('Simple',FishMovement(d,:)); %Calculate dispersal kernel
    
    UnfishedPop=BasePop;
    
    Fish.K=BaseK;
    
    NewPop=fn.GrowPopulation(UnfishedPop,0,'EQ',0,0,'Yes',[FigureFolder 'doh Dispersal is' num2str(d)]);
    
    UnfishedPop=NewPop.Final;
    
    if System.AdjustK==1 && d>2
        
        Fish.K=NewPop.Final';
        
        Delta=100;
        
        while Delta>0.001
            
            OldPop=NewPop.Final;
            
            UnfishedPop=NewPop.Final;
            
            NewPop=fn.GrowPopulation(UnfishedPop,0,'EQ',0,0,'No',[FigureFolder 'doh 2 Dispersal is' num2str(d)]);
            
            Fish.K=NewPop.Final';
            
            UnfishedPop=NewPop.Final;
            
            Delta=abs(sum(UnfishedPop)-sum(OldPop));
        end
        
    end
    
    
    
    for h=1:length(TurfDifference) %Loop over heterogeniety in TURFs
        
        c=c+1; %update counter
        
        StoreK(:,c)=Fish.K';
        
        Turf.q(1)=Turf.q(2).*TurfDifference(h); %Change TURF ability
        
        %Calculate results without ITQ
        
        GameResults=fn.TurfGame([Umsy,Umsy],4,0,UnfishedPop,'EQ',0); %Optimize effort without internal ITQs
        
        DistEffort=fn.DistributeFleet([GameResults.Effort],Turf.TurfNums); %Distribute fishing effort
        
        %Calculate non-ITQ results
        NonITQGameOutcome= fn.GrowPopulation(UnfishedPop,DistEffort,'EQ',0,0,'Yes',[FigureFolder 'Dispersal is' num2str(d) 'Het is' num2str(TurfDifference(h))  'No ITQ Game Outcome  ']);
        
        Results.Biomass(:,c,1)=NonITQGameOutcome.Final;
        
        Results.Profits(:,c,1)=NonITQGameOutcome.FinalProfits;
        
        Results.Effort(:,c,1)=GameResults.Effort;
        
        %Calculate results with internal ITQ
        
        GameResults=fn.TurfGame([Umsy,Umsy],1,1,UnfishedPop,'EQ',0); %Optimize effort with internal ITQ
        
        DistEffort=fn.DistributeFleet([GameResults.Effort],Turf.TurfNums); %Distribute fishing effort
        
        %Calculate results with internal ITQ
        
        ITQGameOutcome= fn.GrowPopulation(UnfishedPop,DistEffort,'EQ',1,0,'Yes',[FigureFolder 'Dispersal is' num2str((d)) 'Het is' num2str(TurfDifference(h))  'With ITQ Game Outcome  ']);
        
        Results.Biomass(:,c,2)=ITQGameOutcome.Final;
        
        Results.Profits(:,c,2)=ITQGameOutcome.FinalProfits;
        
        Results.Effort(:,c,2)=GameResults.Effort;
        
        %Calculate results with grand ITQ
        
        GrandITQFunction=@(Target)fn.SetGrandQuota(Target,1,UnfishedPop,'EQ'); %pass additional parameters
        
        Options=optimset('MaxFunEvals',1000,'Display','notify','Algorithm','active-set'); %set optimization options
        
        [OptimalU,Profits,exitflag,output]=fmincon(GrandITQFunction,3*Fish.Umsy,[],[],[],[],.001,4*Fish.Umsy,[],Options); %optimize effort to maximize turf T profits
        
        GameResults=fn.TurfGame([OptimalU,OptimalU/2],1,1,UnfishedPop,'EQ',OptimalU); %Optimize effort with internal ITQ
        
        
        %Calculate results with grand ITQ
        GrandITQOutcome=fn.GrowPopulation(UnfishedPop,OptimalU,'EQ',1,1,'Yes',[FigureFolder 'Dispersal is' num2str(FishMovement(d)) 'Het is' num2str(TurfDifference(h))  'Grand ITQ Game Outcome  ']);
        
        Results.Biomass(:,c,3)=GrandITQOutcome.Final;
        
        Results.Profits(:,c,3)=GrandITQOutcome.FinalProfits;
        
        Results.Effort(:,c,3)=GrandITQOutcome.Effort(:,end);
        
        UTraded=(GameResults.Effort'-GrandITQOutcome.Effort(:,end)); %Trade value calculated as the marginal price
        
        EffortBought=UTraded(UTraded>0)/Turf.q(UTraded>0)';
        
        if isempty(EffortBought)==1
            EffortBought=0 ;
        end
        Results.TradeValue(:,c)=EffortBought.*Turf.QuotaPrice.*sign(UTraded);
        
        %Calculate Social Planner Outcome
        
        OmniPlannerFunction=@(Target)fn.OmniPlanner(Target,1,'EQ',UnfishedPop);
        
        Options=optimset('MaxFunEvals',1000,'Display','notify','Algorithm','active-set'); %set optimization options
        
        [OptimalAllocation,Profits,exitflag,output]=fmincon(OmniPlannerFunction,[Fish.Umsy,Fish.Umsy],[],[],[],[],.001,10*Fish.Umsy,[],Options); %optimize effort to maximize turf T profits
        
        OmniPlannerOutcome=fn.GrowPopulation(UnfishedPop,OptimalAllocation,'EQ',1,0,'Yes',[FigureFolder 'Dispersal is' num2str((d)) 'Het is' num2str(TurfDifference(h))  'Omni Planner Outcome  ']);
        
        Results.Biomass(:,c,4)=OmniPlannerOutcome.Final;
        
        Results.Profits(:,c,4)=OmniPlannerOutcome.FinalProfits;
        
        Results.Effort(:,c,4)=OmniPlannerOutcome.Effort(:,end);
    end
end


%Plot Figures
% Results=RawResults

GameNames={'No ITQ','Internal ITQ','Grand ITQ','Omni Planner'};

SceneNames={'M0 - Identical TURFs','M0 - Different TURFs','M1 - Identical TURFs','M1 - Different TURFs','M2 - Identical TURFs','M2 - Different TURFs','M3 - Identical TURFs','M3 - Different TURFs'};

Scenes= length(GameNames);

figure %Make Biomass Plots
set(gcf, 'PaperUnits', 'inches');
set(gcf, 'PaperSize', [10 10]);

for s=1:Scenes
    
    subplot(Scenes+1,1,s)
    bar((Results.Biomass(:,:,s)./(StoreK/2))')
    ylim([0,2])
    if s==Scenes
        hx=get(gca,'XLabel');
        set(hx,'Units','data');
        pos = get(hx,'Position');
        y = pos(2);
        % Place the new labels
        for i = 1:length(SceneNames)
            t(i) = text(i,y,SceneNames(i));
        end
        set(t,'Rotation',35,'HorizontalAlignment','right')
    end
    if s==2
        ylabel('Final B/Bmsy')
    end
    title(GameNames(s))
    colormap summer
end

print(gcf,'-depsc','-loose',[FigureFolder 'Biomass Experiment Results.eps'])
close

figure %Make Profit Plots

for s=1:Scenes
    
    subplot(Scenes+1,1,s)
    bar(Results.Profits(:,:,s)')
    ylim([0,1.1.*max(max(max(Results.Profits(:,:,:))))]);
    if s==Scenes
        hx=get(gca,'XLabel');
        set(hx,'Units','data');
        pos = get(hx,'Position');
        y = pos(2);
        % Place the new labels
        for i = 1:length(SceneNames)
            t(i) = text(i,y,SceneNames(i));
        end
        set(t,'Rotation',35,'HorizontalAlignment','right')
    end
    if s==2
        ylabel('Final Profits')
    end
    
    title(GameNames(s))
    colormap summer
end

print(gcf,'-depsc',[FigureFolder 'Profits Experiment Results.eps'])
close

figure %Make Effort Plots

for s=1:Scenes
    
    subplot(Scenes+1,1,s)
    
    bar(Results.Effort(:,:,s)')
    ylim([0 2*Umsy])
    if s==Scenes
        hx=get(gca,'XLabel');
        set(hx,'Units','data');
        pos = get(hx,'Position');
        y = pos(2);
        % Place the new labels
        for i = 1:length(SceneNames)
            t(i) = text(i,y,SceneNames(i));
        end
        set(t,'Rotation',35,'HorizontalAlignment','right')
    end
    if s==2
        ylabel('Effort')
    end
    
    title(GameNames(s))
    colormap summer
    
end

print(gcf,'-depsc',[FigureFolder 'Effort Experiment Results.eps'])
close


%Analysis of Equity in Profits

Equity=abs(reshape(Results.Profits(1,:,:)-Results.Profits(2,:,:),8,4));

figure %Make Total Realtive Effort Plots

for s=1:Scenes
    
    subplot(Scenes,1,s)
    
    HetData=[Equity([1,3,5,7],s) Equity([2,4,6,8],s)];
    
    bar(mean(HetData,1))
    
    if s==Scenes
        
        set(gca,'XTickLabel',{'Identical TURFs','Different TURFs'})
    end
    ylim([0,max(max(mean(Equity,1)))]);
    if(s==2)
        ylabel('Absolute Difference in Profits')
    end
    title(GameNames(s))
    colormap summer
    hold off
end


print(gcf,'-depsc',[FigureFolder 'Subset Equity Experiment Results.eps'])
close


figure
boxplot(abs(reshape(Results.Profits(1,:,:)-Results.Profits(2,:,:),8,4)),'boxstyle','outline','labels',{'No ITQ','Internal ITQ','Grand ITQ','Optimal'},'notch','off')
ylabel('Absolute Difference in TURF Profits')
print(gcf,'-depsc',[FigureFolder 'Total Equity Experiment Results.eps'])
close


%Calculate trading volume (difference in effort under grand vs. internal
%ITQ)

Trading=Results.Effort(:,:,3)-Results.Effort(:,:,2);


%Plot relative figures


% Results=RawResults;

Results.TotalProfits=sum(Results.Profits,1); %Calculate system wide profits

Results.TotalEffort=sum(Results.Effort,1); %Calculate system wide profits

Results.TotalBiomass=sum(Results.Biomass,1); %Calculate system wide profits

RawResults=Results;

Results.TotalProfits=100.*Results.TotalProfits(:,:,1:3)./repmat(Results.TotalProfits(:,:,4),[1,1,3]);

Results.TotalEffort=100.*Results.TotalEffort(:,:,1:3)./repmat(Results.TotalEffort(:,:,4),[1,1,3]);

Results.TotalBiomass=100.*Results.TotalBiomass(:,:,1:3)./repmat(Results.TotalBiomass(:,:,4),[1,1,3]);



Results.Biomass(Results.Biomass<0)=0;

Results.Effort(Results.Effort<0)=0;

Results.Profits(Results.Profits<0)=0;

OmniResults.Biomass= repmat(Results.Biomass(:,:,4),[1,1,3]);

OmniResults.Profits= repmat(Results.Profits(:,:,4),[1,1,3]);

OmniResults.Effort= repmat(Results.Effort(:,:,4),[1,1,3]);


Results.Biomass(:,:,1:3)= 100.*((Results.Biomass(:,:,1:3))./OmniResults.Biomass);
%What is happeinng here, relative values don't look right t all
Results.Profits(:,:,1:3)= 100.*min(2,((Results.Profits(:,:,1:3))./OmniResults.Profits));

Results.Effort(:,:,1:3)= 100.*min(2,((Results.Effort(:,:,1:3))./OmniResults.Effort));

Results.Effort(isinf(Results.Effort))=2;
Results.Effort(isnan(Results.Effort))=0;

GameNames={'No ITQ','Internal ITQ','Grand ITQ'};

% SceneNames={'No Movement - Identical TURFs','No Movement - Different TURFs','High Movement - Identical TURFs','High Movement - Different TURFs','SS1 - Identical TURFs','SS1 - Different TURFs','SS2 - Identical TURFs','SS2 - Different TURFs'};

SceneNames={'M0 - Identical TURFs','M0 - Different TURFs','M1 - Identical TURFs','M1 - Different TURFs','M2 - Identical TURFs','M2 - Different TURFs','M3 - Identical TURFs','M3 - Different TURFs'};

Scenes= length(GameNames);

figure %Make Relative Biomass Plots
set(gcf, 'PaperUnits', 'inches');
set(gcf, 'PaperSize', [10 10]);

for s=1:Scenes
    
    subplot(Scenes+1,1,s)
    bar((Results.Biomass(:,:,s))')
    hold on
    xlim=get(gca,'xlim');
    plot(xlim,[100,100],'--')
    
    ylim([0,200])
    set(gca,'YTick',[0,100,200],'YTickLabel',{'0%','100%','>=200%'})
    if s==Scenes
        hx=get(gca,'XLabel');
        set(hx,'Units','data');
        pos = get(hx,'Position');
        y = pos(2);
        % Place the new labels
        for i = 1:length(SceneNames)
            t(i) = text(i,y,SceneNames(i));
        end
        set(t,'Rotation',35,'HorizontalAlignment','right')
    end
    if(s==2)
        ylabel('% of Optimal Biomass')
    end
    title(GameNames(s))
    colormap summer
    hold off
end

print(gcf,'-depsc','-loose',[FigureFolder 'Relative Biomass Experiment Results.eps'])
close


figure %Make Relative Total Biomass Plots
set(gcf, 'PaperUnits', 'inches');
set(gcf, 'PaperSize', [10 10]);

for s=1:Scenes
    
    subplot(Scenes+1,1,s)
    bar((Results.TotalBiomass(:,:,s))')
    hold on
    xlim=get(gca,'xlim');
    plot(xlim,[100,100],'--')
    
    ylim([0,200])
    set(gca,'YTick',[0,100,200],'YTickLabel',{'0%','100%','>=200%'})
    if s==Scenes
        hx=get(gca,'XLabel');
        set(hx,'Units','data');
        pos = get(hx,'Position');
        y = pos(2);
        % Place the new labels
        for i = 1:length(SceneNames)
            t(i) = text(i,y,SceneNames(i));
        end
        set(t,'Rotation',35,'HorizontalAlignment','right')
    end
    if(s==2)
        ylabel('% of Optimal Biomass')
    end
    title(GameNames(s))
    colormap summer
    hold off
end

print(gcf,'-depsc','-loose',[FigureFolder 'Relative Total Biomass Experiment Results.eps'])
close




figure %Make Profit Plots

for s=1:Scenes
    
    subplot(Scenes+1,1,s)
    bar(Results.Profits(:,:,s)')
    hold on
    xlim=get(gca,'xlim');
    plot(xlim,[100,100],'--')
    
    ylim([0,ceil(max(max(max(Results.Profits(:,:,1:3)))))]);
    set(gca,'YTickLabel',{'0%','100%','>=200%'})
    if s==Scenes
        hx=get(gca,'XLabel');
        set(hx,'Units','data');
        pos = get(hx,'Position');
        y = pos(2);
        % Place the new labels
        for i = 1:length(SceneNames)
            t(i) = text(i,y,SceneNames(i));
        end
        set(t,'Rotation',35,'HorizontalAlignment','right')
    end
    if(s==2)
        ylabel('% of Optimal Profits')
    end
    title(GameNames(s))
    colormap summer
    hold off
end

print(gcf,'-depsc',[FigureFolder 'Relative Profits Experiment Results.eps'])
close


figure %Make Profit Plots

for s=2
    
    subplot(2,1,1)
    bar(Results.Profits(:,:,s)')
    hold on
    xlim=get(gca,'xlim');
    plot(xlim,[100,100],'--')
    
    ylim([0,200]);
    set(gca,'YTick',[0,100,200],'YTickLabel',{'0%','100%','>=200%'})
    if s==2
        hx=get(gca,'XLabel');
        set(hx,'Units','data');
        pos = get(hx,'Position');
        y = pos(2);
        % Place the new labels
        for i = 1:length(SceneNames)
            t(i) = text(i,y,SceneNames(i));
        end
        set(t,'Rotation',35,'HorizontalAlignment','right')
    end
    
    ylabel('% of Optimal Profits')
    
    title(GameNames(s))
    colormap summer
    hold off
end

print(gcf,'-depsc',[FigureFolder 'Internal ITQ Relative Profits Experiment Results.eps'])
close




figure %Make Total Profit Plots

for s=1:Scenes
    
    subplot(Scenes+1,1,s)
    bar(Results.TotalProfits(:,:,s)')
    hold on
    xlim=get(gca,'xlim');
    plot(xlim,[100,100],'--')
    
    ylim([0,110]);
    set(gca,'YTick',[0,50,100],'YTickLabel',{'0%','50%','100%'})
    if s==Scenes
        hx=get(gca,'XLabel');
        set(hx,'Units','data');
        pos = get(hx,'Position');
        y = pos(2);
        % Place the new labels
        for i = 1:length(SceneNames)
            t(i) = text(i,y,SceneNames(i));
        end
        set(t,'Rotation',35,'HorizontalAlignment','right')
    end
    if(s==2)
        ylabel('% of Optimal Total Profits')
    end
    title(GameNames(s))
    colormap summer
    hold off
end

print(gcf,'-depsc',[FigureFolder 'Relative Total Profits Experiment Results.eps'])
close


figure %Make Effort Plots

for s=1:Scenes
    
    subplot(Scenes+1,1,s)
    bar(Results.Effort(:,:,s)')
    ylim([0,200]);
    set(gca,'YTick',[0,100,200],'YTickLabel',{'0%','100%','>=200%'})
    hold on
    xlim=get(gca,'xlim');
    plot(xlim,[100,100],'--')
    if s==Scenes
        hx=get(gca,'XLabel');
        set(hx,'Units','data');
        pos = get(hx,'Position');
        y = pos(2);
        % Place the new labels
        for i = 1:length(SceneNames)
            t(i) = text(i,y,SceneNames(i));
        end
        set(t,'Rotation',35,'HorizontalAlignment','right')
    end
    if(s==2)
        ylabel('% of Optimal Effort')
    end
    title(GameNames(s))
    colormap summer
    hold off
end

print(gcf,'-depsc',[FigureFolder 'Relative Effort Experiment Results.eps'])
close


figure %Make Internal ITQ Effort Plots

for s=2
    
    subplot(2,1,1)
    bar(Results.Effort(:,:,s)')
    %     ylim([0,2]);
    set(gca,'YTick',[0,100,200],'YTickLabel',{'0%','100%','>=200%'})
    hold on
    xlim=get(gca,'xlim');
    plot(xlim,[100,100],'--')
    if s==2
        hx=get(gca,'XLabel');
        set(hx,'Units','data');
        pos = get(hx,'Position');
        y = pos(2);
        % Place the new labels
        for i = 1:length(SceneNames)
            t(i) = text(i,y,SceneNames(i));
        end
        set(t,'Rotation',35,'HorizontalAlignment','right')
    end
    
    ylabel('% of Optimal Effort')
    
    title(GameNames(s))
    colormap summer
    hold off
end

print(gcf,'-depsc',[FigureFolder 'Internal ITQ Relative Effort Experiment Results.eps'])
close


figure %Make Grand ITQ Effort Plots

for s=3
    
    subplot(2,1,1)
    bar(Results.Effort(:,:,s)')
    %     ylim([0,2]);
    set(gca,'YTick',[0,100,200],'YTickLabel',{'0%','100%','>=200%'})
    hold on
    xlim=get(gca,'xlim');
    plot(xlim,[100,100],'--')
    if s==3
        hx=get(gca,'XLabel');
        set(hx,'Units','data');
        pos = get(hx,'Position');
        y = pos(2);
        % Place the new labels
        for i = 1:length(SceneNames)
            t(i) = text(i,y,SceneNames(i));
        end
        set(t,'Rotation',35,'HorizontalAlignment','right')
    end
    
    ylabel('% of Optimal Effort')
    
    title(GameNames(s))
    colormap summer
    hold off
end

print(gcf,'-depsc',[FigureFolder 'Grand ITQ Relative Effort Experiment Results.eps'])
close



figure %Make Total Realtive Effort Plots

for s=1:Scenes
    
    subplot(Scenes+1,1,s)
    bar(Results.TotalEffort(:,:,s)')
    %     ylim([0,2]);
    set(gca,'YTickLabel',{'0','100%','>=200%'})
    hold on
    xlim=get(gca,'xlim');
    plot(xlim,[100,100],'--')
    if s==Scenes
        hx=get(gca,'XLabel');
        set(hx,'Units','data');
        pos = get(hx,'Position');
        y = pos(2);
        % Place the new labels
        for i = 1:length(SceneNames)
            t(i) = text(i,y,SceneNames(i));
        end
        set(t,'Rotation',35,'HorizontalAlignment','right')
    end
    if(s==2) 
        ylabel('% of Optimal Effort')
    end
    title(GameNames(s))
    colormap summer
    hold off
end


print(gcf,'-depsc',[FigureFolder 'Relative Total Effort Experiment Results.eps'])
close


figure %Make Total Profit Plots
boxplot(reshape((Results.TotalProfits),8,3),'boxstyle','outline','labels',{'No ITQ','Internal ITQ','Grand ITQ'},'notch','off')
ylabel('% of Optimal Profits')
print(gcf,'-depsc',[FigureFolder 'Boxplot of Total Profits by Management.eps'])
close

figure %Make Total Biomass Plots
boxplot(reshape((Results.TotalBiomass),8,3),'boxstyle','outline','labels',{'No ITQ','Internal ITQ','Grand ITQ'},'notch','off')
ylabel('% of Optimal Biomass')
print(gcf,'-depsc',[FigureFolder 'Boxplot of Total Biomass by Management.eps'])
close

figure %Make Total Effort Plots
boxplot(reshape((Results.TotalEffort),8,3),'boxstyle','outline','labels',{'No ITQ','Internal ITQ','Grand ITQ'},'notch','off')
ylabel('% of Optimal Effort')
print(gcf,'-depsc',[FigureFolder 'Boxplot of Total Effort by Management.eps'])
close


save([RawFolder,'All Results.mat'])

Biomass = RawResults.Biomass;
Profits = RawResults.Profits;
Effort = RawResults.Effort;
TotalEffort = RawResults.TotalEffort;
TotalBiomass = RawResults.TotalBiomass;
TotalProfits = RawResults.TotalProfits;

save([RawFolder,'raw qTURF Results.mat'],'Biomass','Profits','Effort', 'TotalProfits')

Biomass = Results.Biomass;
Profits = Results.Profits;
Effort = Results.Effort;
TotalEffort = Results.TotalEffort;
TotalBiomass = Results.TotalBiomass;
TotalProfits = Results.TotalProfits;


save([RawFolder,'qTURF Results.mat'],'Biomass','Profits','Effort', 'TotalProfits')

