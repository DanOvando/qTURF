%% qTURF ControlFile
% This file contains the control parameters used in qTURF
%% Population Parameters

System.NumPatches=2;

System.AdjustK=0;

System.HabQuality=ones(1,System.NumPatches);

System.Wrap=1;

System.PopTolerance=1e-4;

System.ITQCosts=.05; %Percent of revenus lost to ITQ implementation

System.cr_ratio_derby = 1;

System.cr_ratio = 0.75;
Fish.r=.2;

Fish.K=[100,100];

Fish.Umsy=(Fish.r/2);

Fish.Movement=0; %Some number from 0 to numpatches

Fish.Dispersal=fn.DispersalKernel('Simple',[0,0]);
%% Fleet Parameters

Turf.NumTurfs=2;

Turf.q=[0.01,.01]; %Catcheability

Turf.TurfNums=1:Turf.NumTurfs;

Turf.TurfLocations= [1,2];


Turf.FleetDiversity= [0.1,.4];

Turf.NumVessels=[3,3];

TurfDim=NaN(Turf.NumTurfs,max(Turf.NumVessels));

for t=1:Turf.NumTurfs
    Turf.PatchesPerTurf(t)=sum(Turf.TurfLocations==t);
    TurfDim(t,1:Turf.NumVessels(t))=1;
end

Turf.FleetSkill= lognrnd(-5.*TurfDim,repmat(Turf.FleetDiversity',1,size(TurfDim,2)));

Turf.TotalSkill=nansum(1./Turf.FleetSkill,2);

Turf.NetCostSlope=nan(size(Turf.NumTurfs));

Turf.Price=[2000];

Turf.MaxEffort= Turf.Price.* nansum((1./Turf.FleetSkill),2);

% Turf.Alpha= [1000,4000 ; 250,1000]; %Structure is row is TURF, column is no ITQ, ITQ

Turf.Alpha= [10,10;10,10]; %Structure is row is TURF, column is no ITQ, ITQ

Turf.Beta= [0.00001,0.00001 ; 0.00001,0.00001]; %Structure is row is TURF, column is no ITQ, ITQ



Turf.DiscountRate=[0,0];

% Turf.Beta= [0,0 ; 0,0]; %Structure is row is TURF, column is no ITQ, ITQ

figure
for t=1:Turf.NumTurfs
    
    Where=Turf.TurfLocations==t;
    
    Turf.TotalSkillByPatch(Where)=Turf.TotalSkill(t);
    
    Turf.NetCostSlope(t)=1./nansum(1./Turf.FleetSkill(t,:));
    
    EffortVector= repmat(linspace(0,1.1*Turf.MaxEffort(t),100),size(TurfDim,2),1);
    
    
    subplot(Turf.NumTurfs,1,t)
    hold on
    plot(EffortVector',(repmat(Turf.FleetSkill(t,:)',1,100).*EffortVector)')
    line(EffortVector(1,:),EffortVector(1,:)./nansum(1./Turf.FleetSkill(t,:)),'LineWidth',2)
    line([0,1.1*Turf.MaxEffort(t)],[Turf.Price,Turf.Price])
    xlabel('E')
    ylabel('$')
    legend('Fisher 1','Fisher 2','Fisher 3','Fleet','Price','Location','Best')
    title(['Turf ' num2str(t)])
    hold off
end
print(gcf,'-depsc',strcat(FigureFolder,'Turf Fleets.eps'))
close
