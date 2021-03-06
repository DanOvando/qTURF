function [Profits, MarginalProfits,revenue, costs]=TurfProfits(Quota,Effort,Pop,WhichTurfs,IsITQ,Method, cr_ratio) %Calculate TURF profits
%% Calculate profits in the selected TURF(s)
global Fish Turf System

% Quota: The quota caught vbbv
% Effort: the effort exerted
% WhichTurfs: TURFs you are calculating profits forb bnm
% Method: Marks how profits are distributed, simple is the only option so far

Biomass=Pop;
if strcmp(Method,'Simple')
    
    if IsITQ==0 %Non-ITQ profits function, with racing
        Col=1;
        
        Alpha2=Turf.Alpha;
        
                % Beta=Alpha2(WhichTurfs,Col)./(2.*Effort);%Increase costs to dissipiate marginal profits
       %    Beta = Turf.Beta(:,Col);

       Beta = (Alpha2(WhichTurfs,Col).* Turf.q(WhichTurfs).*Biomass(WhichTurfs))./(2.*Effort);%Increase costs to dissipiate marginal profits
        
        Beta(isinf(Beta))=0;
    else %ITQ profit function
        
        Col=2;
        
        Alpha2=Turf.Alpha.*(1-System.ITQCosts);
        
        Beta = Turf.Beta(:,2);
        
       %Beta= (Alpha2(:,Col)'.*Turf.q)./2; %Costs scale to catcheability
        
    end
    
    PatchProfits=nan(length(WhichTurfs),System.NumPatches); %Storage
    
    MarginalProfits=nan(length(WhichTurfs),System.NumPatches); %Storage
    
    revenue=nan(length(WhichTurfs),System.NumPatches); %Storage
    
    costs=nan(length(WhichTurfs),System.NumPatches); %Storage
    
    
    ['Profit effort is ' num2str(Effort)];
    
    ['Biomass is ' num2str(Biomass(1)) ' ' num2str(Biomass(2))];
    
    for t=1:length(WhichTurfs) %Loop over TURFs
        
        Where=Turf.TurfLocations==WhichTurfs(t);
        
        PatchProfits(t,Where) = Alpha2(WhichTurfs(t),Col).*Quota(Where)-Beta(WhichTurfs(t)).*Effort(Where).^2; %Calculate profits for TURF t
        
        MarginalProfits(t)=Alpha2(WhichTurfs(t),Col).*Turf.q(WhichTurfs(t)).*Biomass(WhichTurfs(t))- 2.*Beta(WhichTurfs(t)).*Effort(Where);
        
        revenue(t) = Alpha2(WhichTurfs(t),Col).*Quota(Where);
        
        costs(t) = Beta(WhichTurfs(t)).*Effort(Where).^2;
        
        %MarginalProfits(t) = Alpha2(WhichTurfs(t),Col).*Turf.q(WhichTurfs(t)).*Fish.K(WhichTurfs(t)).*(1 - 2.*((Turf.q(WhichTurfs(t)).*Effort(Where))./Fish.r)) - 2.*Beta(WhichTurfs(t)).*Effort(Where);
        
   
    end
    ['TURF ',num2str(WhichTurfs), ' Profit marginal profits are ' num2str(MarginalProfits(1))];
    
    Profits=nansum(PatchProfits,2);
    
    MarginalProfits = nansum(MarginalProfits, 2);
    
    revenue = nansum(revenue,2);
    
    costs = nansum(costs,2);
    
end

end
