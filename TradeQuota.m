function Output= TradeQuota(TotalEffort,IsITQ,Pop)
%% Function to trade quota in the grand ITQ
global Fish Turf System
%  Pop=UnfishedPop;
%    TotalEffort=(2*Umsy)./Turf.q;
%    IsITQ=1;

TurfPop=nan(1,Turf.NumTurfs);

quota_pool = sum(Pop) .* TotalEffort;

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
    
    Alpha2=(Turf.Alpha(:,Col).*(1-System.ITQCosts))';

    Beta= (Alpha2(:,Col)'.*Turf.q)./2;
    
    %         Biomass=Quota./(Turf.q*Effort);
    
    %         Turf.Alpha(WhichTurfs,Col)-(2.*Beta.*Quota)./(Turf.q.*Biomass)
end


for t=1:Turf.NumTurfs %loop over TURFs
    TurfPop(t)=sum(Pop(Turf.TurfLocations==t));
    Pi(t)=Turf.Alpha(t,Col).*Turf.q(t).*TurfPop(t); %Calculate marginal revenue in TURF t
    %     Pi(t)=Turf.Alpha(t,Col).*TurfPop(t);    
end

% Theta=((Pi(2)-Pi(1))/(2*TotalEffort)-Beta(2))./(-Beta(1)-Beta(2));

% Theta=(2.*Beta(2).*Turf.q(1).*TotalEffort + Pi(1).*Turf.q(1)    )



[Theta, arg]=fzero(@(Theta) ZeroFun(Theta,Pi,Beta,Turf,quota_pool,Fish,TurfPop, IsITQ, System),.5); %Find trading that results in equal marginal profits


turf_effort = ([Theta, 1 - Theta] .* quota_pool) ./ (Turf.q .* TurfPop);


Marg = [(Pi(1)-(2.*Beta(1).*turf_effort(1))) (Pi(2)-(2.*Beta(2).*turf_effort(2)))];


['Theta is ' num2str(Theta)];

['Trade Effort is ' ,num2str(((TotalEffort.*Theta))./Turf.q(1)),' ', num2str((TotalEffort.*(1-Theta))./Turf.q(2))];

['Trade biomass is ' num2str(TurfPop(1)),' ',num2str(TurfPop(2))];

% turf1_mps = (Turf.Alpha(1,Col).*Turf.q(1).*TurfPop(1)-(2.*Beta(1).*((TotalEffort.*Theta)))./Turf.q(1))
% 
% turf2_mps = (Turf.Alpha(2,Col).*Turf.q(2).*TurfPop(2)-(2.*Beta(2).*((TotalEffort.*(1 - Theta))))./Turf.q(2))


% turf1_mp = Turf.Alpha(1,Col) .* Turf.q(1).*Fish.K(1).*(1 - 2.*((Turf.q(1) .* Theta .* TotalEffort)/Fish.r)) - 2 .* Beta(1) .* Theta .* TotalEffort;
% 
% turf2_mp = Turf.Alpha(2,Col) .* Turf.q(2).*Fish.K(2).*(1 - 2.*(( Turf.q(2) .* (1-Theta) .* TotalEffort)/Fish.r)) - 2 .* Beta(2) .* (1 - Theta) .* TotalEffort;


% ['Are MPs Equal ' num2str(turf1_mp),' ',...
%     num2str(turf2_mp)];

%   fTheta=fzero(@(Theta) ZeroFun(Theta,Pi,Beta,Turf,TotalEffort,Col),.5);

QuotaPrice=Turf.Alpha(2,Col).*Turf.q(2).*TurfPop(2)-(2.*Beta(2).* turf_effort(2));
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
%Theta;

turf_effort = ([Theta, 1 - Theta] .* quota_pool) ./ (Turf.q .* TurfPop);

% Output=DistributeFleet([TotalEffort.*Theta,TotalEffort.*(1-Theta)],1:2);
Output=DistributeFleet(turf_effort,1:2);


end
