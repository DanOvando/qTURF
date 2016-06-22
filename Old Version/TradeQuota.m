function Output= TradeQuota(TotalEffort,IsITQ,pop, cr_ratio)
%% Function to trade quota in the grand ITQ
global Fish Turf System
%  Pop=UnfishedPop;
%    TotalEffort=(2*Umsy)./Turf.q;
%    IsITQ=1;

TurfPop=nan(1,Turf.NumTurfs);

quota_pool = sum(pop) .* TotalEffort;

for t=1:Turf.NumTurfs %loop over TURFs
    TurfPop(t)=sum(pop(Turf.TurfLocations==t));
    %Pi(t)=Alpha2(t,Col).*Turf.q(t).*TurfPop(t); %Calculate marginal revenue in TURF t
    %     Pi(t)=Turf.Alpha(t,Col).*TurfPop(t);
end

[Theta, arg, flag]=fzero(@(Theta) ZeroFun(Theta,Turf,quota_pool,Fish,TurfPop, IsITQ, System, cr_ratio),.1); %Find trading that results in equal marginal profits
%[Theta, arg, flag]=fmincon(@(Theta) ZeroFun(Theta,Turf,quota_pool,Fish,TurfPop, IsITQ, System),.1,[],[],[],[],.0,1); %Find trading that results in equal marginal profits


turf_effort = ([Theta, 1 - Theta] .* quota_pool) ./ (Turf.q .* TurfPop);


if IsITQ==0 %Non-ITQ profits function, with racing
    Col=1;
    
    Alpha2=Turf.Alpha;
    
    %         Beta=Alpha2(WhichTurfs,Col)./(2.*Effort);%Increase costs to dissipiate marginal profits
    %   Beta = Turf.Beta(:,Col);

    Beta = (Turf.Alpha(:,Col).* Turf.q.* pop)./(2.*turf_effort);%Increase costs to dissipiate marginal profits
    %Beta=Alpha2(:,Col)./(2.*turf_effort);%Increase costs to dissipiate marginal profits
    
    Beta(isinf(Beta))=0;
else %ITQ profit function
    
    Col=2;
    
    Alpha2=(Turf.Alpha(:,Col).*(1-System.ITQCosts))';
        
    Beta = Turf.Beta(:,Col);
        
     Beta= (Alpha2(:,Col)'.*Turf.q)./2; %Costs scale to catcheability
    
    Beta(isinf(Beta))=0;
    
       
end


Pi = Alpha2(:,Col)'.*Turf.q.*TurfPop; %Calculate marginal revenue in TURF t

Marg = [(Pi(1)-(2.*Beta(1).*turf_effort(1))) (Pi(2)-(2.*Beta(2).*turf_effort(2)))];


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
turf_effort = ([Theta, 1 - Theta] .* quota_pool) ./ (Turf.q .* pop);

% Output=DistributeFleet([TotalEffort.*Theta,TotalEffort.*(1-Theta)],1:2);
Output=DistributeFleet(turf_effort,1:2);


end
