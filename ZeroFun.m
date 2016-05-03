function f=ZeroFun(Theta,Pi,Beta,Turf,quota_pool,Fish,pop, IsITQ, System)


turf_effort = ([Theta, 1 - Theta] .* quota_pool) ./ (Turf.q .* pop);


if IsITQ==0 %Non-ITQ profits function, with racing
    Col=1;
    
    Alpha2=Turf.Alpha;
    
    %         Beta=Alpha2(WhichTurfs,Col)./(2.*Effort);%Increase costs to dissipiate marginal profits
    
    %     Beta = (Turf.Alpha(:,Col).* Turf.q.* pop)./(2.*turf_effort);%Increase costs to dissipiate marginal profits
    Beta=Alpha2(:,Col)./(2.*turf_effort);%Increase costs to dissipiate marginal profits
    
    Beta(isinf(Beta))=0;
else %ITQ profit function
    
    Col=2;
    
    Alpha2=(Turf.Alpha(:,Col).*(1-System.ITQCosts))';
    
    %     Beta = 0.77.* (Alpha2.* Turf.q.* pop)./(2.*turf_effort);%Increase costs to dissipiate marginal profits
    
    Beta= (Alpha2(:,Col)'.*Turf.q)./2; %Costs scale to catcheability
    
    Beta(isinf(Beta))=0;
    
    
    %         Beta= (Alpha2(:,Col)'.*Turf.q)./2; %Costs scale to catcheability
    
end

Pi = Alpha2.*Turf.q.*pop;

%turf_effort .* Turf.q .* pop

% turf1_mp = Turf.Alpha(1,Col) .* Turf.q(1).*Fish.K(1).*(1 - 2.*((Turf.q(1) .* Theta .* TotalEffort)/Fish.r)) - 2 .* Beta(1) .* Theta .* TotalEffort;
%
% turf2_mp = Turf.Alpha(2,Col) .* Turf.q(2).*Fish.K(2).*(1 - 2.*(( Turf.q(2) .* (1-Theta) .* TotalEffort)/Fish.r)) - 2 .* Beta(2) .* (1 - Theta) .* TotalEffort;
%
%
% f = turf1_mp - turf2_mp;

% Beta(1) = (Alpha2(WhichTurfs,Col).* Turf.q(WhichTurfs).*Biomass(WhichTurfs))./(2.*Effort);%Increase costs to dissipiate marginal profits

f=(Pi(1)-(2.*Beta(1).*turf_effort(1)))-(Pi(2)-(2.*Beta(2).*turf_effort(2)));

% f=(Pi(1)-(2.*Beta(1).*(Theta.*TotalEffort./Turf.q(1))))-(Pi(2)-(2.*Beta(2).*((1-Theta).*TotalEffort./Turf.q(2))));

end