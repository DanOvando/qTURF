function f=ZeroFun(Theta,Turf,quota_pool,Fish,pop, IsITQ, System,cr_ratio)

turf_effort = ([Theta, 1 - Theta] .* quota_pool) ./ (Turf.q .* pop);


if IsITQ==0 %Non-ITQ profits function, with racing
    Col=1;
    
    Alpha2=Turf.Alpha;
    
    %         Beta=Alpha2(WhichTurfs,Col)./(2.*Effort);%Increase costs to dissipiate marginal profits
    
    Beta = (Turf.Alpha(:,Col).* Turf.q.* pop)./(2.*turf_effort);%Increase costs to dissipiate marginal profits
    
    %Beta = Turf.Beta(:,Col);
    
    %Beta=Alpha2(:,Col)./(2.*turf_effort);%Increase costs to dissipiate marginal profits
    
    Beta(isinf(Beta))=0;
else %ITQ profit function
    
    Col=2;
    
    Alpha2=(Turf.Alpha(:,Col).*(1-System.ITQCosts))';
    
    Beta = Turf.Beta(:,Col);
    
    Beta= (Alpha2(:,Col)'.*Turf.q)./2; %Costs scale to catcheability
    
    Beta(isinf(Beta))=0;
    
end

Pi = Alpha2.*Turf.q.*pop;

f=(Pi(1)-(2.*Beta(1).*turf_effort(1)))- (Pi(2)-(2.*Beta(2).*turf_effort(2)));

end