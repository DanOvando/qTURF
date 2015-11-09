function f=ZeroFun(Theta,Pi,Beta,Turf,TotalEffort,Col)

 f=(Pi(1)-(2.*Beta(1).*((Theta.*TotalEffort)./(Turf.q(1))) ))-(Pi(2)-(2.*Beta(2).*(((1-Theta).*TotalEffort)./(Turf.q(2))) ));

% f=(Pi(1)-(2.*Beta(1).*Theta.*(TotalEffort)))-(Pi(2)-(2.*Beta(2).*(1-Theta).*(TotalEffort)));

end