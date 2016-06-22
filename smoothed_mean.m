function [ smoothed ] = smoothed_mean( x , lags)
%running_mean calculate running mean
%   Detailed explanation goes here


index = 1:length(x);

for i = 1:length(index)

    smoothed(:,i) = sum(x(:,max(1,index(i) - ( (lags - 1) :-1:0))),2 ) ./ lags ;
    
end

end

