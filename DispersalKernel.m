function Dispersal=DispersalKernel(Mode,Dispersal) %Calculate dispersal kernel/probabilities
%% Calculate dispersal probabilities
global Fish Turf System

if strcmp(Mode,'Simple')
    %Really simple distance only works for 2 patch system. Direction is a
    %length 2 vector specifying what % of the population in each patch goes
    %to the other patch. Remainder stays in the home patch. so, .75 means
    %that 75% of individuals in patch 1 go to patch 2    
%     Dispersal=[1,1];å    
    Dii=[1-Dispersal(1), Dispersal(1) ; Dispersal(2), 1-Dispersal(2) ];
    
elseif Fish.Movement==0
    
    Dii=diag(repmat(1,1,System.NumPatches)); %sets up larval dispersal matrix with home cells filled in
else
    punit=2.5/Fish.Movement; %divide 2.5 standard deviations by the max dispersal distance, assumes that larvae will all settle with the bounds of 95% confidence intervals
    prang=-2.5:punit:2.5; %breamp.ks up norm dist into punit units
    pdisp=normpdf(prang); %assigns normal probability of larvae settling at distance punit
    
    pdisp=pdisp./sum(pdisp); %standardizes 95% distritbution to 100%
    
    Dii=diag(repmat(max(pdisp),1,System.NumPatches)); %sets up larval dispersal matrix with home cells filled in
    Dii2=diag(repmat(0,1,System.NumPatches)); %sets up larval dispersal matrix with home cells filled in
    Dii3=diag(repmat(0,1,System.NumPatches)); %sets up larval dispersal matrix with home cells filled in
    
    for i=1:System.NumPatches
        for j=1:System.NumPatches
            dif=abs(j-i);
            if dif==0
                Dii(i,j)= Dii(i,j); %ignores home cells
                
            elseif dif<=Fish.Movement
                Dii(i,j)=pdisp(find(pdisp==max(pdisp))+dif); %fills in remaining cells according to distance from home cell
            end
            if System.Wrap==1
                if  (System.NumPatches-i)<Fish.Movement && (System.NumPatches-j)<Fish.Movement
                    missingR=Fish.Movement-(System.NumPatches-i);
                    Dii2(i,1:missingR)=(pdisp(find(pdisp==max(pdisp))+(((Fish.Movement+1)-missingR):Fish.Movement)));
                end
                if (i-1)<Fish.Movement
                    missingL=(Fish.Movement-(i-1));
                    Dii3(i,((System.NumPatches+1)-missingL):end)=fliplr((pdisp(find(pdisp==max(pdisp))+(((Fish.Movement+1)-missingL):Fish.Movement))));
                end
                
            end
            
        end
    end
    Dii=Dii+Dii2+Dii3;
    
    
end
Dispersal=Dii;
end
