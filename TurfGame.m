function Output=TurfGame(StartingGuess,Nudge,IsITQ,StartPop,Time,FLimit)
%% Game theory model for deciding TURF fishing efforts

global Fish Turf System

% StartingGuess: Starting guess of effort
% Nudge: how many times to nudge the starting guess to see if you end up somewhere different
% IsITQ: are there internal ITQs?
% StartPop: the starting population
% Time: the time to run the model

% Options=optimset('MaxFunEvals',1000,'TolFun',1e-9,'TolCon',1e-9,'MaxIter',1000);
Options=optimset('MaxFunEvals',1000,'Display','notify','Algorithm','active-set');
% StartPop=UnfishedPop
% Time='EQ'
%  StartingGuess=[Umsy,Umsy];

EffortStrategy=StartingGuess;

Tolerance=1e-3; %Tolerance for game

FinalStrategy=nan(Nudge,length(StartingGuess)); %Storage

for n=1:Nudge %Loop over nudges to check for consistent convergence
    
    if n>1
        
        EffortStrategy=StartingGuess.*lognrnd(0,.9,1,length(StartingGuess)); %Nudge starting guess
        
    end
    
    DeltaQuota=999;
    
    while DeltaQuota>Tolerance %While quotas are changing
        
        OldStrategy=EffortStrategy;
        Flip=-1;
        for t=1:Turf.NumTurfs %Pass the game back and forth between each TURF
            
            %             EffortStrategy
            %             pause
            ProfitFunction=@(Target)OptEffort(Target,t,EffortStrategy,IsITQ,StartPop,Time,'No','No'); %pass additional parameters
            
            Bottom=.0001;
            Top=1.9*Fish.Umsy;
            if FLimit>0
             
                Flip=Flip*-1;
                Bottom=.0001;
                Top=FLimit-EffortStrategy(t+Flip);
                
            end

            [OptimalU,Profits,exitflag,output]=fmincon(ProfitFunction,Fish.Umsy/2,[],[],[],[],Bottom,Top,[],Options); %optimize effort to maximize turf T profits
            ['Turf', num2str(t) ,'Profits are' ,num2str(Profits)];
            
            EffortStrategy(t)=OptimalU;
            
        end
        
        NewStrategy=EffortStrategy; % Assign new effort strategy given last step in the game
        
        DeltaQuota=sum(abs(NewStrategy-OldStrategy)); % Calculate change in quota from step to step in the game
        
    end
    
    FinalStrategy(n,:)=EffortStrategy; %Store final strategy
    
end

GameEquilibrium=mean(FinalStrategy,1); %Mean effort strategies over nudges

Flag=sum(abs(mean(FinalStrategy,1)-FinalStrategy(1,:)))>Tolerance; %Check for consistent convergence

if Flag>0
    warning('Game theoretic outcome different under different starting guesses')
end

Output.Effort=GameEquilibrium;

Output.Flag=Flag;

end
