function FishingPressure=DistributeFleet(WhatRate,WhatTurf) %Distribute a fleet over a TURFs patches
%% Distribute fishing effort over patches
global Fish Turf System

% WhatRate: The fishing effort getting distributed
% WhatTurf: What TURF your're dealing with

DistributedU=zeros(1,System.NumPatches); %Storage
% WhatTurf=[1,2]
% WhatRate=TurfEfforts

for t=1:length(WhatTurf) %Loop over TURFs
    where=Turf.TurfLocations==WhatTurf(t); %Find TURF
    DistributedU(where)=WhatRate(t); %Assign effort to the right patches for TURF t
end

FishingPressure=DistributedU; %Distribtued fishing pressure

end
