  function dynSearch1
% a dynamic search display
% Exp.1. replication of Horowitz & Wolfe, 1998
% ver. 1: 21th Nov. 2013

try 
    % initialization: Experiment, Input Device, 
    blk_trials = 30;
    exp = CExp(5,[2 3],'blockFactors',2,'blockRepetition',7); % target present/absent ; set size (3)
    exp.subInfo;            % acquire subject information
    % creat visual display (monitor)
    v = CDisplay('lineSpace',2,'skipSync',1, 'fontSize', 20, ...
        'lineWidth',60,'monitorSize',22, 'bgColor',[92 92 92]);

    % define input device
    kb = CInput('k', [1 2],{'LeftArrow','RightArrow'}); 
     % parameters initialization: text, images.  
    infoText = init_text; %instruction etc. 

    %create T and L shapes
    item_pix = 64;
	t_shape = ones(item_pix,item_pix,3)*v.bgColor(1); %store in rgb format
    t_shape(1:7,:,:) = v.color(1); % horizontal line
    t_shape(:, item_pix/2-3:item_pix/2+3,:) = v.color(1); % vertical line
	l_shape = ones(item_pix,item_pix,3)*v.bgColor(1); %store in rgb format
    l_shape(end-6:end,:,:) = v.color(1); % horizontal line
    l_shape(:, 1:7,:) = v.color(1); % vertical line
    
    t_texture = v.createItem(t_shape);
    l_texture = v.createItem(l_shape);
    
    set_size = [8, 12, 16];
    [X,Y] = meshgrid(-8:2:8,-8:2:8);
    num_pos = length(X)^2; % 
    % add some jitters in X, Y
    X = X + (rand(size(X))-0.5)*0.5; %-0.25 to 0.25 
    Y = Y + (rand(size(Y))-0.5)*0.5; %-0.25 to 0.25 
    
    angles = [0, repmat([0 90 180 270], 1,(num_pos-1)/4)]; % random rotations
    itemSizes = [0.8 0.8]; % size in visual angle degree x and y
    
    dyn_frames = v.sec2frames(0.106); % 10 frames;
    
    % display instruction
    v.dispText(infoText.instruction);
    kb.wait;
    
    % formal experiment
    for iTrl=1: exp.maxTrls
        % start a trial
        % 1. prepare stimuli at given condition(s)
        cond = exp.getCondition; %get condition array
        c_target = cond(1); % target present/absent
        c_size = set_size(cond(2)); % set size
        c_dyn = cond(3); % static or dynamic;
         
        %generate items
        if c_target == 1 % target present 
            items = ones(1,c_size)*l_texture;
            items(1) = t_texture; % replace 1st with target item
        else % target absent
            items = ones(1,c_size)*l_texture;
        end
        
        
        % 2. display stimuli 
        % 2.1 fixation
        v.dispFixation(5,2);
        WaitSecs(0.5);
        
        % 2.2 main display
        beginTime = GetSecs;
        
        for iframe = 1: v.sec2frames(5) % maximum 5 seconds display
            if mod(iframe,dyn_frames) == 1
                if ~(c_dyn == 2 && iframe > 2) % if not static condition
                    % positions
                    xy_idx = randperm(num_pos); %random permute
                    xy_idx = xy_idx(1:c_size);     %select current size
                    xys = [X(xy_idx)', Y(xy_idx)']; % xy positions
                    % rotations
                    rot_idx = randperm(num_pos);
                    rot_idx = rot_idx(1:c_size);
                    rotations = angles(rot_idx);
                end
            end
            % search display 
            v.dispItems(xys, items, itemSizes,rotations);
            
            % 3. acquire response
            [key, rTime] = kb.response(0);          %acquire response
            if key > 0 % some key press
                break;
            end
        end
        
        exp.setResp([key rTime-beginTime]); %store response: key, RT
        v.flip(1);
        % 4. block break or sometime feedback
        if mod(iTrl,blk_trials) == 0
            % calculate correct rate
            rate = mean(exp.resp([iTrl-blk_trials+1:iTrl],1) == exp.seq([iTrl-blk_trials+1:iTrl],1));
            lastBlk = ['In the last block, you made ' num2str(rate*100) '%.\n'];
            v.dispText([lastBlk infoText.blkText]);
            kb.wait;
            WaitSecs(1);
        end
        
        % 5. inter-trial interval
        WaitSecs(1+rand*0.5);
        
        % 6. debugging needs 'stop' function
        if kb.wantStop %stop for debugging
            break;
        end
        

    end %end of trials
    
    %closing the experiment
    exp.saveData;   %save data
    
    v.dispText(infoText.thankyou);
    kb.wait;
    v.close;

catch ME
    
    disp(ME.message);
    disp(ME.stack);
    for iTrl=1:length(ME.stack)
        disp(ME.stack(iTrl).name);
        disp(ME.stack(iTrl).line);
    end
    v.close;
end
end
   
function infoText = init_text
    % specify experimental text
    infoText.instruction = ['Visual Search \n', ...
        ' In this experiment, you will see a display with many letter Ls and T. ', ...
        'Your task is to detect if letter T is presented or not. If it is presented, ',...
        'please press the left key; otherwise, please press the right key.',... 
        ' Please note that T will be presented in some trials and absent in other trials.',...
        '\n\n when you are ready, press any key to start'];
    infoText.question = '?';
    infoText.blkText = ['Please take a break!', ...
        '\n\n when you are ready, press any key to start'];
    infoText.thankyou = 'The experiment is finished! \nThank you very much!';

end
    