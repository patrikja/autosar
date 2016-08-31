function setMaskLabel(block_path, label, num, mode, init)
% SETMASKLABEL 
%   Sets a label of the mask wrapped around the ARSIM block.
% 
% USAGE:
%   This is called from swrapper.c to set port labels for the mask
%   surrounding the S-function. For this to work properly the S-function
%   will need to be connected to MUX and DEMUX blocks. Either of the masks
%   used in CarModel.slx or ACCModel.slx can be used as templates.
%
%   This function overwrites all previous mask initialization commands when
%   called from swrapper.c.
      
  % swrapper.c will get the path to the S-function block. The mask we are
  % interested in is the parent of this block.
  mask = get_param(block_path,'Parent');
  
  % ASSUMPTION: Component already masked.
  % set_param(mask, 'Mask', 'on');
  if strcmp(bdroot, mask)
    fprintf(['[ERROR] setMaskLabel: S-function block %s not properly ', ...
             'masked.\nPlease refer to CarModel.slx or ACCModel.slx ',  ...
             'for usage examples.\n'], block_path); 
    return
  end
  
  % Get mask struct. If 'init' is set, empty contents of initialization
  % code.
  mask_struct = Simulink.Mask.get(mask);
  inits       = [];
  if ~init
    inits = mask_struct.Display;
  end
    
  % Set parameters.
  set_param(mask, 'MaskDisplay', [inits, ';', create_cmd(mode, num, label)]);
  
  function cmd = create_cmd(mode, num, label)
    cmd = ['port_label(''', mode, ''', ', num2str(num), ...
           ', ''', label, ''')'];
  end
end