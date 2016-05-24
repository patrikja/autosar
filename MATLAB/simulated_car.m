function simulated_car(block)
%SIMULATED_CAR MATLAB instance of the NewABS simulated car.
%
%   A MATLAB implementation of the simulated car in the NewABS component
%   for testing the NewARSim Simulink wrapper. Acts as a plug-in
%   replacement for the Haskell version of the car and should thus
%   produce the same results (or at least very similar).
%
%   This is a level 2 MATLAB S-Function.

  setup(block);
end

function setup(block)  
  % Register number of ports
  block.NumInputPorts  = 8;
  block.NumOutputPorts = 8;
  
  % Setup port properties to be inherited or dynamic
  block.SetPreCompInpPortInfoToDynamic;
  block.SetPreCompOutPortInfoToDynamic;
  
  % Override input port properties
  for i = 1:block.NumInputPorts
    block.InputPort(i).Dimensions        = 1;
    block.InputPort(i).DatatypeID        = 0;  % double
    block.InputPort(i).Complexity        = 'Real';
    block.InputPort(i).DirectFeedthrough = true;
    block.InputPort(i).SamplingMode      = 'Sample';
  end
  
  % Override output port properties
  for i = 1:block.NumOutputPorts
    block.OutputPort(i).Dimensions   = 1;
    block.OutputPort(i).DatatypeID   = 0; % double
    block.OutputPort(i).Complexity   = 'Real';
    block.OutputPort(i).SamplingMode = 'Sample';
  end

  % Register parameters
  block.NumDialogPrms     = 0;

  % Register sample times
  %  [0 offset]            : Continuous sample time
  %  [positive_num offset] : Discrete sample time 
  % 
  %  [-1, 0]               : Inherited sample time
  %  [-2, 0]               : Variable sample time
  block.SampleTimes = [-2 0];

  % Specify the block simStateCompliance. The allowed values are:
  %    'UnknownSimState', < The default setting; warn and assume DefaultSimState
  %    'DefaultSimState', < Same sim state as a built-in block
  %    'HasNoSimState',   < No sim state
  %    'CustomSimState',  < Has GetSimState and SetSimState methods
  %    'DisallowSimState' < Error out when saving or restoring the model sim state
  block.SimStateCompliance = 'DefaultSimState';

  % -----------------------------------------------------------------
  % The MATLAB S-function uses an internal registry for all
  % block methods. You should register all relevant methods
  % (optional and required) as illustrated below. You may choose
  % any suitable name for the methods and implement these methods
  % as local functions within the same file. See comments
  % provided for each function for more information.
  % -----------------------------------------------------------------

  block.RegBlockMethod('PostPropagationSetup',    @DoPostPropSetup);
  block.RegBlockMethod('Start', @Start);
  block.RegBlockMethod('Outputs', @Outputs);     % Required
  block.RegBlockMethod('Terminate', @Terminate); % Required
end 

function DoPostPropSetup(block)
  block.NumDworks = 4;

  for i=1:block.NumDworks
    block.Dwork(i).Name       = ['wheel', num2str(i)];
    block.Dwork(i).Dimensions = 2;
    block.Dwork(i).DatatypeID = 0;      % double
    block.Dwork(i).Complexity = 'Real'; % real
  end 
end
  
function Start(block)
  % -- Initial wheel set up -------------------------------------------------
  init_v = 18.0;
  init_a = 0.00;

  for i = 1:block.NumDworks
    block.Dwork(i).Data = [init_v, init_a];  
  end
  
end

function Outputs(block)
  
  % -- Run car ------------------------------------------------------------
  %
  % Here is where we simulate the car. Get actuator input (?),
  % run wheel_f to get wheel speeds etc

  time = block.CurrentTime;                     % Get current time, once

  % for each wheel i, do
  for i = 1:4
    timestep = 1e-2;
    
    % -- Read inputs-------------------------------------------------------
    veloIn   = block.Dwork(i).Data(1);
    accelIn  = block.Dwork(i).Data(2);
    pressure = block.InputPort(2 * i - 1).Data; % unused
    relief   = block.InputPort(2 * i).Data; % unused
    % ---------------------------------------------------------------------
    
    % -- Get new velocity and acceleration, write to output ---------------
    [acc, vel] = wheel_f(i, time + timestep, pressure, relief, veloIn);
    block.Dwork(i).Data(1) = vel;
    block.Dwork(i).Data(2) = acc;
    % ---------------------------------------------------------------------
    
  end
  % -----------------------------------------------------------------------

  block.NextTimeHit = time + timestep;

  % -- Propagate DWork contents to output (not useful right now) ----------
  for i=1:4
    block.OutputPort(2 * i - 1).Data = block.Dwork(i).Data(1); % velocity
    block.OutputPort(2 * i).Data     = block.Dwork(i).Data(2); % acceleration
  end
  % -----------------------------------------------------------------------
  
end 

function [a, v] = wheel_f(idx, time, pressure, relief, velo)
  % WHEEL_F The wheel_f function from NewABS
  %
  %   Simulates the wheels of the car. Slightly modified so that the 
  %   skidding kicks in faster.
  
  function [x, y] = velo_step(a, v) 
    dt = 1e-2;
    x  = a;
    y  = v + a * dt;
  end

  if time < 1.0
    [a, v] = velo_step(0, velo);
    return
  elseif idx ~= 2
    [a, v] = velo_step(-4.5, velo);
    return
  elseif time < 1.6
    [a, v] = velo_step(-10, velo);
    return
  elseif time < 2
    [a, v] = velo_step(-4, velo);
    return
  elseif time < 2.5
    [a, v] = velo_step(-3, velo);
    return
  elseif time < 3
    [a, v] = velo_step(0, velo);
    return
  elseif time < 3.4
    [a, v] = velo_step(-4.5, velo);
    return
  elseif time < 4
    [a, v] = velo_step(-5, velo);
    return
  elseif time < 4.3
    [a, v] = velo_step(-8.4, velo);
    return
  elseif time < 4.7
    [a, v] = velo_step(-4, velo);
    return
  else 
     [a, v] = velo_step(0, velo);
  end
  
  % Commented out stuff from wheel_f
  % pressure && not relief = velostep (-10) velo
  % relief && not pressure = velostep (-1)  velo
  % otherwise              = velostep (-3)  velo
end 

function Terminate(~)
end 


