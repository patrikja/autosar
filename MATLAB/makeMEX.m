function makeMEX(~)
%MAKEMEX Build the MEX file including the protocol code.

if nargin < 1
  mex swrapper.c protocol.c
else
  mex -g swrapper.c protocol.c
end

end

