% Copyright (c) 2014-2016, Johan Nordlander, Jonas Duregård, Michał Pałka,
%                          Patrik Jansson and Josef Svenningsson
% All rights reserved.
%
% Redistribution and use in source and binary forms, with or without
% modification, are permitted provided that the following conditions are met:
%
%    * Redistributions of source code must retain the above copyright notice,
%      this list of conditions and the following disclaimer.
%    * Redistributions in binary form must reproduce the above copyright
%      notice, this list of conditions and the following disclaimer in the
%      documentation and/or other materials provided with the distribution.
%    * Neither the name of the Chalmers University of Technology nor the names of its
%      contributors may be used to endorse or promote products derived from this
%      software without specific prior written permission.
%
% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
% FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
% OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Static info examples %%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- consult('semantics.pl').

e:p0:i ==> e:p3:i.
_ ==> _ :- false.

event(r1:i, timing(300)).
event(r1:i, data_received(e:p3)).

implementation(r1:i, return(void)).

minimum_start_interval(r1:i, 10).

can_be_invoked_concurrently(r2:i).

server_call_point(r1:i, sync(o:p2)).

%%%%%%%%%%%%%%%%

example(L,R) :-
%  Cont1 = fn(X,rte_Exit(x, Cont2)),
%  Cont2 = fn(X,rte_Exit(x, fn(Y,return(void)))),
  Proc1 = rinst(r1:i,nil,[],rte_Enter(x,_Cont1)),
  Proc2 = rinst(r2:i,nil,[],rte_Enter(x,_Cont2)),
  Block = [excl(x:i,free) | [Proc1 | [Proc2 | []]]],
  [excl(z:i,free) | Block] --- L ---> R.
