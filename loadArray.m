function x = loadArray(filename,siz)
%Purpose: You have an array saved in a file (as a column vector) and you
%want to read it into Matlab.
%Example: in Fortran you compute value function V(k,z) with dim (nk,nz)
% You write in a file as V with dim (nk*nz,1)
% You read the file in Matlab and reshape it into (nk,nz)
% SYNTAX: 
% valueFunction = loadArray('valueFunction.txt',[nk,nz]);
% Note: dlmread is faster than load

%x1dim = load(filename);
x1dim = dlmread(filename,'\t');
x     = reshape(x1dim,siz);



end

