if true then true else false;
succ 0;
pred 0;
pred (succ (succ 0));

succ succ 0;
pred succ succ pred 0;

if if iszero succ 0 then true else false then succ 0 else pred succ 0;

succ succ if true then 0 else succ 0;

if 
  if iszero pred succ 0 
  then iszero succ succ 0
  else false
then 
  if true
  then succ succ succ 0
  else pred 0
else 
  pred succ pred succ 0;