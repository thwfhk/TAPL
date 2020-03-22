x/;
x;

lambda x. x;
(lambda x. x) (lambda x. x x); 

(lambda x. lambda y. x y) (lambda z.x);

(lambda f.(lambda x.f (lambda y.x x y))(lambda x.f (lambda y.x x y))) (
  lambda fac. 
    lambda n. (
      if iszero (n-1)
        then n 
        else fac (n-1) * n
    )
) 4;