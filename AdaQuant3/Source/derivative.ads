-- nderiv.adb  compute formulas for numerical derivatives
--             order is order of derivative, 1 = first derivative, 2 = second
--             points is number of points where value of function is known
--             f(x0), f(x1), f(x2) ... f(x points-1)
--             term is the point where derivative is computer
--             f'(x0) = (1/bh)*( a(0)*f(x0) + a(1)*f(x1)
--                      + ... + a(points-1)*f(x points-1)

-- algorithm: use divided differences to get polynomial p(x) that
--            approximates  f(x).  f(x)=p(x)+error term
--            f'(x) = p'(x) + error term'
--            substitute xj = x0 + j*h
--            substitute x = x0 to get  p'(x0) etc

with Maths; use Maths;

Package  Derivative is
  procedure nderiv;
  procedure deriv(order  : in integer;
                 npoints : in integer;
                 point   : in integer;
                 a       : out tInteger_Vector;
                 bb      : out integer);

  procedure check(order   : in integer;
                  npoints : in integer;
                  point   : in integer;
                  a       : in tInteger_Vector;
                  b       : in Integer);

end Derivative;
