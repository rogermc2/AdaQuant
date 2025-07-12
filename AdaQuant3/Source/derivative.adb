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

with Ada.text_io; use  Ada.text_io;
with Ada.Numerics; use  Ada.Numerics;

Package body Derivative is
procedure nderiv is

  type integer_vector is array(integer range <>) of integer;
  type integer_matrix is array(integer range <>, integer range <>) of integer;

  norder : integer := 6;
  npoints : integer := 12;
  a : integer_vector(0..50);         -- coefficients of f(x0), f(x1), ...
  b : integer;

  function gcd(a: integer; b : integer) return integer is
    a1, b1, q, r: integer;
  begin
    if a=0 or b=0 then return 1; end if;
    if abs(a)>abs(b) then
      a1 := abs(a);
      b1 := abs(b);
    else
      a1 := abs(b);
      b1 := abs(a);
    end if;
    r := 1;
    while r/=0 loop
      q := a1 / b1;
      r := a1 - q * b1;
      a1 := b1;
      b1 := r;
    end loop;
    return a1;
  end gcd;


  procedure deriv(order  : integer;
                 npoints : integer;
                 point   : integer;
                 a       : out integer_vector;
                 bb      : out integer) is
    -- compute the exact coefficients to numerically compute a derivative
    -- of order 'order' using 'npoints' at ordinate point,
    -- where order>=1, npoints>=order+1, 0 <= point < npoints,
    -- 'a' array returned with numerator of coefficients,
    -- 'bb' returned with denominator of h^order

    h : integer_vector(0..99);           -- coefficients of h
    x : integer_vector(0..99);           -- coefficients of x, variable for differentiating
    numer : integer_matrix(0..99,0..99); -- numerator of a term numer(term,pos)
    denom : integer_vector(0..99);       -- denominator of coefficient
    j, k, b : integer;
    ipower, isum, iat, r : integer;
    debug : integer := 0;

  begin
    if npoints<=order then
      put_line("ERROR in call to deriv, npoints=" & integer'image(npoints) &
               "< order=%d+1=" & integer'image(order));
      return;
    end if;
    for term in 0..npoints-1 loop
      denom(term) := 1;
      j := 0;
      for i in 0..npoints-2 loop
        if j=term then j := j+1; end if; -- no index j in jth term
        numer(term,i) := -j; -- from (x-x(j))
        denom(term) := denom(term)*(term-j);
        j := j+1;
      end loop;
    end loop; -- end term setting up numerator and denominator
    if debug>0 then
      for i in 0..npoints-1 loop
        put_line("denom(" & integer'image(i) & ")=" & integer'image(denom(i)));
        for j in 0..npoints-2 loop
          put_line("numer(" & integer'image(i) & "," & integer'image(j) &
                          ")=" & integer'image(numer(i,j)));
        end loop;
      end loop;
    end if; -- debug
    -- substitute xj = x0 + j*h, just keep j value in numer(term,i)
    -- don't need to count x0 because same as x's
    -- done by above setup

    -- multiply out polynomial in x with coefficients of h
    -- starting from (x+j1*h)*(x+j2*h) * ...
    -- then differentiate d/dx
    for term in 0..npoints-1 loop
      x(0) := 1; -- initialize
      x(1) := 0;
      h(1) := 0;
      k := 1;
      for i in 0..npoints-2 loop
        for j in 0..k-1 loop            -- multiply by (x + j h)
          h(j) := x(j)*numer(term,i); -- multiply be constant x(j)
        end loop;
        for j in 0..k-1 loop         -- multiply by x
          h(j+1) := h(j+1)+x(j); -- multiply by x and add
        end loop;
        k := k+1;
        for j in 0..k-1 loop
          x(j) := h(j);
        end loop;
        x(k) := 0;
        h(k) := 0;
      end loop;
      for j in 0..k-1 loop
        numer(term,j) := x(j);
        if debug>0 then
          put_line("p(x)=" & integer'image(numer(term,j)) &
                   " x^" & integer'image(j) &
                   " at term=" & integer'image(term));
        end if;
      end loop; -- have p(x) for this 'term'

      -- differentiate 'order' number of times
      for jorder in 0..order-1 loop
        for i in 1..k-1 loop -- differentiate p'(x)
          numer(term,i-1) := i*numer(term,i);
        end loop;
        k := k-1;
      end loop; -- have p^(order) for this term
      if debug>0 then
        for i in 0..k-1 loop
          put_line("p^(" & integer'image(order) &
                       ")(x)=" & integer'image(numer(term,i)) &
                       " x^" & integer'image(i) &
                       " at term=" & integer'image(term));

        end loop;
      end if;
    end loop; -- end 'term' loop  for one 'order', for one 'npoints'

    -- substitute each point for x, evaluate, get coefficients of f(x(j))
    iat := point; -- evaluate at x(iat), substitute
    for jterm in 0..npoints-1 loop -- get each term
      ipower := iat;
      isum := numer(jterm,0);
      for j in 1..k-1 loop
        isum := isum + ipower * numer(jterm,j);
        ipower := ipower * iat;
      end loop;
      a(jterm) := isum;
      if debug>0 then
        put_line("f^(" & integer'image(order) &
                 ")(x(" & integer'image(iat) &
                 ")) = (1/h^" & integer'image(order) &
                 ") (" & integer'image(a(jterm)) &
                 "/" & integer'image(denom(jterm)) &
                 " f(x(" & integer'image(jterm) & ")) + ");
      end if;
    end loop;
    if debug>0 then new_line; end if;
    b := 0;
    for jterm in 0..npoints-1 loop -- clean up each term
      if a(jterm)/=0 then
        r := gcd(a(jterm),denom(jterm));
        a(jterm) := a(jterm) / r;
        denom(jterm) := denom(jterm) /r;
        if denom(jterm)<0 then
          denom(jterm) := -denom(jterm);
          a(jterm) := - a(jterm);
        end if;
        if denom(jterm)>b then
          b := denom(jterm); -- largest denominator
        end if;
      end if;
    end loop;
    for jterm in 0..npoints-1 loop -- check for divisor
      r := (a(jterm) * b) / denom(jterm);
      if r * denom(jterm) /= a(jterm) * b then
        r := gcd(b, denom(jterm));
        b := b * (denom(jterm) / r);
      end if;
    end loop;
    for jterm in 0..npoints-1 loop -- adjust for divisor
      a(jterm) := (a(jterm) * b) / denom(jterm);
      if debug>0 then
        put_line("f^(" & integer'image(order) &
                     ")(x(" & integer'image(iat) &
                     "))=(1/" & integer'image(b) &
                     "h^" & integer'image(order) &
                     ")(" & integer'image(a(jterm)) &
                     " f(x("&integer'image(jterm)&")) + ");
      end if;
    end loop; -- end computing terms of coefficients
    bb := b;
  end deriv;

  function pow(x : long_float; pwr : integer) return long_float is
    val : long_float := 1.0;
  begin
    for i in 0..pwr-1 loop
      val := val * x;
    end loop;
    return val;
  end pow;

  procedure check(order : integer;
                  npoints : integer;
                  point : integer;
                  a : integer_vector;
                  b : integer) is
    pwr : integer; -- simple test using x^pwr
    sum, x, term, err, xpoint, deriv : long_float;
    h : long_float;
  begin
    for degree in npoints-2..npoints+1 loop
      h := 0.125;
      if order>3 then h := h/2.0; end if; -- cumulative
      if order>4 then h := h/2.0; end if;
      if order>5 then h := h/2.0; end if;
      pwr := degree;
      sum := 0.0;
      x := 1.0;
      for i in 0..npoints-1 loop
        term := long_float(a(i))*pow(x, pwr);
        sum := sum + term;
        x := x + h;
      end loop;
      sum := sum / long_float(b);
      sum := sum / pow(h, order);
      xpoint := 1.0+long_float(point)*h;
      deriv := 1.0;
      for i in 0..order-1 loop
        deriv := deriv * long_float(pwr);
        pwr := pwr -1;
      end loop;
      deriv := deriv * pow(xpoint, pwr);
      err := deriv - sum;
      put_line("                err=" & long_float'image(err) &
               ", h=" & long_float'image(h) &
               ", deriv=" & long_float'image(deriv) &
               ", x=" & long_float'image(xpoint) &
               ", pwr=" & integer'image(degree));
    end loop;
  end check;

begin -- nderiv

  put_line("Formulas for numerical computation of derivatives ");
  put_line("  f^(3) means third derivative ");
  put_line("  f^(2)(x(1)) means second derivative computed at point x(1) ");
  put_line("  f(x(2)) means function evaluated at point x(2) ");
  put_line("  Points are x(0), x(1)=x(0)+h, x(2)=x0+2h, ... ");
  put_line("  The error term gives the power of h and ");
  put_line("  derivative order with z chosen for maximum value.");
  put_line("  The error terms, err=, are for test polynomial sum(x^pwr) ");
  put_line("  with h = 0.125 or smaller, and order = 'pwr' ");

  for order in 1..norder loop
    npoints := npoints -2; -- fewer for larger derivatives
    if npoints<=order+1 then npoints := order + 2; end if;
    for points in order+1..npoints loop
      for term in 0..points-1 loop
        put_line("computing order=" & integer'image(order) &
                 ", npoints=" & integer'image(points) &
                 ", at term=" & integer'image(term));
        deriv(order, points, term, a, b);

        for jterm in 0..points-1 loop
          if jterm=0 then
            put_line("f^(" & integer'image(order) &
                     ")(x(" & integer'image(term) &
                     "))=(1/" & integer'image(b) &
                     "h^" & integer'image(order) &
                     ")( " & integer'image(a(jterm)) &
                     " * f(x(" & integer'image(jterm) & ")) + ");
          elsif jterm=points-1 then
            put_line("                      " & integer'image(a(jterm)) &
                     " * f(x(" & integer'image(jterm) &
                     ") ) + O(h^" & integer'image(points-order) &
                     ")f^(" & integer'image(points) & ")(z) ");
          else
            put_line("                      " & integer'image(a(jterm)) &
                     " * f(x(" & integer'image(jterm) & ") + ");
          end if;
        end loop; -- jterm
        check(order, points, term, a, b);
        new_line;
      end loop; -- term
      new_line;
    end loop;  -- points
    new_line;
  end loop; -- order
  put_line("many formulas checked against Abramowitz and Stegun,");
  put_line("Handbook of Mathematical Functions Table 25.2");
end nderiv;
end Derivative;
