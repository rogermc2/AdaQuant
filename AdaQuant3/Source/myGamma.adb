with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with Tools;

Package body Maths is

   Package Elementary_Float_Functions is new Ada.Numerics.Generic_Elementary_Functions(float);
   use Elementary_Float_Functions; -- provides ** for float exponents.
   Package tLong_Float_Elementary_Functions is
     new Ada.Numerics.Generic_Elementary_Functions (Long_Float);
   use tLong_Float_Elementary_Functions; -- provides ** for long_float exponents.
   Package Long_Real_Arrays is new Generic_Real_Arrays (Long_Float);
   Type tLong_Real_Vector is new Long_Real_Arrays.Real_Vector;
   Type tLong_Real_Square_Matrix is new Long_Real_Arrays.Real_Matrix;

   Function gammaP (aa : in long_float; x : in long_float) return long_float;
   Function Lower_Gamma(a : in long_float; x : in long_float) return long_float;

-- ------------------------------------------------------

   Function "+" (Power_1 : in tPower; Power_2 : in tPower)
                 return tPower is
   begin
      return (Power_1.x_Power + Power_2.x_Power,
              Power_1.y_Power + Power_2.y_Power,
              Power_1.z_Power + Power_2.z_Power );
   end "+";

-- ------------------------------------------------------

   Function "-" (Power_1 : in tPower; Power_2 : in tPower)
                 return tPower is
   begin
      return (Power_1.x_Power - Power_2.x_Power,
              Power_1.y_Power - Power_2.y_Power,
              Power_1.z_Power - Power_2.z_Power);
   end "-";

-- ------------------------------------------------------

   Function Binomial (a : in integer; b : in integer) return float is
   begin
      return float(Factorial(a)) / float(Factorial(b) * Factorial(a - b));
   end Binomial;

-- ------------------------------------------------------------
   Function Ceiling (n : in float) return integer is
   begin
      return integer(float'Ceiling(n));
   end Ceiling;

-- --------------------------------------------------------

   -- Dual_Binomial returns the coeeficient Cj of x**j
   -- in the expansion of (x+a)**l (x+b)**m
   --       from Taketa, Huzinaga & O-ohata equation (2.4)
   Function Dual_Binomial (a : in float; b : in float;
                           l : in integer; m : in integer;
                           Coeff : in integer) return float is
      sum : float := 0.0;
      lm  : integer := l + m;
      li  : float;
   begin
      For i in 0 .. l loop
        li := Binomial(l, i);
         For j in 0 .. m loop
            If lm - i - j = Coeff then
               sum := sum + li*Binomial(m, j) * a**i * b**j;
            end if;
        end loop;
      end loop;
      return sum;
   end Dual_Binomial;

-- --------------------------------------------------------

   Function Factorial (n : in integer) return integer is
      Result : Integer :=1;
  begin
    if n > 1 then
      Result := n * Factorial(n - 1);
    end if;
    return result;
  end Factorial;

-- ------------------------------------------------------------

   Function Factorial_2 (n : in integer) return integer is
      -- double factorial function = 1 * 3 * 5 *  .. . * n *
      Result : integer := 1;
   begin
      if n > 1 then
         Result := n * Factorial_2 (n - 2);
      end if;
      return Result;
   end Factorial_2;

-- ------------------------------------------------------------

   Function fFact_2 (n : in integer) return float is
   begin
      return float(Factorial_2(n));
   end fFact_2;

-- ------------------------------------------------------------

   Function Factorial_Ratio2 (a : in integer; b : in integer) return float is
   begin
      return float(Factorial(a)) /float(Factorial(b)) /float(Factorial(a - 2 * b));
   end Factorial_Ratio2;

-- ------------------------------------------------------------

   Function Floor (n : in float) return integer is
   begin
      return integer(float'Floor(n));
   end Floor;

-- ------------------------------------------------------------

  Function Fgamma (m : in integer; x : in float) return float is
      val : long_float := 0.0;
      ml  : float := float (m) + 0.5;
      mlf  : long_float := long_float (ml);
      lfX : long_float := long_float(x);
  begin
    If x < 0.0 or not x'valid then
      Put_Line("Maths.Fgamma, negative x is invalid. x: "
               & float'image (x));
    elsif m < 0 or not m'valid  then
      Put_Line ("Maths.Fgamma, negative m is invalid. m: "
               & integer'image(m));
    elsif x = 0.0 then
      val := 1.0/(2.0*mlf + 1.0);
    else
      val := GammaP (mlf, lfX) * Gamma (mlf) / (2.0 * lfX ** m);
    end if;
    If Not val'Valid then
       Put_Line("Maths.Fgamma result is invalid");
       Put_Line("val: " & long_float'image(val));
       Put("m1, x: " & float'image(ml));
       Put_Line (",   " & float'image (x));
       If x > 0.0 then
         Put("GammaP, Gamma: " & long_float'image(GammaP (mlf, lfX)));
         Put_Line (",   " & long_float'image (Gamma (mlf)));
       end if;
    end if;
      return float(val);
  exception
    when anError :  others =>
       Put_Line ("An exception occurred in Maths.Fgamma.");
       Put_Line (Exception_Information (anError));
       return 0.0;
  end Fgamma;

-- ------------------------------------------------------------

   -- Regularized incomplete Gamma functions from Wikipedia
   -- (Numerical recipes calls these incomplete Gamma functions)

  Function gammaP (aa : in long_float; x : in long_float) return long_float is
      Result      : long_float := 0.0;
      LnGamma     : long_float;
      Log_Lower_G : long_float;
  begin
    If x < 0.0 then
      Put_Line("Maths.GammaP, negative x is invalid. x: "
               & long_float'image (x));
    elsif aa <= 0.0 then
      Put_Line ("Maths.GammaP, a must be > 0. a: "
               & long_float'image(aa));
    else
      LnGamma := lGamma (aa);
      If Not LnGamma'Valid then
         Put_Line("Maths.gammaP, Lower_Gamma result is invalid");
         Put_Line("gammaP.a: " & long_float'image(aa));
         Put_Line("gammaP.LnGamma: " & long_float'image(LnGamma));
      else
         Log_Lower_G := Log(Lower_Gamma (aa, x));
         Result := e**(Log_Lower_G - LnGamma);
         If Result > 1.01 Or Not LnGamma'Valid  Or Not Log_Lower_G'Valid then
            Put_Line("Maths.gammaP result is invalid");
            Put_Line("gammaP.Result: " & long_float'image(Result));
            Put_Line("gammaP.Log(Lower_Gamma): " & long_float'image(Log_Lower_G));
            Put_Line("gammaP.LnGamma: " & long_float'image(LnGamma));
            Put_Line("gammaP.a, x: " & long_float'image(aa) & long_float'image(x));
         elsif Result > 1.0 then
             -- assume rounding error
             Result := 1.0;
         end if;
      end if;
    end if;
    return Result;
  exception
    when anError :  others =>
       Put_Line ("An exception occurred in Maths.gammaP.");
       Put_Line("gammaP.Log(Lower_Gamma): " & long_float'image(Log_Lower_G));
       Put_Line("gammaP.LnGamma: " & long_float'image(LnGamma));
       Put_Line("gammaP.a, x: " & long_float'image(aa) & long_float'image(x));
       Put_Line (Exception_Information (anError));
       return 0.0;
   end gammaP;

-- ------------------------------------------------------------

   -- Based 0n PyQuante LA2.geigh
   Procedure Generalized_Eigensystem (Matrix         : in tReal_Square_Matrix;
                                      Unitary_Matrix : in tReal_Square_Matrix;
                                      Values         : out tEigen_Values;
                                      Vectors        : out tEigen_Vectors) is
   use Tools;
      Size        : Natural := Matrix'Length;
      Transformed : tReal_Square_Matrix(1 .. Size, 1 ..Size);
      eVectors    : Real_Arrays.Real_Matrix(1 .. Size, 1 .. Size);
   begin
      Transformed := Transpose (Unitary_Matrix) * Matrix * Unitary_Matrix;
      -- Ensure that the transformation matrix is symmetric
      For i in  1 .. Size-1 loop
         For j in  i+1 .. Size loop
           Transformed(j,i) := Transformed(i,j);
         end loop;
      end loop;
      Eigensystem (Real_Arrays.Real_Matrix(Transformed), Values, eVectors);
      Vectors := tEigen_Vectors (Unitary_Matrix * tReal_Square_Matrix (eVectors));
   exception
    when anError :  Argument_Error =>
       Put_Line ("An exception occurred in Maths.Generalized_Eigensystem.");
       Put_Line ("Transformed matrix:");
       Tools.Print_Square_Matrix (Transformed);
       Put_Line (Exception_Information (anError));
    when anError :  others =>
       Put_Line ("An exception occurred in Maths.Generalized_Eigensystem.");
       Put_Line (Exception_Information (anError));
   end Generalized_Eigensystem;

-- ------------------------------------------------------------

  -- Lower Gamma function (Wikipedia) *)
  -- The incomplete Gamma function can be approximated fairly
  -- rapidly using an iterative method:
  -- (http://algolist.manual.ru/maths/count_fast/gamma_function.php)
  -- gammaP(a, x) = x**a e**-x sum(n,0,nMax)(x**n / (a)(a+1)..(a+n))
  Function Lower_Gamma(a : in long_float; x : in long_float) return long_float is
      n        : integer := 1;
      term     : long_float := e ** (-x) / a;
      Log_Term : long_float := -(x + Log (a));
      Log_Min  : long_float := -20.0;
      sum      : long_float := 0.0;
      prevSum  : long_float := 0.01;
      Result   : long_float := 0.0;
      xSwitch  : constant long_float := 10.0;
      diffMin  : constant long_float := 1.0/long_float(10E10);
      maxN     : constant integer := 1000;
   begin
      If x < xSwitch then
        While Log_Term > Log_Min And n < maxN
              And Log_Term'Valid And sum'Valid loop
          sum := sum + e**Log_Term;
          Log_Term := Log_Term + Log(x / (a + long_float(n)));
          n := n+1;
        end loop;
      else
        While e**(x)*Abs (sum - prevSum) > diffMin And n < maxN
                  And term'Valid And sum'Valid loop
          prevSum := sum;
          If term'Valid then
            sum := sum + term;
            term := term * x / (a + long_float(n));
          end if;
          n := n+1;
        end loop;
      end if;
     If n > maxN then
        Put_Line("Maths.Lower_Gamma, iteration overflow.");
        If x < xSwitch then
           Put("Sum, Log Term: " & long_float'image(sum));
           Put_Line (",   " & long_float'image (Log_Term));
        else
           Put("Sum, previous sum: " & long_float'image(sum));
           Put_Line (",   " & long_float'image (prevSum));
        end if;
     end if;
     If Not term'Valid Or Not Log_Term'valid Or Not sum'Valid or x**a * sum = 0.0 then
        Put_Line ("Maths.Lower_Gamma result is invalid");
        Put_Line ("a, x, n: " & long_float'image (a) &
                  long_float'image (x) & integer'image (n));
        Put_Line("sum: " & long_float'image(sum));
        If x < xSwitch then
          Put_Line("Log_Term: " & long_float'image(Log_Term));
        else
          Put_Line("prevSum: " & long_float'image(prevSum));
          Put_Line("term: " & long_float'image(term));
        end if;
     end if;

     If x < xSwitch then
        Result := x**a * sum;
     else
        Result := x**a * e**(-x) * sum;
     end if;
     return Result;
  exception
    when anError :  others =>
       Put_Line ("An exception occurred in Maths.Lower_Gamma.");
       Put_Line("term: " & long_float'image(term));
       Put_Line("sum: " & long_float'image(sum));
       Put_Line("prevSum: " & long_float'image(prevSum));
       Put_Line ("a, x, n: " & long_float'image (a) &
                 long_float'image (x) & integer'image (n));
       Put_Line (Exception_Information (anError));
       return 0.0;
  end Lower_Gamma;

-- ------------------------------------------------------------

end Maths;

