with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics; use Ada.Numerics;
with Ada.Exceptions; use Ada.Exceptions;

with Tools;

Package body Maths is

   Package Elementary_Float_Functions is new
           Ada.Numerics.Generic_Elementary_Functions(long_float);
   use Elementary_Float_Functions; -- provides ** for float exponents.

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

   -- Binomial coeffient defined by G. Woan equation (2.122)
   -- Factorial(<1) returns 1.
Function Binomial (n : in integer; b : in integer) return long_float is
   begin
      return long_float (Factorial (n)) /
             long_float (Factorial (b) * Factorial (n - b));
   end Binomial;

-- ------------------------------------------------------------

   Function Ceiling (n : in float) return integer is
   begin
      return integer(float'Ceiling(n));
   end Ceiling;

-- --------------------------------------------------------

   Function Chop (value : in long_float) return long_float is
      result : long_float := value;
   begin
      if Abs (value) < 1.0/(1.0E15) then
         result := 0.0;
      end if;
      return result;
   end Chop;

-- --------------------------------------------------------

   Function Chop (Matrix : in tLong_Float_Matrix)
                  return tLong_Float_Matrix is
      Size    : Positive := Matrix'Length;
      result  : tLong_Float_Matrix := Matrix;

   begin
      For row in 1..Size loop
          For Col in 1..Size loop
             if Abs (result(row,col)) < 1.0/(1.0E15) then
                 result(row,col) := 0.0;
             end if;
         end loop;
      end loop;
      return result;
   end Chop;

-- --------------------------------------------------------

   -- Dual_Binomial returns the coeeficient Cj of x**j
   -- in the expansion of (x+a)**l (x+b)**m
   --       from Taketa, Huzinaga & O-ohata equation (2.4)
   -- gnatAda represents 0.0**0 as 0.0, whereas we require it to be 1.0
   Function Dual_Binomial (PA    : in long_float; PB  : in long_float;
                           l     : in integer;    m   : in integer;
                           Coeff : in integer) return long_float is
      sum : long_float := 0.0;
      lm  : integer := l + m;
      li  : long_float;
   begin
      If Chop(PA) = 0.0 or Chop(PB) = 0.0 then
         If Chop(PA) /= 0.0 then
            For j in 0 .. l loop
              If lm - j = Coeff then
                sum := sum + Binomial(l, j) * PA**j;
              end if;
            end loop;
            -- sum := Binomial(m, 0) * sum; (= 1.0 sum)
         elsif Chop(PB) /= 0.0 then
            For j in 0 .. m loop
              If lm - j = Coeff then
                sum := sum + PB**j * Binomial(m, j);
              end if;
            end loop;
            -- sum := Binomial(l, 0) * sum  (= 1.0 sum)
         elsif lm = Coeff then
            -- Binomial (l, 0) * Binomial (m, 0) = 1
            sum := 1.0;
            -- else sum := 0.0;
            -- If l = 0 and m = 0 then (x+0)^l(x+0)^m = x^lm
            -- thus, coefficient of x^lm is 1.0,
            -- any other coefficent of x^q (q /= lm) is zero.s
         end if;
      else
        For i in 0 .. l loop
            li := Binomial (l, i);
            For j in 0 .. m loop
              If lm - i - j = Coeff then
               sum := sum + li*Binomial(m, j) * PA**i * PB**j;
              end if;
            end loop;
         end loop;
      end if;
      return sum;
   exception
      when anError :  others =>
        Put_Line("An exception occurred in Maths.Dual_Binomial.");
        Put_Line (Exception_Information (anError));
        return 0.0;
   end Dual_Binomial;

-- --------------------------------------------------------

   -- Factorial function from PyQuant, cints.c (line 310)
   Function Factorial (n : in integer) return integer is
      Result : Integer := 1;
  begin
    if n > 1 then
      Result := n * Factorial(n - 1);
    end if;
    return result;
  end Factorial;

-- ------------------------------------------------------------

   Function Factorial_2 (n : in integer) return integer is
      -- double factorial function n!! = 1 * 3 * 5 *  .. . * n *
      Result : integer := 1;
   begin
      if n > 1 then
         Result := n * Factorial_2 (n - 2);
      end if;
      return Result;
   end Factorial_2;

-- ------------------------------------------------------------

   Function fFact_2 (n : in integer) return long_float is
   begin
      return long_float(Factorial_2(n));
   end fFact_2;

-- ------------------------------------------------------------

   Function Factorial_Ratio2 (a : in integer; b : in integer) return long_float is
   begin
      return long_float (Factorial (a)) / long_float (Factorial (b))
        / long_float (Factorial (a - 2 * b));
   end Factorial_Ratio2;

-- ------------------------------------------------------------

   Function Floor (n : in float) return integer is
   begin
      return integer(float'Floor(n));
   end Floor;

-- ------------------------------------------------------------

end Maths;

