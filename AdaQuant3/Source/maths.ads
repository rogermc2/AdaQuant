with Interfaces.C; use Interfaces.C;
with Ada.Numerics.Generic_Elementary_Functions;

with Matrix_Types; use Matrix_Types;
with Cartesian; use  Cartesian;

Package Maths is
   Package tFloat_Elementary_Functions is
     new Ada.Numerics.Generic_Elementary_Functions (Long_Float);
   Type tReal_Cubic_Array is
     Array (Natural range <>, Natural range <>, Natural range <>) of long_float;

   Type tInteger_Vector is Array (Natural range <>) of Integer;

   Type tMath_Procedure is access Procedure (F : in Float);

   Type tPower is record
      x_Power : Integer;
      y_Power : Integer;
      z_Power : Integer;
   end record;

   Function "+" (Power_1 : in tPower; Power_2 : in tPower)
                 return tPower;
   Function "-" (Power_1 : in tPower; Power_2 : in tPower)
                 return tPower;
   Function Binomial (n : in integer; b : in integer) return long_float;
   Function Ceiling (n : in float) return integer;
   Function Chop (value : in long_float) return long_float;
   Function Chop (Matrix : in tLong_Float_Matrix)
                                      return tLong_Float_Matrix;
   Function Dual_Binomial (PA    : in long_float;   PB : in long_float;
                           l     : in integer; m  : in integer;
                           Coeff : in integer) return long_float;
   Function Factorial (n : in integer) return integer;
   Function Factorial_2 (n : in integer) return integer;
   Function Factorial_Ratio2 (a : in integer; b : in integer) return long_float;
   Function fFact_2 (n : in integer) return long_float;
   Function Floor (n : in float) return integer;

   -- C functions from /usr/local/ada-4.4/include/c++/4.4.3/cmath
   -- From man gamma:
   -- tgamma() calculates the gamma function of x.
   -- lgamma() calculates the natural logorithm
   --    of the absolute value of the gamma function of x.
   -- gamma() is the same function as tgamma. Its use is deprecated.
   -- lgamma_r() is a thread-safe alternative to lgamma().
   -- tgamma(0) returns +-infinity and raises the "divide-by-zero"
   --    floating - point exception.
   -- tgamma(x) returns a NaN and raises the "invalid" floating-point exception
   --    if x is a negative integer.
   -- tgamma(-infinity) returns a NaN and raises the "invalid" floating-point
   --    exception.
   -- tgamma(+infinity) returns +infinity.
   -- tgamma(x) generates a domain error if x is a negative integer or
   --    if the result cannnot be respresented when x is 0.
   -- A range error may occur if the magnitude of x is too large or too small.
   -- lgamma(1) returns +0.
   -- lgamma(2) returns +0.
   -- lgamma(x) returns +infinity and raises the "divide-by-zero"
   -- floating - point exception if x is a negative integer or 0.
   -- lgamma(+-infinity) returns +infinity.
   -- lgamma(x) generates a range error if x is too large.
   -- A range error may occur if x is a negative integer or 0.

   -- See also man math

   function erf (x : in float) return float;
   function erfc(x : in float) return float;
   function gamma(x : in long_float) return long_float;
   function lgamma(x : in long_float) return long_float;
   function lgamma_r (x : in float) return float;

   function gsl_gamma(x : in long_float) return long_float;
   function gsl_lgamma (x : in long_float) return long_float;

   -- gsl_deriv_central computes the numerical derivative of the function f
   -- at the point x using an adaptive central difference algorithm with a
   -- step-size of h.
   -- The initial value of h is used to estimate an optimal step-size,
   -- based on the scaling of the truncation error and round-off error
   -- in the derivative calculation.
   -- The derivative is computed using a 5-point rule for equally spaced abscissae
   -- at x-h, x-h/2, x, x+h/2 and x+h with an error estimate taken from the
   -- difference between the 5-point rule and the corresponding 3-point rule
   -- x-h, x, x+h.
   -- Note that the value of the function at x does not contribute to the
   -- derivative calculation so only 4 points are actually used.

   Procedure gsl_deriv_backward (aFunction : in not null tMath_Procedure;
                                x   : in long_float; Step_Size : in long_float;
                                Result : out long_float; Error : out long_float);
   Procedure gsl_deriv_central (aFunction : in not null tMath_Procedure;
                                x   : in long_float; Step_Size : in long_float;
                                Result    : out long_float; Error : out long_float);

   Procedure gsl_deriv_forward (aFunction : in not null tMath_Procedure;
                                x   : in long_float; Step_Size : in long_float;
                                Result    : out long_float; Error : out long_float);


private
   pragma Import (C, erf, "erf");
   pragma Import (C, erfc, "erfc");
   pragma Import (C, gamma, "tgamma");
   pragma Import (C, lgamma, "lgamma");
   pragma Import (C, lgamma_r, "lgamma_r");

   pragma Import (C, gsl_gamma, "gsl_sf_gamma");
   pragma Import (C, gsl_lgamma, "gsl_sf_lngamma");
   pragma Import (C, gsl_deriv_backward, "gsl_deriv_backward");
   pragma Import (C, gsl_deriv_central, "gsl_deriv_central");
   pragma Import (C, gsl_deriv_forward, "gsl_deriv_forward");

end Maths;
