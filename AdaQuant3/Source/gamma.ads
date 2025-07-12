
Package Gamma is

   Function Fgamma (m : in integer; x : in long_float) return long_float;

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
   function gsl_lgamma(x : in long_float) return long_float;

private
   pragma Import (C, erf, "erf");
   pragma Import (C, erfc, "erfc");
   pragma Import (C, gamma, "tgamma");
   pragma Import (C, lgamma, "lgamma");
   pragma Import (C, lgamma_r, "lgamma_r");

   pragma Import (C, gsl_gamma, "gsl_sf_gamma");
   pragma Import (C, gsl_lgamma, "gsl_sf_lngamma");

end Gamma;
