with System;

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Generic_Elementary_Functions; use Ada.Numerics;
with Ada.Exceptions; use Ada.Exceptions;

with Gauss_Legendre;

Package body Gamma is

   Package tLong_Float_Elementary_Functions is
     new Ada.Numerics.Generic_Elementary_Functions (Long_Float);
   use tLong_Float_Elementary_Functions; -- provides ** for long_float exponents.

      Max_Dig : constant Integer := long_float'Digits;
      LF_Min  : constant long_float := 10.0 ** (-Max_Dig);
      LF_Max  : constant long_float := 10.0 ** Max_Dig;
      Min_Val : constant long_float := 10.0 ** (-Max_Dig / 2);

   Function Approx_Gamma(a : in long_float; x : in long_float) return long_float;
   Function Continued_Fraction_Gamma (a : in long_float; x : in long_float)
                                     return long_float;
   Function GammaP (a : in long_float; x : in long_float) return long_float;
   Function Serial_Gamma(a : in long_float; x : in long_float) return long_float;

-- ------------------------------------------------------

   Function Approx_Gamma(a : in long_float; x : in long_float) return long_float is
   use Gauss_Legendre;
      LnGamma : long_float := lGamma (a);
      a1      : long_float := a - 1.0;
      Log_a1  : long_float := Log(a1);
      xu      : long_float;
      t       : long_float;
      sum     : long_float := 0.0;
      Result  : long_float := 0.0;
      j       : integer := 0;
   begin
      If Not LnGamma'Valid then
         Put_Line("Gamma.Approx_Gamma result is invalid");
         Put_Line("a: " & long_float'image(a));
         Put_Line("LnGamma: " & long_float'image(LnGamma));
      else
         If x > a1 then
            xu := long_float'Max (a1 + 11.5 * SqRt (a1), x + 6.0 * SqRt (a1));
         else
            xu := long_float'Max(0.0, long_float'Min(a1-7.5*SqRt(a1), x - 5.0*SqRt(a1)));
         end if;
         While j < GL_Length loop
            t := x + (xu - x) * e ** GL_Abscissa (j);
            sum := sum + GL_Weight(j)*e**(-t+a1*(1.0+Log(t)-Log_a1));
            j := j + 1;
         end loop;
         Result := sum * (xu - x) * e ** (a1 * (Log_a1 - 1.0)-LnGamma);
         If Result > 0.0 then
            Result := 1.0 -Result;
         end if;
      end if;
      return Result;
   end Approx_Gamma;

-- ------------------------------------------------------------

   Function Continued_Fraction_Gamma (a : in long_float; x : in long_float)
                                      return long_float is
   use System;
      LnGamma : long_float := lGamma (a);
      an      : long_float;
      b       : long_float := x + 1.0 - a;
      c       : long_float := LF_Max;
      d       : long_float := 1.0 / b;
      del     : long_float := LF_Max;
      h       : long_float := d;
      count   : long_float := 1.0;
      Result  : long_float := 0.0;
   begin
      If Not LnGamma'Valid then
         Put_Line("Gamma.Continued_Fraction_Gamma result is invalid");
         Put_Line("a: " & long_float'image(a));
         Put_Line("LnGamma: " & long_float'image(LnGamma));
      else
         While Abs(del - 1.0) > Min_Val loop
            an := -Count * (Count - a);
            b := b + 2.0;
            d := an * d + b;
            If Abs(d) < LF_Min then
               d := LF_Min;
            end if;
            c := b + an / c;
            If Abs(c) < LF_Min then
               c := LF_Min;
            end if;
            d := 1.0 / d;
            del := d * c;
            h := h *del;
            count := Count + 1.0;
         end loop;
         Result := 1.0-h*e**(-x+a*Log(x)-LnGamma);
      end if;
      return Result;
  exception
    when anError :  others =>
       Put_Line ("An exception occurred in Gamma.Continued_Fraction_Gamma.");
       Put_Line("Continued_Fraction_Gamma.a, x: " & long_float'image(a) & long_float'image(x));
       Put_Line (Exception_Information (anError));
       return 0.0;
   end Continued_Fraction_Gamma;

-- ------------------------------------------------------------

   Function Fgamma (m : in integer; x : in long_float) return long_float is
      val  : long_float := 0.0;
      mlf  : long_float := long_float (m) + 0.5;
      lfX  : long_float := x;
   begin
      If x < 0.0 or not x'valid then
        Put_Line("Gamma.Fgamma, negative x is invalid. x: "
                 & long_float'image (x));
      elsif m < 0 or not m'valid  then
        Put_Line ("Gamma.Fgamma, negative m is invalid. m: "
                 & integer'image(m));
      elsif x = 0.0 then
        val := 1.0/long_float(2*m + 1);
      else
        val := GammaP (mlf, lfX) * Gamma (mlf) / (2.0 * lfX ** mlf);
      end if;

      If Not val'Valid then
         Put_Line("Gamma.Fgamma result is invalid");
         Put_Line("val: " & long_float'image(val));
         Put("m, x: " & integer'image(m));
         Put_Line (",   " & long_float'image (x));
         If x > 0.0 then
           Put("Gamma.Fgamma, GammaP, Gamma: " & long_float'image(GammaP (mlf, lfX)));
           Put_Line (",   " & long_float'image (Gamma (mlf)));
         end if;
      end if;
      return val;
   exception
       when anError :  others =>
         Put_Line ("An exception occurred in Gamma.Fgamma.");
         Put_Line (Exception_Information (anError));
         return 0.0;
   end Fgamma;

-- ------------------------------------------------------------

   -- Regularized incomplete Gamma functions from Wikipedia
   -- (Numerical recipes calls these incomplete Gamma functions)

  Function gammaP (a : in long_float; x : in long_float) return long_float is
      A_Switch : constant long_float := 100.0;
      Result   : long_float := 0.0;
  begin
    If x < 0.0 then
      Put_Line("Gamma.GammaP, negative x is invalid. x: "
               & long_float'image (x));
    elsif a <= 0.0 then
      Put_Line ("Gamma.GammaP, a must be > 0. a: "
               & long_float'image(a));
    elsif x > 0.0 then
      -- Result is 0 for x = 0.
      If a > A_Switch then
        Result := Approx_Gamma(a, x);
      elsif x < a + 10.0 then
        Result := Serial_Gamma (a, x);
      else
        Result := Continued_Fraction_Gamma (a, x);
      end if;
    end if;
    return Result;
  exception
    when anError :  others =>
       Put_Line ("An exception occurred in Gamma.gammaP.");
       Put_Line("gammaP.a, x: " & long_float'image(a) & long_float'image(x));
       Put_Line (Exception_Information (anError));
       return 0.0;
   end gammaP;

-- ------------------------------------------------------------

   Function Serial_Gamma(a : in long_float; x : in long_float)
                         return long_float is
      LnGamma : long_float := lGamma (a);
      ap      : long_float := a;
      Sum     : long_float := 1.0/a;
      del     : long_float := Sum;
      Result  : long_float := 0.0;
   begin
      If Not LnGamma'Valid then
         Put_Line("Gamma.Serial_Gamma result is invalid");
         Put_Line("a: " & long_float'image(a));
         Put_Line("LnGamma: " & long_float'image(LnGamma));
      else
         While Abs (del) > Abs (Sum) * Min_Val loop
            ap := ap + 1.0;
            del := del * x / ap;
            sum := sum + del;
         end loop;
         Result := Sum*e**(-x+a*Log(x)-LnGamma);
      end if;
      return Result;
  exception
    when anError :  others =>
       Put_Line ("An exception occurred in Gamma.Serial_Gamma.");
       Put_Line("Serial_Gamma.a, x: " & long_float'image(a) & long_float'image(x));
       Put_Line (Exception_Information (anError));
       return 0.0;
   end Serial_Gamma;

-- ------------------------------------------------------

end Gamma;

