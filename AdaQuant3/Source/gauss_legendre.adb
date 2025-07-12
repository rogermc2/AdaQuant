Package body Gauss_Legendre is

   ngau : Positive := 18;
   Gauss_Legendre : Array (1 .. ngau, 1..2) of long_float;

   function GL_Abscissa (index : in integer) return long_float is
   begin
      return Gauss_Legendre(index,1);
   end GL_Abscissa;

   function GL_Length return Positive is
   begin
      return ngau;
   end GL_Length;

   function GL_Weight (index : in integer) return long_float is
   begin
      return Gauss_Legendre(index,2);
   end GL_Weight;
begin
    Gauss_Legendre(1, 1) := 0.0021695375159141994;
    Gauss_Legendre(1, 2) := 0.0055657196642445571;
    Gauss_Legendre(2, 1) := 0.011413521097787704;
    Gauss_Legendre(2, 2) := 0.012915947284065419;
    Gauss_Legendre(3, 1) := 0.027972308950302116;
    Gauss_Legendre(3, 2) := 0.020181515297735382;
    Gauss_Legendre(4, 1) := 0.051727015600492421;
    Gauss_Legendre(4, 2) := 0.027298621498568734;
    Gauss_Legendre(5, 1) := 0.082502225484340941;
    Gauss_Legendre(5, 2) := 0.034213810770299537;
    Gauss_Legendre(6, 1) := 0.12007019910960293;
    Gauss_Legendre(6, 2) := 0.048075750923643261;
    Gauss_Legendre(7, 1) := 0.16415283300752470;
    Gauss_Legendre(7, 2) := 0.047235083490265582;
    Gauss_Legendre(8, 1) := 0.21442376986779355;
    Gauss_Legendre(8, 2) := 0.053244713977759692;
    Gauss_Legendre(9, 1) := 0.27051082840644336;
    Gauss_Legendre(9, 2) := 0.058860144245324798;
    Gauss_Legendre(10, 1) := 0.33199876341447887;
    Gauss_Legendre(10, 2) := 0.064039797355015485;
    Gauss_Legendre(11, 1) := 0.39843234186401943;
    Gauss_Legendre(11, 2) := 0.068745323835736408;
    Gauss_Legendre(12, 1) := 0.46931971407375483;
    Gauss_Legendre(12, 2) := 0.072941885005653087;
    Gauss_Legendre(13, 1) := 0.54413605556657973;
    Gauss_Legendre(13, 2) := 0.076598410645870640;
    Gauss_Legendre(14, 1) := 0.62232745288031077;
    Gauss_Legendre(14, 2) := 0.079687828912071670;
    Gauss_Legendre(15, 1) := 0.70331500465597174;
    Gauss_Legendre(15, 2) := 0.082187266704339706;
    Gauss_Legendre(16, 1) := 0.78649910768313447;
    Gauss_Legendre(16, 2) := 0.084078218979661945;
    Gauss_Legendre(17, 1) := 0.87126389619061517;
    Gauss_Legendre(17, 2) := 0.085346685739338721;
    Gauss_Legendre(18, 1) := 0.95698180152629142;
    Gauss_Legendre(18, 2) := 0.085983275670394821;
end Gauss_Legendre;
