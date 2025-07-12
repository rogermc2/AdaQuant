
With Types; use Types;

Package Element_Shell_Symbol_Maps is

   function Element_Symbol_Map_Contains (Element : in tString_2) return Boolean;
   function Shell_Symbol_Map_Contains (Shells : in tString_2) return Boolean;
   function Shells_Code (Shells : in tString_2) return tBasis_Shell_Code;
   function Element_Symbol (Element : in tString_2) return tElement;

 end Element_Shell_Symbol_Maps;
