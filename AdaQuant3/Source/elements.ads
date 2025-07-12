with Types; use Types;

with AdaQuant_Types;

Package Elements is

   Type tElement_Data is record
      Z : AdaQuant_Types.tNuclear_Charge;
   end record;

   function Element_Data_Contains (Element : in tElement) return Boolean;
   function Element_Data (Element : in tElement) return tElement_Data;

 end Elements;
