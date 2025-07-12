with Ada.Containers.Ordered_Maps;

with Types; use Types;

Package Shell_Data_Base is
  Type tBasis_Shell_Data_Base is private;
  procedure Add_Basis_Item(Basis_Data : in tBasisData);

 private
  Package Basis_Shell_Maps is new  Ada.Containers.Ordered_Maps
                       (Key_Type => String_2, Element_Type => tBasisShell);

  Type tBasis_Shell_Data_Base is new Basis_Shell_Maps.Map with Null record;

end Shell_Data_Base;
