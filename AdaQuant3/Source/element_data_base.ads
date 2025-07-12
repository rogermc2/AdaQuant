with ada.text_io; use ada.text_io;
with Ada.Containers.Ordered_Maps;

with Types; use Types;
with Shell_Data_Base; use Shell_Data_Base;

Package Element_Data_Base is
  Package Element_Maps is new  Ada.Containers.Ordered_Maps
                       (Key_Type => String_2, Element_Type => tElement);
   Element_Map :  Element_Maps.Map := Element_Maps.Empty_Map;

   procedure Add_Shell_Item(Basis_Data : in tBasisData);
   function Contains(element : String_2) return Boolean ;
end Element_Data_Base;
