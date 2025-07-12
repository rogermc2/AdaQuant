
Package body  Element_Data_Base is
  Package Element_Maps is new  Ada.Containers.Ordered_Maps
                       (Key_Type => String_2, Element_Type => tElement);

   procedure Add_Shell_Item(Basis_Data : in tBasisData) is
     the_Coefficient_Exponent : tExponent_Coefficient;
     the_Shell_Data_Base : tBasis_Shell_Data_Base;
     begin
      the_Coefficient_Exponent.Coefficient := Basis_Data.Coefficient;
      the_Coefficient_Exponent.Exponent  := Basis_Data.Exponent ;
      the_Shell_Data_Base.Insert(Basis_Data.Shell,the_Coefficient_Exponent);
    If Element_Map.Contains(Basis_Data.Element) then
      the_Shell_Data_Base := the_Bases_Data_Base.Element(Basis_Data.Element);
      the_Bases_Data_Base.Replace(Basis_Data.Element, the_Shell_Data_Base);
    else
      the_Bases_Data_Base.Include(Basis_Data.Element, the_Shell_Data_Base);
    end if;
  end Add_Basis_Item;

   function Contains(element : String_2) return Boolean is
   begin
     return Element_Map.Contains(element))
   end;

end Element_Data_Base;
