
Package body Shell_Data_Base is
  Package Shell_Data_Base is new  Ada.Containers.Ordered_Maps
                       (Key_Type => tBasisShell, Element_Type => tExponent_Coefficient);
  the_Shell_Data_Base : Shell_Data_Base.Map := Shell_Data_Base.Empty_Map;

  procedure Add_Basis_Item(Basis_Data : in tBasisData) is
     the_Coefficient_Exponent : tExponent_Coefficient;
     the_Shell_Data_Base : tBasis_Shell_Data_Base;
     begin
      the_Coefficient_Exponent.Coefficient := Basis_Data.Coefficient;
      the_Coefficient_Exponent.Exponent  := Basis_Data.Exponent ;
      the_Shell_Data_Base.Include(Basis_Data.Shell,the_Coefficient_Exponent);
    If Element_Map.Contains(Basis_Data.Element) then
      the_Shell_Data_Base := the_Bases_Data_Base.Element(Basis_Data.Element);
      the_Bases_Data_Base.Replace(Basis_Data.Element, the_Shell_Data_Base);
    else
      the_Bases_Data_Base.Include(Basis_Data.Element, the_Shell_Data_Base);
    end if;
  end Add_Basis_Item;

end Shell_Data_Base;
