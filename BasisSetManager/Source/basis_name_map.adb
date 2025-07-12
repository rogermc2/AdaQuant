with Ada.Text_IO; use Ada.Text_IO;
with Ada.Strings; use Ada.Strings;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Exceptions; use Ada.Exceptions;

Package body Basis_Name_Map is
   Package Basis_Names is new  Ada.Containers.Ordered_Maps
                       (Key_Type=>Unbounded_String, Element_Type=>tBasis_Set_Symbol);
   Basis_Name_Map :  Basis_Names.Map := Basis_Names.Empty_Map;
   Type Map_Cursor is new Basis_Names.Cursor;

   function Contains (basis_name : tBasis_Name)  return Boolean is
   begin
      return Basis_Name_Map.Contains (basis_name);
   exception
     when others
           => Put_Line ("An exception occurred in Basis_Name_Map.Contains!");
              return False;
   end Contains;

   function Element(Basis_Name_Key : tData_Base_Name) return tBasis_Set_Symbol is
   begin
     return Basis_Name_Map.Element(Basis_Name_Key);
   exception
     when others
           => Put_Line ("An exception occurred in Basis_Name_Map.Element!");
              return Unknown;
   end;

   Function Get_Basis_Set_Symbol (text_name :  tBasis_Name)
                                  return tBasis_Set_Symbol is
      name             : Unbounded_String;
      BasisDimension   : Integer :=1;
      Basis_Set_Symbol : tBasis_Set_Symbol;
   begin
      name := To_Unbounded_String(To_Lower (To_String(text_name)));
      if Basis_Name_Map.Contains (name) then
         Basis_Set_Symbol:= Basis_Name_Map.Element(name);
      else
      	 Put_Line ("Basis_Name_Map.get_basis_set_symbol, name of basis set "
                           & To_String(text_name) & " is unknown");
      end If;
      return Basis_Set_Symbol;

   exception
      when anError : others =>
        Put_Line ("An exception occurred in Basis_Name_Map.get_basis_set_symbol! ");
        Put_Line(Exception_Information(anError));
        return Unknown;
    end get_basis_set_symbol;

  begin
    Basis_Name_Map.Include(To_Unbounded_String("6-31g**"), p631ss);
    Basis_Name_Map.Include(To_Unbounded_String("6-31g(d,p)"), p631ss);
    Basis_Name_Map.Include(To_Unbounded_String("6-31g**++"), p631ppss);
    Basis_Name_Map.Include(To_Unbounded_String("6-31g++**"), p631ppss);
    Basis_Name_Map.Include(To_Unbounded_String("6-311g**"), p6311ss);
    Basis_Name_Map.Include(To_Unbounded_String("6-311g++(2d,2p)"), p6311pp_2d_2p);
    Basis_Name_Map.Include(To_Unbounded_String("6-311g++(3d,3p)"), p6311pp_3d_3p);
    Basis_Name_Map.Include(To_Unbounded_String("6-311g++(3df,3pd)"), p6311pp_3df_3pd);
    Basis_Name_Map.Include(To_Unbounded_String("3-21g"), p321);
    Basis_Name_Map.Include(To_Unbounded_String("sto3g"), sto3g);
    Basis_Name_Map.Include (To_Unbounded_String ("sto-3g"), sto3g);
    -- sto-3gso contains data from Szabo and Ostlunds's Appendix B
    Basis_Name_Map.Include(To_Unbounded_String("sto-3gso"), sto3gso);
    Basis_Name_Map.Include(To_Unbounded_String("sto-6g"), sto6g);
    Basis_Name_Map.Include(To_Unbounded_String("lacvp"), lacvp);
    Basis_Name_Map.Include(To_Unbounded_String("ccpvdz"), ccpvdz);
    Basis_Name_Map.Include(To_Unbounded_String("cc-pvdz"), ccpvdz);
    Basis_Name_Map.Include(To_Unbounded_String("ccpvtz"), ccpvtz);
    Basis_Name_Map.Include(To_Unbounded_String("cc-pvtz"), ccpvtz);
    Basis_Name_Map.Include(To_Unbounded_String("dzvp"), dzvp);
end Basis_Name_Map;
