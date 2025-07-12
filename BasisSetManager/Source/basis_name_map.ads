with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Ordered_Maps; use Ada.Containers;

with Types; use Types;

Package Basis_Name_Map is

  function Element(Basis_Name_Key : tData_Base_Name) return tBasis_Set_Symbol;
  function get_basis_set_symbol (text_name :  tBasis_Name)
                                 return tBasis_Set_Symbol ;
  function Contains(basis_name :  tBasis_Name)  return Boolean;

  end Basis_Name_Map;
