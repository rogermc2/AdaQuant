with ada.text_io; use ada.text_io;
with Ada.Containers.Ordered_Maps;

with Types; use Types;
with Shell_Data_Base; use Shell_Data_Base;

Package Data_Base is
       Function Add_Basis_Set(Basis_Name : tBasis_Name;Source_file : File_Type) return Boolean;
end Data_Base;
