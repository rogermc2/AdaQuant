with ada.text_io; use ada.text_io;

with Types; use Types;

Package Bases_Data_Base is
  function addToDataBase(File_Name : tFile_Name) return tFile_Result;
  function get_basis(Basis_Name : tBasis_Name) return tBasis;
end Bases_Data_Base;
