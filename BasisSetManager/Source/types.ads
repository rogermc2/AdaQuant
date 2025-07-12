with Ada.Strings.Unbounded; use  Ada.Strings.Unbounded;
With Ada.Text_IO;

Package Types is

  Subtype tFile_Type is Ada.Text_IO.File_Type;
  Subtype tFile_Type_Ptr is Ada.Text_IO.File_Access;
  Subtype tFile_Name is Unbounded_String;

  Subtype tData_Base_Name is Unbounded_String;
  Subtype tBasis_Set_Name is Unbounded_String;
  Subtype tBasis_Name is Unbounded_String;
  Subtype tCoefficient is long_float;
  Subtype tExponent is long_float;
  Type    tNormalization is new long_float;
  Subtype tString_2 is String (1 .. 2);
  Subtype tEnergy_Level is Positive;

  Type tFile_Result is (No_Error, eName_Error, eStatus_Error, eEnd_Of_File);
  Type tSource_Data_Error is
         (No_Error, eError, eArgument_Error, eCancelled, eConstraint_Error,
          eFile_Name_Error, eStatus_Error, eEnd_Error, eEnd_Of_File, eUnknown_Basis_Set,
          eShell_Symbol_Error, eElement_Symbol_Error, eLevel_Symbol_Error,
          eDirectory_Name_Error, eData_Error, eUnits_Error);

  Type tBasis_Set_Symbol is (Unknown, p631ss, p631ppss, p6311ss, p6311pp_2d_2p,
           p6311pp_3d_3p, p6311pp_3df_3pd, p321, sto3g, sto3gso, sto6g, lacvp,
           ccpvdz, ccpvtz, dzvp);
  Type tElement is (H,He,Li,Be,B,C,N,O,F,Ne,Na,Mg,Al,Si,P,S,Cl,Ar,
                                        K,Ca,Sc,Ti,V,Cr,Mn,Fe,Co,Ni,Cu,Zn,Ga,Ge,As,Se,Br,Kr);
  Type tElectron_Shell is (none, s, p, d, f);
  Type tBasis_Shell_Code is (s, sp, d);
  Type tElectron_Key is new String (1 .. 4);
  Type tInteger_Range is record
     First : Integer;
     Last  : Integer;
  end record;

  Type tBasis_Data is record
     element      : tElement;
     energy_level : tEnergy_Level;
     shell        : tElectron_Shell;
     exponent     : tExponent;
     normed_coeff : tCoefficient;
  end record;

     One_Blank : constant String(1..1) := " ";
     Two_Blanks : constant String(1..2) := "  ";

end Types;
