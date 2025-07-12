with Types; use Types;

Package Initialize_Basis_Set_Loader is

   procedure Initialize (Descriptor_File  : in out tFile_Type;
                         Basis_Set_Symbol : out tBasis_Set_Symbol;
                         Error            : out tSource_Data_Error);
   procedure Initialize (Basis_Set_Name    : in tBasis_Name;
                         Descriptor_File   : in out tFile_Type;
                         Basis_Set_Symbol  : out tBasis_Set_Symbol;
                         Error             : out tSource_Data_Error);

end Initialize_Basis_Set_Loader;
