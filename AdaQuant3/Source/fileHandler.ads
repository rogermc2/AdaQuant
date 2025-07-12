
with ada.text_io; use ada.text_io;
with ada.integer_text_io; use ada.integer_text_io;
with ada.strings.bounded; use  ada.strings.bounded;

Package fileHandler is

  Data_File     : File_Type;
  Hpos           : Integer;

  LF             : constant Character := Character'Val(10);       -- Line Feed.
  CR             : constant Character := Character'Val(13);    -- Carr. Return.

function Open_Data_File(Data_File_Name : String) return Boolean;

end fileHandler;
