
Package body fileHandler is

function Open_Data_File(Data_File_Name : String) return Boolean is
  File_OK       : Boolean := False;
  Input : String(1..80);
  Len   : Integer;
begin
  Open(Data_File, Mode => In_File, Name => Data_File_Name);
   File_OK := is_Open(Data_File);
   If File_OK then
    Get_Line(Data_File, Input, Len);
    Len := Len - 1;
   end if;
  return File_OK;
exception
  when Name_Error =>
    Put("I'm sorry.  The file " & Data_File_Name);
    Put_Line(" seems to be missing.");
  return False;
  when others =>
    Put("I'm sorry.  The file " & Data_File_Name);
    Put_Line(" seems to have the wrong form.");
  return False;
end Open_Data_File;

procedure Open_User_File (User_File_Name : String) is
  Input : String(1 .. 6);
  Len   : Integer;
begin
  Text_IO.Open(User_File, Mode => Text_IO.In_File, Name => User_File_Name);
  Text_IO.Get_Line(User_File, Input, Len);
  Quitting_SN      := Integer'Value(Input(1 .. Len-1));
  Old_SN           := Quitting_SN;
  Text_IO.Get_Line(User_File, Input, Len);
  Foregrnd_Color   := Color'Val(Integer'Value(Input(1 .. Len-1)));
  Fore_Color_Digit := Input(2);
  Normal_Colors(6) := Fore_Color_Digit;
  Text_IO.Get_Line(User_File, Input, Len);
  Backgrnd_Color   := Color'Val(Integer'Value(Input(1 .. Len-1)));
  Back_Color_Digit := Input(2);
  Normal_Colors(9) := Back_Color_Digit;
  Text_IO.Get_Line(User_File, Input, Len);
  Border_Color     := Color'Val(Integer'Value(Input(1 .. Len-1)));
  Text_IO.Close(User_File);
exception
  when Constraint_Error =>
    Put_Line("Constraint error raised for " & User_File_Name);
    Files_OK := False;
  when Text_IO.Name_Error =>
    Put("I'm sorry.  The file " & User_File_Name);
    Put_Line(" seems to be missing.");
    Files_OK := False;
  when others =>
    Put("I'm sorry.  The file " & User_File_Name);
    Put_Line(" seems to have the wrong form.");
    Files_OK := False;
end Open_User_File;

end fileHandler;
