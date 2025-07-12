with Ada.Numerics.Elementary_Functions;
use Ada.Numerics.Elementary_Functions;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

Package body Cartesian is

  function "+" (Coord_1 : in tCartesian_Coordinates;
                Coord_2 : in tCartesian_Coordinates)
                return tCartesian_Coordinates is
   begin
      return (Coord_1.x + Coord_2.x,
              Coord_1.y + Coord_2.y,
              Coord_1.z + Coord_2.z );
   end "+";

-- ------------------------------------------------------

  function "-" (Coord_1 : in tCartesian_Coordinates;
                Coord_2 : in tCartesian_Coordinates)
                return tCartesian_Coordinates is
   begin
      return (Coord_1.x - Coord_2.x,
              Coord_1.y - Coord_2.y,
              Coord_1.z - Coord_2.z );
   end "-";

-- ------------------------------------------------------

   function "*" (mult : in float;
                Coord : in tCartesian_Coordinates)
                return tCartesian_Coordinates is
   begin
      return (pfloat(mult) * Coord.x,
              pfloat(mult) * Coord.y,
              pfloat(mult) * Coord.z );
   end "*";

   -- ------------------------------------------------------

   function "*" (mult : in long_float;
                Coord : in tCartesian_Coordinates)
                return tCartesian_Coordinates is
   begin
      return (pfloat(mult) * Coord.x,
              pfloat(mult) * Coord.y,
              pfloat(mult) * Coord.z );
   end "*";

-- ------------------------------------------------------

   function "/" (Coord : in tCartesian_Coordinates;
                 div   : in float)
                 return tCartesian_Coordinates is
   begin
      return (Coord.x / pfloat(div),
              Coord.y / pfloat(div),
              Coord.z / pfloat(div));
   end "/";

-- ------------------------------------------------------

   function "/" (Coord : in tCartesian_Coordinates;
                 div   : in long_float)
                 return tCartesian_Coordinates is
   begin
      return (Coord.x / pfloat(div),
              Coord.y / pfloat(div),
              Coord.z / pfloat(div));
   end "/";

-- ------------------------------------------------------

  function DistanceSq (Pos_1 : in tCartesian_Coordinates;
                       Pos_2 : in tCartesian_Coordinates)
                     return Long_Float is
   begin
      return Long_Float((Pos_1.x - Pos_2.x)**2
                 + (Pos_1.y - Pos_2.y)**2
                 + (Pos_1.z - Pos_2.z)**2);
   exception
      when  anError : others =>
         Put_Line ("An exception occurred in Cartesian.DistanceSq!");
         Put_Line (Exception_Information (anError));
         return 0.0;
   end DistanceSq;

-- ------------------------------------------------------

  function Distance (Pos_1 : in tCartesian_Coordinates;
                     Pos_2 : in tCartesian_Coordinates)
                     return tDistance is
   begin
      return tDistance(SqRt(float(Pos_1.x - Pos_2.x)**2
                 + float(Pos_1.y - Pos_2.y)**2
                 + float(Pos_1.z - Pos_2.z)**2));
   exception
      when  anError : others =>
         Put_Line ("An exception occurred in Cartesian.Distance!");
         Put_Line (Exception_Information (anError));
         return 0.0;
   end Distance;

-- ------------------------------------------------------

   procedure Print_Coordinates (Label    : in String;
                                Position : in tCartesian_Coordinates) is
   begin
      Put (Label);
      Put ("x, y, z: " & Float'image (float(Position.x)));
      Put (",   : " & float'image (float(Position.y)));
      Put_Line (",   : " & Float'image (float(Position.z)));
   end Print_Coordinates;

end Cartesian;
