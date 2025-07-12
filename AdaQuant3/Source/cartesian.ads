Package Cartesian is
   Type pFloat is new long_float;
   Type tDistance is new pFloat range 0.0..pFloat'last;
   Type tCartesian_Coordinates is record
      x           : pFloat;
      y           : pFloat;
      z           : pFloat;
   end record;

   function "+" (Coord_1 : in tCartesian_Coordinates;
                 Coord_2 : in tCartesian_Coordinates)
                 return tCartesian_Coordinates;
   function "-" (Coord_1 : in tCartesian_Coordinates;
                 Coord_2 : in tCartesian_Coordinates)
                 return tCartesian_Coordinates;

   function "*" (mult  : in float;
                 Coord : in tCartesian_Coordinates)
                 return tCartesian_Coordinates;
   function "*" (mult  : in long_float;
                 Coord : in tCartesian_Coordinates)
                 return tCartesian_Coordinates;
   function "/" (Coord : in tCartesian_Coordinates;
                 div   : in float)
                 return tCartesian_Coordinates;
   function "/" (Coord : in tCartesian_Coordinates;
                 div   : in long_float)
                 return tCartesian_Coordinates;
   function Distance (Pos_1 : in tCartesian_Coordinates;
                      Pos_2 : in tCartesian_Coordinates)
                      return tDistance;
   function DistanceSq (Pos_1 : in tCartesian_Coordinates;
                        Pos_2 : in tCartesian_Coordinates)
                        return long_float;
   procedure Print_Coordinates (Label    : in String;
                                Position : in tCartesian_Coordinates);
end Cartesian;
