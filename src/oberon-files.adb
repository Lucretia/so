------------------------------------------------------------------------------------------------------------------------
--  See COPYING for licence information.
------------------------------------------------------------------------------------------------------------------------
with Ada.Directories;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Text_IO.Text_Streams;
with Ada.Streams.Stream_IO;

package body Oberon.Files is
   function Open (File_Name : String) return File is
      package Dirs renames Ada.Directories;

      Actual_Name : String := File_Name & ".obn";
      Size        : Dirs.File_Size := Dirs.Size (Actual_Name);
      Data_File   : Ada.Text_IO.File_Type;
      Data        : File (1 .. Natural (Size));
      Stream      : Ada.Text_IO.Text_Streams.Stream_Access := null;

      use type Ada.Text_IO.File_Mode;
   begin
      Ada.Text_IO.Open (File => Data_File, Mode => Ada.Text_IO.In_File, Name => Actual_Name);

      Stream := Ada.Text_IO.Text_Streams.Stream (File => Data_File);

      File'Read (Stream, Data);

      Ada.Text_IO.Close (File => Data_File);

      return Data;
   exception
      when others =>
         Put_Line ("Error, reading source file, " & Actual_Name);
   end Open;
end Oberon.Files;
