------------------------------------------------------------------------------------------------------------------------
--  See COPYING for licence information.
------------------------------------------------------------------------------------------------------------------------
--  Simple Oberon Compiler
--  This is the driver of the compiler.
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Oberon.Files;

procedure SOC is
begin
   if Argument_Count = 0 then
      Put_Line ("Simple Oberon Compiler");
      Put_Line ("  (C) 2015 Luke A. Guest");
      New_Line;
      Put_Line ("Usage:");
      Put_Line ("  soc <filename> (without .obn extension)");
   else
      declare
         Src : Oberon.Files.File := Oberon.Files.Open (Argument (1));
      begin
         for Index in Src'Range loop
            Put (Src (Index));
         end loop;
      end;
   end if;
end SOC;
