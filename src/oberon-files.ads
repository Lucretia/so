------------------------------------------------------------------------------------------------------------------------
--  See COPYING for licence information.
------------------------------------------------------------------------------------------------------------------------
--  Oberon.Files
--  This is the root package for file handling.
package Oberon.Files is
   subtype File is String; -- (Positive range <>);

   function Open (File_Name : String) return File;
end Oberon.Files;
