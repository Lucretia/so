------------------------------------------------------------------------------------------------------------------------
--  See COPYING for licence information.
------------------------------------------------------------------------------------------------------------------------
--  Oberon.Files
--  This is the root package for file handling.
package Oberon.Files is
   subtype File is String;
   type File_Access is access all File;

   function Open (File_Name : String) return File;
end Oberon.Files;
