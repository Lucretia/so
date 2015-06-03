------------------------------------------------------------------------------------------------------------------------
--  See COPYING for licence information.
------------------------------------------------------------------------------------------------------------------------
--  Oberon.Scanner
--  This is the root package for the Oberon compiler lexical analyser.
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Hashed_Maps;
with ada.Finalization;
with Ada.Strings.Unbounded;
with Ada.Strings.Hash_Case_Insensitive;
with Oberon.Files;

package Oberon.Scanner is
   Scan_Error : exception;

   type Token_Values is (T_EOF,                 --  End of file
                         T_Unknown,             --  Any symbol not recognised.
                         T_Identifier,          --  letter { letter | digit }
                         T_Integer,             --  digit { digit }
                         T_Times,               --  *
                         T_Logic_And,           --  &
                         T_Plus,                --  +
                         T_Minus,               --  -
                         T_Equal,               --  =
                         T_Not_Equal,           --  #
                         T_Less_Than,           --  <
                         T_Less_Than_Equal,     --  <=
                         T_Greater_Than,        --  >
                         T_Greater_Than_Equal,  --  >=
                         T_Dot,                 --  .
                         T_Comma,               --  ,
                         T_Colon,               --  :
                         T_Semi_Colon,          --  ;
                         T_Left_Parenthesis,    --  (
                         T_Right_Parenthesis,   --  )
                         T_Left_Bracket,        --  [
                         T_Right_Bracket,       --  ]
                         T_Tilde,               --  ~
                         T_Assignment,          --  :=
                         T_Divide,              --  div
                         T_Modulus,             --  mod
                         T_Logic_Or,            --  or
                         T_Of,
                         T_Then,
                         T_Do,
                         T_End,
                         T_Else,
                         T_Elsif,
                         T_If,
                         T_While,
                         T_Array,
                         T_Record,
                         T_Const,
                         T_Type,
                         T_Var,
                         T_Procedure,
                         T_Begin,
                         T_Module);

   type Lines   is range 1 .. Positive'Last;
   type Columns is range 1 .. Positive'Last;

   --  The Tokens type contains other information about the token, the text value and where in the source file
   --  it is located.
   type Tokens is private;

   EOF_Token : constant Tokens;

   function "=" (Left, Right : in Tokens) return Boolean;

   type Scanner is limited private;

   function Lexeme (Self : in Tokens; From : in Scanner) return String;
   function Line (Self : in Tokens) return Lines;
   function Column (Self : in Tokens) return Columns;

   procedure Scan (Self : in out Scanner);
   function File_Name (Self : in Scanner) return String;

   function Token (Self : in out Scanner) return Tokens;

   package Makers is
      function Create (File_Name : in String) return Scanner;
   end Makers;
private
   type Tokens is
      record
         Token        : Token_Values;
         Lexeme_Start : Positive;      --  Place in the file buffer where the start of the scanned token starts.
         Lexeme_End   : Positive;
         Line         : Lines;         --  Position within the file.
         Column       : Columns;
      end record;

   EOF_Token     : constant Tokens := Tokens'(Token => T_EOF, others => <>);
   Null_Token    : constant Tokens := Tokens'(others => <>);

   package Token_Lists is new Ada.Containers.Doubly_Linked_Lists (Element_Type => Tokens);

   type Scanner is new ada.Finalization.Limited_Controlled with
      record
         Name          : Ada.Strings.Unbounded.Unbounded_String;
         Source        : Oberon.Files.File_Access := null;
         --Index         : Natural                  := Natural'First;           --  Actual position within Source.
         --Line          : Lines                    := Lines'First;
         --Column        : Columns                  := Columns'First;
         Token_Stream  : Token_Lists.List         := Token_Lists.Empty_List;
         Current_Token : Token_Lists.Cursor       := Token_Lists.No_Element;
      end record;

   overriding
   procedure Finalize (Object : in out Scanner);

   package Keyword_Hashed_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Element_Type    => Token_Values,
      Key_Type        => String,
      Hash            => Ada.Strings.Hash_Case_Insensitive,
      Equivalent_Keys => "=");

   Keywords : Keyword_Hashed_Maps.Map := Keyword_Hashed_Maps.Empty_Map;
end Oberon.Scanner;
