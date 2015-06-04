------------------------------------------------------------------------------------------------------------------------
--  See COPYING for licence information.
------------------------------------------------------------------------------------------------------------------------
with Ada.Characters.Handling;
with Ada.Characters.Latin_1;
with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;

package body Oberon.Scanner is
   function Lexeme (Self : in Tokens; From : in Scanner) return String is
   begin
      if Self = EOF_Token then
         return "EOF";
      end if;

      --  Make sure we always return a lower case string except for EOF.
      return Ada.Characters.Handling.To_Lower (From.Source (Self.Lexeme_Start .. Self.Lexeme_End));
   end Lexeme;

   function Line (Self : in Tokens) return Lines is
   begin
      return Self.Line;
   end Line;

   function Column (Self : in Tokens) return Columns is
   begin
      return Self.Column;
   end Column;

   function "=" (Left, Right : in Tokens) return Boolean is
   begin
      if Left.Token = Right.Token and Left.Lexeme_Start = Right.Lexeme_Start and Left.Lexeme_End = Right.Lexeme_End and
        Left.Line = Right.Line and Left.Column = Right.Column then
         return True;
      end if;

      return False;
   end "=";

   --  Scan the entire file in one go creating a list of tokens.
   procedure Scan (Self : in out Scanner) is
      package Latin_1 renames Ada.Characters.Latin_1;

      --  The states the DFA can be in.
      type States is (Start,
                      In_Identifier,
                      In_Integer,
                      In_Comment,
                      In_White_Space);

      Current_Token     : Tokens   := Null_Token;
      Current_State     : States   := Start;
      Current_Character : Natural  := Natural'First;
      Current_Line      : Lines    := Lines'First;
      Current_Column    : Columns  := Columns'First;
      Do_New_Line       : Boolean  := False;

      procedure Push_Back with
        Inline => True;

      procedure Push_Back is
      begin
         Current_Character := Current_Character - 1;
         Current_Column    := Current_Column - 1;
         --  Make sure the column is also updated, otherwise we get out of
         --  sync with the Current_Character value.
      end Push_Back;
   begin
      while Current_Character /= Self.Source'Last loop
         Current_Character := Current_Character + 1;

         case Current_State is
            when Start =>
               case Self.Source (Current_Character) is
                  when 'a' .. 'z' | 'A' .. 'Z' =>
                     Current_State := In_Identifier;
                     Current_Token := (Token        => T_Identifier,
                                       Lexeme_Start => Current_Character,
                                       Line         => Current_Line,
                                       Column       => Current_Column,
                                       others       => <>);
                  when '0' .. '9' =>
                     Current_State := In_Integer;
                     Current_Token := (Token        => T_Integer,
                                       Lexeme_Start => Current_Character,
                                       Line         => Current_Line,
                                       Column       => Current_Column,
                                       others       => <>);
                  when Latin_1.Left_Parenthesis =>
                     if Self.Source (Current_Character + 1) = Latin_1.Asterisk then
                        --  Skip over the current character (parenthesis), the top of the loop on the next iteration
                        --  will skip over THe asterisk.
                        Current_Character := Current_Character + 1;

                        Current_State := In_Comment;
                     else
                        Current_Token := (Token        => T_Left_Parenthesis,
                                          Lexeme_Start => Current_Character,
                                          Lexeme_End   => Current_Character,
                                          Line         => Current_Line,
                                          Column       => Current_Column);

                        Self.Token_Stream.Append (New_Item => Current_Token);

                        Current_Token := Null_Token;
                        Current_State := Start;
                     end if;
                  when Latin_1.Asterisk =>
                     Current_Token := (Token        => T_Times,
                                       Lexeme_Start => Current_Character,
                                       Lexeme_End   => Current_Character,
                                       Line         => Current_Line,
                                       Column       => Current_Column);

                     Self.Token_Stream.Append (New_Item => Current_Token);

                     Current_Token := Null_Token;
                     Current_State := Start;
                  when Latin_1.Ampersand =>
                     Current_Token := (Token        => T_Logic_And,
                                       Lexeme_Start => Current_Character,
                                       Lexeme_End   => Current_Character,
                                       Line         => Current_Line,
                                       Column       => Current_Column);

                     Self.Token_Stream.Append (New_Item => Current_Token);

                     Current_Token := Null_Token;
                     Current_State := Start;
                  when Latin_1.Plus_Sign =>
                     Current_Token := (Token        => T_Plus,
                                       Lexeme_Start => Current_Character,
                                       Lexeme_End   => Current_Character,
                                       Line         => Current_Line,
                                       Column       => Current_Column);

                     Self.Token_Stream.Append (New_Item => Current_Token);

                     Current_Token := Null_Token;
                     Current_State := Start;
                  when Latin_1.Minus_Sign =>
                     Current_Token := (Token        => T_Minus,
                                       Lexeme_Start => Current_Character,
                                       Lexeme_End   => Current_Character,
                                       Line         => Current_Line,
                                       Column       => Current_Column);

                     Self.Token_Stream.Append (New_Item => Current_Token);

                     Current_Token := Null_Token;
                     Current_State := Start;
                  when Latin_1.Equals_Sign =>
                     Current_Token := (Token        => T_Equal,
                                       Lexeme_Start => Current_Character,
                                       Lexeme_End   => Current_Character,
                                       Line         => Current_Line,
                                       Column       => Current_Column);

                     Self.Token_Stream.Append (New_Item => Current_Token);

                     Current_Token := Null_Token;
                     Current_State := Start;
                  when Latin_1.Pound_Sign =>
                     --  Hash sign or #.
                     Current_Token := (Token        => T_Not_Equal,
                                       Lexeme_Start => Current_Character,
                                       Lexeme_End   => Current_Character,
                                       Line         => Current_Line,
                                       Column       => Current_Column);

                     Self.Token_Stream.Append (New_Item => Current_Token);

                     Current_Token := Null_Token;
                     Current_State := Start;
                  when Latin_1.Less_Than_Sign =>
                     if Self.Source (Current_Character + 1) = Latin_1.Equals_Sign then
                        Current_Token := (Token        => T_Less_Than_Equal,
                                          Lexeme_Start => Current_Character,
                                          Lexeme_End   => Current_Character + 2,
                                          Line         => Current_Line,
                                          Column       => Current_Column);

                        Current_Character := Current_Character + 1;

                        Self.Token_Stream.Append (New_Item => Current_Token);

                        Current_Token := Null_Token;
                        Current_State := Start;
                     else
                        Current_Token := (Token        => T_Less_Than,
                                          Lexeme_Start => Current_Character,
                                          Lexeme_End   => Current_Character,
                                          Line         => Current_Line,
                                          Column       => Current_Column);


                        Self.Token_Stream.Append (New_Item => Current_Token);

                        Current_Token := Null_Token;
                        Current_State := Start;
                     end if;
                  when Latin_1.Greater_Than_Sign =>
                     if Self.Source (Current_Character + 1) = Latin_1.Equals_Sign then
                        Current_Token := (Token        => T_Greater_Than_Equal,
                                          Lexeme_Start => Current_Character,
                                          Lexeme_End   => Current_Character + 2,
                                          Line         => Current_Line,
                                          Column       => Current_Column);

                        Current_Character := Current_Character + 1;

                        Self.Token_Stream.Append (New_Item => Current_Token);

                        Current_Token := Null_Token;
                        Current_State := Start;
                     else
                        Current_Token := (Token        => T_Greater_Than,
                                          Lexeme_Start => Current_Character,
                                          Lexeme_End   => Current_Character,
                                          Line         => Current_Line,
                                          Column       => Current_Column);


                        Self.Token_Stream.Append (New_Item => Current_Token);

                        Current_Token := Null_Token;
                        Current_State := Start;
                     end if;
                  when Latin_1.Full_Stop =>
                     CurreNt_Token := (Token        => T_Dot,
                                       Lexeme_Start => Current_Character,
                                       Lexeme_End   => Current_Character,
                                       Line         => Current_Line,
                                       Column       => Current_Column);

                     Self.Token_Stream.Append (New_Item => Current_Token);

                     Current_Token := Null_Token;
                     Current_State := Start;
                  when Latin_1.Comma =>
                     Current_Token := (Token        => T_Comma,
                                       Lexeme_Start => Current_Character,
                                       Lexeme_End   => Current_Character,
                                       Line         => Current_Line,
                                       Column       => Current_Column);

                     Self.Token_Stream.Append (New_Item => Current_Token);

                     Current_Token := Null_Token;
                     Current_State := Start;
                  when Latin_1.Colon =>
                     if Self.Source (Current_Character + 1) = Latin_1.Equals_Sign then
                        Current_Token := (Token        => T_Assignment,
                                          Lexeme_Start => Current_Character,
                                          Lexeme_End   => Current_Character + 2,
                                          Line         => Current_Line,
                                          Column       => Current_Column);

                        Current_Character := Current_Character + 1;

                        Self.Token_Stream.Append (New_Item => Current_Token);

                        Current_Token := Null_Token;
                        Current_State := Start;
                     else
                        Current_Token := (Token        => T_Colon,
                                          Lexeme_Start => Current_Character,
                                          Lexeme_End   => Current_Character,
                                          Line         => Current_Line,
                                          Column       => Current_Column);


                        Self.Token_Stream.Append (New_Item => Current_Token);

                        Current_Token := Null_Token;
                        Current_State := Start;
                     end if;
                  when Latin_1.Semicolon =>
                     Current_Token := (Token        => T_Semi_Colon,
                                       Lexeme_Start => Current_Character,
                                       Lexeme_End   => Current_Character,
                                       Line         => Current_Line,
                                       Column       => Current_Column);

                     Self.Token_Stream.Append (New_Item => Current_Token);

                     Current_Token := Null_Token;
                     Current_State := Start;
                  when Latin_1.Right_Parenthesis =>
                     Current_Token := (Token        => T_Right_Parenthesis,
                                       Lexeme_Start => Current_Character,
                                       Lexeme_End   => Current_Character,
                                       Line         => Current_Line,
                                       Column       => Current_Column);

                     Self.Token_Stream.Append (New_Item => Current_Token);

                     Current_Token := Null_Token;
                     Current_State := Start;
                  when Latin_1.Left_Square_Bracket =>
                     Current_Token := (Token        => T_Left_Bracket,
                                       Lexeme_Start => Current_Character,
                                       Lexeme_End   => Current_Character,
                                       Line         => Current_Line,
                                       Column       => Current_Column);

                     Self.Token_Stream.Append (New_Item => Current_Token);

                     Current_Token := Null_Token;
                     Current_State := Start;
                  when Latin_1.Right_Square_Bracket =>
                     Current_Token := (Token        => T_Right_Bracket,
                                       Lexeme_Start => Current_Character,
                                       Lexeme_End   => Current_Character,
                                       Line         => Current_Line,
                                       Column       => Current_Column);

                     Self.Token_Stream.Append (New_Item => Current_Token);

                     Current_Token := Null_Token;
                     Current_State := Start;
                  when Latin_1.Tilde =>
                     Current_Token := (Token        => T_Tilde,
                                       Lexeme_Start => Current_Character,
                                       Lexeme_End   => Current_Character,
                                       Line         => Current_Line,
                                       Column       => Current_Column);

                     Self.Token_Stream.Append (New_Item => Current_Token);

                     Current_Token := Null_Token;
                     Current_State := Start;
                  when Latin_1.NUL .. Latin_1.HT | Latin_1.VT .. Latin_1.FF | Latin_1.SO .. Latin_1.Space =>
                     Current_State := In_White_Space;
                  when Latin_1.CR =>
                     --  Windows style newline, otherwise MacOS style.
                     if Self.Source (Current_Character + 1) = Latin_1.LF then
                        Current_Character := Current_Character + 1;
                     end if;

                     Do_New_Line := True;
                  when Latin_1.LF =>
                     --  Unix style newline.
                     Do_New_Line := True;
                  when others =>
                     --  We have an unknown symbol in the source file.
                     Current_Token := (Token        => T_Unknown,
                                       Lexeme_Start => Current_Character,
                                       Lexeme_End   => Current_Character,
                                       Line         => Current_Line,
                                       Column       => Current_Column);

                     Self.Token_Stream.Append (New_Item => Current_Token);

                     Current_Token := Null_Token;
                     Current_State := Start;
               end case;
            when In_Identifier =>
               --  At the end of the identifier, change state and update the lexeme's last position.
               if Self.Source (Current_Character) not in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' then
                  Push_Back;

                  Current_Token.Lexeme_End := Current_Character;

                  --  Check To Make Sure this isn't a keyword.
                  declare
                     Text   : String                     := Lexeme (Current_Token, Self);
                     Cursor : Keyword_Hashed_Maps.Cursor := Keywords.Find (Text);

                     use type Keyword_Hashed_Maps.Cursor;
                  begin
                     if Cursor /= Keyword_Hashed_Maps.No_Element then
                        Current_Token.Token := Keywords.Element (Text);
                     end if;
                  end;

                  Self.Token_Stream.Append (New_Item => Current_Token);

                  Current_Token := Null_Token;
                  Current_State := Start;
               end if;
            when In_Integer =>
               --  At the end of the integer, change state and update the lexeme's last position.
               if Self.Source (Current_Character) not in '0' .. '9' then
                  Push_Back;

                  Current_Token.Lexeme_End := Current_Character;

                  Self.Token_Stream.Append (New_Item => Current_Token);

                  Current_Token := Null_Token;
                  Current_State := Start;
               end if;
            when In_Comment =>
               if Self.Source (Current_Character) = Latin_1.Asterisk then
                  if Self.Source (Current_Character + 1) = Latin_1.Right_Parenthesis then
                     --  Skip over the current character (asterisk), the top of the loop on the next iteration
                     --  will skip over the parenthesis.
                     Current_Character := Current_Character + 1;

                     --  We don't want to keep the comment anywhere as it's not useful for a compiler.
                     Current_State := Start;
                  end if;
               end if;
            when In_White_Space =>
               if Self.Source (Current_Character) not in Latin_1.NUL .. Latin_1.HT | Latin_1.VT .. Latin_1.FF
                 | Latin_1.SO .. Latin_1.Space then
                  Push_Back;

                  Current_State := Start;
               end if;
            when others =>
               null;
         end case;

         if Do_New_Line then
            Current_Line   := Current_Line + 1;
            Current_Column := Columns'First;
            Do_New_Line    := False;
         else
            Current_Column := Current_Column + 1;
         end if;
      end loop;

      --  Make Sure We Add The EOF Token To The Stream, Otherwise We'Ll Never Finish Parsing The File!
      Self.Token_Stream.Append (New_Item => EOF_Token);

      --  Dump out The Tokens.
      Self.Current_Token := Self.Token_Stream.First;

      Put_Line ("Tokens read:");
      New_Line;
      --  Dump them out!
      declare
         package Token_IO is new Ada.Text_IO.Enumeration_IO (Token_Values);
         use Token_IO;
         package Line_IO is new Ada.Text_IO.Integer_IO (Lines);
         use Line_IO;
         package Column_IO is new Ada.Text_IO.Integer_IO (Columns);
         use Column_IO;
         T : Tokens := Token (Self);
      begin
         while T /= EOF_Token loop
            Put ("Token: ");
            Put (Item => T.Token, Width => 30);
            Put (" '" & Lexeme (T, Self) & "':");
            Put (Item => T.Line, Width => 0);
            Put (":");
            Put (Item => T.Column, Width => 0);
            New_Line;

            T := Token (Self);
         end loop;
      end;
   end Scan;

   function File_Name (Self : in Scanner) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Self.Name);
   end File_Name;

   function Token (Self : in out Scanner) return Tokens is
      Current_Cursor : Token_Lists.Cursor := Self.Current_Token;

      use type Token_Lists.Cursor;
   begin
      if Current_Cursor = Token_Lists.No_Element then
         return EOF_Token;
      end if;

      Self.Current_Token := Token_Lists.Next (Current_Cursor);

      return Token_Lists.Element (Current_Cursor);
   end Token;

   package body Makers is
      function Create (File_Name : in String) return Scanner is
      begin
         return S : Scanner do
            S.Name   := Ada.Strings.Unbounded.To_Unbounded_String (File_Name);
            S.Source := new Oberon.Files.File'(Oberon.Files.Open (File_Name));
         end return;
      end Create;
   end Makers;

   overriding
   procedure Finalize (Object : in out Scanner) is
      procedure Free is new Ada.Unchecked_Deallocation (Object => Oberon.Files.File, Name => Oberon.Files.File_Access);
   begin
      Free (Object.Source);

      Object.Source := null;
   end Finalize;
begin
   --  Create a map of keyword strings and Token_Values.
   Keywords.Include (Key => "div",       New_Item => T_Divide);
   Keywords.Include (Key => "mod",       New_Item => T_Modulus);
   Keywords.Include (Key => "or",        New_Item => T_Logic_Or);
   Keywords.Include (Key => "of",        New_Item => T_Of);
   Keywords.Include (Key => "then",      New_Item => T_Then);
   Keywords.Include (Key => "do",        New_Item => T_Do);
   Keywords.Include (Key => "end",       New_Item => T_End);
   Keywords.Include (Key => "else",      New_Item => T_Else);
   Keywords.Include (Key => "elsif",     New_Item => T_Elsif);
   Keywords.Include (Key => "while",     New_Item => T_While);
   Keywords.Include (Key => "array",     New_Item => T_Array);
   Keywords.Include (Key => "record",    New_Item => T_Record);
   Keywords.Include (Key => "const",     New_Item => T_Const);
   Keywords.Include (Key => "type",      New_Item => T_Type);
   Keywords.Include (Key => "var",       New_Item => T_Var);
   Keywords.Include (Key => "procedure", New_Item => T_Procedure);
   Keywords.Include (Key => "begin",     New_Item => T_Begin);
   Keywords.Include (Key => "module",    New_Item => T_Module);
end Oberon.Scanner;
