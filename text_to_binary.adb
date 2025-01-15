with Ada.Command_Line;           use Ada.Command_Line;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Text_IO;                use Ada.Text_IO;
with Ada.Integer_Text_IO;        use Ada.Integer_Text_IO;


procedure Text_To_Binary is

textFile : File_Type;
binaryFile : File_Type;
fileName : Unbounded_String;
fileCharacter : Character;
asciiCode : Integer;
binaryCode : String (1 .. 8) := "00000000";
tempString : CONSTANT String := Argument (1) (1 .. Argument (1)'Length - 4);

begin
   fileName := To_Unbounded_String (tempString) & "BINARY" & ".txt";
   Open (textFile, In_File, Argument (1));
   Create (binaryFile, Out_File, To_String (fileName));
   while not End_Of_File (textFile) loop
      Get (textFile, fileCharacter);
      asciiCode := Character'Pos (fileCharacter);
      Put (asciiCode, 1); Put_Line (" "); 
      for i in 1 .. 8 loop
         if asciiCode >= 2 ** (8 - i) then
            binaryCode (i) := '1';
            asciiCode := asciiCode - 2 ** (8 - i);
         end if;
      end loop;
      Put (binaryFile, binaryCode);
      Put (binaryCode); Put_Line (" ");
      binaryCode := "00000000";
   end loop;
end Text_To_Binary;