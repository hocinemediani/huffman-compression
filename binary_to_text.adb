with Ada.Command_Line;           use Ada.Command_Line;
with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Text_IO;                use Ada.Text_IO;


procedure Binary_To_Text is

binaryFile : File_Type;
textFile : File_Type;
fileName : Unbounded_String;
fileCharacter : Character;
textCharacter : Character;
asciiCode : Integer := 0;
tempString : CONSTANT String := Argument (1) (1 .. Argument (1)'Length - 16);

begin
   fileName := To_Unbounded_String (tempString) & "TEXT" & ".txt";
   Open (binaryFile, In_File, Argument (1));
   Create (textFile, Out_File, To_String (fileName));
   while not End_Of_File (binaryFile) loop
      for k in 1 .. 8 loop
         Get (binaryFile, fileCharacter);
         if fileCharacter = '1' then
            asciiCode := asciiCode + 2 ** (8 - k);
         end if;
      end loop;
      textCharacter := Character'Val (asciiCode);
      Put (textFile, textCharacter);
      asciiCode := 0;
   end loop;
end Binary_To_Text;