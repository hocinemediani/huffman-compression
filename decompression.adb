with Ada.Command_Line;           use Ada.Command_Line;
with Ada.Integer_Text_IO;        use Ada.Integer_Text_IO;

package body DECOMPRESSION is

   fileName : Unbounded_String;
   extractedExtension : Unbounded_String;
   extension : CONSTANT String := ".hff";

procedure DecompressionProcedure is
   begin
      if Argument_Count = 0 then
         Put_Line ("Décompresser prend en paramètre au moins un argument (dans ce cas la, le nom du fichier avec extension .hff");
         return;
      else
         fileName := To_Unbounded_String (Argument (Argument_Count));
         declare
            fileString : CONSTANT String := To_String (fileName);
         begin
            extractedExtension := To_Unbounded_String (fileString (fileString'Length - 3 .. fileString'Length));
            if To_String (extractedExtension) = extension then
               -- Procedures to decompress the file
               Null;
            else
               Put_Line ("Le fichier a decompresser doit avoir l'extension .hff");
               return;
            end if;
         end;
      end if;
   end DecompressionProcedure;
end DECOMPRESSION;