with Ada.Command_Line;           use Ada.Command_Line;
with Ada.Integer_Text_IO;        use Ada.Integer_Text_IO;

package body DECOMPRESSION is

   procedure ExploreText (encodedFile : in File_Type; fileName : in Unbounded_String; binaryTree : out treeNode; symbolsArray : in out stringArray) is

   fileCharacter : String (1 .. 1);
   previous : String (1 .. 8) := "00000000";
   currentResult : String (1 .. 8) := "11111111";
   infixTree : Unbounded_String;
   count : Integer := 1;

   begin
      Open (encodedFile, In_File, To_String (fileName));
      while not End_Of_File (encodedFile) loop
         for i in 1 .. 8 loop
            Get (encodedFile, fileCharacter);
            currentResult (i) := fileCharacter (fileCharacter'First);
         end loop;
         symbolsArray (count) := currentResult;
         if previous = currentResult then
            while fileCharacter /= '.' loop
               infixTree := infixTree & fileCharacter;
            end loop;
            ReconstructHuffmanTree (encodedFile, binaryTree, infixTree, null, symbolsArray);
         end if;
         -- Recuperer les charactères encodés.
         previous := currentResult;
         count := count + 1;
      end loop;
   end ExploreText;


   procedure ReconstructHuffmanTree (it : Integer; encodedFile : in File_Type; binaryTree : in out treeNode; infixTree : in out Unbounded_String; previous : treeNodePointer; symbolsArray : in stringArray) is

   infixString : String := To_String (infixTree);
   newNode : treeNodePointer;
   rightChild : treeNodePointer;
   leftChild : treeNodePointer;
   toCall : treeNodePointer;

   begin
      if symbolsArray (it) = "00000000" then
         return;
      end if;
      if infixString (1) = 0 then
         rightChild := new treeNode' ("--------", 0, null, null, newNode, False);
         leftChild := new treeNode' ("--------", 0, null, null, newNode, False);
         newNode := new treeNode' ("--------", 0, rightChild, leftChild, previous, False);
         toCall := leftChild;
      else
         previous.isSeen := True;
         previous.symbol := symbolsArray (it);
         it := it + 1;
         if previous.parent.isSeen then
            while previous.isSeen loop
               previous := previous.parent;
            end loop;
            previous.isSeen := True;
            toCall := previous.rightChild;
         else
            toCall := previous.parent.rightChild;
            previous.parent.isSeen := True;
         end if;
      end if;
      
      ReconstructHuffmanTree (encodedFile, binaryTree, infixTree, toCall, symbolsArray);
   end ReconstructHuffmanTree;


   it : Integer := 1;
   binaryTree : treeNode;
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
               ExploreText (encodedFile, fileName, binaryTree);
            else
               Put_Line ("Le fichier a decompresser doit avoir l'extension .hff");
               return;
            end if;
         end;
      end if;
   end DecompressionProcedure;
end DECOMPRESSION;