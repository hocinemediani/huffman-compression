with Ada.Command_Line;           use Ada.Command_Line;

package body DECOMPRESSION is

   procedure ExploreText (encodedFile : in out File_Type; decodedFile : in out File_Type; fileName : in Unbounded_String; symbolsArray : in out stringArray; binaryTree : in treeNodePointer) is

   fileCharacter : String (1 .. 1);
   previous : String (1 .. 8) := "00000000";
   currentResult : String (1 .. 8) := "11111111";
   infixTree : Unbounded_String;
   count : Integer := 1;
   encodedString : Unbounded_String;
   isSymbol : Boolean := True;
   fileName2 : CONSTANT Unbounded_String := fileName & ".d";

   begin
      Open (encodedFile, In_File, To_String (fileName));
      Open (decodedFile, Out_File, To_String (fileName2));
      while not End_Of_File (encodedFile) loop
         -- Récupérer les symboles et leur nombre.
         while isSymbol loop
            previous := currentResult;
            for i in 1 .. 8 loop
               Get (encodedFile, fileCharacter);
               currentResult (i) := fileCharacter (fileCharacter'First);
            end loop;
            symbolsArray (count) := currentResult;
            count := count + 1;
            -- Stopper lorsque l'on à 2 fois le même byte.
            if previous = currentResult then
               isSymbol := False;
            end if;
         end loop;
         -- Récupérer l'arbre infixe en stoppant suivant le nombre de 1.
         for i in 1 .. count loop
            Get (encodedFile, fileCharacter);
            infixTree := infixTree & fileCharacter;
         end loop;
         -- Parcourir tous les bits du texte et continuer à parcourir si on n'est pas sur une feuille         
         -- On doit alors changer ExploreTree, et juste prendre en paramètre un pointeur et une instruction
         -- 0 -> aller à gauche, 1 -> aller à droite. si c'est une feuille on le dit sinon on update juste la position du pointeur
      end loop;
      ReconstructHuffmanTree (it, encodedFile, binaryTree, infixTree, previousPointer, symbolsArray);
   end ExploreText;


   function ExploreTree (code : in String; root : in treeNodePointer) return treeNodePointer is

   begin
      if root.leftChild = null then
         return root;
      end if;
      if code = "0" then
         return root.leftChild;
      else
         return root.rightChild;
      end if;
   end ExploreTree;


   procedure ReconstructHuffmanTree (it : in out Integer; encodedFile : in File_Type; binaryTree : in out treeNodePointer; infixTree : in out Unbounded_String; previous : in out treeNodePointer; symbolsArray : in stringArray) is

   infixString : CONSTANT String := To_String (infixTree);
   newNode : treeNodePointer;
   rightChild : treeNodePointer;
   leftChild : treeNodePointer;
   toCall : treeNodePointer;

   begin
      if symbolsArray (it) = "00000000" then
         return;
      end if;
      if infixString (1) = '0' then
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
      ReconstructHuffmanTree (it, encodedFile, binaryTree, infixTree, toCall, symbolsArray);
   end ReconstructHuffmanTree;


   it : Integer := 1;
   bababa : treeNodePointer := null;
   infixTree : Unbounded_String;
   binaryTree : treeNodePointer;
   encodedFile : File_Type;
   decodedFile : File_Type;
   symbolsArray : stringArray;
   fileName : Unbounded_String;
   extractedExtension : Unbounded_String;
   extension : CONSTANT String := ".hff";

procedure DecompressionProcedure is
   begin
      if Argument_Count = 0 then
         Put_Line ("Decompresser prend en parametre un argument (dans ce cas la, le nom du fichier avec extension .hff)");
         return;
      else
         fileName := To_Unbounded_String (Argument (Argument_Count));
         declare
            fileString : CONSTANT String := To_String (fileName);
            decodedName : CONSTANT Unbounded_String := fileName & ".d";
         begin
            extractedExtension := To_Unbounded_String (fileString (fileString'Length - 3 .. fileString'Length));
            if To_String (extractedExtension) = extension then
               -- Procedures to decompress the file
               Create (decodedFile, Out_File, To_String (decodedName));
               ExploreText (encodedFile, decodedFile, fileName, symbolsArray, binaryTree);
               ReconstructHuffmanTree (it, encodedFile, binaryTree, infixTree, bababa, symbolsArray);
            else
               Put_Line ("Le fichier a decompresser doit avoir l'extension .hff");
               return;
            end if;
         end;
      end if;
   end DecompressionProcedure;
end DECOMPRESSION;