with Ada.Command_Line;       	use Ada.Command_Line;

package body DECOMPRESSION is

   procedure ExploreText (encodedFile : in out File_Type; decodedFile : in out File_Type; symbolsArray : in out stringArray) is

   fileCharacter : String (1 .. 1);
   previousCode : String (1 .. 8) := "00000002";
   currentResult : String (1 .. 8) := "11111112";
   infixTree : Unbounded_String;
   count : Integer := 1;
   isSymbol : Boolean := True;
   isTree : Boolean := True;
   isInfix : Boolean := True;
   currentPointer :TreeNodePointer := new treeNode' ("--------", 0, null, null, null, False);
   it : Integer := 1;
   countInfix : Integer := 1;


   begin
      while not End_Of_File (encodedFile) loop
         -- Récupérer les symboles et leur nombre.
         while isSymbol loop
            previousCode := currentResult;
            for i in 1 .. 8 loop
               Get (encodedFile, fileCharacter);
               currentResult (i) := fileCharacter (fileCharacter'First);
            end loop;
            symbolsArray (count) := currentResult;
            count := count + 1;
            -- Stopper lorsque l'on à 2 fois le même byte.
            if previousCode = currentResult then
               isSymbol := False;
            end if;
         end loop;
         -- Récupérer l'arbre infixe en stoppant suivant le nombre de 1.
         if isInfix then   
            while count > 2 loop
               Get (encodedFile, fileCharacter);
               infixTree := infixTree & fileCharacter;
               if fileCharacter = "1" then
                  count := count - 1;
               end if;
            end loop;
            isInfix := False;
         end if;
         -- Parcourir tous les bits du texte et continuer à parcourir si on n'est pas sur une feuille    	 
         -- On doit alors changer ExploreTree, et juste prendre en paramètre un pointeur et une instruction
         -- 0 -> aller à gauche, 1 -> aller à droite. si c'est une feuille on le dit sinon on update juste la position du pointeur
         if isTree then
            ReconstructHuffmanTree (it, countInfix, encodedFile, infixTree, currentPointer, symbolsArray);
            isTree := False; 	 
         else
            GetRoot (currentPointer);
            ExploreTree (currentPointer, encodedFile, decodedFile);
         end if;
      end loop;
   end ExploreText;


   Procedure ExploreTree  (root : in  treeNodePointer; encodedFile : in out File_Type; decodedFile : in out File_Type ) is
   
   code : String (1 .. 1);
   
   begin
      if root.leftChild = null then
         if root.symbol /= "11111111" then
            Put (decodedFile, root.symbol);
         end if;
         return;
      end if;
      Get (encodedFile, code);
      if code = "0" then
         ExploreTree (root.leftChild, encodedFile, decodedFile);
      else
         ExploreTree (root.rightChild, encodedFile, decodedFile);
      end if;
   end ExploreTree;


   procedure GetRoot (root : in out treeNodePointer) is
   begin    
      while root.parent /= Null loop
         root := root.parent;
      end loop;
   end GetRoot;


   procedure ReconstructHuffmanTree (it : in out Integer; countInfix : in out Integer; encodedFile : in File_Type; infixTree : in out Unbounded_String; current : in out treeNodePointer; symbolsArray : in stringArray) is

   infixString : CONSTANT String := To_String (infixTree);
   rightChild : treeNodePointer;
   leftChild : treeNodePointer;
   toCall : treeNodePointer;
   
   begin
      if infixString (countInfix) = '0' then
         rightChild := new treeNode' ("--------", 0, null, null, current, False);
         leftChild := new treeNode' ("--------", 0, null, null, current, False);
         current.leftChild := leftChild;
         current.rightChild := rightChild;
         toCall := leftChild;
      else
         current.isSeen := True;
         current.symbol := symbolsArray (it);
         if current.parent.isSeen then
            while current.isSeen loop
               if current.parent = null then
                  return;
               end if;
               current := current.parent;
            end loop;
            current.isSeen := True;
            toCall := current.rightChild;
         else
            toCall := current.parent.rightChild;
            current.parent.isSeen := True;
         end if;
         it := it + 1;
      end if;
      countInfix := countInfix + 1;
      ReconstructHuffmanTree (it, countInfix, encodedFile, infixTree, toCall, symbolsArray);
   end ReconstructHuffmanTree;



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
               Open (encodedFile, In_File, To_String (fileName));
               ExploreText (encodedFile, decodedFile, symbolsArray);
               Close(encodedFile);
               Close(decodedFile);
            else
               Put_Line ("Le fichier a decompresser doit avoir l'extension .hff");
               return;
            end if;
         end;
      end if;
   end DecompressionProcedure;
end DECOMPRESSION;