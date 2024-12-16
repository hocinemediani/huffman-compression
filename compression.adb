with Ada.Unchecked_Deallocation;
with Ada.Command_Line;     use Ada.Command_Line;

package body COMPRESSION is

   hashTableSize : CONSTANT Integer := 256;

   procedure Free3 is
      new Ada.Unchecked_Deallocation (Object => treeNode, Name => treeNodePointer);

-- Beginning of GetSymbols.
   procedure GetSymbols (textToCompress : in out File_type; symbolsHashTable : out hashMap) is

   fileCharacter : String (1 .. 1);
   result : String (1 .. 8);

   begin
      InitialiseHashTable (symbolsHashTable, hashTableSize);
      Open (textToCompress, In_File, Argument (Argument_Count));
      while not End_Of_File (textToCompress) loop
         result := "00000000";
         for i in 1 .. 8 loop
            Get (textToCompress, fileCharacter);
            result (i) := fileCharacter (fileCharacter'First);
         end loop;
         if IsIn (symbolsHashTable, result) then
            Register (symbolsHashTable, result, ValueOf (symbolsHashTable, result) + 1);
         else
            Register (symbolsHashTable, result, 1);
         end if;
      end loop; 
      Register (symbolsHashTable, "11111111", 0);
      Close (textToCompress);
   end GetSymbols;


-- Beginning of BuildHuffmanTree.
   procedure SortArray (storageTree : in out treeQueue) is
   
   intermediateValue : treeNodePointer;

   begin
      for i in 1 .. (storageTree.realSize - 1) loop
         for j in 1 .. (storageTree.realSize - 1) loop
            if storageTree.storageArray (j).occurrences < storageTree.storageArray (j + 1).occurrences then
               intermediateValue := storageTree.storageArray (j);
               storageTree.storageArray (j) := storageTree.storageArray (j + 1);
               storageTree.storageArray (j + 1) := intermediateValue;
            end if;
         end loop;
      end loop;
      end SortArray;


   procedure CreateNode (storageTree : in out treeQueue) is

   firstNode : CONSTANT treeNodePointer := storageTree.storageArray (storageTree.realSize);
   secondNode : CONSTANT treeNodePointer := storageTree.storageArray (storageTree.realSize - 1);
   newNode : treeNodePointer;

   begin
      newNode := new treeNode' ("--------", secondNode.occurrences + firstNode.occurrences, secondNode, firstNode, null, False);
      firstNode.parent := newNode;
      secondNode.parent := newNode;
      storageTree.last := newNode;
      storageTree.realSize := storageTree.realSize - 1;
      storageTree.storageArray (storageTree.realSize) := newNode;
   end CreateNode;


   procedure BuildHuffmanTree (symbolsHashTable : in hashMap; binaryTree : out treeNodePointer) is
   
   current : Integer := 1;
   storageTree : treeQueue;
   newNode : treeNodePointer;
   
   begin
      storageTree.realSize := 0;
      storageTree.last := null;
      for k in 1 .. 256 loop
         storageTree.storageArray (k) := null;
      end loop;
      for i in 1 .. hashTableSize loop
         if symbolsHashTable.entryNodeArray (i) /= null then
               newNode := new treeNode' (symbolsHashTable.entryNodeArray (i).key, symbolsHashTable.entryNodeArray (i).value, null, null, null, False);
               storageTree.storageArray (current) := newNode;
               current := current + 1;
         end if;
      end loop;
      storageTree.realSize := current - 1;
      storageTree.last := storageTree.storageArray (storageTree.realSize);
      while storageTree.realSize /= 1 loop
         SortArray (storageTree);
         CreateNode (storageTree);
      end loop;
      binaryTree := storageTree.storageArray (1);
   end BuildHuffmanTree;



-- Beginning of GetTextCode.
   procedure ExploreTree (binaryTree : in treeNodePointer; code : in out Unbounded_String; encodedSymbols : in out hashMap2) is

   leftCode, rightCode : Unbounded_String;

   begin
      if binaryTree.leftChild = null then
         Register2 (encodedSymbols, To_Unbounded_String (binaryTree.All.symbol), code);
      else
         leftCode := code & "0";
         ExploreTree (binaryTree.leftChild, leftCode, encodedSymbols);
         rightCode := code & "1";
         ExploreTree (binaryTree.rightChild, rightCode, encodedSymbols);
      end if;
   end ExploreTree;


   procedure GetTextCode (binaryTree : in treeNodePointer; encodedSymbols : out hashMap2) is
   
   code : Unbounded_String := To_Unbounded_String ("");
   
   begin
      InitialiseHashTable2 (encodedSymbols, hashTableSize);
      ExploreTree (binaryTree, code, encodedSymbols);
   end GetTextCode;



-- Beginning of CreateFile.
   procedure PutSymbols (symbolsHashTable : in hashMap; it : in Integer; symbol : in String; encodedFile : in out File_Type) is

   begin
      Put (encodedFile, symbol);
      if it = symbolsHashTable.size then
         Put (encodedFile, symbol);
         New_Line (encodedFile);
      end if;
   end PutSymbols;


   procedure InfixBrowsing (it : in out Integer; symbolsHashTable : in hashMap; binaryTree : in treeNodePointer; infixTree : out Unbounded_String; encodedFile : in out File_Type) is
   
   current : treeNodePointer;
   
   begin
      if it < symbolsHashTable.size then
         current := binaryTree;
         if current.leftChild /= null then
            if current.leftChild.isSeen and current.rightChild.isSeen then
               current.isSeen := True;
               InfixBrowsing (it, symbolsHashTable, current.parent, infixTree, encodedFile);
            elsif current.leftChild.isSeen then
               InfixBrowsing (it, symbolsHashTable, current.rightChild, infixTree, encodedFile);
            else
               infixTree := infixTree & To_Unbounded_String ("0");
               current := current.leftChild;
               InfixBrowsing (it, symbolsHashTable, current, infixTree, encodedFile);
            end if;
         else
            current.isSeen := True;
            infixTree := infixTree & To_Unbounded_String ("1");
            it := it + 1;
            PutSymbols (symbolsHashTable, it, current.symbol, encodedFile);
            InfixBrowsing (it, symbolsHashTable, current.parent, infixTree, encodedFile);
            Free3 (current);
         end if;
      end if;
   end InfixBrowsing;


   procedure DisplayTree(it : in out Integer; storageTree : in treeQueue; symbolsHashTable : in hashMap; binaryTree : in treeNodePointer; depth : in out Integer; leftOrRight : in out Boolean) is
  
   current : treeNodePointer;
   
   begin
  	if it < symbolsHashTable.size then
      current := binaryTree;
     	if current.leftChild /= null then -- on est pas sur une feuille
        	if current.leftChild.isSeen and current.rightChild.isSeen then -- les deux fils sont vus on remonte
            current.isSeen := True;
            depth := depth - 1;
            DisplayTree (it, storageTree, symbolsHashTable, current.parent, depth, leftOrRight);
        	elsif current.parent = null and current.leftChild.isSeen then
           	New_Line;
           	Put("   ");
           	Put("\--1--(" & Integer'Image(current.rightChild.occurrences) & " )");
           	current := current.rightChild; -- on va a gauche car parcours infixe
           	leftOrRight := True;
           	current.isSeen := True;
           	depth := depth + 1;
           	DisplayTree(it, storageTree, symbolsHashTable, current, depth, leftOrRight);
        	elsif current.leftChild.isSeen then  -- gauche vue et non null on va a droite
            New_Line;
            Put("   ");
            Put("|");
            for i in 0 .. depth - 1 loop
               Put("      ");
            end loop;
            Put("\--1--(" & Integer'Image(current.rightChild.occurrences) & " )");
            depth := depth + 1;
            current := current.rightChild;
            leftOrRight := True;   	 
            DisplayTree(it, storageTree, symbolsHashTable, current, depth, leftOrRight);
	      elsif current.parent = null and not current.leftChild.isSeen and not current.RightChild.isSeen then -- on est a la racine au debut
           	Put("(" & Integer'Image(current.occurrences) & " )");
           	New_Line;
           	Put("   ");
           	Put("\--0--(" & Integer'Image(current.leftChild.occurrences) & " )");
           	current := current.leftChild; -- on va a gauche car parcours infixe
           	leftOrRight := False;
           	depth := depth + 1;
           	current.isSeen := True;
           	DisplayTree(it, storageTree, symbolsHashTable, current, depth, leftOrRight);
        	else -- on est sur un noeud ou le gauche n'est pas vue
           	New_Line;
           	Put("   ");
            Put("|");
            for i in 0 .. depth - 1 loop
               Put("      ");
            end loop;
           	Put("\--0--(" & Integer'Image(current.leftChild.occurrences) & " )");
           	depth := depth + 1;
           	leftOrRight := False;
           	DisplayTree (it, storageTree, symbolsHashTable, current.leftChild, depth, leftOrRight);
        	end if;
     	else --feuille
        	current.isSeen := True;
        	it := it + 1;
         if current.occurrences /= 0 then
            Put(" '" & Character'Val(HashKey(current.symbol)) & "'");
         else 
            Put(" '\$'");
         end if;
        	depth := depth - 1;
        	DisplayTree (it, storageTree, symbolsHashTable, current.parent, depth, leftOrRight);
        	Free3(Current);
     	end if;
  	end if;
   end DisplayTree;


   procedure EncodeText (textToCompress : in out File_Type; encodedFile : in out File_Type; encodedSymbols : in hashMap2) is
   
   fileCharacter : String (1 .. 1);
   result : String (1 .. 8);
   hashedKey : Integer;
   
   begin
      Open (textToCompress, In_File, Argument (Argument_Count));
      New_Line (encodedFile);
      while not End_Of_File (textToCompress) loop
         for i in 1 .. 8 loop
            Get (textToCompress, fileCharacter);
            result (i) := fileCharacter (fileCharacter'First);
         end loop;
         hashedKey := HashKey2 (result);
         Put (encodedFile, To_String (encodedSymbols.entryNodeArray (hashedKey).value)); Put (encodedFile, ".");
      end loop;
      Put (encodedFile, To_String (encodedSymbols.entryNodeArray (255).value));
      Close (textToCompress);
      Close (encodedFile);
   end EncodeText;


   procedure CreateFile (modeBavard : in Boolean; fileName : in Unbounded_String; storageTree : in treeQueue; symbolsHashTable : in hashMap; binaryTree : in treeNodePointer; encodedSymbols : in hashMap2; encodedFile : out File_Type; infixTree : in out Unbounded_String) is

   inputFile : File_Type;
   it : Integer := 0;
   it2 : Integer := 0;
   depth : Integer := 0;
   leftOrRight : Boolean := False;

   begin
      Create (encodedFile, Out_File, To_String (fileName));
      InfixBrowsing (it, symbolsHashTable, binaryTree, infixTree, encodedFile);
      Put (encodedFile, To_String (infixTree));
      EncodeText (inputFile, encodedFile, encodedSymbols);
      if modeBavard then
         DisplayTree (it2, storageTree, symbolsHashTable, binaryTree, depth, leftOrRight);
         New_Line;
         New_Line;
         New_Line;
         DisplayHashTable2 (encodedSymbols);
      end if;
      
   end CreateFile;


   procedure DestroyEverything (symbolsHashTable : in out hashMap; encodedSymbols: in out hashMap2; treeStorageArray : in out treeNodeArray) is
   begin
      DestroyHashTable (symbolsHashTable);
      DestroyHashTable2 (encodedSymbols);
      for i in 1 .. 256 loop
         if treeStorageArray (i) /= null then
            Free3 (treeStorageArray (i));
         end if;
      end loop;
   end DestroyEverything;


   textToCompress : File_Type;
   symbolsHashTable : hashMap;
   storageTree : treeQueue;
   binaryTree : treeNodePointer;
   encodedSymbols : hashMap2;
   encodedFile : File_Type;
   infixTree : Unbounded_String;
   fileName : Unbounded_String;
   modeBavard : Boolean := True;

procedure MainProcedure is
   begin
      if Argument_Count = 0 then
         Put_Line ("Compresser prend en parametre au moins un argument (dans ce cas la, le nom de fichier avec son extension)");
         return;
      else
         fileName := To_Unbounded_String (Argument (Argument_Count)) & ".hff";
         if Argument_Count > 1 then
            if Argument (Argument_Count - 1) = "-s" then
               modeBavard := False;
            end if;
         end if;
      end if;

      GetSymbols (textToCompress, symbolsHashTable);
      BuildHuffmanTree (symbolsHashTable, binaryTree);
      GetTextCode (binaryTree, encodedSymbols);
      CreateFile (modeBavard, fileName, storageTree, symbolsHashTable, binaryTree, encodedSymbols, encodedFile, infixTree);
      DestroyEverything (symbolsHashTable, encodedSymbols, storageTree.storageArray);
   end MainProcedure;

end COMPRESSION;