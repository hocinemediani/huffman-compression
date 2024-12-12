package body COMPRESSION is

   hashTableSize : CONSTANT Integer := 128;


-- Beginning of GetSymbols.
   procedure GetSymbols (textToCompress : in out File_type; symbolsHashTable : out hashMap) is

   fileCharacter : String (1 .. 1);
   result : String (1 .. 8);

   begin
      InitialiseHashTable (symbolsHashTable, hashTableSize);
      Open (textToCompress, In_File, "input.txt");
      while not End_Of_File (textToCompress) loop
         result := "00000000";
         for i in 1 .. 8 loop
            Get (fileCharacter);
            result (i) := fileCharacter (fileCharacter'First);
         end loop;
         if IsIn (symbolsHashTable, result) then
            Register (symbolsHashTable, result, 1);
         else
            Register (symbolsHashTable, result, ValueOf (symbolsHashTable, result) + 1);
         end if;
      end loop;
      Register (symbolsHashTable, "11111111", 0);
      Close (textToCompress);
   end GetSymbols;


-- Beginning of BuildHuffmanTree.
   procedure SortArray (storageTree : in out treeQueue) is
   
   intermediateValue : Integer;

   begin
      for i in 1 .. storageTree.realSize loop
         for j in 1 .. storageTree.realSize loop
            if storageTree.storageArray (j).All.occurrences < storageTree.storageArray (j + 1).All.occurrences then
               intermediateValue := storageTree.storageArray (j).All.occurrences;
               storageTree.storageArray (j).All.occurrences := storageTree.storageArray (j + 1).All.occurrences;
               storageTree.storageArray (j + 1).All.occurrences := intermediateValue;
            end if;
         end loop;
      end loop;
   end SortArray;


   procedure CreateNode (storageTree : in out treeQueue) is

   firstNode : treeNodePointer := storageTree.storageArray (storageTree.realSize);
   secondNode : treeNodePointer := storageTree.storageArray (storageTree.realSize - 1);

   begin
      secondNode.All.leftChild := firstNode;
      secondNode.All.rightChild := secondNode;
      secondNode.All.symbol := "--------";
      secondNode.All.occurrences := secondNode.occurrences + firstNode.occurrences;
      storageTree.last := secondNode;
      storageTree.realSize := storageTree.realSize - 1;
   end CreateNode;


   procedure BuildHuffmanTree (symbolsHashTable : in hashMap; binaryTree : out treeNodePointer) is
   
   capacity : CONSTANT Integer := symbolsHashTable.size;
   current : Integer := 1;
   storageTree : treeQueue;
   
   begin
      for i in 1 .. hashTableSize loop
         if symbolsHashTable.entryNodeArray (i) /= null then
            storageTree.storageArray (current) := new treeNode' (symbolsHashTable.entryNodeArray (i).key, symbolsHashTable.entryNodeArray (i).value, null, null);
            current := current + 1;
         end if;
      end loop;
      storageTree.realSize := current - 1;
      while storageTree.realSize /= 1 loop
         SortArray (storageTree);
         CreateNode (storageTree);
      end loop;
      binaryTree := storageTree.storageArray (1);
   end BuildHuffmanTree;



-- Beginning of GetTextCode.
   procedure ExploreTree (binaryTree : in out treeNodePointer; code : out Unbounded_String; encodedSymbols : in out hashMap2) is

   leftCode, rightCode : Unbounded_String;
   hashedKey : Integer;

   begin
      if binaryTree.leftChild = null then
         hashedKey := Hash2 (binaryTree.All.symbol);
         Register2 (encodedSymbols, binaryTree.All.symbol, To_String (code));
      else
         leftCode := code & "0";
         ExploreTree (binaryTree.leftChild, leftCode, encodedSymbols);
         rightCode := code & "1";
         ExploreTree (binaryTree.rightChild, rightCode, encodedSymbols);
      end if;
   end ExploreTree;


   procedure GetTextCode (binaryTree : in treeNodePointer; symbolsHashTable : in hashMap; encodedSymbols : out hashMap2) is
   
   code : Unbounded_String;
   
   begin
      InitialiseHashTable2 (encodedSymbols, hashTableSize);
      ExploreTree (binaryTree, code, encodedSymbols);
   end GetTextCode;



-- Beginning of CreateFile.
   procedure PutSymbols (encodedSymbols : in hashMap2; encodedFile : in out File_Type) is
   begin
      for i in 1 .. encodedSymbols.length loop
         if encodedSymbols.entryNodeArray (i) /= null then
            Put (encodedFile, "'"); Put (encodedFile, encodedSymbols.entryNodeArray (i).key); Put (encodedFile, "', ");
         end if;
      end loop;
      New_Line (encodedFile);
   end PutSymbols;


   procedure InfixBrowsing (binaryTree : in treeNodePointer; infixTree : out Unbounded_String; it : in out Integer) is
   
   current, previous : treeNodePointer;
   
   begin
      current := binaryTree;
      while current.leftChild /= null loop
         infixTree := infixTree & "0";
         previous := current;
         current := current.leftChild;
      end loop;
      infixTree := infixTree & "1";
      if previous.rightChild.leftChild = null  and it /= 1 then
         InfixBrowsing (root.rightChild, infixTree, 0);
      end if;
   end InfixBrowsing;


   procedure EncodeText (inputFile : in File_Type; encodedFile : in out File_Type; encodedSymbols : in hashMap2) is
   
   fileCharacter : String (1 .. 1);
   result : String (1 .. 8);
   hashedKey : Integer;
   
   begin
      New_Line (encodedFile);
      while not End_Of_File (inputFile) loop
         for i in 1 .. 8 loop
            Get (fileCharacter);
            result (i) := fileCharacter (fileCharacter'First);
            hashedKey := Hash2 (result);
            Put (encodedFile, To_String (encodedSymbols.entryNodeArray (hashedKey).value)); Put (encodedFile, ".");
         end loop;
      end loop;
   end EncodeText;


   procedure CreateFile (binaryTree : in tree; encodedSymbols : in hashMap; encodedFile : out File_Type) is

   infixTree : Unbounded_String;

   begin
      Create (encodedFile, Out_File, "output.hff");
      PutSymbols (encodedSymbols, encodedFile);
      InfixBrowsing (binaryTree, encodedFile, infixTree);
      EncodeText (inputFile, encodedFile, encodedSymbols);
   end CreateFile;

end COMPRESSION;