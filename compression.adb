package body COMPRESSION is

   hashTableSize : CONSTANT Integer := 128;

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


-- Beginning of BuildHuffmanTree
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


-- Beginning of GetTextCode
   procedure GetTextCode (binaryTree : in tree; symbolsHashTable : in hashMap; encodedSymbols : out hashMap) is
   begin
      Null;
   end GetTextCode;


   procedure ExploreTree (binaryTree : in tree; symbol : out Character; code : out String) is
   begin
      Null;
   end ExploreTree;


   procedure UpdateEncodedHashTable (symbol : in Character; code : in String; encodedSymbols : out hashMap) is
   begin
      Null;
   end UpdateEncodedHashTable;


   procedure CreateFile (binaryTree : in tree; encodedSymbols : in hashMap; encodedFile : out File_Type) is
   begin
      Null;
   end CreateFile;


   procedure PutSymbols (encodedSymbols : in hashMap; encodedFile : out File_Type) is
   begin
      Null;
   end PutSymbols;


   procedure InfixBrowsing (binaryTree : in tree; encodedFile : in out File_Type; infixTree : out Unbounded_String) is
   begin
      Null;
   end InfixBrowsing;


   procedure EncodeText (encodedFile : in out File_Type; encodedSymbols : in hashMap) is
   begin
      Null;
   end EncodeText;

end COMPRESSION;