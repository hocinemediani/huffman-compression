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
   
   begin
      for i in 1 .. hashTableSize loop
         if symbolsHashTable.entryNodeArray (i) /= null then
            storageTree.storageArray (current) := new treeNode' (symbolsHashTable.entryNodeArray (i).key, symbolsHashTable.entryNodeArray (i).value, null, null, null, False);
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
   procedure PutSymbols (encodedSymbols : in hashMap2; encodedFile : in out File_Type) is
   begin
      for i in 1 .. encodedSymbols.length loop
         if encodedSymbols.entryNodeArray (i) /= null then
            Put (encodedFile, "'"); Put (encodedFile, To_String (encodedSymbols.entryNodeArray (i).key)); Put (encodedFile, "', ");
         end if;
      end loop;
      New_Line (encodedFile);
   end PutSymbols;


   procedure InfixBrowsing (symbolsHashTable : in hashMap; binaryTree : in out treeNodePointer; infixTree : out Unbounded_String) is
   
   current, previous : treeNodePointer;
   
   begin
      current := binaryTree;
      if current.parent = null and current.leftChild.isSeen and current.rightChild.isSeen then
         Null;
      else
         if current.leftChild /= null then
            if current.leftChild.isSeen and current.rightChild.isSeen then
               current.All.isSeen := True;
               InfixBrowsing (symbolsHashTable, current.parent, infixTree);
            elsif current.leftChild.isSeen then
               InfixBrowsing (symbolsHashTable, current.rightChild, infixTree);
            else
               infixTree := infixTree & To_Unbounded_String ("0");
               current := current.leftChild;
               InfixBrowsing (symbolsHashTable, current, infixTree);
            end if;
         else
            current.All.isSeen := True;
            infixTree := infixTree & To_Unbounded_String ("1");
            InfixBrowsing (symbolsHashTable, current.parent, infixTree);
         end if;
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


   procedure CreateFile (symbolsHashTable : in hashMap; binaryTree : in treeNodePointer; encodedSymbols : in hashMap2; encodedFile : out File_Type) is

   infixTree : Unbounded_String;
   inputFile : File_Type;
   it : Integer := 0;

   begin
      Create (encodedFile, Out_File, "output.hff");
      PutSymbols (encodedSymbols, encodedFile);
      InfixBrowsing (symbolsHashTable, binaryTree, infixTree, it);
      EncodeText (inputFile, encodedFile, encodedSymbols);
   end CreateFile;

end COMPRESSION;