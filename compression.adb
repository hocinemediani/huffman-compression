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
      DisplayHashTable (symbolsHashTable);
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
   
   begin
      storageTree.realSize := 0;
      storageTree.last := null;
      for k in 1 .. 256 loop
         storageTree.storageArray (k) := null;
      end loop;
      for i in 1 .. hashTableSize loop
         if symbolsHashTable.entryNodeArray (i) /= null then
            declare
               newNode : CONSTANT treeNodePointer := new treeNode' (symbolsHashTable.entryNodeArray (i).key, symbolsHashTable.entryNodeArray (i).value, null, null, null, False);
            begin
               storageTree.storageArray (current) := newNode;
               current := current + 1;
            end;
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
      DisplayHashTable2 (encodedSymbols);
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


   procedure InfixBrowsing (storageTree : in treeQueue; symbolsHashTable : in hashMap; binaryTree : in treeNodePointer; infixTree : out Unbounded_String) is
   
   current : treeNodePointer;
   
   begin
      current := binaryTree;
      if current = storageTree.storageArray (1) and current.leftChild /= null and current.leftChild.isSeen and current.rightChild /= null and current.rightChild.isSeen then
         Null;
      else
         if current.leftChild /= null then
            if current.leftChild.isSeen and current.rightChild.isSeen then
               current.All.isSeen := True;
               InfixBrowsing (storageTree, symbolsHashTable, current.parent, infixTree);
            elsif current.leftChild.isSeen then
               InfixBrowsing (storageTree, symbolsHashTable, current.rightChild, infixTree);
            else
               infixTree := infixTree & To_Unbounded_String ("0");
               current := current.leftChild;
               InfixBrowsing (storageTree, symbolsHashTable, current, infixTree);
            end if;
         else
            current.All.isSeen := True;
            infixTree := infixTree & To_Unbounded_String ("1");
            InfixBrowsing (storageTree, symbolsHashTable, current.parent, infixTree);
         end if;
      end if;
   end InfixBrowsing;


   procedure EncodeText (textToCompress : in out File_Type; encodedFile : in out File_Type; encodedSymbols : in hashMap2) is
   
   fileCharacter : String (1 .. 1);
   result : String (1 .. 8);
   hashedKey : Integer;
   
   begin
      Open (textToCompress, In_File);
      New_Line (encodedFile);
      while not End_Of_File (textToCompress) loop
         for i in 1 .. 8 loop
            Get (fileCharacter);
            result (i) := fileCharacter (fileCharacter'First);
            hashedKey := HashKey2 (result);
            Put (encodedFile, To_String (encodedSymbols.entryNodeArray (hashedKey).value)); Put (encodedFile, ".");
         end loop;
      end loop;
   end EncodeText;


   procedure CreateFile (storageTree : in treeQueue; symbolsHashTable : in hashMap; binaryTree : in treeNodePointer; encodedSymbols : in hashMap2; encodedFile : out File_Type; infixTree : in out Unbounded_String) is

   inputFile : File_Type;

   begin
      Put_Line ("ici");
      Create (encodedFile, Out_File, "output.txt.hff");
      Put_Line ("ici");
      PutSymbols (encodedSymbols, encodedFile);
      Put_Line ("ici");
      EncodeText (inputFile, encodedFile, encodedSymbols);
      Put_Line ("ici");
   end CreateFile;


   procedure DestroyEverything (symbolsHashTable : in out hashMap; encodedSymbols: in out hashMap2; treeStorageArray : in out treeNodeArray) is
   begin
      DestroyHashTable (symbolsHashTable);
      DestroyHashTable2 (encodedSymbols);
      for i in 1 .. 128 loop
         Free3 (treeStorageArray (i));
      end loop;
   end DestroyEverything;


   procedure DisplayHuffmanTree (binaryTree : treeNodePointer; treeArray : treeNodeArray) is
   begin
      Null;
   end DisplayHuffmanTree;


   textToCompress : File_Type;
   symbolsHashTable : hashMap;
   storageTree : treeQueue;
   binaryTree : treeNodePointer;
   encodedSymbols : hashMap2;
   encodedFile : File_Type;
   infixTree : Unbounded_String;

procedure MainProcedure is

   begin
      if Argument_Count = 1 then
         Open (textToCompress, 
               In_File, 
               Argument (1));
      elsif Argument_Count = 2 then
         if Argument (1) = "-b" then
            Put_Line ("Le mode selectionné est bavard");
         end if;
         Open (textToCompress, 
               In_File, 
               Argument (2));
      elsif Argument_Count = 0 then
         Put_Line ("Compression s'utilise de cette manière :");
         Put_Line ("    /compression -b texte.txt");
         Put_Line ("OU  /compression texte.txt");
      else
         Put_Line ("ntm mets au max 2 arguments");
      end if;

      GetSymbols (textToCompress, symbolsHashTable);
      BuildHuffmanTree (symbolsHashTable, binaryTree);
      GetTextCode (binaryTree, encodedSymbols);
      CreateFile (storageTree, symbolsHashTable, binaryTree, encodedSymbols, encodedFile, infixTree);
      DestroyEverything (symbolsHashTable, encodedSymbols, storageTree.storageArray);
      Close (textToCompress);
      Close (encodedFile);

   end MainProcedure;

end COMPRESSION;