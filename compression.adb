package body COMPRESSION is

   hashTableSize : CONSTANT Integer := 256;

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
      Close (textToCompress);
   end GetSymbols;


   procedure BuildHuffmanTree (symbolsHashTable: in hashMap; binaryTree : out tree) is
   begin
      Null;
   end BuildHuffmanTree;


   procedure SortHashTable (symbolsHashTable : in hashMap; sortedHashTable : out hashMap) is
   begin
      Null;
   end SortHashTable;


   procedure CreateNode (firstNode : in treeNode; secondNode : in treeNode; resultNode : out treeNode; treeArray : out treeNodeArray) is
   begin
      Null;
   end CreateNode;


   procedure PutNode (node : in treeNode; sortedHashTable : in hashMap; fullHashTable : out hashMap) is
   begin
      Null;
   end PutNode;


   procedure UpdateTree (treeArray : in treeNodeArray; binaryTree : out tree) is
   begin
      Null;
   end UpdateTree;


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