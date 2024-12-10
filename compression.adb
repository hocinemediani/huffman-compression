package body COMPRESSION is

   hashTableSize : CONSTANT Integer := 256;

   procedure GetSymbols (textToCompress : in File_type; symbolsHashTable : out hashMap) is

   fileCharacter : Character;

   begin
      InitialiseHashTable (symbolsHashTable, hashTableSize);
      Open (textToCompress, In_File, "input.txt");
      while not End_Of_File (textToCompress) loop
         Get (fileCharacter);
         if IsIn (symbolsHashTable, fileCharacter) then
            Register (symbolsHashTable, fileCharacter, 1);
         else
            Register (symbolsHashTable, fileCharacter, ValueOf (symbolsHashTable, fileCharacter) + 1);
         end if;
      end loop;
      Close (textToCompress);
   end GetSymbols;


   procedure BuildHuffmanTree (textToCompress : in File_type; binaryTree : out tree) is
   begin
      binaryTree := new treeNode' ('A', 5, null, null);
   end BuildHuffmanTree;


   procedure GetTreeStructure (binaryTree : in tree; huffmanTree : out tree) is
   begin
      huffmanTree := new treeNode' ('A', 5, null, null);
   end GetTreeStructure;


   procedure GetTextCode (symbolsHashTable : in hashMap; encodedSymbols : out hashMap) is
   begin
      InitialiseHashTable (encodedSymbols, hashTableSize);
   end GetTextCode;

end COMPRESSION;