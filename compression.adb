package body COMPRESSION is

   hashTableSize : CONSTANT Integer := 256;

   procedure GetSymbols (textToCompress : in File_type; symbolsHashTable : out hashMap) is
   begin
      InitialiseHashTable (symbolsHashTable, hashTableSize);
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