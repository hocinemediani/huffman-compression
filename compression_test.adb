with Ada.Text_IO;             use Ada.Text_IO;
with COMPRESSION;             use COMPRESSION;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with TH;                      use TH;
with THCharCode;              use THCharCode;

procedure COMPRESSION_TEST is

   inputFile : File_Type;
   binaryTree : treeNodePointer;
   infixTree : Unbounded_String;
   symbolsTable : hashMap;
   encodedSymbols : hashMap2;
   encodedFile : File_Type;
   it : Integer := 0;
   storageTree : treeQueue;
   

   function "+" (Item : in String) return Unbounded_String
		renames To_Unbounded_String;


   -- Used to initialise a hash table with the characters present in the inputFile.
   arrayLength : CONSTANT Integer := 12;

   Symbols : CONSTANT array (1 .. arrayLength) of String (1 .. 8)
      := ("01101110", "01101101", "11111111", "01110100", "01110011", "01101111", "01100010", "01100001", "01110000", "01101100","01100101", "00100000");
   
   Occurences : CONSTANT array (1 .. arrayLength) of Integer
      := (2, 2, 0, 1, 1, 2, 1, 1, 1, 1, 3, 3);

   HuffmanCode : CONSTANT array (1 .. arrayLength) of Unbounded_String
      := (+"000", +"001", +"01000", +"01001", +"0101", +"011", +"1000", +"1001", +"1010", +"1011", +"110", +"111"); 
   
   InfixHuffmanTree : CONSTANT Unbounded_String := (+"00011000111100011011011");

   procedure TestGetSymbols is
   begin
      GetSymbols (inputFile, symbolsTable);
      
      for i in 0 .. arrayLength loop
         pragma Assert (IsIn (symbolsTable, Symbols (i)));
      end loop;

      for j in 0 .. arrayLength loop
         pragma Assert (ValueOf (symbolsTable, Symbols (j)) = Occurences (j));
      end loop;
   end TestGetSymbols;


   procedure TestBuildHuffmanTree is
   begin
      BuildHuffmanTree (symbolsTable, binaryTree);
      InfixBrowsing (it, storageTree, symbolsTable, binaryTree, infixTree, encodedFile);
      pragma Assert (infixTree = InfixHuffmanTree);
   end TestBuildHuffmanTree;


   procedure TestGetTextCode is
   begin
      GetTextCode (binaryTree, encodedSymbols);

      for j in 0 .. arrayLength loop
        pragma Assert (ValueOf2 (encodedSymbols, To_Unbounded_String (Symbols(j))) = To_String (HuffmanCode(j))); 
      end loop;
   end TestGetTextCode;


begin
   TestGetSymbols;
   TestBuildHuffmanTree;
   TestGetTextCode;
   Put_Line ("Fin des tests!");
end COMPRESSION_TEST;