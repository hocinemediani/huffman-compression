with Ada.Text_IO;             use Ada.Text_IO;
with COMPRESSION;             use COMPRESSION;
with TH;                      use TH;

procedure COMPRESSION_TEST is

   -- The file we wanna compress.
   inputFile : File_Type;
   -- A handmade compressed version of the file.
   correctFile : File_Type;

   symbolsTable : hashMap;
   binaryTree : tree;

   -- Used to initialise a hash table with the characters present in the inputFile.
   arrayLength : CONSTANT Integer := 11;
   HashTable : hashMap;

   Symbols : CONSTANT array (0 .. 11) of Character
      := ('l', 'a', 'p', 'o', 'm', 'e', 's', 't', 'b', 'n', ' ', '.');
   
   Occurences : CONSTANT array (0 .. 11) of Integer
      := (1, 1, 1, 2, 2, 3, 1, 1, 1, 2, 3, 1);


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

   end TestBuildHuffmanTree;


   procedure TestGetTextCode is
   begin

   end TestGetTextCode;


   procedure TestCreateFile is
   begin

   end TestCreateFile;


begin
   TestGetSymbols;
   TestBuildHuffmanTree;
   TestGetTextCode;
   TestCreateFile;
end COMPRESSION_TEST;