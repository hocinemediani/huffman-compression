with Ada.Text_IO;             use Ada.Text_IO;
with COMPRESSION;             use COMPRESSION;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with TH;                      use TH;

procedure COMPRESSION_TEST is

   -- The file we wanna compress.
   inputFile : File_Type;
   -- A handmade compressed version of the file.
   correctFile : File_Type;
   correctFileCharacter : Character;
   outputFileCharacter : Character;
   -- The file compressed.
   outputFile : File_Type;

   binaryTree : tree;
   symbolsTable : hashMap;
   encodedSymbols : hashMap;
   encodedFile : File_Type;
   
   -- Used to initialise a hash table with the characters present in the inputFile.
   arrayLength : CONSTANT Integer := 11;
   HashTable : hashMap;

   Symbols : CONSTANT array (0 .. arrayLength) of Character
      := ('o', 'm', 'n', 'a', 'p', 's', 't', 'b', "\$", 'l', ' ', "e");
   
   Occurences : CONSTANT array (0 .. arrayLength) of Integer
      := (2, 2, 2, 1, 1, 1, 1, 1, 0, 1, 3, 3);

   HuffmanCode : CONSTANT array (0 .. arrayLength) of Unbounded_String
      := ("000", "001", "010", "0110", "0111", "1000", "1001", "1010", "10110", "10111", "110", "111"); 
   
   InfixHuffmanTree : CONSTANT Unbounded_String := ("00011010110001101011011");

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
      PutSymbols (encodedSymbols , encodedFile);
      InfixBrowsing (binaryTree, encodedSymbols, infixTree);
      pragma Assert (infixTree = InfixHuffmanTree);
   end TestBuildHuffmanTree;


   procedure TestGetTextCode is
   begin
      GetTextCode (binaryTree, symbolsTable, encodedSymbols);

      for j in 0 .. arrayLength loop
        pragma Assert (ValueOf (encodedSymbols, Symbols(j)) = HuffmanCode(j)); 
      end loop;
   end TestGetTextCode;


   procedure TestCreateFile is
   begin
      CreateFile (binaryTree, encodedSymbols, outputFile);
      Open (correctFile, InFile, "correct.txt");
      Open (outputFile, Infile, "outputFile.txt");
      while not End_Of_File (correctFile) and not End_Of_File (outputFile) loop
        Get (outputFile, outputFileCharacter);
        Get (correctFile, correctFileCharacter);
        pragma Assert (outputFileCharacter = correctFileCharacter);
      end loop;
      Close (correctFile);
      Close (outputFile);
   end TestCreateFile;


begin
   TestGetSymbols;
   TestBuildHuffmanTree;
   TestGetTextCode;
   TestCreateFile;
end COMPRESSION_TEST;