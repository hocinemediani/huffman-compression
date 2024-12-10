with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;             use Ada.Text_IO;
with TH;                      use TH;

package COMPRESSION is

   type treeNode;
   type tree;

   type treeNodePointer is access treeNode;

   type treeNodeArray is array (0 .. 256) of treeNodePointer;

   type tree is record
      root : treeNodePointer;
      treeArray : treeNodeArray;
   end record;

	type treeNode is record
		symbol : Character;
		occurrences : Integer;
		rightChild : tree;
		leftChild : tree;
	end record;

   -- Store the sybols used in the text file.
	procedure GetSymbols (textToCompress : in File_Type; symbolsHashTable : out hashMap) with
         Pre => not End_Of_File (textToCompress);

   -- This procedure will go through the entire text file and add each character to the symbolsHashTable.
      -- If a character is already inside the hashtable then we need to update the number of occurences by 1.
         procedure IsInTable (symbol : in Character; symbolsHashTable : in hashMap) with 
               Pre => symbolsHashTable.size > 0;
      -- Else we create the entry.
      -- In the end, we obtain a hash table with each used character and the number of occurences.
      -- The last character is always added and is the ending symbol, with an occurence of 0.


   -- Build the Huffman tree from the symbols and their number of occurences
	procedure BuildHuffmanTree (symbolsHashTable: in hashMap; binaryTree : out tree) with
         Pre => symbolsHashTable.size > 0,
         Post => binaryTree.root /= Null;

   -- This procedure will create the Huffman Tree from the characters present in the text files and their occurences.
      -- First we need to order the indexes of the characters in the hash table from lowest occurence to highest.
      procedure SortHashTable (symbolsHashTable : in hashMap; sortedHashTable : out hashMap) with
            Pre => symbolsHashTable.size > 0,
            Post => sortedHashTable.size > 0;

      -- Then at each iterations, we take the two least used characters and create a node.
         -- The resulting node has a nul character, thus indicating that it is only a node with a weight, not characters.
         -- We also keep track of the created sub-trees in the treeArray
         procedure CreateNode (firstNode : in treeNode; secondNode : in treeNode; resultNode : out treeNode; treeArray : out treeNodeArray);
         -- We then put the node in the hash map, where it would be stored if it was a character.
         procedure PutNode (node : in treeNode; sortedHashTable : in hashMap; fullHashTable : out hashMap) with
               Pre => sortedHashTable.size > 0,
               Post => fullHashTable.size > 0;

         -- And we call BuildHuffmanTree again, with the fullHashTable as the first argument.
         -- The iterations stop when we only have one entry in the hash table, with a nul character.
         -- We also need to update the tree structure each time we create a new node (some ndoes will point to another, some will be pointed to)
         procedure UpdateTree (treeArray : in treeNodeArray; binaryTree : out tree) with
               Post => binaryTree.root /= Null;
        
   -- Store the encoding of each symbols in a new hash table.
	procedure GetTextCode (binaryTree : in tree; symbolsHashTable : in hashMap; encodedSymbols : out hashMap) with
         Pre => symbolsHashTable.size > 0,
         Post => encodedSymbols.size = symbolsHashTable.size;

   -- This procedure will encode each character by exploring the Huffman tree we created.
      -- So we need to explore the Tree until we have maped each character to a code.
      procedure ExploreTree (binaryTree : in tree; symbol : out Character; code : out String);
      -- And create a final hash table that will store the characters and their code.
      procedure UpdateEncodedHashTable (symbol : in Character; code : in String; encodedSymbols : out hashMap);


	-- Create the file with the symbols, the tree structure and the encoded text.
	procedure CreateFile (binaryTree : in tree; encodedSymbols : in hashMap; encodedFile : out File_Type) with
         Pre => encodedSymbols.size > 0,
         Post => not End_Of_File (encodedFile);

   -- This procedure will create a new file that contains all of the symbols used, the tree structure and the encoded text.
      -- We first need to put in the file every symbols used, sorted by number of occurences.
      procedure PutSymbols (encodedSymbols : in hashMap; encodedFile : out File_Type);
      -- Then we need to find the tree structure by infixed browsing of the tree and put it next to the characters used.
      procedure InfixBrowsing (binaryTree : in tree; encodedFile : in out File_Type; infixTree : out Unbounded_String);
      -- And finally we need to encode the text.
      -- For that, we iterate through the characters in the raw text file and we store the encoded version of it.
      procedure EncodeText (encodedFile : in out File_Type; encodedSymbols : in hashMap);

end COMPRESSION;