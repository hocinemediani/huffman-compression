with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;             use Ada.Text_IO;
with TH;                      use TH;
with THCharCode;              use THCharCode;

package COMPRESSION is

   type treeNode;

   type treeNodePointer is access treeNode;

   type treeNodeArray is array (1 .. 256) of treeNodePointer;

   type treeQueue is record
      last : treeNodePointer;
      storageArray : treeNodeArray;
      realSize : Integer;
   end record;

	type treeNode is record
		symbol : String (1 .. 8);
		occurrences : Integer;
		rightChild : treeNodePointer;
		leftChild : treeNodePointer;
      parent : treeNodePointer;
      isSeen : Boolean;
	end record;

   procedure MainProcedure;

   -- Store the sybols used in the text file.
	procedure GetSymbols (textToCompress : in out File_Type; symbolsHashTable : out hashMap) with
         Pre => not End_Of_File (textToCompress);

   -- This procedure will go through the entire text file and add each character to the symbolsHashTable.
      -- If a character is already inside the hashtable then we need to update the number of occurences by 1.
      -- Else we create the entry.
      -- In the end, we obtain a hash table with each used character and the number of occurences.
      -- The last character is always added and is the ending symbol, with an occurence of 0.


   -- Build the Huffman tree from the symbols and their number of occurences
	procedure BuildHuffmanTree (symbolsHashTable : in hashMap; binaryTree : out treeNodePointer) with
         Pre => symbolsHashTable.size > 0,
         Post => binaryTree /= Null;

   -- This procedure will create the Huffman Tree from the characters present in the text files and their occurences.
      -- First we need to order the indexes of the characters in the hash table from lowest occurence to highest.
      procedure SortArray (storageTree : in out treeQueue) with
            Pre => storageTree.realSize > 0,
            Post => storageTree.realSize > 0;

      -- Then at each iterations, we take the two least used characters and create a node.
         -- The resulting node has a nul character, thus indicating that it is only a node with a weight, not characters.
         -- We also keep track of the created sub-trees in the treeArray
      procedure CreateNode (storageTree : in out treeQueue);
         -- We then put the node in the hash map, where it would be stored if it was a character.
         -- And we call BuildHuffmanTree again, with the fullHashTable as the first argument.
         -- The iterations stop when we only have one entry in the hash table, with a nul character.
         -- We also need to update the tree structure each time we create a new node (some nodes will point to another, some will be pointed to).
        
   -- Store the encoding of each symbols in a new hash table.
	procedure GetTextCode (binaryTree : in treeNodePointer; encodedSymbols : out hashMap2);

   -- This procedure will encode each character by exploring the Huffman tree we created.
      -- So we need to explore the Tree until we have maped each character to a code.
      procedure ExploreTree (binaryTree : in treeNodePointer; code : in out Unbounded_String; encodedSymbols : in out hashMap2);
      -- And create a final hash table that will store the characters and their code.


	-- Create the file with the symbols, the tree structure and the encoded text.
	procedure CreateFile (modeBavard : in Boolean; fileName : in Unbounded_String; storageTree : in treeQueue; symbolsHashTable : in hashMap; binaryTree : in treeNodePointer; encodedSymbols : in hashMap2; encodedFile : out File_Type; infixTree : in out Unbounded_String) with
         Pre => encodedSymbols.size > 0,
         Post => not End_Of_File (encodedFile);

   -- This procedure will create a new file that contains all of the symbols used, the tree structure and the encoded text.
      -- We first need to put in the file every symbols used, sorted by number of occurences.
      procedure PutSymbols (symbolsHashTable : in hashMap; it : in Integer; symbol : in String; encodedFile : in out File_Type);
      -- Then we need to find the tree structure by infixed browsing of the tree and put it next to the characters used.
      procedure InfixBrowsing (it : in out Integer; storageTree : in treeQueue; symbolsHashTable : in hashMap; binaryTree : in treeNodePointer; infixTree : out Unbounded_String; encodedFile : in out File_Type);

      procedure DisplayTree (it : in out Integer; storageTree : in treeQueue; symbolsHashTable : in hashMap; binaryTree : in treeNodePointer; depth : in out Integer; LeftOrRight : in out Boolean);
      -- And finally we need to encode the text.
      -- For that, we iterate through the characters in the raw text file and we store the encoded version of it.
      procedure EncodeText (textToCompress : in out File_Type; encodedFile : in out File_Type; encodedSymbols : in hashMap2);

      procedure DestroyEverything (symbolsHashTable : in out hashMap; encodedSymbols: in out hashMap2; treeStorageArray : in out treeNodeArray);

end COMPRESSION;