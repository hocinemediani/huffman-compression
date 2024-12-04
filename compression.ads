with Ada.Text_IO; use Ada.Text_IO;
with TH;          use TH;

generic

package COMPRESSION is

	type treeNode is private;
	type tree is private;
	type treeNodeArray is private;

   -- Store the sybols used in the text file.
	procedure GetSymbols (textToCompress : in File_type; symbolsHashTable : out hashMap);


   -- Build the Huffman tree from the symbols and their number of occurences
	procedure BuildHuffmanTree (textToCompress : in File_type; binaryTree : out tree);


	-- Stocker la structure de l'arbre de Huffman
	procedure GetTreeStructure (binaryTree : in tree; huffmanTree : out tree);


	-- Stocker l'encodage des différents caractères présents dans le fichier
	procedure GetTextCode (symbolsHashTable : in hashMap; encodedSymbols : out hashMap);

private

	type treeNode is record
		symbol : Character;
		occurrences : Integer;
		rightChild : tree;
		leftChild : tree;
	end record;
	
	type tree is access treeNode;
	
	type treeNodeArray is array (0 .. 256) of tree;

end COMPRESSION;