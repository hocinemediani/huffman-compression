with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;             use Ada.Text_IO;

package DECOMPRESSION is

   type treeNode;

   type treeNodePointer is access treeNode;

   type treeNodeArray is array (1 .. 256) of treeNodePointer;

   type stringArray is array (1 .. 256) of String (1 .. 8);

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

-- Main procedure
procedure DecompressionProcedure;


procedure ExploreText (encodedFile : in out File_Type; decodedFile : in out File_Type; symbolsArray : in out StringArray);


procedure ExploreTree (root : in treeNodePointer; encodedFile : in out File_Type; decodedFile : in out File_Type);


procedure GetRoot(root : in out treeNodePointer);


procedure ReconstructHuffmanTree (it : in out Integer; countInfix : in out Integer; encodedFile : in File_Type ; infixTree : in out Unbounded_String; current : in out treeNodePointer; symbolsArray : in stringArray);

end DECOMPRESSION;
