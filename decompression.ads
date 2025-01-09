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


procedure ExploreText (encodedFile : in out File_Type; decodedFile : in out File_Type; fileName : in Unbounded_String; symbolsArray : in out StringArray; binaryTree : in treeNodePointer);


function ExploreTree (code : in out Unbounded_String; root : in treeNodePointer) return String;


procedure ReconstructHuffmanTree (it : in out Integer; encodedFile : in File_Type; binaryTree : in out treeNodePointer; infixTree : in out Unbounded_String; previous : in out treeNodePointer; symbolsArray : in stringArray);

end DECOMPRESSION;