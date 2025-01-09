with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;             use Ada.Text_IO;
with TH;                      use TH;
with THCharCode;              use THCharCode;

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


procedure ExploreText (encodedFile : in File_Type; fileName : in Unbounded_String; binaryTree : out treeNode);


procedure ReconstructHuffmanTree (encodedFile : in File_Type; binaryTree : out treeNode; infixTree : in out Unbounded_String; previous : treeNodePointer; symbolsArray : in stringArray);

end DECOMPRESSION;