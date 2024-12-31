with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Text_IO;             use Ada.Text_IO;

package DECOMPRESSION is

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

procedure DecompressionProcedure;

end DECOMPRESSION;