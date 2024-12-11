with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

package THCharCode is

   type entryNode2;

   type entryNodePointer2 is access entryNode2;

   type nodeArray2 is array (0 .. 256) of entryNodePointer2;

   type entryNode2 is record
      key : Character;
      value : Integer;
      -- Only used if two or more nodes have the same hashed key.
      next : entryNodePointer2;
   end record;

   type hashMap2 is record
      -- The actual used size of the hash map.
      size : Integer;
      -- The total size of the hash map (used for hashing too).
      length : Integer;
      entryNodeArray : nodeArray2;
   end record;


   -- Initialize an empty hash map.
   procedure InitialiseHashTable2 (HashTable : in out hashMap; Length : in Integer) with
      Post => IsEmpty2 (HashTable);


   -- Destroy the hash map.
   procedure DestroyHashTable2 (HashTable : in out hashMap);


   -- Check if a hash map is empty.
   function IsEmpty2 (HashTable : in hashMap) return Boolean;


   -- Get the number of elements in a hash map.
   function GetSize2 (HashTable : in hashMap) return Integer with
      Post => GetSize2'Result >= 0
         and (GetSize2'Result = 0) = IsEmpty2 (HashTable);


   -- Registers a new value associated to a key or update it.
   procedure Register2 (HashTable : in out hashMap; Key : in Character; Value : in Integer) with
      Post => IsIn2 (HashTable, Key) and (ValueOf2 (HashTable, Key) = Value)
         and (not (IsIn2 (HashTable, Key)'Old) or GetSize2 (HashTable) = GetSize2 (HashTable)'Old)
         and (IsIn2 (HashTable, Key)'Old or GetSize2 (HashTable) = GetSize2 (HashTable)'Old + 1);


   -- Deletes a node in the hash map with the exception Cle_Absente_Exception.
   procedure Delete2 (HashTable : in out hashMap; Key : in Character) with
      Post => GetSize2 (HashTable) = GetSize2 (HashTable)'Old - 1
         and not IsIn2 (HashTable, Key);


   -- Check if a key is in the hash map.
   function IsIn2 (HashTable : in hashMap; Key : in Character) return Boolean;


   -- Get the value associated to a key with the exception Cle_Absente_Exception.
   function ValueOf2 (HashTable : in hashMap; Key : in Character) return Integer;


   -- Display a node.
   procedure Display2 (Key : in Character; Value : in Integer);


   -- Display the hash map.
   procedure DisplayHashTable2 (HashTable : in hashMap);

   -- Apply a treatment to all of the hash table.
   generic
		with procedure Treat2 (Cle : in Character; Valeur: in Integer);
	procedure ForAll2 (HashTable : in hashMap);

end THCharCode;