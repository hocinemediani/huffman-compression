with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;

package THCharCode is

   type entryNode2;

   type entryNodePointer2 is access entryNode2;

   type nodeArray2 is array (1 .. 256) of entryNodePointer2;

   type entryNode2 is record
      key : Unbounded_String;
      value : Unbounded_String;
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
   procedure InitialiseHashTable2 (HashTable : in out hashMap2; Length : in Integer) with
      Post => IsEmpty2 (HashTable);


   -- Destroy the hash map.
   procedure DestroyHashTable2 (HashTable : in out hashMap2);


   -- Check if a hash map is empty.
   function IsEmpty2 (HashTable : in hashMap2) return Boolean;


   -- Get the number of elements in a hash map.
   function GetSize2 (HashTable : in hashMap2) return Integer with
      Post => GetSize2'Result >= 0
         and (GetSize2'Result = 0) = IsEmpty2 (HashTable);


   -- Hash a key.
   function HashKey2 (Key : in String) return Integer;


   -- Registers a new value associated to a key or update it.
   procedure Register2 (HashTable : in out hashMap2; Key : in Unbounded_String; Value : in Unbounded_String) with
      Post => IsIn2 (HashTable, Key) and (ValueOf2 (HashTable, Key) = Value)
         and (not (IsIn2 (HashTable, Key)'Old) or GetSize2 (HashTable) = GetSize2 (HashTable)'Old)
         and (IsIn2 (HashTable, Key)'Old or GetSize2 (HashTable) = GetSize2 (HashTable)'Old + 1);


   -- Deletes a node in the hash map with the exception Cle_Absente_Exception.
   procedure Delete2 (HashTable : in out hashMap2; Key : in Unbounded_String) with
      Post => GetSize2 (HashTable) = GetSize2 (HashTable)'Old - 1
         and not IsIn2 (HashTable, Key);


   -- Check if a key is in the hash map.
   function IsIn2 (HashTable : in hashMap2; Key : in Unbounded_String) return Boolean;


   -- Get the value associated to a key with the exception Cle_Absente_Exception.
   function ValueOf2 (HashTable : in hashMap2; Key : in Unbounded_String) return Unbounded_String;


   -- Display the hash map.
   procedure DisplayHashTable2 (HashTable : in hashMap2);

   -- Apply a treatment to all of the hash table.
   generic
		with procedure Treat2 (Cle : in Unbounded_String; Valeur: in Unbounded_String);
	procedure ForAll2 (HashTable : in hashMap2);

end THCharCode;