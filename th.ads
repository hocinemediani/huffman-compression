package TH is

   type characterByte is mod 2**8;

   type entryNode;

   type entryNodePointer is access entryNode;

   type nodeArray is array (0 .. 256) of entryNodePointer;

   type entryNode is record
      key : String (1 .. 8);
      value : Integer;
      -- Only used if two or more nodes have the same hashed key.
      next : entryNodePointer;
   end record;

   type hashMap is record
      -- The actual used size of the hash map.
      size : Integer;
      -- The total size of the hash map (used for hashing too).
      length : Integer;
      entryNodeArray : nodeArray;
   end record;


   -- Initialize an empty hash map.
   procedure InitialiseHashTable (HashTable : in out hashMap; Length : in Integer) with
      Post => IsEmpty (HashTable);


   -- Destroy the hash map.
   procedure DestroyHashTable (HashTable : in out hashMap);


   -- Check if a hash map is empty.
   function IsEmpty (HashTable : in hashMap) return Boolean;


   -- Get the number of elements in a hash map.
   function GetSize (HashTable : in hashMap) return Integer with
      Post => GetSize'Result >= 0
         and (GetSize'Result = 0) = IsEmpty (HashTable);


   -- Hash a key.
   function Hash (Key : in String) return Integer;


   -- Registers a new value associated to a key or update it.
   procedure Register (HashTable : in out hashMap; Key : in String; Value : in Integer) with
      Post => IsIn(HashTable, Key) and (ValueOf (HashTable, Key) = Value)
         and (not (IsIn (HashTable, Key)'Old) or GetSize (HashTable) = GetSize (HashTable)'Old)
         and (IsIn (HashTable, Key)'Old or GetSize (HashTable) = GetSize (HashTable)'Old + 1);


   -- Deletes a node in the hash map with the exception Cle_Absente_Exception.
   procedure Delete (HashTable : in out hashMap; Key : in String) with
      Post => GetSize (HashTable) = GetSize (HashTable)'Old - 1
         and not IsIn (HashTable, Key);


   -- Check if a key is in the hash map.
   function IsIn (HashTable : in hashMap; Key : in String) return Boolean;


   -- Get the value associated to a key with the exception Cle_Absente_Exception.
   function ValueOf (HashTable : in hashMap; Key : in String) return Integer;


   -- Display a node.
   procedure Display (Key : in String; Value : in Integer);


   -- Display the hash map.
   procedure DisplayHashTable (HashTable : in hashMap);

   -- Apply a treatment to all of the hash table.
   generic
		with procedure Treat (Cle : in String; Valeur: in Integer);
	procedure ForAll (HashTable : in hashMap);

end TH;