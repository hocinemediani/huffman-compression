with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Integer_Text_IO;    use Ada.Integer_Text_IO;
with SDA_Exceptions;         use SDA_Exceptions;
with Ada.Unchecked_Deallocation;

package body TH is

   procedure Free is
      new Ada.Unchecked_Deallocation (Object => entryNode, Name => entryNodePointer);


   procedure InitialiseHashTable (HashTable : in out hashMap; Length : in Integer) is
   begin
      HashTable.size := 0;
      HashTable.length := Length;
      for i in 0 .. (HashTable.length - 1) loop
         HashTable.entryNodeArray (i) := null;
      end loop;
   end InitialiseHashTable;


   procedure DestroyHashTable (HashTable : in out hashMap) is

   previous, current : entryNodePointer;

   begin
      -- Exploring the nodes.
      for i in 0 .. (HashTable.length - 1) loop
         current := HashTable.entryNodeArray (i);
         if current /= null then
            while current /= null loop
               previous := current;
               current := previous.next;
               Free (previous);
            end loop;
            Free (previous);
            Free (current);
         end if;
         HashTable.entryNodeArray (i) := null;
      end loop;
      HashTable.size := 0;
    end DestroyHashTable;


   function IsEmpty (HashTable : in hashMap) return Boolean is
   begin
      -- Size is the actual numbers of entry in the hash map.
      return HashTable.size = 0;
   end IsEmpty;


   function GetSize (HashTable : in hashMap) return Integer is
   begin
      return HashTable.size;
   end GetSize;


   procedure Register (HashTable : in out hashMap; Key : in Unbounded_String; Value : in Integer) is

   current, previous, firstNode : entryNodePointer;
   hashedKey : CONSTANT Integer := To_String (Key)'Length mod HashTable.length;

   begin
      current := HashTable.entryNodeArray (hashedKey);
      firstNode := current;
      while current /= null loop
         if current.key = Key then
            current.value := Value;
            return;
         end if;
         previous := current;
         current := current.next;
      end loop;
      declare
         newNode : CONSTANT entryNodePointer := new entryNode' (key => Key, value => Value, next => null);
      begin
         if current = firstNode then
            HashTable.entryNodeArray (hashedKey) := newNode;
            HashTable.size := HashTable.size + 1;
            return;
         end if;
         previous.next := newNode;
         HashTable.size := HashTable.size + 1;
      end;
   end Register;


   procedure Delete (HashTable : in out hashMap; Key : in Unbounded_String) is

   previous, current : entryNodePointer;
   hashedKey : CONSTANT Integer := To_String (Key)'Length mod HashTable.length;

   begin
      current := HashTable.entryNodeArray (hashedKey);
      previous := HashTable.entryNodeArray (hashedKey);
      while current /= null loop
         if current.key = Key then
            if current = HashTable.entryNodeArray (hashedKey) then
               HashTable.entryNodeArray (hashedKey) := HashTable.entryNodeArray (hashedKey).next;
            end if;
            if current.next /= null then
               previous.next := current.next;
            end if;
            if current.next = null then
               previous.next := null;
            end if;
            Free (current);
            HashTable.size := HashTable.size - 1;
            return;
         end if;
         previous := current;
         current := current.next;
      end loop;
   end Delete;


   function IsIn (HashTable : in hashMap; Key : in Unbounded_String) return Boolean is
    
   current : entryNodePointer;
   hashedKey : CONSTANT Integer := To_String (Key)'Length mod HashTable.length;
    
   begin
      current := HashTable.EntryNodeArray (hashedKey);
      while current /= null loop
         if current.key = Key then
            return True;
         end if;
         current := current.next;
      end loop;
      return False;
   end IsIn;


   function ValueOf (HashTable : in hashMap; Key : in Unbounded_String) return Integer is

   current : entryNodePointer;
   hashedKey : CONSTANT Integer := To_String (Key)'Length mod HashTable.length;

   begin
      current := HashTable.entryNodeArray (hashedKey);
      while current /= null loop
         if current.key = Key then
            return current.value;
         end if;
         current := current.next;
      end loop;
      raise Cle_Absente_Exception;
   end ValueOf;


   procedure Display (Key : in Unbounded_String; Value : in Integer) is
   begin
      Put("-->["); Put ('"'); Put (To_String (Key)); Put ('"'); Put (" : "); Put (Value, 1); Put("]");
   end Display;


   procedure DisplayHashTable (HashTable : in hashMap) is

   current : entryNodePointer;
        
   begin
      for i in 0 .. (HashTable.length - 1) loop
         current := HashTable.entryNodeArray (i);
         Put (i, 1); Put (" : ");
         if current /= null then
            Display (HashTable.entryNodeArray (i).key, HashTable.entryNodeArray (i).value);
            while current.next /= null loop
               Display (current.next.key, current.next.value);
               current := current.next;
            end loop;
         end if;
         Put_Line("--E");
      end loop;
      New_Line;
   end DisplayHashTable;


   procedure ForAll (HashTable : in hashMap) is

   current : entryNodePointer;

   begin
      for i in 0 .. (HashTable.length - 1) loop
         current := HashTable.entryNodeArray (i);
         while current /= null loop
            begin
               Treat (current.key, current.value);
               exception
                  when others => null;
            end;
            current := current.next;
         end loop;
      end loop;
   end ForAll;

end TH;