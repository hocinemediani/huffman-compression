with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Integer_Text_IO;    use Ada.Integer_Text_IO;
with SDA_Exceptions;         use SDA_Exceptions;
with Ada.Unchecked_Deallocation;

package body THCharCode is

   procedure Free is
      new Ada.Unchecked_Deallocation (Object => entryNode2, Name => entryNodePointer2);


   procedure InitialiseHashTable2 (HashTable : in out hashMap2; Length : in Integer) is
   begin
      HashTable.size := 0;
      HashTable.length := Length;
      for i in 0 .. (HashTable.length - 1) loop
         HashTable.entryNodeArray (i) := null;
      end loop;
   end InitialiseHashTable2;


   procedure DestroyHashTable2 (HashTable : in out hashMap2) is

   previous, current : entryNodePointer2;

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
    end DestroyHashTable2;


   function IsEmpty2 (HashTable : in hashMap2) return Boolean is
   begin
      -- Size is the actual numbers of entry in the hash map.
      return HashTable.size = 0;
   end IsEmpty;


   function GetSize2 (HashTable : in hashMap2) return Integer is
   begin
      return HashTable.size;
   end GetSize2;


   function Hash2 (Key : in String) return Integer is  

   result : Integer := 0;

   begin
      for i in 1 .. 8 loop
         if Integer'Value (Key (i)'Image) = 1 then
            result := result + 2 ** (8 - i);
         end if;
      end loop;

      return result;
   end Hash;


   procedure Register2 (HashTable : in out hashMap2; Key : in String; Value : in String) is

   current, previous, firstNode : entryNodePointer2;
   hashedKey : CONSTANT Integer := Character'Pos (Key);

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
         newNode : CONSTANT entryNodePointer2 := new entryNode2' (key => Key, value => Value, next => null);
      begin
         if current = firstNode then
            HashTable.entryNodeArray (hashedKey) := newNode;
            HashTable.size := HashTable.size + 1;
            return;
         end if;
         previous.next := newNode;
         HashTable.size := HashTable.size + 1;
      end;
   end Register2;


   procedure Delete2 (HashTable : in out hashMap2; Key : in String) is

   previous, current : entryNodePointer2;
   hashedKey : CONSTANT Integer := Character'Pos (Key);

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
   end Delete2;


   function IsIn2 (HashTable : in hashMap2; Key : in String) return Boolean is
    
   current : entryNodePointer2;
   hashedKey : CONSTANT Integer := Character'Pos (Key);
    
   begin
      current := HashTable.entryNodeArray (hashedKey);
      while current /= null loop
         if current.key = Key then
            return True;
         end if;
         current := current.next;
      end loop;
      return False;
   end IsIn2;


   function ValueOf2 (HashTable : in hashMap2; Key : in String) return String is

   current : entryNodePointer2;
   hashedKey : CONSTANT Integer := Character'Pos (Key);

   begin
      current := HashTable.entryNodeArray (hashedKey);
      while current /= null loop
         if current.key = Key then
            return current.value;
         end if;
         current := current.next;
      end loop;
      raise Cle_Absente_Exception;
   end ValueOf2;


   procedure Display2 (Key : in String; Value : in String) is
   begin
      Put("-->["); Put ('"'); Put (Key); Put ('"'); Put (" : "); Put (Value); Put("]");
   end Display2;


   procedure DisplayHashTable2 (HashTable : in hashMap2) is

   current : entryNodePointer2;
        
   begin
      for i in 0 .. (HashTable.length - 1) loop
         current := HashTable.entryNodeArray (i);
         Put (i, 1); Put (" : ");
         if current /= null then
            Display2 (HashTable.entryNodeArray (i).key, HashTable.entryNodeArray (i).value);
            while current.next /= null loop
               Display2 (current.next.key, current.next.value);
               current := current.next;
            end loop;
         end if;
         Put_Line("--E");
      end loop;
      New_Line;
   end DisplayHashTable2;


   procedure ForAll2 (HashTable : in hashMap2) is

   current : entryNodePointer2;

   begin
      for i in 0 .. (HashTable.length - 1) loop
         current := HashTable.entryNodeArray (i);
         while current /= null loop
            begin
               Treat2 (current.key, current.value);
               exception
                  when others => null;
            end;
            current := current.next;
         end loop;
      end loop;
   end ForAll2;

end THCharCode;