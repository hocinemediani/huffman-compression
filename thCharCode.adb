with Ada.Text_IO;            use Ada.Text_IO;
with Ada.Unchecked_Deallocation;

package body THCharCode is

   procedure Free2 is
      new Ada.Unchecked_Deallocation (Object => entryNode2, Name => entryNodePointer2);


   procedure InitialiseHashTable2 (HashTable : in out hashMap2; Length : in Integer) is
   begin
      HashTable.size := 0;
      HashTable.length := Length;
      for i in 1 .. HashTable.length loop
         HashTable.entryNodeArray (i) := null;
      end loop;
   end InitialiseHashTable2;


   procedure DestroyHashTable2 (HashTable : in out hashMap2) is

   previous, current : entryNodePointer2;

   begin
      -- Exploring the nodes.
      for i in 1 .. HashTable.length loop
         current := HashTable.entryNodeArray (i);
         if current /= null then
            while current /= null loop
               previous := current;
               current := previous.next;
               Free2 (previous);
            end loop;
            Free2 (previous);
            Free2 (current);
         end if;
         HashTable.entryNodeArray (i) := null;
      end loop;
      HashTable.size := 0;
    end DestroyHashTable2;


   function IsEmpty2 (HashTable : in hashMap2) return Boolean is
   begin
      -- Size is the actual numbers of entry in the hash map.
      return HashTable.size = 0;
   end IsEmpty2;


   function GetSize2 (HashTable : in hashMap2) return Integer is
   begin
      return HashTable.size;
   end GetSize2;


   function HashKey2 (Key : in String) return Integer is  

   result : Integer := 0;

   begin
      for i in 1 .. 8 loop
         if Key (i) = '1' then
            result := result + 2 ** (8 - i);
         end if;
      end loop;

      return result;
   end HashKey2;


   procedure Register2 (HashTable : in out hashMap2; Key : in Unbounded_String; Value : in Unbounded_String) is

   current, previous, firstNode : entryNodePointer2;
   hashedKey : CONSTANT Integer := HashKey2 (To_String (Key));

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


   procedure Delete2 (HashTable : in out hashMap2; Key : in Unbounded_String) is

   previous, current : entryNodePointer2;
   hashedKey : CONSTANT Integer := HashKey2 (To_String (Key));

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
            Free2 (current);
            HashTable.size := HashTable.size - 1;
            return;
         end if;
         previous := current;
         current := current.next;
      end loop;
   end Delete2;


   function IsIn2 (HashTable : in hashMap2; Key : in Unbounded_String) return Boolean is
    
   current : entryNodePointer2;
   hashedKey : CONSTANT Integer := HashKey2 (To_String (Key));
    
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


   function ValueOf2 (HashTable : in hashMap2; Key : in Unbounded_String) return Unbounded_String is

   current : entryNodePointer2;
   hashedKey : CONSTANT Integer := HashKey2 (To_String (Key));
   result : Unbounded_String;

   begin
   result := To_Unbounded_String ("");
      current := HashTable.entryNodeArray (hashedKey);
      while current /= null loop
         if current.key = Key then
            result := current.value;
         end if;
         current := current.next;
      end loop;
      return result;
   end ValueOf2;


   procedure DisplayHashTable2 (HashTable : in hashMap2) is

   current : entryNodePointer2;
        
   begin
      for i in 1 .. HashTable.length loop
         current := HashTable.entryNodeArray (i);
         if current /= null then
            if i = 255 then
               Put ("'"); Put ("\$"); Put("'"); Put (" --> ");
            else
               Put ("'"); Put (Character'Val (i)); Put("'"); Put ("  --> ");
            end if;
            Put (To_String (HashTable.entryNodeArray (i).value));
            while current.next /= null loop
            Put (To_String  (current.next.value));
               current := current.next;
            end loop;
            New_Line;
         end if;
      end loop;
   end DisplayHashTable2;


   procedure ForAll2 (HashTable : in hashMap2) is

   current : entryNodePointer2;

   begin
      for i in 1 .. HashTable.length loop
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