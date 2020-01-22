pragma Ada_2012;
package body DDS.Request_Reply is

   -----------
   -- Image --
   -----------

   function Image (Item : Guid_T) return DDS.String is
   begin
      return Ret : DDS.String do
         Copy (Ret, String'(Image (Item)));
      end return;
   end Image;

   -----------
   -- Image --
   -----------
   function Image (Item : Octet) return Standard.String is
      use Interfaces;
      Map : constant array (Unsigned_8'(0) .. Unsigned_8'(15)) of Character := "0123456789ABCDEF";
   begin
      return Map (Item / 16) & Map (Item mod 16);
   end;

   function Image (Item : Guid_T) return Standard.String is
      Cursor : Standard.Natural := 1;
   begin
      return Ret : Standard.String (1 .. (Item.Value'Length * 2)) do
         for V of Item.Value loop
            Ret (Cursor .. Cursor + 1) := Image (V);
            Cursor := Cursor + 2;
         end loop;
      end return;
   end Image;

end DDS.Request_Reply;
