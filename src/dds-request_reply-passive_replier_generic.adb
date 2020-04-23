pragma Ada_2012;
package body DDS.Request_Reply.Passive_Replier_Generic is

   -------------------------
   -- On_Request_Avalible --
   -------------------------

   procedure On_Request_Avalible
     (Self      : not null access Ref;
      Replier   : not null access Actual_Replier.Ref'Class)
   is
   begin
      for I of Replier.Take_Requests (Max_Reply_Count => DDS.INFINITE) loop
         if I.Sample_Info.Valid_Data then
            Ref'Class (Self.all).Compute_And_Reply (Replier, I.Data.all, Get_Sample_Identity (I.Sample_Info.all));
         end if;
      end loop;
   end On_Request_Avalible;

end DDS.Request_Reply.Passive_Replier_Generic;
