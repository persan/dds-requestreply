pragma Ada_2012;
package body DDS.Request_Reply.Replier.Typed_Replier_Generic.Passive_Replier_Generic is

   -------------------------
   -- On_Request_Avalible --
   -------------------------

   procedure On_Request_Avalible
     (Self      : not null access Ref;
      Replier   : not null access Typed_Replier_Generic.Ref'Class)
   is
   begin
      for I of Replier.Take_Request loop
         if  I.Sample_Info.Valid_Data then
            Self.all.Listner.Compute_And_Reply (Typed_Replier_Generic.Ref_Access(Replier), I.Data.all, Get_Sample_Identity (I.Sample_Info.all));
         end if;
      end loop;
   end On_Request_Avalible;

end DDS.Request_Reply.Replier.Typed_Replier_Generic.Passive_Replier_Generic;
