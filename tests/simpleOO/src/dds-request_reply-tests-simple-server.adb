pragma Ada_2012;
package body DDS.Request_Reply.Tests.Simple.Server is

   ------------------------
   -- Compute_And_Replie --
   ------------------------
   task body Ref2 is
      Octets_Data : DDS.Octets;
      String_Data : DDS.String;
      L_Replier   : DDS.Request_Reply.Replier.Ref_Access;
      L_Id        : DDS.SampleIdentity_T;
   begin
      select
         accept Compute_And_Reply (Replier : Octets_Replier.Ref_Access;
                                   Data    : DDS.Octets;
                                   Id      : DDS.SampleIdentity_T)
         do
            Dds.Copy (Octets_Data, Data);
            L_Id := Id;
            L_Replier := Request_Reply.Replier.Ref_Access (Replier);
         end Compute_And_Reply;
         declare
            L      : constant Standard.String := "Ret Octets";
            Result : constant Dds.Octets := Dds.Octets'(Length => L'Length,
                                                        Value  => L'Address);
         begin
            Octets_Replier.Ref_Access (L_Replier).Send_Reply (Result, Id => L_Id);
         end;
      or
         accept Compute_And_Reply (Replier : String_Replier.Ref_Access;
                                   Data    : DDS.String;
                                   Id      : DDS.SampleIdentity_T)
         do
            Dds.Copy (String_Data, Data);
            L_Id := Id;
            L_Replier := Request_Reply.Replier.Ref_Access (Replier);
         end Compute_And_Reply;
         declare
            Ret : DDS.String;
         begin
            Copy (Ret, "In reply to: """);
            Append (Ret, String_Data);
            Append (Ret, """");
            String_Replier.Ref_Access (L_Replier).Send_Reply (Ret, Id => L_Id);
            Finalize (Ret);
         end;
      or
         terminate;
      end select;
   end Ref2;

   procedure Compute_And_Reply (Self    : not null access Ref;
                                Replier : Octets_Replier.Ref_Access;
                                Data    : DDS.Octets;
                                Id      : DDS.SampleIdentity_T)
   is
      L      : constant Standard.String := "Ret Octets";
      Result : constant Dds.Octets := Dds.Octets'(Length => L'Length,
                                                  Value  => L'Address);
   begin
      Replier.Send_Reply (Result, Id => Id);
   end Compute_And_Reply;

   ------------------------
   -- Compute_And_Replie --
   ------------------------

   procedure Compute_And_Reply
     (Self      : not null access Ref;
      Replier : String_Replier.Ref_Access;
      Data      : DDS.String;
      Id        : DDS.SampleIdentity_T)
   is
      Ret : String;
   begin
      Copy (Ret, "In reply to: """);
      Append (Ret, Data);
      Append (Ret, """");
      Replier.Send_Reply (Ret, Id => Id);
   end Compute_And_Reply;

end DDS.Request_Reply.Tests.Simple.Server;
