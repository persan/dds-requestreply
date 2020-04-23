pragma Ada_2012;
package body DDS.Request_Reply.Tests.Simple.Server is

   ------------------------
   -- Compute_And_Replie --
   ------------------------

   procedure Compute_And_Reply
     (Self      : not null access Ref;
      Requester : not null access Octets_Replier.Ref'Class;
      Data      : DDS.Octets;
      Id        : DDS.SampleIdentity_T)
   is
      L      : constant Standard.String := "Ret Octets";
      Result : constant Dds.Octets := Dds.Octets'(Length => L'Length,
                                                  Value  => L'Address);
   begin
      Requester.Send_Reply (Result, Id => Id);
   end Compute_And_Reply;

   ------------------------
   -- Compute_And_Replie --
   ------------------------

   procedure Compute_And_Reply
     (Self      : not null access Ref;
      Requester : not null access String_Replier.Ref'Class;
      Data      : DDS.String;
      Id        : DDS.SampleIdentity_T)
   is
      Ret : String;
   begin
      Copy (Ret, "In reply to: """);
      Append (Ret, Data);
      Append (Ret, """");
      Requester.Send_Reply (Ret, Id => Id);
   end Compute_And_Reply;

end DDS.Request_Reply.Tests.Simple.Server;
