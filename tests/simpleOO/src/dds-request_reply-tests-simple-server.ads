with DDS.Request_Reply.Tests.Simple.Octets_Replier;
with DDS.Request_Reply.Tests.Simple.String_Replier;
with DDS.Request_Reply.Replier.Typed_Replier_Generic.Passive_Replier_Generic;
package DDS.Request_Reply.Tests.Simple.Server is

   type Ref_Base is limited interface;
   package Octets_Srv is new Octets_Replier.Passive_Replier_Generic (Ref_Base);
   package String_Srv is new String_Replier.Passive_Replier_Generic (Octets_Srv.Listners.Ref);

   task type Ref2 is new String_Srv.Listners.Ref with
      entry Compute_And_Reply (Replier : Octets_Replier.Ref_Access;
                               Data    : DDS.Octets;
                               Id      : DDS.SampleIdentity_T);

      entry Compute_And_Reply (Replier : String_Replier.Ref_Access;
                               Data    : DDS.String;
                               Id      : DDS.SampleIdentity_T);
   end Ref2;

   type Ref is new String_Srv.Listners.Ref with null record;

   procedure Compute_And_Reply (Self    : not null access Ref;
                                Replier : Octets_Replier.Ref_Access;
                                Data    : DDS.Octets;
                                Id      : DDS.SampleIdentity_T);

   procedure Compute_And_Reply (Self    : not null access Ref;
                                Replier : String_Replier.Ref_Access;
                                Data    : DDS.String;
                                Id      : DDS.SampleIdentity_T);

end DDS.Request_Reply.Tests.Simple.Server;
