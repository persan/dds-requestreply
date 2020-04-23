with DDS.Request_Reply.Tests.Simple.Octets_Replier;
with DDS.Request_Reply.Tests.Simple.String_Replier;
with DDS.Request_Reply.Passive_Replier_Generic;
package DDS.Request_Reply.Tests.Simple.Server is

   type Ref_Base is limited interface;

   package Octets_Srv is new DDS.Request_Reply.Passive_Replier_Generic (Ref_Base, Octets_Replier);
   package String_Srv is new DDS.Request_Reply.Passive_Replier_Generic (Octets_Srv.Ref, String_Replier);

   type Ref is new String_Srv.Ref with null record;

   procedure Compute_And_Reply (Self      : not null access Ref;
                                Requester : not null access Octets_Replier.Ref'Class;
                                Data      : DDS.Octets;
                                Id        : DDS.SampleIdentity_T);

   procedure Compute_And_Reply (Self      : not null access Ref;
                                Requester : not null access String_Replier.Ref'Class;
                                Data      : DDS.String;
                                Id        : DDS.SampleIdentity_T);

end DDS.Request_Reply.Tests.Simple.Server;
