with DDS.Request_Reply.Replier.Typed_Replier_Generic;
generic
   type Base is abstract tagged limited private;
   with package Actual_Replier is new DDS.Request_Reply.Replier.Typed_Replier_Generic (<>);
package DDS.Request_Reply.Passive_Replier_Generic is
   type Ref is abstract new Base and Actual_Replier.Replyer_Listeners.Ref with null record;

   procedure On_Request_Avalible (Self      : not null access Ref;
                                  Replier   : not null access Actual_Replier.Ref'Class);

   procedure Compute_And_Reply (Self      : not null access Ref;
                                Replier   : not null access Actual_Replier.Ref'Class;
                                Data      : Actual_Replier.Request_DataReader.Treats.Data_Type;
                                Id        : DDS.SampleIdentity_T) is abstract;
end DDS.Request_Reply.Passive_Replier_Generic;
