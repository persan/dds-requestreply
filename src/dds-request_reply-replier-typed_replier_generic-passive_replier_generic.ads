generic
   type Base is abstract tagged limited private;
package DDS.Request_Reply.Replier.Typed_Replier_Generic.Passive_Replier_Generic is

   package Listners is
      type Ref is interface;
      type Ref_Access is access all Ref'Class;
      procedure Compute_And_Reply
        (Self      : not null access Ref;
         Replier   : not null access Typed_Replier_Generic.Ref'Class;
         Data      : Request_DataReader.Treats.Data_Type;
         Id        : DDS.SampleIdentity_T) is abstract;
   end Listners;

   type Ref (Listner : not null Listners.Ref_Access) is abstract new Base and Replyer_Listeners.Ref with null record;

   procedure On_Request_Avalible (Self      : not null access Ref;
                                  Replier   : not null access Typed_Replier_Generic.Ref'Class);

end DDS.Request_Reply.Replier.Typed_Replier_Generic.Passive_Replier_Generic;
