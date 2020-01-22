with DDS.Treats_Generic;
with DDS.Entity;
package DDS.Request_Reply is

   type Ref is limited interface and DDS.Entity.Ref;
   type Ref_Access  is access all Ref'Class;
   procedure DDSLog_Exception (Log : Standard.String) is null;

   function Image (Item : Guid_T) return DDS.String;
   function Image (Item : Guid_T) return Standard.String;
   procedure Append (To : DDS.String; Data : DDS.String);
   procedure Append (To : DDS.String; Data : Standard.String);

end DDS.Request_Reply;
