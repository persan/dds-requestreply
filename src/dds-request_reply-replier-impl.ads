with Dds.Topic;
with DDS.Entity_Params;
package DDS.Request_Reply.Replier.Impl is
   type Ref is limited new Replier.Ref with private;
   type Ref_Access is access all Ref;


   function Create_Writer_Topic
     (Self   : not null access Ref;
      Params : DDS.Entity_Params.EntityParams;
      Name   : DDS.String) return DDS.Topic.Ref_Access;

   function Create_Reader_Topic
     (Self   : not null access Ref;
      Params : DDS.Entity_Params.EntityParams;
      Name   : DDS.String) return DDS.Topic.Ref_Access;


   procedure Wait_For_Requests
     (Self      : not null access Ref;
      Min_Count : DDS.Integer;
      Max_Wait  : DDS.Duration_T);
private
   type Ref is limited new Replier.Ref with record
      null;
   end record;

end DDS.Request_Reply.Replier.Impl;
