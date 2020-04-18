with Dds.Topic;
with DDS.Entity_Params;
with DDS.DataReader;
with DDS.DataWriter;
package DDS.Request_Reply.Replier is

   type Ref is limited interface and DDS.Request_Reply.Ref;
   type Ref_Access is access all Ref;


   function Create_Writer_Topic
     (Self   : not null access Ref;
      Params : DDS.Entity_Params.EntityParams;
      Name   : DDS.String) return DDS.Topic.Ref_Access is abstract;

   function Create_Reader_Topic
     (Self   : not null access Ref;
      Params : DDS.Entity_Params.EntityParams;
      Name   : DDS.String) return DDS.Topic.Ref_Access is abstract;


   procedure Wait_For_Requests
     (Self      : not null access Ref;
      Min_Count : DDS.Integer;
      Max_Wait  : DDS.Duration_T) is abstract;

   function Get_Request_Data_Reader
     (Self : not null access Ref)
      return DDS.DataReader.Ref_Access  is abstract;

   function Get_Reply_Data_Writer
     (Self : not null access Ref)
      return DDS.DataWriter.Ref_Access is abstract;

end DDS.Request_Reply.Replier;
