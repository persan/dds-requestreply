with DDS.Topic;
with DDS.Entity_Params;
with DDS.Request_Reply.Impl;
private package  DDS.Request_Reply.Replier.Impl is
   type Ref is limited new DDS.Request_Reply.Impl.Ref and DDS.Request_Reply.Replier.Ref with record
      Reply_DataWriter   : DDS.DataWriter.Ref_Access;
      Request_DataReader : DDS.DataReader.Ref_Access;
   end record;
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

   function Get_Request_Data_Reader
     (Self : not null access Ref)
      return DDS.DataReader.Ref_Access;

   function Get_Reply_Data_Writer
     (Self : not null access Ref)
      return DDS.DataWriter.Ref_Access;
end DDS.Request_Reply.Replier.Impl;
