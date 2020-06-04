with DDS.DataReader;
with DDS.DataWriter;
with DDS.Publisher;
with DDS.Subscriber;
private package DDS.Request_Reply.Impl is

   type Ref is limited new Request_Reply.Ref with record
      Participant        : DDS.DomainParticipant.Ref_Access;
      Request_Topic      : DDS.Topic.Ref_Access;
      Reply_Topic        : DDS.Topic.Ref_Access;
      Reader             : DDS.DataReader.Ref_Access;
      Writer             : DDS.DataWriter.Ref_Access;
   end record;

   type Ref_Access  is access all Ref'Class;
   procedure Log_Exception (Log : Standard.String) is null;

   function Create_Request_Topic_Name_From_Service_Name
     (Service_Name     : DDS.String) return DDS.String is
     (DDS.To_DDS_String (DDS.To_Standard_String (Service_Name) & "Request"));


   function Create_Reply_Topic_Name_From_Service_Name
     (Service_Name     : DDS.String) return DDS.String is
     (DDS.To_DDS_String (DDS.To_Standard_String (Service_Name) & "Reply"));

   function Create_Request_Topic
     (Self       : not null access Ref;
      Topic_Name : DDS.String;
      Type_Name  : DDS.String) return DDS.Topic.Ref_Access;

   function Create_Reply_Topic
     (Self       : not null access Ref;
      Topic_Name : DDS.String;
      Type_Name  : DDS.String) return DDS.Topic.Ref_Access;

   function Create_Request_Topic_With_Profile
     (Self               : not null access Ref;
      Topic_Name         : DDS.String;
      Type_Name          : DDS.String;
      Library_Name       : DDS.String;
      Profile_Name       : DDS.String) return DDS.Topic.Ref_Access;

   function Create_Reply_Topic_With_Profile
     (Self               : not null access Ref;
      Topic_Name         : DDS.String;
      Type_Name          : DDS.String;
      Library_Name       : DDS.String;
      Profile_Name       : DDS.String) return DDS.Topic.Ref_Access;

   procedure Validate (Self       : not null access Ref;
                       Publisher  : DDS.Publisher.Ref_Access;
                       Subscriber : DDS.Subscriber.Ref_Access);

end DDS.Request_Reply.Impl;
