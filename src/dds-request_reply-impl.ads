with DDS.DataReader_Impl;
with DDS.DataWriter_Impl;
with DDS.DomainParticipant;
with DDS.Publisher;
with DDS.Subscriber;
with DDS.Topic;
with DDS.WaitSet;
with DDS.ReadCondition;

private package DDS.Request_Reply.Impl is

   type Ref is new Ada.Finalization.Limited_Controlled and Request_Reply.Ref with record
      Participant          : DDS.DomainParticipant.Ref_Access;
      Publisher            : DDS.Publisher.Ref_Access;
      Subscriber           : DDS.Subscriber.Ref_Access;
      Reply_Topic          : DDS.Topic.Ref_Access;
      Request_Topic        : DDS.Topic.Ref_Access;
      Reader               : DDS.DataReader_Impl.Ref_Access;
      Writer               : DDS.DataWriter_Impl.Ref_Access;
      Waitset              : aliased DDS.WaitSet.Ref;
      Not_Read_Sample_Cond : DDS.ReadCondition.Ref_Access;
      Any_Sample_Cond      : DDS.ReadCondition.Ref_Access;
      Sample_Size          : Natural;
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
      Type_Name  : DDS.String;
      QoS        : DDS.TopicQos := DDS.DomainParticipant.TOPIC_QOS_DEFAULT) return DDS.Topic.Ref_Access;

   function Create_Reply_Topic
     (Self       : not null access Ref;
      Topic_Name : DDS.String;
      Type_Name  : DDS.String;
      QoS        : DDS.TopicQos := DDS.DomainParticipant.TOPIC_QOS_DEFAULT) return DDS.Topic.Ref_Access;

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

   CORRELATION_FIELD_NAME : constant DDS.String := To_DDS_String ("@related_sample_identity.writer_guid.value");


   function CreateContentFilteredTopicName ( Self             : not null access Ref;
                                             RelatedTopicName : DDS.String;
                                             Guid             : Guid_T) return Standard.String;
   procedure Wait_For_Any_Sample (Self : not null access Ref;
                                  Max_Wait : DDS.Duration_T;
                                  Min_Sample_Count : DDS.Natural);

   procedure Wait_For_Samples (Self              : not null access Ref;
                               Max_Wait          : DDS.Duration_T;
                               Min_Sample_Count  : DDS.Integer;
                               WaitSet           : not null DDS.WaitSet.Ref_Access;
                               Initial_Condition : not null DDS.ReadCondition.Ref_Access;
                               Condition         : not null DDS.ReadCondition.Ref_Access);


end DDS.Request_Reply.Impl;
