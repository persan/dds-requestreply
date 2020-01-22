with DDS.DomainParticipantListener;
with DDS.Treats_Generic;
with DDS.Entity;
with DDS.DomainParticipant;
with DDS.TopicListener;
with DDS.Topic;
with DDS.DomainParticipantFactory;
package DDS.Request_Reply is

   type Ref is limited interface and DDS.Entity.Ref;
   type Ref_Access  is access all Ref'Class;
   procedure DDSLog_Exception (Log : Standard.String) is null;

   --  =========================================================================
   --  package DDS

   function Image (Item : Guid_T) return DDS.String;
   function Image (Item : Guid_T) return Standard.String;

   function Value (Item : Standard.String) return Guid_T with
     Pre => (Item'Length = GUID_T_Value_Array'Length * 2);

   procedure Append (To : in out DDS.String; Data : DDS.String);
   procedure Append (To : in out DDS.String; Data : Standard.String);

   procedure Prepend (To : in out DDS.String; Data : DDS.String);
   procedure Prepend (To : in out DDS.String; Data : Standard.String);


   procedure Copy (To : out DataReaderQoS; From : DataReaderQoS);
   procedure Copy (To : out DataWriterQos; From : DataWriterQos);

   --  #########################################################################
   --  ##

   --  =========================================================================
   --  package DDS.DomainParticipant
   function  Get_Or_Create_Topic
     (Self       : not null DDS.DomainParticipant.Ref_Access;
      Topic_Name : in DDS.String;
      Type_Name  : in DDS.String;
      Qos        : in DDS.TopicQos := DDS.DomainParticipant.TOPIC_QOS_DEFAULT;
      A_Listener : in DDS.TopicListener.Ref_Access := null;
      Mask       : in DDS.StatusMask := STATUS_MASK_NONE)return  DDS.Topic.Ref_Access;

   function  Get_Or_Create_Topic_With_Profile
     (Self         : not null DDS.DomainParticipant.Ref_Access;
      Topic_Name   : in DDS.String;
      Type_Name    : in DDS.String;
      Library_Name : in DDS.String;
      Profile_Name : in DDS.String;
      A_Listener   : in DDS.TopicListener.Ref_Access := null;
      Mask         : in DDS.StatusMask := STATUS_MASK_NONE) return  DDS.Topic.Ref_Access;





   --  =========================================================================
   --  package DDS.DomainParticipantFactory
   function  Get_Or_Create_Participant
     (Self       : not null DDS.DomainParticipantFactory.Ref_Access;
      Domain_Id  : in DDS.DomainId_T := Default_Domain;
      Qos        : in DDS.DomainParticipantQos := DDS.DomainParticipantFactory.PARTICIPANT_QOS_DEFAULT;
      A_Listener : in DDS.DomainParticipantListener.Ref_Access := null;
      Mask       : in DDS.StatusMask := STATUS_MASK_NONE)return  DDS.DomainParticipant.Ref_Access;

   function  Get_Or_Create_Participant_With_Profile
     (Self         : not null DDS.DomainParticipantFactory.Ref_Access;
      Domain_Id    : in DDS.DomainId_T := Default_Domain;
      Library_Name : in DDS.String;
      Profile_Name : in DDS.String;
      A_Listener   : in DDS.DomainParticipantListener.Ref_Access := null;
      Mask         : in DDS.StatusMask := STATUS_MASK_NONE) return  DDS.DomainParticipant.Ref_Access;


end DDS.Request_Reply;
