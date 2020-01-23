with DDS.DataReader;
with DDS.DataWriter;
with DDS.DomainParticipant;
with DDS.Publisher;
with DDS.Requester;
with DDS.Subscriber;
with DDS.Topic;
with DDS.Requester.Impl;
with DDS.Typed_DataWriter_Generic;
with DDS.Typed_DataReader_Generic;
with DDS.Request_Reply.Connext_C_Requester;

generic
   with package Reply_DataWriters is new DDS.Typed_DataWriter_Generic (<>);
   with package Request_DataReaders is new DDS.Typed_DataReader_Generic (<>);
package DDS.Typed_Replyer_Generic is

   type Ref is new DDS.Requester.Impl.Ref with private;
   type Ref_Access is access all Ref'Class;

   package Request_Listeners is
      type Ref is limited interface;
      type Ref_Access is access all Ref'Class;

      procedure On_Reply (Self : not null access Ref;
                       Requester : not null access DDS.Requester.Ref_Access) is null;
   end Request_Listeners;

   function Get_Request_Data_Writer
     (Self : not null access Ref)
      return DDS.DataWriter.Ref_Access;

   function Get_Reply_Data_Reader
     (Self : not null access Ref)
      return DDS.DataReader.Ref_Access;

   function Create
     (Participant        : DDS.DomainParticipant.Ref_Access;
      Service_Name       : DDS.String;
      Qos_Library_Name   : DDS.String;
      Qos_Profile_Name   : DDS.String;
      Publisher          : DDS.Publisher.Ref_Access := null;
      Subscriber         : DDS.Subscriber.Ref_Access := null;
      A_Listner          : Request_Listeners.Ref_Access := null;
      Mask               : DDS.StatusMask := DDS.STATUS_MASK_NONE)return Ref_Access;

   function Create
     (Participant        : DDS.DomainParticipant.Ref_Access;
      Request_Topic_Name : DDS.String;
      Reply_Topic_Name   : DDS.String;
      Qos_Library_Name   : DDS.String;
      Qos_Profile_Name   : DDS.String;
      Publisher          : DDS.Publisher.Ref_Access := null;
      Subscriber         : DDS.Subscriber.Ref_Access := null;
      A_Listner          : Request_Listeners.Ref_Access := null;
      Mask               : DDS.StatusMask := DDS.STATUS_MASK_NONE)return Ref_Access;

   function Create
     (Participant        : DDS.DomainParticipant.Ref_Access;
      Service_Name       : DDS.String;
      Datawriter_Qos     : DDS.DataWriterQos;
      Datareader_Qos     : DDS.DataReaderQos;
      Publisher          : DDS.Publisher.Ref_Access := null;
      Subscriber         : DDS.Subscriber.Ref_Access := null;
      A_Listner          : Request_Listeners.Ref_Access := null;
      Mask               : DDS.StatusMask := DDS.STATUS_MASK_NONE)return Ref_Access;

   function Create
     (Participant        : DDS.DomainParticipant.Ref_Access;
      Request_Topic_Name : DDS.String;
      Reply_Topic_Name   : DDS.String;
      Datawriter_Qos     : DDS.DataWriterQos;
      Datareader_Qos     : DDS.DataReaderQos;
      Publisher          : DDS.Publisher.Ref_Access := null;
      Subscriber         : DDS.Subscriber.Ref_Access := null;
      A_Listner          : Request_Listeners.Ref_Access := null;
      Mask               : DDS.StatusMask := DDS.STATUS_MASK_NONE)return Ref_Access;

   procedure Delete (Self : in out Ref_Access);


   function Send_Reply
     (Self    : not null access Ref;
      Reply   : access Reply_DataWriters.Treats.Data_Type;
      Id      : access DDS.SampleIdentity_T)return DDS.ReturnCode_T;

   function Send_Reply
     (Self    : not null access Ref;
      Reply   : Reply_DataWriters.Treats.Data_Type;
      Id      : DDS.SampleIdentity_T)return DDS.ReturnCode_T;

   procedure Send_Reply
     (Self    : not null access Ref;
      Reply   : access Reply_DataWriters.Treats.Data_Type;
      Id      : access DDS.SampleIdentity_T);

   procedure Send_Reply
     (Self    : not null access Ref;
      Reply   : Reply_DataWriters.Treats.Data_Type;
      Id      : DDS.SampleIdentity_T);


   function Receive_Request
     (Self     : not null access Ref;
      Request  : in out Request_DataReaders.Treats.Data_Type;
      Info_Seq : in out DDS.SampleInfo;
      Timeout  : DDS.Duration_T := DDS.DURATION_INFINITE) return DDS.ReturnCode_T;

   procedure Receive_Request
     (Self     : not null access Ref;
      Request  : in out Request_DataReaders.Treats.Data_Type;
      Info_Seq : in out DDS.SampleInfo;
      Timeout  : DDS.Duration_T := DDS.DURATION_INFINITE);

   function Receive_Request
     (Self     : not null access Ref;
      Timeout  : DDS.Duration_T) return Request_DataReaders.Container;

   function Receive_Requests
     (Self                 : not null access Ref;
      Requests             : not null Request_DataReaders.Treats.Data_Sequences.Sequence_Access;
      Sample_Info          : not null access DDS.SampleInfo_Seq.Sequence;
      Min_Requests_Count   : DDS.Long := 1;
      Max_Requests_Count   : DDS.Long := DDS.INFINITE;
      Timeout              : DDS.Duration_T := DDS.DURATION_INFINITE) return DDS.ReturnCode_T;

   procedure Receive_Requests
     (Self                 : not null access Ref;
      Requests             : not null Request_DataReaders.Treats.Data_Sequences.Sequence_Access;
      Sample_Info          : not null access DDS.SampleInfo_Seq.Sequence;
      Min_Reply_Count      : DDS.long := 1;
      Max_Reply_Count      : DDS.long := DDS.INFINITE;
      Timeout              : DDS.Duration_T := DDS.DURATION_INFINITE);

   procedure Receive_Requests
     (Self                 : not null access Ref;
      Requests             : in out Request_DataReaders.Treats.Data_Sequences.Sequence;
      Sample_Info          : in out DDS.SampleInfo_Seq.Sequence;
      Min_Reply_Count      : DDS.long := 1;
      Max_Reply_Count      : DDS.long := DDS.INFINITE;
      Timeout              : DDS.Duration_T := DDS.DURATION_INFINITE);


   function Receive_Request
     (Self                 : not null access Ref;
      Min_Reply_Count      : DDS.long := 1;
      Max_Reply_Count      : DDS.long := DDS.INFINITE;
      Timeout              : DDS.Duration_T := DDS.DURATION_INFINITE) return Request_DataReaders.Container;


   function Receive_Request
     (Self            : not null access Ref;
      Requests        : not null Request_DataReaders.Treats.Data_Sequences.Sequence_Access;
      Sample_Info     : not null DDS.SampleInfo_Seq.Sequence_Access;
      Min_Reply_Count : DDS.Natural;
      Max_Reply_Count : DDS.Long;
      Timeout         : Duration) return DDS.ReturnCode_T;


   function Receive_Request
     (Self            : not null access Ref;
      Min_Reply_Count : DDS.Natural;
      Max_Reply_Count : DDS.Long;
      Timeout         : Duration) return Request_DataReaders.Container;


   function Take_Request
     (Self        : not null access Ref;
      Requests    : aliased Request_DataReaders.Treats.Data_Type;
      Sample_Info : not null access DDS.SampleInfo_Seq.Sequence;
      Timeout     : DDS.Duration_T) return DDS.ReturnCode_T;

   function Take_Request
     (Self            : not null access Ref;
      Requests        : not null Request_DataReaders.Treats.Data_Sequences.Sequence_Access;
      Sample_Info     : not null access DDS.SampleInfo_Seq.Sequence;
      Min_Reply_Count : DDS.Natural := 1;
      Max_Reply_Count : DDS.Long := DDS.INFINITE;
      Timeout         : DDS.Duration_T) return DDS.ReturnCode_T;

   function Take_Request
     (Self            : not null access Ref;
      Min_Reply_Count : DDS.Natural;
      Max_Reply_Count : DDS.Long;
      Timeout         : DDS.Duration_T) return  Request_DataReaders.Container;



   function Read_Request
     (Self        : not null access Ref;
      Request     : aliased Request_DataReaders.Treats.Data_Type;
      Sample_Info : not null access DDS.SampleInfo_Seq.Sequence;
      Timeout     : DDS.Duration_T) return DDS.ReturnCode_T;

   function Read_Request
     (Self            : not null access Ref;
      Requests        : not null Request_DataReaders.Treats.Data_Sequences.Sequence_Access;
      Sample_Info     : not null access DDS.SampleInfo_Seq.Sequence;
      Min_Reply_Count : DDS.Natural;
      Max_Reply_Count : DDS.Long;
      Timeout         : DDS.Duration_T) return DDS.ReturnCode_T;

   function Read_Request
     (Self            : not null access Ref;
      Min_Reply_Count : DDS.Natural;
      Max_Reply_Count : DDS.Long;
      Timeout         : DDS.Duration_T) return Request_DataReaders.Container'Class;




   procedure Return_Loan (Self         : not null access Ref;
                          Replies      : not null Request_DataReaders.Treats.Data_Sequences.Sequence_Access;
                          Sample_Info  : DDS.SampleInfo_Seq.Sequence_Access);

   procedure Return_Loan (Self         : not null access Ref;
                          Replies      : Request_DataReaders.Treats.Data_Sequences.Sequence;
                          Sample_Info  : DDS.SampleInfo_Seq.Sequence);
   procedure Delete (This : in out Ref);

   function Get_Request_DataReader (Self : not null access Ref) return Request_DataReaders.Ref_Access;
   function Get_Reply_Datawriter (Self : not null access Ref) return Reply_DataWriters.Ref_Access;

private
   type Ref is new DDS.Requester.Impl.Ref   with record
      Request_Topic      : DDS.Topic.Ref_Access;
      Reply_Topic        : DDS.Topic.Ref_Access;
      Reply_DataWriter   : Reply_DataWriters.Ref_Access;
      Request_DataReader : Request_DataReaders.Ref_Access;
      Listner            : Request_Listeners.Ref_Access;
   end record;

end DDS.Typed_Replyer_Generic;
