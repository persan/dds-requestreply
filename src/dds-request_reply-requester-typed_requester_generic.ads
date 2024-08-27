-- ---------------------------------------------------------------------
--                                                                    --
--               Copyright (c) per.sandberg@bahnhof.se                --
--                                                                    --
--  Permission is hereby granted, free of charge, to any person       --
--  obtaining a copy of this software and associated documentation    --
--  files (the "Software"), to deal in the Software without           --
--  restriction, including without limitation the rights to use,      --
--  copy, modify, merge, publish, distribute, sublicense, and/or sell --
--  copies of the Software, and to permit persons to whom the Software--
--  is furnished to do so, subject to the following conditions:       --
--                                                                    --
--  The above copyright notice and this permission notice             --
--  (including the next paragraph) shall be included in all copies or --
--  substantial portions of the Software.                             --
--                                                                    --
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,   --
--  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF--
--  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND             --
--  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT       --
--  HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,      --
--  WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,--
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER     --
--  DEALINGS IN THE SOFTWARE.                                         --
--                                                                    --
--  <spdx: MIT>
--                                                                    --
-- ---------------------------------------------------------------------
with DDS.DataReader;
with DDS.DataReaderListener;
with DDS.DataWriter;
with DDS.DataWriterListener;
with DDS.DomainParticipant;
with DDS.Publisher;
with DDS.Subscriber;
with DDS.Typed_DataWriter_Generic;
with DDS.Typed_DataReader_Generic;
private with DDS.Request_Reply.Requester.Impl;
generic
   with package Request_DataWriter is new DDS.Typed_DataWriter_Generic (<>);
   with package Reply_DataReader is new DDS.Typed_DataReader_Generic (<>);
package DDS.Request_Reply.Requester.Typed_Requester_Generic is

   type Ref (<>) is limited new DDS.Request_Reply.Requester.Ref with private;
   type Ref_Access is access all Ref'Class;

   package Request_Listeners is
      type Ref is limited interface;
      type Ref_Access is access all Ref'Class;

      procedure On_Reply_Avalible
        (Self      : not null access Ref;
         Requester : not null access Typed_Requester_Generic.Ref_Access) is null;

      procedure On_Offered_Deadline_Missed
        (Self   : not null access Ref;
         Writer : access Typed_Requester_Generic.Ref'Class;
         Status : in DDS.OfferedDeadlineMissedStatus) is null;
      --  <dref>DataWriterListener_on_offered_deadline_missed</dref>

      procedure On_Offered_Incompatible_Qos
        (Self   : not null access Ref;
         Writer : access Typed_Requester_Generic.Ref'Class;
         Status : in DDS.OfferedIncompatibleQosStatus) is null;
      --  <dref>DataWriterListener_on_offered_incompatible_qos</dref>

      procedure On_Liveliness_Lost
        (Self   : not null access Ref;
         Writer : access Typed_Requester_Generic.Ref'Class;
         Status : in DDS.LivelinessLostStatus) is null;
      --  <dref>DataWriterListener_on_liveliness_lost</dref>

      procedure On_Publication_Matched
        (Self   : not null access Ref;
         Writer : access Typed_Requester_Generic.Ref'Class;
         Status : in DDS.PublicationMatchedStatus) is null;
      --  <dref>DataWriterListener_on_publication_matched</dref>

      procedure On_Reliable_Writer_Cache_Changed
        (Self   : not null access Ref;
         Writer : access Typed_Requester_Generic.Ref'Class;
         Status : in DDS.ReliableWriterCacheChangedStatus) is null;
      --  <dref>DataWriterListener_on_reliable_writer_cache_changed</dref>

      procedure On_Reliable_Reader_Activity_Changed
        (Self   : not null access Ref;
         Writer : access Typed_Requester_Generic.Ref'Class;
         Status : in DDS.ReliableReaderActivityChangedStatus) is null;
      --  <dref>DataWriterListener_on_reliable_reader_activity_changed</dref>

      procedure On_Destination_Unreachable
        (Self     : not null access Ref;
         Writer   : access Typed_Requester_Generic.Ref'Class;
         Instance : in DDS.InstanceHandle_T;
         Locator  : in DDS.Locator_T) is null;
      --  <dref internal="true">DataWriterListener_on_destination_unreachable</dref>

      procedure On_Data_Request
        (Self     : not null access Ref;
         Writer   : access Typed_Requester_Generic.Ref'Class;
         Cookie   : in DDS.Cookie_T;
         Request  : in out System.Address) is null;
      --  <dref internal="true">DataWriterListener_on_data_request</dref>

      procedure On_Data_Return
        (Self     : not null access Ref;
         Writer   : access Typed_Requester_Generic.Ref'Class;
         Arg      : System.Address;
         Cookie   : in DDS.Cookie_T) is null;
      --  <dref internal="true">DataWriterListener_on_data_return</dref>

      procedure On_Sample_Removed
        (Self   : not null access Ref;
         Writer : access Typed_Requester_Generic.Ref'Class;
         Cookie : in DDS.Cookie_T) is null;
      --  <dref internal="true">DataWriterListener_on_sample_removed</dref>

      procedure On_Instance_Replaced
        (Self     : not null access Ref;
         Writer   : access Typed_Requester_Generic.Ref'Class;
         Instance : in DDS.InstanceHandle_T) is null;
      --  <dref>DataWriterListener_on_instance_replaced</dref>

      procedure On_Application_Acknowledgment
        (Self   : not null access Ref;
         Writer : access Typed_Requester_Generic.Ref'Class;
         Info   : in DDS.AcknowledgmentInfo) is null;
      --  <dref>DataWriterListener_on_application_acknowledgment</dref>

      procedure On_Service_Request_Accepted
        (Self   : not null access Ref;
         Writer : access Typed_Requester_Generic.Ref'Class;
         Info   : in DDS.ServiceRequestAcceptedStatus) is null;
      --  <dref>DataWriterListener_on_service_request_accepted</dref>

      procedure On_Requested_Deadline_Missed
        (Self       : not null access Ref;
         The_Reader : in Typed_Requester_Generic.Ref_Access;
         Status     : in DDS.RequestedDeadlineMissedStatus)
      is null;
      --  <dref>DataReaderListener_on_requested_deadline_missed</dref>
      --  <internal>
      --  Handles the StatusKind.REQUESTED_DEADLINE_MISSED_STATUS
      --  communication status.
      --  </internal>

      procedure On_Requested_Incompatible_Qos
        (Self       : not null access Ref;
         The_Reader : in Typed_Requester_Generic.Ref_Access;
         Status     : in DDS.RequestedIncompatibleQosStatus)
      is null;
      --  <dref>DataReaderListener_on_requested_incompatible_qos</dref>
      --  <internal>
      --  Handles the StatusKind.REQUESTED_INCOMPATIBLE_QOS_STATUS
      --  communication status.
      --  </internal>

      procedure On_Sample_Rejected
        (Self       : not null access Ref;
         The_Reader : in Typed_Requester_Generic.Ref_Access;
         Status     : in DDS.SampleRejectedStatus)
      is null;
      --  <dref>DataReaderListener_on_sample_rejected</dref>
      --  <internal>
      --  Handles the StatusKind.SAMPLE_REJECTED_STATUS
      --  communication status.
      --  </internal>

      procedure On_Liveliness_Changed
        (Self       : not null access Ref;
         The_Reader : in Typed_Requester_Generic.Ref_Access;
         Status     : in DDS.LivelinessChangedStatus)
      is null;
      --  <dref>DataReaderListener_on_liveliness_changed</dref>
      --  <internal>
      --  Handles the StatusKind.LIVELINESS_CHANGED_STATUS
      --  communication status.
      --  </internal>
   end Request_Listeners;

   function Get_Request_Data_Writer
     (Self : not null access Ref)
      return not null DDS.DataWriter.Ref_Access;

   function Get_Reply_Data_Reader
     (Self : not null access Ref)
      return not null DDS.DataReader.Ref_Access;

   function Create
     (Participant    : DDS.DomainParticipant.Ref_Access;
      Service_Name   : DDS.String;
      Library_Name   : DDS.String;
      Profile_Name   : DDS.String;
      Publisher      : DDS.Publisher.Ref_Access := null;
      Subscriber     : DDS.Subscriber.Ref_Access := null;
      A_Listner      : Request_Listeners.Ref_Access := null;
      Mask           : DDS.StatusKind := DDS.STATUS_MASK_NONE) return not null Ref_Access;

   function Create
     (Participant        : DDS.DomainParticipant.Ref_Access;
      Request_Topic_Name : DDS.String;
      Reply_Topic_Name   : DDS.String;
      Library_Name       : DDS.String;
      Profile_Name       : DDS.String;
      Publisher          : DDS.Publisher.Ref_Access := null;
      Subscriber         : DDS.Subscriber.Ref_Access := null;
      A_Listner          : Request_Listeners.Ref_Access := null;
      Mask               : DDS.StatusKind := DDS.STATUS_MASK_NONE) return not null Ref_Access;

   function Create
     (Participant      : DDS.DomainParticipant.Ref_Access;
      Service_Name     : DDS.String;
      Datawriter_Qos   : DDS.DataWriterQos := DDS.Publisher.DATAWRITER_QOS_DEFAULT;
      Datareader_Qos   : DDS.DataReaderQos := DDS.Subscriber.DATAREADER_QOS_DEFAULT;
      Topic_Qos        : DDS.TopicQos := DDS.DomainParticipant.TOPIC_QOS_DEFAULT;
      Publisher        : DDS.Publisher.Ref_Access := null;
      Subscriber       : DDS.Subscriber.Ref_Access := null;
      A_Listner        : Request_Listeners.Ref_Access := null;
      Mask             : DDS.StatusKind := DDS.STATUS_MASK_NONE) return not null Ref_Access;

   function Create
     (Participant        : DDS.DomainParticipant.Ref_Access;
      Request_Topic_Name : DDS.String;
      Reply_Topic_Name   : DDS.String;
      Datawriter_Qos     : DDS.DataWriterQos := DDS.Publisher.DATAWRITER_QOS_DEFAULT;
      Datareader_Qos     : DDS.DataReaderQos := DDS.Subscriber.DATAREADER_QOS_DEFAULT;
      Topic_Qos          : DDS.TopicQos := DDS.DomainParticipant.TOPIC_QOS_DEFAULT;
      Publisher          : DDS.Publisher.Ref_Access := null;
      Subscriber         : DDS.Subscriber.Ref_Access := null;
      A_Listner          : Request_Listeners.Ref_Access := null;
      Mask               : DDS.StatusKind := DDS.STATUS_MASK_NONE) return not null Ref_Access;

   procedure Delete (Self : in out Ref_Access);



   procedure Send_Request (Self : not null access Ref;
                           Data : Request_DataWriter.Treats.Data_Type);


   function Send_Request (Self : not null access Ref;
                          Data : Request_DataWriter.Treats.Data_Type)
                          return Reply_DataReader.Treats.Data_Type;



   function Send_Request
     (Self    : not null access Ref;
      Request : Request_DataWriter.Treats.Data_Type) return Reply_DataReader.Container'Class;

   function Send_Request
     (Self            : not null access Ref;
      Request         : Request_DataWriter.Treats.Data_Type;
      Min_Reply_Count : DDS.Natural;
      Max_Reply_Count : DDS.Long;
      Timeout         : DDS.Duration_T := DURATION_INFINITE) return Reply_DataReader.Container'Class;

   procedure Send_Request
     (Self         : not null access Ref;
      Request      : Request_DataWriter.Treats.Data_Type;
      Request_Info : DDS.WriteParams_T);

   function Receive_Reply
     (Self     : not null access Ref;
      Replies  : aliased Reply_DataReader.Treats.Data_Type;
      Info_Seq : not null access DDS.SampleInfo_Seq.Sequence;
      Timeout  : DDS.Duration_T) return DDS.ReturnCode_T;

   function Receive_Reply
     (Self     : not null access Ref;
      Timeout  : DDS.Duration_T) return Reply_DataReader.Container'Class;

   procedure Receive_Reply
     (Self     : not null access Ref;
      Handler  : not null access procedure (Requester : not null access ref; data : Reply_DataReader.Treats.Data_Type);
      Min_Reply_Count : DDS.Natural;
      Max_Reply_Count : DDS.Long;
      Timeout  : DDS.Duration_T);

   function Receive_Replies
     (Self                 : not null access Ref;
      Replies              : not null Reply_DataReader.Treats.Data_Sequences.Sequence_Access;
      Sample_Info          : not null access DDS.SampleInfo_Seq.Sequence;
      Min_Reply_Count      : DDS.Natural;
      Max_Reply_Count      : DDS.Long;
      Timeout              : DDS.Duration_T) return DDS.ReturnCode_T;


   procedure Receive_Replies
     (Self                 : not null access Ref;
      Replies              : not null Reply_DataReader.Treats.Data_Sequences.Sequence_Access;
      Sample_Info          : not null access DDS.SampleInfo_Seq.Sequence;
      Min_Reply_Count      : DDS.Natural;
      Max_Reply_Count      : DDS.Long;
      Timeout              : DDS.Duration_T);

   function Receive_Replies
     (Self                 : not null access Ref;
      Min_Reply_Count      : DDS.Natural;
      Max_Reply_Count      : DDS.Long;
      Timeout              : DDS.Duration_T) return Reply_DataReader.Container'Class;

   function Take_Reply
     (Self        : not null access Ref;
      Replies     : aliased Reply_DataReader.Treats.Data_Type;
      Sample_Info : not null access DDS.SampleInfo_Seq.Sequence;
      Timeout     : DDS.Duration_T) return DDS.ReturnCode_T;

   function Take_Replies
     (Self            : not null access Ref;
      Replies         : not null Reply_DataReader.Treats.Data_Sequences.Sequence_Access;
      Sample_Info     : not null access DDS.SampleInfo_Seq.Sequence;
      Min_Reply_Count : DDS.Natural;
      Max_Reply_Count : DDS.Long;
      Timeout         : DDS.Duration_T) return DDS.ReturnCode_T;

   function Take_Replies
     (Self            : not null access Ref;
      Min_Reply_Count : DDS.Natural;
      Max_Reply_Count : DDS.Long;
      Timeout         : DDS.Duration_T) return  Reply_DataReader.Container'Class;



   function Take_Reply_For_Related_Request
     (Self                 : not null access Ref;
      Replies              : aliased Reply_DataReader.Treats.Data_Type;
      Sample_Info          : not null access DDS.SampleInfo_Seq.Sequence;
      Related_Request_Info : not null access DDS.SampleIdentity_T) return DDS.ReturnCode_T;

   function Take_Replies_For_Related_Request
     (Self                 : not null access Ref;
      Replies              : not null Reply_DataReader.Treats.Data_Sequences.Sequence_Access;
      Sample_Info          : not null access DDS.SampleInfo_Seq.Sequence;
      Related_Request_Info : not null access DDS.SampleIdentity_T) return DDS.ReturnCode_T;

   function Take_Replies_For_Related_Request
     (Self                 : not null access Ref;
      Related_Request_Info : not null access DDS.SampleIdentity_T) return Reply_DataReader.Container'Class;


   function Read_Reply
     (Self        : not null access Ref;
      Replies     : aliased Reply_DataReader.Treats.Data_Type;
      Sample_Info : not null access DDS.SampleInfo_Seq.Sequence;
      Timeout     : DDS.Duration_T) return DDS.ReturnCode_T;

   function Read_Replies
     (Self            : not null access Ref;
      Replies         : not null Reply_DataReader.Treats.Data_Sequences.Sequence_Access;
      Sample_Info     : not null access DDS.SampleInfo_Seq.Sequence;
      Min_Reply_Count : DDS.Natural;
      Max_Reply_Count : DDS.Long;
      Timeout         : DDS.Duration_T) return DDS.ReturnCode_T;

   function Read_Replies
     (Self            : not null access Ref;
      Min_Reply_Count : DDS.Natural;
      Max_Reply_Count : DDS.Long;
      Timeout         : DDS.Duration_T) return Reply_DataReader.Container'Class;



   function Read_Reply_For_Related_Request
     (Self                 : not null access Ref;
      Replies              : aliased Reply_DataReader.Treats.Data_Type;
      Sample_Info          : not null access DDS.SampleInfo_Seq.Sequence;
      Related_Request_Info : DDS.SampleIdentity_T) return DDS.ReturnCode_T;


   function Read_Replies_For_Related_Request
     (Self                 : not null access Ref;
      Replies              : not null Reply_DataReader.Treats.Data_Sequences.Sequence_Access;
      Sample_Info          : not null access DDS.SampleInfo_Seq.Sequence;
      Related_Request_Info : DDS.SampleIdentity_T) return DDS.ReturnCode_T;

   function Read_Replies_For_Related_Request
     (Self                 : not null access Ref;
      Related_Request_Info : DDS.SampleIdentity_T) return Reply_DataReader.Container'Class;


   procedure Return_Loan (Self         : not null access Ref;
                          Replies      : not null Reply_DataReader.Treats.Data_Sequences.Sequence_Access;
                          Sample_Info  : DDS.SampleInfo_Seq.Sequence_Access);

   procedure Return_Loan (Self        : not null access Ref;
      Replies     : in out Reply_DataReader.Treats.Data_Sequences.Sequence;
                          Sample_Info : in out DDS.SampleInfo_Seq.Sequence);
   procedure Delete (Self : in out Ref);

   procedure Wait_For_Replies
     (Self : in out Ref; Min_Count : Dds.Long; Max_Wait : DDS.Duration_T);


   procedure Wait_For_Replies_For_Related_Reques
     (Self               : in out Ref;
      Min_Count          : Dds.Long;
      Max_Wait           : DDS.Duration_T;
      Related_Request_Id : DDS.SampleIdentity_T);

   function Get_Request_DataWriter (Self : not null access Ref) return Request_DataWriter.Ref_Access;
   function Get_Reply_DataReader (Self : not null access Ref) return Reply_DataReader.Ref_Access;

   pragma Warnings (Off);
   function Touch_Samples
     (Self           : not null access Ref;
      Max_Count      : DDS.Integer;
      Read_Condition : DDS.ReadCondition.Ref_Access) return Integer is (0);

   function Wait_For_Any_Sample
     (Self             : not null access Ref;
      Max_Wait         : DDS.Duration_T;
      Min_Sample_Count : DDS.Integer) return DDS.ReturnCode_T;
   pragma Warnings (On);

private
   package Listners_Impl is
      type DataReader_Listner (Parent : not null access Ref )is new DDS.DataReaderListener.Ref with null record;

      procedure On_Offered_Deadline_Missed
        (Self   : not null access DataReader_Listner;
         Writer : access DDS.DataWriter.Ref'Class;
         Status : in DDS.OfferedDeadlineMissedStatus);

      procedure On_Data_Available
        (Self       : not null access DataReader_Listner;
         The_Reader : in DDS.DataReaderListener.DataReader_Access);

      procedure On_Offered_Incompatible_Qos
        (Self   : not null access DataReader_Listner;
         Writer : access DDS.DataWriter.Ref'Class;
         Status : in DDS.OfferedIncompatibleQosStatus);
      --  <dref>DataWriterListener_on_offered_incompatible_qos</dref>

      procedure On_Liveliness_Lost
        (Self   : not null access DataReader_Listner;
         Writer : access DDS.DataWriter.Ref'Class;
         Status : in DDS.LivelinessLostStatus);
      --  <dref>DataWriterListener_on_liveliness_lost</dref>

      procedure On_Publication_Matched
        (Self   : not null access DataReader_Listner;
         Writer : access DDS.DataWriter.Ref'Class;
         Status : in DDS.PublicationMatchedStatus);
      --  <dref>DataWriterListener_on_publication_matched</dref>

      procedure On_Reliable_Writer_Cache_Changed
        (Self   : not null access DataReader_Listner;
         Writer : access DDS.DataWriter.Ref'Class;
         Status : in DDS.ReliableWriterCacheChangedStatus);
      --  <dref>DataWriterListener_on_reliable_writer_cache_changed</dref>

      procedure On_Reliable_Reader_Activity_Changed
        (Self   : not null access DataReader_Listner;
         Writer : access DDS.DataWriter.Ref'Class;
         Status : in DDS.ReliableReaderActivityChangedStatus);


      procedure On_Destination_Unreachable
        (Self     : not null access DataReader_Listner;
         Writer   : access DDS.DataWriter.Ref'Class;
         Instance : in DDS.InstanceHandle_T;
         Locator  : in DDS.Locator_T);

      procedure On_Data_Request
        (Self     : not null access DataReader_Listner;
         Writer   : access DDS.DataWriter.Ref'Class;
         Cookie   : in DDS.Cookie_T;
         Request  : in out System.Address);

      procedure On_Data_Return
        (Self     : not null access DataReader_Listner;
         Writer   : access DDS.DataWriter.Ref'Class;
         Arg      : System.Address;
         Cookie   : in DDS.Cookie_T);
      procedure On_Sample_Removed
        (Self   : not null access DataReader_Listner;
         Writer : access DDS.DataWriter.Ref'Class;
         Cookie : in DDS.Cookie_T);

      procedure On_Instance_Replaced
        (Self     : not null access DataReader_Listner;
         Writer   : access DDS.DataWriter.Ref'Class;
         Instance : in DDS.InstanceHandle_T);
      --  <dref>DataWriterListener_on_instance_replaced</dref>

      procedure On_Application_Acknowledgment
        (Self   : not null access DataReader_Listner;
         Writer : access DDS.DataWriter.Ref'Class;
         Info   : in DDS.AcknowledgmentInfo);
      --  <dref>DataWriterListener_on_application_acknowledgment</dref>

      procedure On_Service_Request_Accepted
        (Self   : not null access DataReader_Listner;
         Writer : access DDS.DataWriter.Ref'Class;
         Info   : in DDS.ServiceRequestAcceptedStatus);


      type DataWriter_Listner (Parent : not null access Ref )is new DDS.DataWriterListener.Ref with null record;
      procedure On_Requested_Deadline_Missed
        (Self       : not null access DataWriter_Listner;
         The_Reader : in DDS.DataReaderListener.DataReader_Access;
         Status     : in DDS.RequestedDeadlineMissedStatus);

      procedure On_Requested_Incompatible_Qos
        (Self       : not null access DataWriter_Listner;
         The_Reader : in DDS.DataReaderListener.DataReader_Access;
         Status     : in DDS.RequestedIncompatibleQosStatus);

      procedure On_Sample_Rejected
        (Self       : not null access DataWriter_Listner;
         The_Reader : in DDS.DataReaderListener.DataReader_Access;
         Status     : in DDS.SampleRejectedStatus);

      procedure On_Liveliness_Changed
        (Self       : not null access DataWriter_Listner;
         The_Reader : in DDS.DataReaderListener.DataReader_Access;
         Status     : in DDS.LivelinessChangedStatus);

      procedure On_Data_Available
        (Self       : not null access DataWriter_Listner;
         The_Reader : in DDS.DataReaderListener.DataReader_Access);

      procedure On_Subscription_Matched
        (Self       : not null access DataWriter_Listner;
         The_Reader : in DDS.DataReaderListener.DataReader_Access;
         Status     : in DDS.SubscriptionMatchedStatus);

      procedure On_Sample_Lost
        (Self       : not null access DataWriter_Listner;
         The_Reader : in DDS.DataReaderListener.DataReader_Access;
         Status     : in DDS.SampleLostStatus);
   end Listners_Impl;

   type Ref is limited new
     DDS.Request_Reply.Requester.Impl.Ref
   with record
      Listner            : Request_Listeners.Ref_Access;
      Writer_Listner     : aliased Listners_Impl.DataWriter_Listner (Ref'Access);
      Reader_Listner     : aliased Listners_Impl.DataReader_Listner (Ref'Access);
      Mask               : DDS.StatusKind;
   end record;

   procedure Initialize (Self : in out Ref) is null;
   procedure Finalize (Self : in out Ref);

   function Get_Reply_DataReader
     (Self : not null access Ref)
      return Reply_DataReader.Ref_Access is
     (Reply_DataReader.Ref_Access (Self.Reader));

   function Get_Request_DataWriter
     (Self : not null access Ref)
      return Request_DataWriter.Ref_Access is
     (Request_DataWriter.Ref_Access (Self.Writer));

end DDS.Request_Reply.Requester.Typed_Requester_Generic;
