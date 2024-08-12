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

with DDS.DataReaderListener;
with DDS.DataWriter;
with DDS.DataWriterListener;
with DDS.DomainParticipant;
with DDS.Publisher;
with DDS.Subscriber;
with DDS.Typed_DataWriter_Generic;
with DDS.Typed_DataReader_Generic;
private with DDS.Request_Reply.Replier.Impl;

generic
   with package Reply_DataWriter is new DDS.Typed_DataWriter_Generic (<>);
   with package Request_DataReader is new DDS.Typed_DataReader_Generic (<>);
package DDS.Request_Reply.Replier.Typed_Replier_Generic is

   type Ref (<>) is limited new DDS.Request_Reply.Replier.Ref with private;
   type Ref_Access is access all Ref'Class;

   package Replyer_Listeners is
      type Ref is limited interface;
      type Ref_Access is access all Ref'Class;

      procedure On_Request_Avalible (Self      : not null access Ref;
                                     Requester : not null access Typed_Replier_Generic.Ref'Class) is abstract;

      procedure On_Offered_Deadline_Missed
        (Self   : not null access Ref;
         Writer : access Typed_Replier_Generic.Ref'Class;
         Status : in DDS.OfferedDeadlineMissedStatus) is null;
      --  <dref>DataWriterListener_on_offered_deadline_missed</dref>

      procedure On_Offered_Incompatible_Qos
        (Self   : not null access Ref;
         Writer : access Typed_Replier_Generic.Ref'Class;
         Status : in DDS.OfferedIncompatibleQosStatus) is null;
      --  <dref>DataWriterListener_on_offered_incompatible_qos</dref>

      procedure On_Liveliness_Lost
        (Self   : not null access Ref;
         Writer : access Typed_Replier_Generic.Ref'Class;
         Status : in DDS.LivelinessLostStatus) is null;
      --  <dref>DataWriterListener_on_liveliness_lost</dref>

      procedure On_Publication_Matched
        (Self   : not null access Ref;
         Writer : access Typed_Replier_Generic.Ref'Class;
         Status : in DDS.PublicationMatchedStatus) is null;
      --  <dref>DataWriterListener_on_publication_matched</dref>

      procedure On_Reliable_Writer_Cache_Changed
        (Self   : not null access Ref;
         Writer : access Typed_Replier_Generic.Ref'Class;
         Status : in DDS.ReliableWriterCacheChangedStatus) is null;
      --  <dref>DataWriterListener_on_reliable_writer_cache_changed</dref>

      procedure On_Reliable_Reader_Activity_Changed
        (Self   : not null access Ref;
         Writer : access Typed_Replier_Generic.Ref'Class;
         Status : in DDS.ReliableReaderActivityChangedStatus) is null;
      --  <dref>DataWriterListener_on_reliable_reader_activity_changed</dref>

      procedure On_Destination_Unreachable
        (Self     : not null access Ref;
         Writer   : access Typed_Replier_Generic.Ref'Class;
         Instance : in DDS.InstanceHandle_T;
         Locator  : in DDS.Locator_T) is null;
      --  <dref internal="true">DataWriterListener_on_destination_unreachable</dref>

      procedure On_Data_Request
        (Self     : not null access Ref;
         Writer   : access Typed_Replier_Generic.Ref'Class;
         Cookie   : in DDS.Cookie_T;
         Request  : in out System.Address) is null;
      --  <dref internal="true">DataWriterListener_on_data_request</dref>

      procedure On_Data_Return
        (Self     : not null access Ref;
         Writer   : access Typed_Replier_Generic.Ref'Class;
         Arg      : System.Address;
         Cookie   : in DDS.Cookie_T) is null;
      --  <dref internal="true">DataWriterListener_on_data_return</dref>

      procedure On_Sample_Removed
        (Self   : not null access Ref;
         Writer : access Typed_Replier_Generic.Ref'Class;
         Cookie : in DDS.Cookie_T) is null;
      --  <dref internal="true">DataWriterListener_on_sample_removed</dref>

      procedure On_Instance_Replaced
        (Self     : not null access Ref;
         Writer   : access Typed_Replier_Generic.Ref'Class;
         Instance : in DDS.InstanceHandle_T) is null;
      --  <dref>DataWriterListener_on_instance_replaced</dref>

      procedure On_Application_Acknowledgment
        (Self   : not null access Ref;
         Writer : access Typed_Replier_Generic.Ref'Class;
         Info   : in DDS.AcknowledgmentInfo) is null;
      --  <dref>DataWriterListener_on_application_acknowledgment</dref>

      procedure On_Service_Request_Accepted
        (Self   : not null access Ref;
         Writer : access Typed_Replier_Generic.Ref'Class;
         Info   : in DDS.ServiceRequestAcceptedStatus) is null;
      --  <dref>DataWriterListener_on_service_request_accepted</dref>

      procedure On_Requested_Deadline_Missed
        (Self       : not null access Ref;
         The_Reader : in Typed_Replier_Generic.Ref_Access;
         Status     : in DDS.RequestedDeadlineMissedStatus)
      is null;
      --  <dref>DataReaderListener_on_requested_deadline_missed</dref>
      --  <internal>
      --  Handles the StatusKind.REQUESTED_DEADLINE_MISSED_STATUS
      --  communication status.
      --  </internal>

      procedure On_Requested_Incompatible_Qos
        (Self       : not null access Ref;
         The_Reader : in Typed_Replier_Generic.Ref_Access;
         Status     : in DDS.RequestedIncompatibleQosStatus)
      is null;
      --  <dref>DataReaderListener_on_requested_incompatible_qos</dref>
      --  <internal>
      --  Handles the StatusKind.REQUESTED_INCOMPATIBLE_QOS_STATUS
      --  communication status.
      --  </internal>

      procedure On_Sample_Rejected
        (Self       : not null access Ref;
         The_Reader : in Typed_Replier_Generic.Ref_Access;
         Status     : in DDS.SampleRejectedStatus)
      is null;
      --  <dref>DataReaderListener_on_sample_rejected</dref>
      --  <internal>
      --  Handles the StatusKind.SAMPLE_REJECTED_STATUS
      --  communication status.
      --  </internal>

      procedure On_Liveliness_Changed
        (Self       : not null access Ref;
         The_Reader : in Typed_Replier_Generic.Ref_Access;
         Status     : in DDS.LivelinessChangedStatus)
      is null;
      --  <dref>DataReaderListener_on_liveliness_changed</dref>
      --  <internal>
      --  Handles the StatusKind.LIVELINESS_CHANGED_STATUS
      --  communication status.
      --  </internal>



      procedure On_Subscription_Matched
        (Self       : not null access Ref;
         The_Reader : in Typed_Replier_Generic.Ref_Access;
         Status     : in DDS.SubscriptionMatchedStatus)
      is null;
      --  <dref>DataReaderListener_on_subscription_matched</dref>
      --  <internal>
      --  Handles the StatusKind.SUBSCRIPTION_MATCHED_STATUS communication status.
      --  </internal>

      procedure On_Sample_Lost
        (Self       : not null access Ref;
         The_Reader : in Typed_Replier_Generic.Ref_Access;
         Status     : in DDS.SampleLostStatus)
      is null;
   end Replyer_Listeners;


   function Create
     (Participant        : DDS.DomainParticipant.Ref_Access;
      Service_Name       : DDS.String;
      Library_Name       : DDS.String;
      Profile_Name       : DDS.String;
      Publisher          : DDS.Publisher.Ref_Access := null;
      Subscriber         : DDS.Subscriber.Ref_Access := null;
      Listner            : Replyer_Listeners.Ref_Access := null;
      Mask               : DDS.StatusKind := DDS.STATUS_MASK_NONE) return not null Ref_Access;

   function Create
     (Participant        : DDS.DomainParticipant.Ref_Access;
      Request_Topic_Name : DDS.String;
      Reply_Topic_Name   : DDS.String;
      Library_Name       : DDS.String;
      Profile_Name       : DDS.String;
      Publisher          : DDS.Publisher.Ref_Access := null;
      Subscriber         : DDS.Subscriber.Ref_Access := null;
      Listner            : Replyer_Listeners.Ref_Access := null;
      Mask               : DDS.StatusKind := DDS.STATUS_MASK_NONE)return not null  Ref_Access;

   function Create
     (Participant        : DDS.DomainParticipant.Ref_Access;
      Service_Name       : DDS.String;
      Datawriter_Qos     : DDS.DataWriterQos;
      Datareader_Qos     : DDS.DataReaderQos;
      Reply_Topic_Qos    : DDS.TopicQos;
      Request_Topic_Qos  : DDS.TopicQos;
      Publisher          : DDS.Publisher.Ref_Access := null;
      Subscriber         : DDS.Subscriber.Ref_Access := null;
      Listner            : Replyer_Listeners.Ref_Access := null;
      Mask               : DDS.StatusKind := DDS.STATUS_MASK_NONE)return not null  Ref_Access;

   function Create
     (Participant        : DDS.DomainParticipant.Ref_Access;
      Request_Topic_Name : DDS.String;
      Reply_Topic_Name   : DDS.String;
      Datawriter_Qos     : DDS.DataWriterQos;
      Datareader_Qos     : DDS.DataReaderQos;
      Reply_Topic_Qos    : DDS.TopicQos;
      Request_Topic_Qos  : DDS.TopicQos;
      Publisher          : DDS.Publisher.Ref_Access     := null;
      Subscriber         : DDS.Subscriber.Ref_Access    := null;
      Listner            : Replyer_Listeners.Ref_Access := null;
      Mask               : DDS.StatusKind := DDS.STATUS_MASK_NONE) return not null Ref_Access;


   procedure Delete (Self : in out Ref_Access);



   procedure Send_Reply
     (Self    : not null access Ref;
      Reply   : Reply_DataWriter.Treats.Data_Type;
      Id      : DDS.SampleIdentity_T);

   function Receive_Request
     (Self       :        not null access Ref;
      Request    : in out Request_DataReader.Treats.Data_Type;
      SampleInfo : in out DDS.SampleInfo;
      Timeout    :        DDS.Duration_T := DDS.DURATION_INFINITE) return DDS.ReturnCode_T;


   procedure Receive_Request
     (Self     : not null access Ref;
      Request  : in out Request_DataReader.Treats.Data_Type;
      Info_Seq : in out DDS.SampleInfo;
      Timeout  : DDS.Duration_T := DDS.DURATION_INFINITE);


   function Receive_Request
     (Self                 : not null access Ref;
      Min_Reply_Count      : DDS.Long := 1;
      Max_Reply_Count      : DDS.Long := DDS.INFINITE;
      Timeout              : DDS.Duration_T := DDS.DURATION_INFINITE) return Request_DataReader.Container'Class;


   function Read_Request
     (Self            : not null access Ref;
      Max_Reply_Count : DDS.Long := DDS.INFINITE)
      return Request_DataReader.Container'Class;

   procedure Delete (This : in out Ref);

   function Take_Request
     (Self            : not null access Ref;
      Max_Reply_Count : DDS.Long := DDS.INFINITE)
      return Request_DataReader.Container'Class;

private

   procedure Return_Loan (Self         : not null access Ref;
                          Replies      : not null Request_DataReader.Treats.Data_Sequences.Sequence_Access;
                          Sample_Info  : DDS.SampleInfo_Seq.Sequence_Access);

   procedure Return_Loan (Self        : not null access Ref;
                          Replies     : in out Request_DataReader.Treats.Data_Sequences.Sequence;
                          Sample_Info : in out DDS.SampleInfo_Seq.Sequence);

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


   procedure On_Subscription_Matched
     (Self       : not null access DataWriter_Listner;
      The_Reader : in DDS.DataReaderListener.DataReader_Access;
      Status     : in DDS.SubscriptionMatchedStatus);

   procedure On_Sample_Lost
     (Self       : not null access DataWriter_Listner;
      The_Reader : in DDS.DataReaderListener.DataReader_Access;
      Status     : in DDS.SampleLostStatus);

   type Ref is limited new
     DDS.Request_Reply.Replier.Impl.Ref and DDS.Request_Reply.Replier.Ref
   with record
      Listner            : Replyer_Listeners.Ref_Access;
      Writer_Listner     : aliased DataWriter_Listner (Ref'Access);
      Reader_Listner     : aliased DataReader_Listner (Ref'Access);
      Mask               : DDS.StatusKind;
   end record;

   function Get_Request_Data_Reader
     (Self : not null access Ref)
      return Request_DataReader.Ref_Access is
     (Request_DataReader.Ref_Access (Self.Reader));

   function Get_Reply_Data_Writer
     (Self : not null access Ref)
      return Reply_DataWriter.Ref_Access is
     (Reply_DataWriter.Ref_Access (Self.Writer));

   procedure Send_Sample (Self                 : not null access Ref;
                          Data                 : Reply_DataWriter.Treats.Data_Type;
                          Related_Request_Info : DDS.SampleIdentity_T;
                          WriteParams          : in out DDS.WriteParams_T);

end DDS.Request_Reply.Replier.Typed_Replier_Generic;
