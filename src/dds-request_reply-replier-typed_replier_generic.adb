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

with Ada.Unchecked_Deallocation;
with DDS.DataReader_Impl;
with DDS.Request_Reply.Impl;
with DDS.DataWriter_Impl;
package body DDS.Request_Reply.Replier.Typed_Replier_Generic is
   use type DDS.Publisher.Ref_Access;
   use type DDS.Subscriber.Ref_Access;
   procedure Free is new Ada.Unchecked_Deallocation (Ref'Class, Ref_Access);

   ------------
   -- Create --
   ------------

   function Create
     (Participant  : DDS.DomainParticipant.Ref_Access;
      Service_Name : DDS.String;
      Library_Name : DDS.String;
      Profile_Name : DDS.String;
      Publisher    : DDS.Publisher.Ref_Access     := null;
      Subscriber   : DDS.Subscriber.Ref_Access    := null;
      Listner      : Replyer_Listeners.Ref_Access := null;
      Mask         : DDS.StatusKind := DDS.STATUS_MASK_NONE) return Ref_Access
   is
      Request_Topic_Name : DDS.String := DDS.Request_Reply.Impl.Create_Request_Topic_Name_From_Service_Name (Service_Name);
      Reply_Topic_Name   : DDS.String := DDS.Request_Reply.Impl.Create_Reply_Topic_Name_From_Service_Name (Service_Name);
      Ret                : Ref_Access;
   begin
      Ret := Create (Participant,
                     Request_Topic_Name,
                     Reply_Topic_Name,
                     Library_Name,
                     Profile_Name,
                     Publisher,
                     Subscriber,
                     Listner, Mask);
      Finalize (Request_Topic_Name);
      Finalize (Reply_Topic_Name);
      return Ret;

   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Participant        : DDS.DomainParticipant.Ref_Access;
      Request_Topic_Name : DDS.String;
      Reply_Topic_Name   : DDS.String;
      Library_Name       : DDS.String;
      Profile_Name       : DDS.String;
      Publisher          : DDS.Publisher.Ref_Access     := null;
      Subscriber         : DDS.Subscriber.Ref_Access    := null;
      Listner            : Replyer_Listeners.Ref_Access := null;
      Mask               : DDS.StatusKind := DDS.STATUS_MASK_NONE) return Ref_Access
   is
      Datawriter_Qos    : DDS.DataWriterQos;
      Datareader_Qos    : DDS.DataReaderQos;
      Request_Topic_QoS : DDS.TopicQos;
      Reply_Topic_Qos   : DDS.TopicQos;
   begin
      Participant.Get_Factory.Get_Datareader_Qos_From_Profile_W_Topic_Name (Datareader_Qos, Library_Name, Profile_Name, Request_Topic_Name);
      Participant.Get_Factory.Get_Datawriter_Qos_From_Profile_W_Topic_Name (DataWriter_Qos, Library_Name, Profile_Name, Reply_Topic_Name);
      Participant.Get_Factory.Get_Topic_Qos_From_Profile_W_Topic_Name (Request_Topic_QoS, Library_Name, Profile_Name, Request_Topic_Name);
      Participant.Get_Factory.Get_Topic_Qos_From_Profile_W_Topic_Name (Reply_Topic_Qos, Library_Name, Profile_Name, Reply_Topic_Name);

      return Create (Participant        => Participant,
                     Request_Topic_Name => Request_Topic_Name,
                     Reply_Topic_Name   => Reply_Topic_Name,
                     Datawriter_Qos     => Datawriter_Qos,
                     Datareader_Qos     => Datareader_Qos,
                     Reply_Topic_Qos    => Reply_Topic_Qos,
                     Request_Topic_QoS  => Request_Topic_QoS,
                     Publisher          => Publisher,
                     Subscriber         => Subscriber,
                     Listner            => Listner,
                     Mask               => Mask);

   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Participant       : DDS.DomainParticipant.Ref_Access;
      Service_Name      : DDS.String;
      Datawriter_Qos    : DDS.DataWriterQos;
      Datareader_Qos    : DDS.DataReaderQos;
      Reply_Topic_Qos   : DDS.TopicQos;
      Request_Topic_Qos : DDS.TopicQos;
      Publisher         : DDS.Publisher.Ref_Access     := null;
      Subscriber        : DDS.Subscriber.Ref_Access    := null;
      Listner           : Replyer_Listeners.Ref_Access := null;
      Mask              : DDS.StatusKind := DDS.STATUS_MASK_NONE) return Ref_Access
   is
      Request_Topic_Name : DDS.String := DDS.Request_Reply.Impl.Create_Request_Topic_Name_From_Service_Name (Service_Name);
      Reply_Topic_Name   : DDS.String := DDS.Request_Reply.Impl.Create_Reply_Topic_Name_From_Service_Name (Service_Name);
      Ret                : Ref_Access;
   begin
      Ret :=  Create (Participant        => Participant,
                      Request_Topic_Name => Request_Topic_Name,
                      Reply_Topic_Name   => Reply_Topic_Name,
                      Datawriter_Qos     => Datawriter_Qos,
                      Datareader_Qos     => Datareader_Qos,
                      Reply_Topic_Qos    => Reply_Topic_Qos,
                      Request_Topic_Qos  => Request_Topic_Qos,
                      Publisher          => Publisher,
                      Subscriber         => Subscriber,
                      Listner            => Listner,
                      Mask               => Mask);
      Finalize (Request_Topic_Name);
      Finalize (Reply_Topic_Name);
      return Ret;
   end Create;

   ------------
   -- Create --
   ------------
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
      Mask               : DDS.StatusKind := DDS.STATUS_MASK_NONE) return Ref_Access
   is
      Ret : Ref_Access := new Ref;
   begin
      Ret.Listner := Listner;
      Ret.Mask := Mask;
      Ret.Participant := Participant;
      Ret.Publisher := (if Publisher /= null
                        then
                           Publisher
                        else
                           Participant.Get_Implicit_Publisher);

      Ret.Subscriber := (if Subscriber /= null
                         then
                            Subscriber
                         else
                            Participant.Get_Implicit_Subscriber);

      Ret.Reply_Topic := Ret.Create_Reply_Topic (Reply_Topic_Name, Reply_DataWriter.Treats.Get_Type_Name, Reply_Topic_Qos);

      Ret.Request_Topic := Ret.Create_Request_Topic (Request_Topic_Name, Request_DataReader.Treats.Get_Type_Name, Request_Topic_Qos);

      Ret.Reader := DataReader_Impl.Ref_Access
        (Ret.Subscriber.Create_DataReader
           (Topic      => Ret.Request_Topic.As_TopicDescription,
            Qos        => Datareader_Qos,
            A_Listener => Ret.Reader_Listner'Access,
            Mask       => DDS.STATUS_MASK_ALL));

      Ret.Writer := DataWriter_Impl.Ref_Access
        (Ret.Publisher.Create_DataWriter
           (A_Topic    => Ret.Reply_Topic,
            Qos        => Datawriter_Qos,
            A_Listener => Ret.Writer_Listner'Access,
            Mask       => DDS.STATUS_MASK_ALL));
      Ret.Any_Sample_Cond := Ret.Reader.Create_Readcondition
        (Sample_States   => DDS.ANY_SAMPLE_STATE,
         View_States     => DDS.ANY_VIEW_STATE,
         Instance_States => DDS.ANY_INSTANCE_STATE);
      Ret.Not_Read_Sample_Cond := Ret.Reader.Create_Readcondition
        (Sample_States   => DDS.NOT_READ_SAMPLE_STATE,
         View_States     => DDS.ANY_VIEW_STATE,
         Instance_States => DDS.ANY_INSTANCE_STATE);
      return Ret;
   exception
      when others =>
         Ret.Delete;
         Free (Ret);
         raise;
   end Create;

   ------------
   -- Delete --
   ------------

   procedure Delete (Self : in out Ref_Access) is
   begin
      --        pragma Compile_Time_Warning (Standard.True, "Delete unimplemented");
      raise Program_Error with "Unimplemented procedure Delete";
   end Delete;

   ----------------
   -- Send_Reply --
   ----------------

   ----------------
   -- Send_Reply --
   ----------------

   procedure Send_Reply
     (Self  : not null access Ref;
      Reply : Reply_DataWriter.Treats.Data_Type;
      Id    : DDS.SampleIdentity_T)
   is
      Params : DDS.WriteParams_T;
   begin
      Self.Send_Sample (Data => Reply , Related_Request_Info => Id, WriteParams => Params);
   end Send_Reply;

   ---------------------
   -- Receive_Request --
   ---------------------

   function Receive_Request
     (Self       :        not null access Ref;
      Request    : in out Request_DataReader.Treats.Data_Type;
      SampleInfo : in out DDS.SampleInfo;
      Timeout    :        DDS.Duration_T := DDS.DURATION_INFINITE) return DDS.ReturnCode_T
   is
   begin
      return Ret : DDS.ReturnCode_T := DDS.RETCODE_NO_DATA do
         for I of Self.Receive_Request (Min_Reply_Count => 1, Max_Reply_Count => 1, Timeout => Timeout) loop
            if I.Sample_Info.Valid_Data then
               Request_DataReader.Treats.Copy (Request, I.Data.all);
               Copy (SampleInfo, I.Sample_Info.all);
               Ret := DDS.RETCODE_OK;
            end if;
         end loop;
      end return;
   end Receive_Request;

   ---------------------
   -- Receive_Request --
   ---------------------

   procedure Receive_Request
     (Self     :        not null access Ref;
      Request  : in out Request_DataReader.Treats.Data_Type;
      Info_Seq : in out DDS.SampleInfo;
      Timeout  :        DDS.Duration_T := DDS.DURATION_INFINITE)
   is
   begin
      Dds.Ret_Code_To_Exception (Self.Receive_Request (Request, Info_Seq, Timeout));
   end Receive_Request;

   ---------------------
   -- Receive_Request --
   ---------------------

   function Receive_Request
     (Self : not null access Ref; Timeout : DDS.Duration_T)
      return Request_DataReader.Container'Class
   is
   begin
      return Request_DataReader.Ref_Access (Self.Reader).Take;
   end Receive_Request;



   ----------------------
   -- Receive_Requests --
   ----------------------


   ----------------------
   -- Receive_Requests --
   ----------------------


   ---------------------
   -- Receive_Request --
   ---------------------

   function Take_Request
     (Self            : not null access Ref;
      Max_Reply_Count : DDS.Long       := DDS.INFINITE)
      return Request_DataReader.Container'Class is
   begin
      return Self.Get_Request_Data_Reader.Take (Max_Samples   => Max_Reply_Count);
   end;

   function Receive_Request
     (Self            : not null access Ref;
      Min_Reply_Count : DDS.Long := 1;
      Max_Reply_Count : DDS.Long       := DDS.INFINITE;
      Timeout         : DDS.Duration_T := DDS.DURATION_INFINITE)
      return Request_DataReader.Container'Class
   is
   begin
      Self.Wait_For_Requests (Min_Count => Min_Reply_Count, Max_Wait => Timeout);
      return Self.Take_Request (Max_Reply_Count => Max_Reply_Count);
   end Receive_Request;



   ------------------
   -- Read_Request --
   ------------------

   function Read_Request
     (Self            : not null access Ref;
      Max_Reply_Count : DDS.Long := DDS.INFINITE)
      return Request_DataReader.Container'Class
   is
   begin
      Self.Wait_For_Requests (Min_Count => 1, Max_Wait => DDS.DURATION_ZERO);
      return Request_DataReader.Ref_Access (Self.Reader).Take (Max_Samples => 1);
   end Read_Request;

   -----------------
   -- Return_Loan --
   -----------------

   procedure Return_Loan
     (Self        : not null access Ref;
      Replies     : not null Request_DataReader.Treats.Data_Sequences.Sequence_Access;
      Sample_Info : DDS.SampleInfo_Seq.Sequence_Access)
   is
   begin
      Self.Return_Loan (Replies.all, Sample_Info.all);
   end Return_Loan;

   -----------------
   -- Return_Loan --
   -----------------

   procedure Return_Loan
     (Self        : not null access Ref;
      Replies     : in out Request_DataReader.Treats.Data_Sequences.Sequence;
      Sample_Info : in out DDS.SampleInfo_Seq.Sequence)
   is
   begin
      Request_DataReader.Ref_Access (Self.Reader).Return_Loan (Replies, Sample_Info);
   end Return_Loan;

   ------------
   -- Delete --
   ------------

   procedure Delete (This : in out Ref) is
   begin
      --        pragma Compile_Time_Warning (Standard.True, "Delete unimplemented");
      raise Program_Error with "Unimplemented procedure Delete";
   end Delete;


   --===========================================================================
   --===========================================================================

   use type Replyer_Listeners.Ref_Access;
   --------------------------------
   -- On_Offered_Deadline_Missed --
   --------------------------------

   procedure On_Offered_Deadline_Missed
     (Self   :    not null access DataReader_Listner;
      Writer :    access DDS.DataWriter.Ref'Class;
      Status : in DDS.OfferedDeadlineMissedStatus)
   is
   begin
      if Self.Parent.Listner /= null then
         Self.Parent.Listner.On_Offered_Deadline_Missed (Self.Parent.all'Access, Status);
      end if;
   end On_Offered_Deadline_Missed;

   -----------------------
   -- On_Data_Available --
   -----------------------

   procedure On_Data_Available
     (Self       :    not null access DataReader_Listner;
      The_Reader : in DDS.DataReaderListener.DataReader_Access)
   is
   begin
      if Self.Parent.Listner /= null then
         Self.Parent.Listner.On_Request_Avalible (Self.Parent.all'Access);
      end if;
   end On_Data_Available;

   ---------------------------------
   -- On_Offered_Incompatible_Qos --
   ---------------------------------

   procedure On_Offered_Incompatible_Qos
     (Self   :    not null access DataReader_Listner;
      Writer :    access DDS.DataWriter.Ref'Class;
      Status : in DDS.OfferedIncompatibleQosStatus)
   is
   begin
      if Self.Parent.Listner /= null then
         Self.Parent.Listner.On_Offered_Incompatible_Qos (Self.Parent.all'Access, Status);
      end if;
   end On_Offered_Incompatible_Qos;

   ------------------------
   -- On_Liveliness_Lost --
   ------------------------

   procedure On_Liveliness_Lost
     (Self   :    not null access DataReader_Listner;
      Writer :    access DDS.DataWriter.Ref'Class;
      Status : in DDS.LivelinessLostStatus)
   is
   begin
      if Self.Parent.Listner /= null then
         Self.Parent.Listner.On_Liveliness_Lost (Self.Parent.all'Access, Status);
      end if;
   end On_Liveliness_Lost;

   ----------------------------
   -- On_Publication_Matched --
   ----------------------------

   procedure On_Publication_Matched
     (Self   :    not null access DataReader_Listner;
      Writer :    access DDS.DataWriter.Ref'Class;
      Status : in DDS.PublicationMatchedStatus)
   is
   begin
      if Self.Parent.Listner /= null then
         Self.Parent.Listner.On_Publication_Matched (Self.Parent.all'Access, Status);
      end if;
   end On_Publication_Matched;

   --------------------------------------
   -- On_Reliable_Writer_Cache_Changed --
   --------------------------------------

   procedure On_Reliable_Writer_Cache_Changed
     (Self   : not null access DataReader_Listner;
      Writer : access DDS.DataWriter.Ref'Class;
      Status : in DDS.ReliableWriterCacheChangedStatus)
   is
   begin
      if Self.Parent.Listner /= null then
         Self.Parent.Listner.On_Reliable_Writer_Cache_Changed (Self.Parent.all'Access, Status);
      end if;
   end On_Reliable_Writer_Cache_Changed;

   -----------------------------------------
   -- On_Reliable_Reader_Activity_Changed --
   -----------------------------------------

   procedure On_Reliable_Reader_Activity_Changed
     (Self   :    not null access DataReader_Listner;
      Writer :    access DDS.DataWriter.Ref'Class;
      Status : in DDS.ReliableReaderActivityChangedStatus)
   is
   begin
      if Self.Parent.Listner /= null then
         Self.Parent.Listner.On_Reliable_Reader_Activity_Changed (Self.Parent.all'Access, Status);
      end if;
   end On_Reliable_Reader_Activity_Changed;

   --------------------------------
   -- On_Destination_Unreachable --
   --------------------------------

   procedure On_Destination_Unreachable
     (Self     :    not null access DataReader_Listner;
      Writer   :    access DDS.DataWriter.Ref'Class;
      Instance : in DDS.InstanceHandle_T; Locator : in DDS.Locator_T)
   is
   begin
      if Self.Parent.Listner /= null then
         Self.Parent.Listner.On_Destination_Unreachable (Self.Parent.all'Access, Instance, Locator);
      end if;
   end On_Destination_Unreachable;

   ---------------------
   -- On_Data_Request --
   ---------------------

   procedure On_Data_Request
     (Self    :        not null access DataReader_Listner;
      Writer  :    access DDS.DataWriter.Ref'Class;
      Cookie  : in DDS.Cookie_T;
      Request : in out System.Address)
   is
   begin
      if Self.Parent.Listner /= null then
         Self.Parent.Listner.On_Data_Request (Self.Parent.all'Access, Cookie, Request);
      end if;
   end On_Data_Request;

   --------------------
   -- On_Data_Return --
   --------------------

   procedure On_Data_Return
     (Self   :    not null access DataReader_Listner;
      Writer :    access DDS.DataWriter.Ref'Class;
      Arg    : System.Address;
      Cookie : in DDS.Cookie_T)
   is
   begin
      if Self.Parent.Listner /= null then
         Self.Parent.Listner.On_Data_Return (Self.Parent.all'Access, Arg, Cookie);
      end if;
   end On_Data_Return;

   -----------------------
   -- On_Sample_Removed --
   -----------------------

   procedure On_Sample_Removed
     (Self   : not null access DataReader_Listner;
      Writer : access DDS.DataWriter.Ref'Class;
      Cookie : in DDS.Cookie_T)
   is
   begin
      if Self.Parent.Listner /= null then
         Self.Parent.Listner.On_Sample_Removed (Self.Parent.all'Access, Cookie);
      end if;
   end On_Sample_Removed;

   --------------------------
   -- On_Instance_Replaced --
   --------------------------

   procedure On_Instance_Replaced
     (Self     :    not null access DataReader_Listner;
      Writer   :    access DDS.DataWriter.Ref'Class;
      Instance : in DDS.InstanceHandle_T)
   is
   begin
      if Self.Parent.Listner /= null then
         Self.Parent.Listner.On_Instance_Replaced (Self.Parent.all'Access, Instance);
      end if;
   end On_Instance_Replaced;

   -----------------------------------
   -- On_Application_Acknowledgment --
   -----------------------------------

   procedure On_Application_Acknowledgment
     (Self   :    not null access DataReader_Listner;
      Writer :    access DDS.DataWriter.Ref'Class;
      Info   : in DDS.AcknowledgmentInfo)
   is
   begin
      if Self.Parent.Listner /= null then
         Self.Parent.Listner.On_Application_Acknowledgment (Self.Parent.all'Access, Info);
      end if;
   end On_Application_Acknowledgment;

   ---------------------------------
   -- On_Service_Request_Accepted --
   ---------------------------------

   procedure On_Service_Request_Accepted
     (Self   :    not null access DataReader_Listner;
      Writer :    access DDS.DataWriter.Ref'Class;
      Info   : in DDS.ServiceRequestAcceptedStatus)
   is
   begin
      if Self.Parent.Listner /= null then
         Self.Parent.Listner.On_Service_Request_Accepted (Self.Parent.all'Access, Info);
      end if;
   end On_Service_Request_Accepted;

   ----------------------------------
   -- On_Requested_Deadline_Missed --
   ----------------------------------

   procedure On_Requested_Deadline_Missed
     (Self       :    not null access DataWriter_Listner;
      The_Reader : in DDS.DataReaderListener.DataReader_Access;
      Status     : in DDS.RequestedDeadlineMissedStatus)
   is
   begin
      if Self.Parent.Listner /= null then
         Self.Parent.Listner.On_Requested_Deadline_Missed (Self.Parent.all'Access, Status);
      end if;
   end On_Requested_Deadline_Missed;

   -----------------------------------
   -- On_Requested_Incompatible_Qos --
   -----------------------------------

   procedure On_Requested_Incompatible_Qos
     (Self       :    not null access DataWriter_Listner;
      The_Reader : in DDS.DataReaderListener.DataReader_Access;
      Status     : in DDS.RequestedIncompatibleQosStatus)
   is
   begin
      if Self.Parent.Listner /= null then
         Self.Parent.Listner.On_Requested_Incompatible_Qos (Self.Parent.all'Access, Status);
      end if;
   end On_Requested_Incompatible_Qos;

   ------------------------
   -- On_Sample_Rejected --
   ------------------------

   procedure On_Sample_Rejected
     (Self       :    not null access DataWriter_Listner;
      The_Reader : in DDS.DataReaderListener.DataReader_Access;
      Status     : in DDS.SampleRejectedStatus)
   is
   begin
      if Self.Parent.Listner /= null then
         Self.Parent.Listner.On_Sample_Rejected (Self.Parent.all'Access, Status);
      end if;
   end On_Sample_Rejected;

   ---------------------------
   -- On_Liveliness_Changed --
   ---------------------------

   procedure On_Liveliness_Changed
     (Self       :    not null access DataWriter_Listner;
      The_Reader : in DDS.DataReaderListener.DataReader_Access;
      Status     : in DDS.LivelinessChangedStatus)
   is
   begin
      if Self.Parent.Listner /= null then
         Self.Parent.Listner.On_Liveliness_Changed (Self.Parent.all'Access, Status);
      end if;
   end On_Liveliness_Changed;

   -----------------------
   -- On_Data_Available --
   -----------------------

   --     procedure On_Data_Available
   --       (Self       :    not null access DataWriter_Listner;
   --        The_Reader : in DDS.DataReaderListener.DataReader_Access)
   --     is
   --     begin
   --        if Self.Parent.Listner /= null then
   --           Self.Parent.Listner.On_Request_Avalible (Self.all'Access);
   --        end if;
   --     end On_Data_Available;

   -----------------------------
   -- On_Subscription_Matched --
   -----------------------------

   procedure On_Subscription_Matched
     (Self       :    not null access DataWriter_Listner;
      The_Reader : in DDS.DataReaderListener.DataReader_Access;
      Status     : in DDS.SubscriptionMatchedStatus)
   is
   begin
      if Self.Parent.Listner /= null then
         Self.Parent.Listner.On_Subscription_Matched (Self.Parent.all'Access, Status);
      end if;
   end On_Subscription_Matched;

   --------------------
   -- On_Sample_Lost --
   --------------------

   procedure On_Sample_Lost
     (Self       :    not null access DataWriter_Listner;
      The_Reader : in DDS.DataReaderListener.DataReader_Access;
      Status     : in DDS.SampleLostStatus)
   is
   begin
      if Self.Parent.Listner /= null then
         Self.Parent.Listner.On_Sample_Lost (Self.Parent.all'Access, Status);
      end if;
   end On_Sample_Lost;

   procedure Send_Sample (Self                 : not null access Ref;
                          Data                 : Reply_DataWriter.Treats.Data_Type;
                          Related_Request_Info : DDS.SampleIdentity_T;
                          WriteParams          : in out DDS.WriteParams_T) is
   begin
      Self.Configure_Params_For_Reply (WriteParams, Related_Request_Info);
      Reply_DataWriter.Ref_Access (Self.Writer).Write_W_Params (Data, WriteParams);
   end;

end DDS.Request_Reply.Replier.Typed_Replier_Generic;
