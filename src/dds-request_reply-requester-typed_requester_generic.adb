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

with DDS.Request_Reply.Impl;
with DDS.DomainParticipantFactory;
with DDS.DataWriter_Impl;
with DDS.DataReader_Impl;
package body DDS.Request_Reply.Requester.Typed_Requester_Generic is
   use DDS.Subscriber;
   use DDS.Publisher;
   -----------------------------
   -- Get_Request_Data_Writer --
   -----------------------------

   function Get_Request_Data_Writer
     (Self : not null access Ref)
      return not null DDS.DataWriter.Ref_Access
   is
   begin
      return DDS.DataWriter.Ref_Access (Self.Writer);
   end Get_Request_Data_Writer;

   ---------------------------
   -- Get_Reply_Data_Reader --
   ---------------------------

   function Get_Reply_Data_Reader
     (Self : not null access Ref)
      return not null DDS.DataReader.Ref_Access
   is
   begin
      return DDS.DataReader.Ref_Access (Self.Reader);
   end Get_Reply_Data_Reader;

   ------------
   -- Create --
   ------------
   procedure Finalize (Self : in out Ref) is
   begin
      Self.Participant.Delete_DataWriter (DDS.DataWriter.Ref_Access (Self.Writer));
      Self.Participant.Delete_DataReader (DDS.DataReader.Ref_Access (Self.Reader));
      Self.Participant.Delete_Topic (Self.Request_Topic);
      Self.Participant.Delete_Topic (Self.Reply_Topic);
      Impl.Ref (Self).Finalize;
   end;

   function Create
     (Participant    : DDS.DomainParticipant.Ref_Access;
      Service_Name   : DDS.String;
      Library_Name   : DDS.String;
      Profile_Name   : DDS.String;
      Publisher      : DDS.Publisher.Ref_Access := null;
      Subscriber     : DDS.Subscriber.Ref_Access := null;
      A_Listner      : Request_Listeners.Ref_Access := null;
      Mask           : DDS.StatusKind := DDS.STATUS_MASK_NONE) return not null Ref_Access
   is
      Request_Topic_Name : DDS.String := DDS.Request_Reply.Impl.Create_Request_Topic_Name_From_Service_Name (Service_Name);
      Reply_Topic_Name   : DDS.String := DDS.Request_Reply.Impl.Create_Reply_Topic_Name_From_Service_Name (Service_Name);
      Ret : Ref_Access;
   begin
      Ret := Create (Participant        => Participant,
                     Request_Topic_Name => Request_Topic_Name,
                     Reply_Topic_Name   => Reply_Topic_Name ,
                     Library_Name       => Library_Name,
                     Profile_Name       => Profile_Name,
                     Publisher          => Publisher,
                     Subscriber         => Subscriber,
                     A_Listner          => A_Listner,
                     Mask               => Mask);
      Finalize (Request_Topic_Name);
      Finalize (Reply_Topic_Name);
      return ret;
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
      Publisher          : DDS.Publisher.Ref_Access := null;
      Subscriber         : DDS.Subscriber.Ref_Access := null;
      A_Listner          : Request_Listeners.Ref_Access := null;
      Mask               : DDS.StatusKind := DDS.STATUS_MASK_NONE) return not null Ref_Access
   is
      Datawriter_Qos : DDS.DataWriterQos;
      Datareader_Qos : DDS.DataReaderQoS;
      Topic_Qos      : DDS.TopicQos;
      Factory        : constant DDS.DomainParticipantFactory.Ref_Access := DDS.DomainParticipantFactory.Get_Instance;
   begin
      Factory.Get_Datareader_Qos_From_Profile_W_Topic_Name
        (QoS          => Datareader_Qos,
         Library_Name => Library_Name,
         Profile_Name => Profile_Name,
         Topic_Name   => Reply_Topic_Name);

      Factory.Get_Datawriter_Qos_From_Profile_W_Topic_Name
        (QoS          => Datawriter_Qos,
         Library_Name => Library_Name,
         Profile_Name => Profile_Name,
         Topic_Name   => Request_Topic_Name);

      Factory.Get_Topic_Qos_From_Profile_W_Topic_Name
        (QoS          => Topic_Qos,
         Library_Name => Library_Name,
         Profile_Name => Profile_Name,
         Topic_Name   => Request_Topic_Name);

      return Create (Participant        => Participant,
                     Request_Topic_Name => Request_Topic_Name,
                     Reply_Topic_Name   => Reply_Topic_Name,
                     Datawriter_Qos     => Datawriter_Qos,
                     Datareader_Qos     => Datareader_Qos,
                     Topic_Qos          => Topic_Qos,
                     Publisher          => Publisher,
                     Subscriber         => Subscriber,
                     A_Listner          => A_Listner,
                     Mask               => Mask);
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Participant      : DDS.DomainParticipant.Ref_Access;
      Service_Name     : DDS.String;
      Datawriter_Qos   : DDS.DataWriterQos := DDS.Publisher.DATAWRITER_QOS_DEFAULT;
      Datareader_Qos   : DDS.DataReaderQos := DDS.Subscriber.DATAREADER_QOS_DEFAULT;
      Topic_Qos        : DDS.TopicQos := DDS.DomainParticipant.TOPIC_QOS_DEFAULT;
      Publisher        : DDS.Publisher.Ref_Access := null;
      Subscriber       : DDS.Subscriber.Ref_Access := null;
      A_Listner        : Request_Listeners.Ref_Access := null;
      Mask             : DDS.StatusKind := DDS.STATUS_MASK_NONE) return not null Ref_Access
   is
      Request_Topic_Name : DDS.String := DDS.Request_Reply.Impl.Create_Request_Topic_Name_From_Service_Name (Service_Name);
      Reply_Topic_Name   : DDS.String := DDS.Request_Reply.Impl.Create_Reply_Topic_Name_From_Service_Name (Service_Name);
      Ret : Ref_Access;
   begin
      Ret := Create (Participant          => Participant,
                     Request_Topic_Name   => Request_Topic_Name,
                     Reply_Topic_Name     => Reply_Topic_Name ,
                     Datawriter_Qos       => Datawriter_Qos,
                     Datareader_Qos       => Datareader_Qos,
                     Topic_Qos            => Topic_Qos,
                     Publisher            => Publisher,
                     Subscriber           => Subscriber,
                     A_Listner            => A_Listner,
                     Mask                 => Mask);
      Finalize (Request_Topic_Name);
      Finalize (Reply_Topic_Name);
      return Ret;
   end Create;

   ------------
   -- Create --
   ------------
   procedure Free is new Ada.Unchecked_Deallocation (Ref'Class, Ref_Access);
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
      Mask               : DDS.StatusKind := DDS.STATUS_MASK_NONE) return not null Ref_Access
   is
      Ret : Ref_Access := new Ref;
   begin
      Ret.Mask := Mask;
      Ret.Participant := Participant;
      RET.Listner := A_Listner;
      Ret.Validate (Publisher, Subscriber);
      Ret.Request_Topic := Ret.Create_Request_Topic (Request_Topic_Name, Request_DataWriter.Treats.Get_Type_Name, Topic_Qos);
      Ret.Reply_Topic := Ret.Create_Reply_Topic (Reply_Topic_Name, Reply_DataReader.Treats.Get_Type_Name, Topic_Qos);
      Ret.Subscriber := (if Subscriber = null then Participant.Get_Implicit_Subscriber else Subscriber);
      Ret.Publisher := (if Publisher = null then Participant.Get_Implicit_Publisher else Publisher);
      Ret.Writer := DDS.DataWriter_Impl.Ref_Access (Ret.Publisher.Create_DataWriter (Ret.Request_Topic, Datawriter_Qos, Ret.Writer_Listner'Access, DDS.STATUS_MASK_ALL));
      Ret.Reader :=  DDS.DataReader_Impl.Ref_Access (Ret.Subscriber.Create_DataReader (Ret.Reply_Topic.As_TopicDescription, Datareader_Qos, Ret.Reader_Listner'Access, DDS.STATUS_MASK_ALL));
      return Ret;
   exception
      when others =>
         Free (Ret);
         raise;
   end Create;

   ------------
   -- Delete --
   ------------

   procedure Delete (Self : in out Ref_Access) is
   begin
      Free (Self);
   end Delete;

   ------------------
   -- Send_Request --
   ------------------


   procedure Send_Request
     (Self : not null access Ref;
      Data : Request_DataWriter.Treats.Data_Type)
   is
      wp : DDS.WriteParams_t;
   begin
      Self.Send_Request(data, wp);

   end Send_Request;

   ------------------
   -- Send_Request --
   ------------------

   function Send_Request
     (Self : not null access Ref;
      Data : Request_DataWriter.Treats.Data_Type)
      return Reply_DataReader.Treats.Data_Type
   is
   begin
      for Replies of Reply_DataReader.Container'Class'(Self.Send_Request
                                                       (Data,
                                                          Min_Reply_Count => 1,
                                                          Max_Reply_Count => 1)) loop
         if Replies.Sample_Info.Valid_Data then
            return Ret : Reply_DataReader.Treats.Data_Type do
               Reply_DataReader.Treats.Copy (Ret, Replies.Data.all);
            end return;
         end if;
      end loop;
      return raise Constraint_Error with "No data";
   end Send_Request;

   ------------------
   -- Send_Request --
   ------------------

   function Send_Request
     (Self    : not null access Ref;
      Request : Request_DataWriter.Treats.Data_Type)
      return Reply_DataReader.Container'Class
   is
   begin
      return Self.Send_Request (Request, 1, 1);
   end Send_Request;

   ------------------
   -- Send_Request --
   ------------------

   function Send_Request
     (Self            : not null access Ref;
      Request         : Request_DataWriter.Treats.Data_Type;
      Min_Reply_Count : DDS.Natural;
      Max_Reply_Count : DDS.Long;
      Timeout         : DDS.Duration_T := DURATION_INFINITE)
      return Reply_DataReader.Container'Class
   is
   begin
      return Raise Program_Error with "Send_Request Not implemented";
   end Send_Request;

   ------------------
   -- Send_Request --
   ------------------

   procedure Send_Request
     (Self         : not null access Ref;
      Request      : Request_DataWriter.Treats.Data_Type;
      Request_Info : DDS.WriteParams_T)
   is
      Temp_Request_Info : DDS.WriteParams_T := Request_Info;
   begin
      Temp_Request_Info.Identity  := DDS.AUTO_SAMPLE_IDENTITY;
      Temp_Request_Info.Replace_Auto:= True;
      Temp_Request_Info.Flush_On_Write := True;

      Self.Writer.Write_W_Params (Request'Address, Temp_Request_Info, Self.Writer.Get_Metp);
   end Send_Request;


   -------------------
   -- Receive_Reply --
   -------------------

   function Receive_Reply
     (Self     :         not null access Ref;
      Replies  : aliased Reply_DataReader.Treats.Data_Type;
      Info_Seq :         not null access DDS.SampleInfo_Seq.Sequence;
      Timeout  :         DDS.Duration_T) return DDS.ReturnCode_T
   is
   begin
      return raise Program_Error with "Unimplemented function Receive_Reply";
   end Receive_Reply;

   -------------------
   -- Receive_Reply --
   -------------------

   function Receive_Reply
     (Self    : not null access Ref;
      Timeout : DDS.Duration_T)
      return Reply_DataReader.Container'Class
   is
   begin
      return raise Program_Error with "Unimplemented function Receive_Reply";
   end Receive_Reply;

   ---------------------
   -- Receive_Replies --
   ---------------------

   function Receive_Replies
     (Self            : not null access Ref;
      Replies         : not null Reply_DataReader.Treats.Data_Sequences.Sequence_Access;
      Sample_Info     : not null access DDS.SampleInfo_Seq.Sequence;
      Min_Reply_Count : DDS.Natural;
      Max_Reply_Count : DDS.Long;
      Timeout         : DDS.Duration_T) return DDS.ReturnCode_T
   is
   begin
      Self.Reader.Wait (DATA_AVAILABLE_STATUS, Timeout);

      return Reply_DataReader.Ref_Access (Self.Reader).Take (Received_Data => Replies,
                                                             Info_Seq      => Sample_Info,
                                                             Max_Samples   => Max_Reply_Count);

   end Receive_Replies;

   ---------------------
   -- Receive_Replies --
   ---------------------

   procedure Receive_Replies
     (Self            : not null access Ref;
      Replies         : not null Reply_DataReader.Treats.Data_Sequences.Sequence_Access;
      Sample_Info     : not null access DDS.SampleInfo_Seq.Sequence;
      Min_Reply_Count : DDS.Natural;
      Max_Reply_Count : DDS.Long;
      Timeout         : DDS.Duration_T)
   is
   begin
      Ret_Code_To_Exception (Self.Receive_Replies (Replies, Sample_Info, Min_Reply_Count, Max_Reply_Count, Timeout));
   end Receive_Replies;

   ---------------------
   -- Receive_Replies --
   ---------------------

   function Receive_Replies
     (Self            : not null access Ref;
      Min_Reply_Count : DDS.Natural;
      Max_Reply_Count : DDS.Long;
      Timeout         : DDS.Duration_T)
      return Reply_DataReader.Container'Class
   is
      Reader : constant Reply_DataReader.Ref_Access := Reply_DataReader.Ref_Access (Self.Reader);
   begin
      Self.Reader.Wait (DATA_READER_CACHE_STATUS, Timeout);
      return Reader.Take (Max_Samples => Max_Reply_Count);
   end Receive_Replies;




   ----------------
   -- Take_Reply --
   ----------------

   function Take_Reply
     (Self        :         not null access Ref;
      Replies     : aliased Reply_DataReader.Treats.Data_Type;
      Sample_Info :         not null access DDS.SampleInfo_Seq.Sequence;
      Timeout     :         DDS.Duration_T) return DDS.ReturnCode_T
   is
   begin
      return raise Program_Error with "Unimplemented function Take_Reply";
   end Take_Reply;

   ------------------
   -- Take_Replies --
   ------------------

   function Take_Replies
     (Self            : not null access Ref;
      Replies         : not null Reply_DataReader.Treats.Data_Sequences.Sequence_Access;
      Sample_Info     : not null access DDS.SampleInfo_Seq.Sequence;
      Min_Reply_Count : DDS.Natural; Max_Reply_Count : DDS.Long;
      Timeout         : DDS.Duration_T) return DDS.ReturnCode_T
   is
   begin
      return raise Program_Error with "Unimplemented function Take_Replies";
   end Take_Replies;

   ------------------
   -- Take_Replies --
   ------------------

   function Take_Replies
     (Self            : not null access Ref; Min_Reply_Count : DDS.Natural;
      Max_Reply_Count : DDS.Long; Timeout : DDS.Duration_T)
      return Reply_DataReader.Container'Class
   is
   begin
      return raise Program_Error with "Unimplemented function Take_Replies";
   end Take_Replies;

   ------------------------------------
   -- Take_Reply_For_Related_Request --
   ------------------------------------

   function Take_Reply_For_Related_Request
     (Self                 :         not null access Ref;
      Replies              : aliased Reply_DataReader.Treats.Data_Type;
      Sample_Info          : not null access DDS.SampleInfo_Seq.Sequence;
      Related_Request_Info :         not null access DDS.SampleIdentity_T)
      return DDS.ReturnCode_T
   is
   begin
      return raise Program_Error with "Unimplemented function Take_Reply_For_Related_Request";
   end Take_Reply_For_Related_Request;

   --------------------------------------
   -- Take_Replies_For_Related_Request --
   --------------------------------------

   function Take_Replies_For_Related_Request
     (Self                 : not null access Ref;
      Replies              : not null Reply_DataReader.Treats.Data_Sequences
      .Sequence_Access;
      Sample_Info          : not null access DDS.SampleInfo_Seq.Sequence;
      Related_Request_Info : not null access DDS.SampleIdentity_T)
      return DDS.ReturnCode_T
   is
   begin
      return raise Program_Error with "Unimplemented function Take_Replies_For_Related_Request";
   end Take_Replies_For_Related_Request;

   --------------------------------------
   -- Take_Replies_For_Related_Request --
   --------------------------------------

   function Take_Replies_For_Related_Request
     (Self                 : not null access Ref;
      Related_Request_Info : not null access DDS.SampleIdentity_T)
      return Reply_DataReader.Container'Class
   is
   begin
      return raise Program_Error with "Unimplemented function Take_Replies_For_Related_Request";
   end Take_Replies_For_Related_Request;

   ----------------
   -- Read_Reply --
   ----------------

   function Read_Reply
     (Self        :         not null access Ref;
      Replies     : aliased Reply_DataReader.Treats.Data_Type;
      Sample_Info :         not null access DDS.SampleInfo_Seq.Sequence;
      Timeout     :         DDS.Duration_T) return DDS.ReturnCode_T
   is
   begin
      return raise Program_Error with "Unimplemented function Read_Reply";
   end Read_Reply;

   ------------------
   -- Read_Replies --
   ------------------

   function Read_Replies
     (Self            : not null access Ref;
      Replies         : not null Reply_DataReader.Treats.Data_Sequences
      .Sequence_Access;
      Sample_Info     : not null access DDS.SampleInfo_Seq.Sequence;
      Min_Reply_Count : DDS.Natural; Max_Reply_Count : DDS.Long;
      Timeout         : DDS.Duration_T) return DDS.ReturnCode_T
   is
   begin
      return raise Program_Error with "Unimplemented function Read_Replies";
   end Read_Replies;

   ------------------
   -- Read_Replies --
   ------------------

   function Read_Replies
     (Self            : not null access Ref; Min_Reply_Count : DDS.Natural;
      Max_Reply_Count : DDS.Long; Timeout : DDS.Duration_T)
      return Reply_DataReader.Container'Class
   is
   begin
      return raise Program_Error with "Unimplemented function Read_Replies";
   end Read_Replies;

   ------------------------------------
   -- Read_Reply_For_Related_Request --
   ------------------------------------

   function Read_Reply_For_Related_Request
     (Self                 : not null access Ref;
      Replies              : aliased Reply_DataReader.Treats.Data_Type;
      Sample_Info          : not null access DDS.SampleInfo_Seq.Sequence;
      Related_Request_Info : DDS.SampleIdentity_T) return DDS.ReturnCode_T
   is
   begin
      return raise Program_Error with "Unimplemented function Read_Reply_For_Related_Request";
   end Read_Reply_For_Related_Request;

   --------------------------------------
   -- Read_Replies_For_Related_Request --
   --------------------------------------

   function Read_Replies_For_Related_Request
     (Self                 : not null access Ref;
      Replies              : not null Reply_DataReader.Treats.Data_Sequences.Sequence_Access;
      Sample_Info          : not null access DDS.SampleInfo_Seq.Sequence;
      Related_Request_Info : DDS.SampleIdentity_T) return DDS.ReturnCode_T
   is
   begin
      return raise Program_Error with "Unimplemented function Read_Replies_For_Related_Request";
   end Read_Replies_For_Related_Request;

   --------------------------------------
   -- Read_Replies_For_Related_Request --
   --------------------------------------

   function Read_Replies_For_Related_Request
     (Self                 : not null access Ref;
      Related_Request_Info : DDS.SampleIdentity_T)
      return Reply_DataReader.Container'Class
   is
   begin
      return raise Program_Error with "Unimplemented function Read_Replies_For_Related_Request";
   end Read_Replies_For_Related_Request;

   -----------------
   -- Return_Loan --
   -----------------

   procedure Return_Loan
     (Self        : not null access Ref;
      Replies     : not null Reply_DataReader.Treats.Data_Sequences.Sequence_Access;
      Sample_Info : DDS.SampleInfo_Seq.Sequence_Access)
   is
   begin
      raise Program_Error with "Unimplemented procedure Return_Loan";
   end Return_Loan;

   -----------------
   -- Return_Loan --
   -----------------

   procedure Return_Loan
     (Self        : not null access Ref;
      Replies     : in out Reply_DataReader.Treats.Data_Sequences.Sequence;
      Sample_Info : in out DDS.SampleInfo_Seq.Sequence)
   is
   begin
      Reply_DataReader.Ref_Access(self.Reader).Return_Loan(Replies,Sample_Info);
   end Return_Loan;

   ------------
   -- Delete --
   ------------

   procedure Delete (This : in out Ref) is
   begin
      raise Program_Error with "Unimplemented procedure Delete";
   end Delete;

   ----------------------
   -- Wait_For_Replies --
   ----------------------

   procedure Wait_For_Replies
     (This : in out Ref; Min_Count : Dds.Long; Max_Wait : DDS.Duration_T)
   is
   begin
      raise Program_Error with "Unimplemented procedure Wait_For_Replies";
   end Wait_For_Replies;

   -----------------------------------------
   -- Wait_For_Replies_For_Related_Reques --
   -----------------------------------------

   procedure Wait_For_Replies_For_Related_Reques
     (This               : in out Ref;
      Min_Count          : Dds.Long;
      Max_Wait           : DDS.Duration_T;
      Related_Request_Id : DDS.SampleIdentity_T)
   is
   begin
      raise Program_Error with "Unimplemented procedure Wait_For_Replies_For_Related_Reques";
   end Wait_For_Replies_For_Related_Reques;

   ----------------------------
   -- Get_Request_DataWriter --
   ----------------------------




   package body Listners_Impl is separate;

end DDS.Request_Reply.Requester.Typed_Requester_Generic;
