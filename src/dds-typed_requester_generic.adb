pragma Ada_2012;
package body DDS.Typed_Requester_Generic is

   -----------------------------
   -- Get_Request_Data_Writer --
   -----------------------------

   function Get_Request_Data_Writer
     (Self : not null access Ref) return DDS.DataWriter.Ref_Access
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Get_Request_Data_Writer unimplemented");
      return raise Program_Error
          with "Unimplemented function Get_Request_Data_Writer";
   end Get_Request_Data_Writer;

   ---------------------------
   -- Get_Reply_Data_Reader --
   ---------------------------

   function Get_Reply_Data_Reader
     (Self : not null access Ref) return DDS.DataReader.Ref_Access
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Get_Reply_Data_Reader unimplemented");
      return raise Program_Error
          with "Unimplemented function Get_Reply_Data_Reader";
   end Get_Reply_Data_Reader;

   ------------
   -- Create --
   ------------

   function Create
     (Participant      : DDS.DomainParticipant.Ref_Access;
      Service_Name     : DDS.String; Qos_Library_Name : DDS.String;
      Qos_Profile_Name : DDS.String;
      Publisher        : DDS.Publisher.Ref_Access     := null;
      Subscriber       : DDS.Subscriber.Ref_Access    := null;
      A_Listner        : Request_Listeners.Ref_Access := null;
      Mask : DDS.StatusMask := DDS.STATUS_MASK_NONE) return Ref_Access
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Create unimplemented");
      return raise Program_Error with "Unimplemented function Create";
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Participant        : DDS.DomainParticipant.Ref_Access;
      Request_Topic_Name : DDS.String; Reply_Topic_Name : DDS.String;
      Qos_Library_Name   : DDS.String; Qos_Profile_Name : DDS.String;
      Publisher          : DDS.Publisher.Ref_Access     := null;
      Subscriber         : DDS.Subscriber.Ref_Access    := null;
      A_Listner          : Request_Listeners.Ref_Access := null;
      Mask : DDS.StatusMask := DDS.STATUS_MASK_NONE) return Ref_Access
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Create unimplemented");
      return raise Program_Error with "Unimplemented function Create";
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Participant    : DDS.DomainParticipant.Ref_Access;
      Service_Name   : DDS.String; Datawriter_Qos : DDS.DataWriterQos;
      Datareader_Qos : DDS.DataReaderQos;
      Publisher      : DDS.Publisher.Ref_Access     := null;
      Subscriber     : DDS.Subscriber.Ref_Access    := null;
      A_Listner      : Request_Listeners.Ref_Access := null;
      Mask : DDS.StatusMask := DDS.STATUS_MASK_NONE) return Ref_Access
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Create unimplemented");
      return raise Program_Error with "Unimplemented function Create";
   end Create;

   ------------
   -- Create --
   ------------

   function Create
     (Participant        : DDS.DomainParticipant.Ref_Access;
      Request_Topic_Name : DDS.String; Reply_Topic_Name : DDS.String;
      Datawriter_Qos : DDS.DataWriterQos; Datareader_Qos : DDS.DataReaderQos;
      Publisher          : DDS.Publisher.Ref_Access     := null;
      Subscriber         : DDS.Subscriber.Ref_Access    := null;
      A_Listner          : Request_Listeners.Ref_Access := null;
      Mask : DDS.StatusMask := DDS.STATUS_MASK_NONE) return Ref_Access
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Create unimplemented");
      return raise Program_Error with "Unimplemented function Create";
   end Create;

   ------------------
   -- Send_Request --
   ------------------

   function Send_Request
     (Self : not null access Ref; Data : Request_DataWriters.Treats.Data_Type)
      return DDS.ReturnCode_T
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Send_Request unimplemented");
      return raise Program_Error with "Unimplemented function Send_Request";
   end Send_Request;

   ------------------
   -- Send_Request --
   ------------------

   procedure Send_Request
     (Self : not null access Ref; Data : Request_DataWriters.Treats.Data_Type)
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Send_Request unimplemented");
      raise Program_Error with "Unimplemented procedure Send_Request";
   end Send_Request;

   ------------------
   -- Send_Request --
   ------------------

   function Send_Request
     (Self : not null access Ref; Data : Request_DataWriters.Treats.Data_Type)
      return Reply_DataReaders.Treats.Data_Type
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Send_Request unimplemented");
      return raise Program_Error with "Unimplemented function Send_Request";
   end Send_Request;

   ------------------
   -- Send_Request --
   ------------------

   procedure Send_Request
     (Self    : not null access Ref;
      Request : access Request_DataWriters.Treats.Data_Type)
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Send_Request unimplemented");
      raise Program_Error with "Unimplemented procedure Send_Request";
   end Send_Request;

   ------------------
   -- Send_Request --
   ------------------

   function Send_Request
     (Self    : not null access Ref;
      Request : access Request_DataWriters.Treats.Data_Type)
      return Reply_DataReaders.Container
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Send_Request unimplemented");
      return raise Program_Error with "Unimplemented function Send_Request";
   end Send_Request;

   ------------------
   -- Send_Request --
   ------------------

   function Send_Request
     (Self    : not null access Ref;
      Request : Request_DataWriters.Treats.Data_Type) return Reply_DataReaders
     .Container
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Send_Request unimplemented");
      return raise Program_Error with "Unimplemented function Send_Request";
   end Send_Request;

   ------------------
   -- Send_Request --
   ------------------

   function Send_Request
     (Self            : not null access Ref;
      Request         : access Request_DataWriters.Treats.Data_Type;
      Min_Reply_Count : DDS.Natural; Max_Reply_Count : DDS.Long;
      Timeout : DDS.Duration_T := DURATION_INFINITE) return Reply_DataReaders
     .Container
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Send_Request unimplemented");
      return raise Program_Error with "Unimplemented function Send_Request";
   end Send_Request;

   ------------------
   -- Send_Request --
   ------------------

   procedure Send_Request
     (Self         : not null access Ref;
      Request      : access Request_DataWriters.Treats.Data_Type;
      Request_Info : DDS.WriteParams_T)
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Send_Request unimplemented");
      raise Program_Error with "Unimplemented procedure Send_Request";
   end Send_Request;

   -------------------
   -- Receive_Reply --
   -------------------

   function Receive_Reply
     (Self     :         not null access Ref;
      Replies  : aliased Reply_DataReaders.Treats.Data_Type;
      Info_Seq :         not null access DDS.SampleInfo_Seq.Sequence;
      Timeout  :         DDS.Duration_T) return DDS.ReturnCode_T
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Receive_Reply unimplemented");
      return raise Program_Error with "Unimplemented function Receive_Reply";
   end Receive_Reply;

   -------------------
   -- Receive_Reply --
   -------------------

   function Receive_Reply
     (Self : not null access Ref; Timeout : DDS.Duration_T)
      return Reply_DataReaders.Container
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Receive_Reply unimplemented");
      return raise Program_Error with "Unimplemented function Receive_Reply";
   end Receive_Reply;

   ---------------------
   -- Receive_Replies --
   ---------------------

   function Receive_Replies
     (Self    : not null access Ref;
      Replies : not null Reply_DataReaders.Treats.Data_Sequences
        .Sequence_Access;
      Sample_Info     : not null access DDS.SampleInfo_Seq.Sequence;
      Min_Reply_Count : DDS.Natural; Max_Reply_Count : DDS.Long;
      Timeout         : DDS.Duration_T) return DDS.ReturnCode_T
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Receive_Replies unimplemented");
      return raise Program_Error with "Unimplemented function Receive_Replies";
   end Receive_Replies;

   ---------------------
   -- Receive_Replies --
   ---------------------

   function Receive_Replies
     (Self            : not null access Ref; Min_Reply_Count : DDS.Natural;
      Max_Reply_Count : DDS.Long; Timeout : DDS.Duration_T)
      return Reply_DataReaders.Container
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Receive_Replies unimplemented");
      return raise Program_Error with "Unimplemented function Receive_Replies";
   end Receive_Replies;

   ---------------------
   -- Receive_Replies --
   ---------------------

   function Receive_Replies
     (Self    : not null access Ref;
      Replies : not null Reply_DataReaders.Treats.Data_Sequences
        .Sequence_Access;
      Sample_Info     : not null access DDS.SampleInfo_Seq.Sequence;
      Min_Reply_Count : DDS.Natural; Max_Reply_Count : DDS.Long;
      Timeout         : Duration) return DDS.ReturnCode_T
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Receive_Replies unimplemented");
      return raise Program_Error with "Unimplemented function Receive_Replies";
   end Receive_Replies;

   ---------------------
   -- Receive_Replies --
   ---------------------

   function Receive_Replies
     (Self            : not null access Ref; Min_Reply_Count : DDS.Natural;
      Max_Reply_Count : DDS.Long; Timeout : Duration) return Reply_DataReaders
     .Container
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Receive_Replies unimplemented");
      return raise Program_Error with "Unimplemented function Receive_Replies";
   end Receive_Replies;

   ----------------
   -- Take_Reply --
   ----------------

   function Take_Reply
     (Self        :         not null access Ref;
      Replies     : aliased Reply_DataReaders.Treats.Data_Type;
      Sample_Info :         not null access DDS.SampleInfo_Seq.Sequence;
      Timeout     :         DDS.Duration_T) return DDS.ReturnCode_T
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Take_Reply unimplemented");
      return raise Program_Error with "Unimplemented function Take_Reply";
   end Take_Reply;

   ------------------
   -- Take_Replies --
   ------------------

   function Take_Replies
     (Self    : not null access Ref;
      Replies : not null Reply_DataReaders.Treats.Data_Sequences
        .Sequence_Access;
      Sample_Info     : not null access DDS.SampleInfo_Seq.Sequence;
      Min_Reply_Count : DDS.Natural; Max_Reply_Count : DDS.Long;
      Timeout         : DDS.Duration_T) return DDS.ReturnCode_T
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Take_Replies unimplemented");
      return raise Program_Error with "Unimplemented function Take_Replies";
   end Take_Replies;

   ------------------
   -- Take_Replies --
   ------------------

   function Take_Replies
     (Self            : not null access Ref; Min_Reply_Count : DDS.Natural;
      Max_Reply_Count : DDS.Long; Timeout : DDS.Duration_T)
      return Reply_DataReaders.Container
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Take_Replies unimplemented");
      return raise Program_Error with "Unimplemented function Take_Replies";
   end Take_Replies;

   ------------------------------------
   -- Take_Reply_For_Related_Request --
   ------------------------------------

   function Take_Reply_For_Related_Request
     (Self                 :         not null access Ref;
      Replies              : aliased Reply_DataReaders.Treats.Data_Type;
      Sample_Info          : not null access DDS.SampleInfo_Seq.Sequence;
      Related_Request_Info : not null access DDS.SampleIdentity_T) return DDS
     .ReturnCode_T
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Take_Reply_For_Related_Request unimplemented");
      return raise Program_Error
          with "Unimplemented function Take_Reply_For_Related_Request";
   end Take_Reply_For_Related_Request;

   --------------------------------------
   -- Take_Replies_For_Related_Request --
   --------------------------------------

   function Take_Replies_For_Related_Request
     (Self    : not null access Ref;
      Replies : not null Reply_DataReaders.Treats.Data_Sequences
        .Sequence_Access;
      Sample_Info          : not null access DDS.SampleInfo_Seq.Sequence;
      Related_Request_Info : not null access DDS.SampleIdentity_T) return DDS
     .ReturnCode_T
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Take_Replies_For_Related_Request unimplemented");
      return raise Program_Error
          with "Unimplemented function Take_Replies_For_Related_Request";
   end Take_Replies_For_Related_Request;

   --------------------------------------
   -- Take_Replies_For_Related_Request --
   --------------------------------------

   function Take_Replies_For_Related_Request
     (Self                 : not null access Ref;
      Related_Request_Info : not null access DDS.SampleIdentity_T)
      return Reply_DataReaders.Container
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Take_Replies_For_Related_Request unimplemented");
      return raise Program_Error
          with "Unimplemented function Take_Replies_For_Related_Request";
   end Take_Replies_For_Related_Request;

   ----------------
   -- Read_Reply --
   ----------------

   function Read_Reply
     (Self        :         not null access Ref;
      Replies     : aliased Reply_DataReaders.Treats.Data_Type;
      Sample_Info :         not null access DDS.SampleInfo_Seq.Sequence;
      Timeout     :         DDS.Duration_T) return DDS.ReturnCode_T
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Read_Reply unimplemented");
      return raise Program_Error with "Unimplemented function Read_Reply";
   end Read_Reply;

   ------------------
   -- Read_Replies --
   ------------------

   function Read_Replies
     (Self    : not null access Ref;
      Replies : not null Reply_DataReaders.Treats.Data_Sequences
        .Sequence_Access;
      Sample_Info     : not null access DDS.SampleInfo_Seq.Sequence;
      Min_Reply_Count : DDS.Natural; Max_Reply_Count : DDS.Long;
      Timeout         : DDS.Duration_T) return DDS.ReturnCode_T
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Read_Replies unimplemented");
      return raise Program_Error with "Unimplemented function Read_Replies";
   end Read_Replies;

   ------------------
   -- Read_Replies --
   ------------------

   function Read_Replies
     (Self            : not null access Ref; Min_Reply_Count : DDS.Natural;
      Max_Reply_Count : DDS.Long; Timeout : DDS.Duration_T)
      return Reply_DataReaders.Container'Class
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Read_Replies unimplemented");
      return raise Program_Error with "Unimplemented function Read_Replies";
   end Read_Replies;

   ------------------------------------
   -- Read_Reply_For_Related_Request --
   ------------------------------------

   function Read_Reply_For_Related_Request
     (Self                 :         not null access Ref;
      Replies              : aliased Reply_DataReaders.Treats.Data_Type;
      Sample_Info          : not null access DDS.SampleInfo_Seq.Sequence;
      Related_Request_Info : DDS.SampleIdentity_T) return DDS.ReturnCode_T
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Read_Reply_For_Related_Request unimplemented");
      return raise Program_Error
          with "Unimplemented function Read_Reply_For_Related_Request";
   end Read_Reply_For_Related_Request;

   --------------------------------------
   -- Read_Replies_For_Related_Request --
   --------------------------------------

   function Read_Replies_For_Related_Request
     (Self    : not null access Ref;
      Replies : not null Reply_DataReaders.Treats.Data_Sequences
        .Sequence_Access;
      Sample_Info          : not null access DDS.SampleInfo_Seq.Sequence;
      Related_Request_Info : DDS.SampleIdentity_T) return DDS.ReturnCode_T
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Read_Replies_For_Related_Request unimplemented");
      return raise Program_Error
          with "Unimplemented function Read_Replies_For_Related_Request";
   end Read_Replies_For_Related_Request;

   --------------------------------------
   -- Read_Replies_For_Related_Request --
   --------------------------------------

   function Read_Replies_For_Related_Request
     (Self : not null access Ref; Related_Request_Info : DDS.SampleIdentity_T)
      return Reply_DataReaders.Container'Class
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Read_Replies_For_Related_Request unimplemented");
      return raise Program_Error
          with "Unimplemented function Read_Replies_For_Related_Request";
   end Read_Replies_For_Related_Request;

   -----------------
   -- Return_Loan --
   -----------------

   procedure Return_Loan
     (Self    : not null access Ref;
      Replies : not null Reply_DataReaders.Treats.Data_Sequences
        .Sequence_Access;
      Sample_Info : DDS.SampleInfo_Seq.Sequence_Access)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Return_Loan unimplemented");
      raise Program_Error with "Unimplemented procedure Return_Loan";
   end Return_Loan;

   ------------
   -- Delete --
   ------------

   procedure Delete (This : in out Ref) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Delete unimplemented");
      raise Program_Error with "Unimplemented procedure Delete";
   end Delete;

   ----------------------
   -- Wait_For_Replies --
   ----------------------

   procedure Wait_For_Replies
     (This : in out Ref; Min_Count : Dds.Long; Max_Wait : DDS.Duration_T)
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Wait_For_Replies unimplemented");
      raise Program_Error with "Unimplemented procedure Wait_For_Replies";
   end Wait_For_Replies;

   -----------------------------------------
   -- Wait_For_Replies_For_Related_Reques --
   -----------------------------------------

   procedure Wait_For_Replies_For_Related_Reques
     (This : in out Ref; Min_Count : Dds.Long; Max_Wait : DDS.Duration_T;
      Related_Request_Id :        DDS.SampleIdentity_T)
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Wait_For_Replies_For_Related_Reques unimplemented");
      raise Program_Error
        with "Unimplemented procedure Wait_For_Replies_For_Related_Reques";
   end Wait_For_Replies_For_Related_Reques;

   ----------------------------
   -- Get_Request_DataWriter --
   ----------------------------

   function Get_Request_DataWriter
     (Self : not null access Ref) return Request_DataWriters.Ref_Access
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Get_Request_DataWriter unimplemented");
      return raise Program_Error
          with "Unimplemented function Get_Request_DataWriter";
   end Get_Request_DataWriter;

   --------------------------
   -- Get_Reply_DataReader --
   --------------------------

   function Get_Reply_DataReader
     (Self : not null access Ref) return Reply_DataReaders.Ref_Access
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Get_Reply_DataReader unimplemented");
      return raise Program_Error
          with "Unimplemented function Get_Reply_DataReader";
   end Get_Reply_DataReader;
   procedure Delete (Self : in out Ref_Access) is null;

   procedure Receive_Replies
     (Self                 : not null access Ref;
      Replies              : not null Reply_DataReaders.Treats.Data_Sequences.Sequence_Access;
      Sample_Info          : not null access DDS.SampleInfo_Seq.Sequence;
      Min_Reply_Count      : DDS.Natural;
      Max_Reply_Count      : DDS.Long;
      Timeout              : DDS.Duration_T)is null;
   procedure Return_Loan (Self         : not null access Ref;
                          Replies      : Reply_DataReaders.Treats.Data_Sequences.Sequence;
                          Sample_Info  : DDS.SampleInfo_Seq.Sequence) is null;

end DDS.Typed_Requester_Generic;
