pragma Ada_2012;
package body DDS.Typed_Replyer_Generic is

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

   ------------
   -- Delete --
   ------------

   procedure Delete (Self : in out Ref_Access) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Delete unimplemented");
      raise Program_Error with "Unimplemented procedure Delete";
   end Delete;

   ----------------
   -- Send_Reply --
   ----------------

   function Send_Reply
     (Self : not null access Ref; Data : Request_DataReaders.Treats.Data_Type)
      return DDS.ReturnCode_T
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Send_Reply unimplemented");
      return raise Program_Error with "Unimplemented function Send_Reply";
   end Send_Reply;

   ----------------
   -- Send_Reply --
   ----------------

   procedure Send_Reply
     (Self : not null access Ref; Data : Request_DataReaders.Treats.Data_Type)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Send_Reply unimplemented");
      raise Program_Error with "Unimplemented procedure Send_Reply";
   end Send_Reply;

   ----------------
   -- Send_Reply --
   ----------------

   procedure Send_Reply
     (Self    : not null access Ref;
      Request : access Request_DataReaders.Treats.Data_Type)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Send_Reply unimplemented");
      raise Program_Error with "Unimplemented procedure Send_Reply";
   end Send_Reply;

   ---------------------
   -- Receive_Request --
   ---------------------

   function Receive_Request
     (Self     :         not null access Ref;
      Replies  : aliased Request_DataReaders.Treats.Data_Type;
      Info_Seq :         not null access DDS.SampleInfo_Seq.Sequence;
      Timeout  :         DDS.Duration_T) return DDS.ReturnCode_T
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Receive_Request unimplemented");
      return raise Program_Error with "Unimplemented function Receive_Request";
   end Receive_Request;

   ---------------------
   -- Receive_Request --
   ---------------------

   function Receive_Request
     (Self : not null access Ref; Timeout : DDS.Duration_T)
      return Request_DataReaders.Container
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Receive_Request unimplemented");
      return raise Program_Error with "Unimplemented function Receive_Request";
   end Receive_Request;

   ---------------------
   -- Receive_Request --
   ---------------------

   function Receive_Request
     (Self    : not null access Ref;
      Replies : not null Request_DataReaders.Treats.Data_Sequences
        .Sequence_Access;
      Sample_Info     : not null access DDS.SampleInfo_Seq.Sequence;
      Min_Reply_Count : DDS.Natural; Max_Reply_Count : DDS.Long;
      Timeout         : DDS.Duration_T) return DDS.ReturnCode_T
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Receive_Request unimplemented");
      return raise Program_Error with "Unimplemented function Receive_Request";
   end Receive_Request;

   ---------------------
   -- Receive_Request --
   ---------------------

   procedure Receive_Request
     (Self    : not null access Ref;
      Replies : not null Request_DataReaders.Treats.Data_Sequences
        .Sequence_Access;
      Sample_Info     : not null access DDS.SampleInfo_Seq.Sequence;
      Min_Reply_Count : DDS.Natural; Max_Reply_Count : DDS.Long;
      Timeout         : DDS.Duration_T)
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Receive_Request unimplemented");
      raise Program_Error with "Unimplemented procedure Receive_Request";
   end Receive_Request;

   ---------------------
   -- Receive_Request --
   ---------------------

   function Receive_Request
     (Self            : not null access Ref; Min_Reply_Count : DDS.Natural;
      Max_Reply_Count : DDS.Long; Timeout : DDS.Duration_T)
      return Request_DataReaders.Container
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Receive_Request unimplemented");
      return raise Program_Error with "Unimplemented function Receive_Request";
   end Receive_Request;

   ---------------------
   -- Receive_Request --
   ---------------------

   function Receive_Request
     (Self    : not null access Ref;
      Replies : not null Request_DataReaders.Treats.Data_Sequences
        .Sequence_Access;
      Sample_Info     : not null access DDS.SampleInfo_Seq.Sequence;
      Min_Reply_Count : DDS.Natural; Max_Reply_Count : DDS.Long;
      Timeout         : Duration) return DDS.ReturnCode_T
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Receive_Request unimplemented");
      return raise Program_Error with "Unimplemented function Receive_Request";
   end Receive_Request;

   ---------------------
   -- Receive_Request --
   ---------------------

   function Receive_Request
     (Self            : not null access Ref; Min_Reply_Count : DDS.Natural;
      Max_Reply_Count : DDS.Long; Timeout : Duration)
      return Request_DataReaders.Container
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Receive_Request unimplemented");
      return raise Program_Error with "Unimplemented function Receive_Request";
   end Receive_Request;

   ------------------
   -- Take_Request --
   ------------------

   function Take_Request
     (Self        :         not null access Ref;
      Replies     : aliased Request_DataReaders.Treats.Data_Type;
      Sample_Info :         not null access DDS.SampleInfo_Seq.Sequence;
      Timeout     :         DDS.Duration_T) return DDS.ReturnCode_T
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Take_Request unimplemented");
      return raise Program_Error with "Unimplemented function Take_Request";
   end Take_Request;

   ------------------
   -- Take_Request --
   ------------------

   function Take_Request
     (Self    : not null access Ref;
      Replies : not null Request_DataReaders.Treats.Data_Sequences
        .Sequence_Access;
      Sample_Info     : not null access DDS.SampleInfo_Seq.Sequence;
      Min_Reply_Count : DDS.Natural; Max_Reply_Count : DDS.Long;
      Timeout         : DDS.Duration_T) return DDS.ReturnCode_T
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Take_Request unimplemented");
      return raise Program_Error with "Unimplemented function Take_Request";
   end Take_Request;

   ------------------
   -- Take_Request --
   ------------------

   function Take_Request
     (Self            : not null access Ref; Min_Reply_Count : DDS.Natural;
      Max_Reply_Count : DDS.Long; Timeout : DDS.Duration_T)
      return Request_DataReaders.Container
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Take_Request unimplemented");
      return raise Program_Error with "Unimplemented function Take_Request";
   end Take_Request;

   ------------------
   -- Read_Request --
   ------------------

   function Read_Request
     (Self        :         not null access Ref;
      Replies     : aliased Request_DataReaders.Treats.Data_Type;
      Sample_Info :         not null access DDS.SampleInfo_Seq.Sequence;
      Timeout     :         DDS.Duration_T) return DDS.ReturnCode_T
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Read_Request unimplemented");
      return raise Program_Error with "Unimplemented function Read_Request";
   end Read_Request;

   ------------------
   -- Read_Request --
   ------------------

   function Read_Request
     (Self    : not null access Ref;
      Replies : not null Request_DataReaders.Treats.Data_Sequences
        .Sequence_Access;
      Sample_Info     : not null access DDS.SampleInfo_Seq.Sequence;
      Min_Reply_Count : DDS.Natural; Max_Reply_Count : DDS.Long;
      Timeout         : DDS.Duration_T) return DDS.ReturnCode_T
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Read_Request unimplemented");
      return raise Program_Error with "Unimplemented function Read_Request";
   end Read_Request;

   ------------------
   -- Read_Request --
   ------------------

   function Read_Request
     (Self            : not null access Ref; Min_Reply_Count : DDS.Natural;
      Max_Reply_Count : DDS.Long; Timeout : DDS.Duration_T)
      return Request_DataReaders.Container'Class
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Read_Request unimplemented");
      return raise Program_Error with "Unimplemented function Read_Request";
   end Read_Request;

   -----------------
   -- Return_Loan --
   -----------------

   procedure Return_Loan
     (Self    : not null access Ref;
      Replies : not null Request_DataReaders.Treats.Data_Sequences
        .Sequence_Access;
      Sample_Info : DDS.SampleInfo_Seq.Sequence_Access)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Return_Loan unimplemented");
      raise Program_Error with "Unimplemented procedure Return_Loan";
   end Return_Loan;

   -----------------
   -- Return_Loan --
   -----------------

   procedure Return_Loan
     (Self        : not null access Ref;
      Replies     : Request_DataReaders.Treats.Data_Sequences.Sequence;
      Sample_Info : DDS.SampleInfo_Seq.Sequence)
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

   ----------------------------
   -- Get_Request_DataReader --
   ----------------------------

   function Get_Request_DataReader
     (Self : not null access Ref) return Request_DataReaders.Ref_Access
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Get_Request_DataReader unimplemented");
      return raise Program_Error
          with "Unimplemented function Get_Request_DataReader";
   end Get_Request_DataReader;

   --------------------------
   -- Get_Reply_Datawriter --
   --------------------------

   function Get_Reply_Datawriter
     (Self : not null access Ref) return Reply_DataWriters.Ref_Access
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
         "Get_Reply_Datawriter unimplemented");
      return raise Program_Error
          with "Unimplemented function Get_Reply_Datawriter";
   end Get_Reply_Datawriter;

end DDS.Typed_Replyer_Generic;
