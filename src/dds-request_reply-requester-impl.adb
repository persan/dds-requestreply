with DDS.WaitSet;
pragma Warnings (Off);
--  ----------------------------------------------------------------------------
--  Note this is an implementation package and is subject to change att any time.
--  ----------------------------------------------------------------------------
package body DDS.Request_Reply.Requester.Impl is
   use DDS.ContentFilteredTopic;
   use DDS.ReadCondition;
   use DDS.Topic;

   -------------------
   -- Touch_Samples --
   ----------------------------
   -- create_correlation_cft --
   ----------------------------
   --@RequesterUntypedImpl.c:47
   function Create_Correlation_Cft
     (Participant      : not null DDS.DomainParticipant.Ref_Access;
      Topic            : not null DDS.Topic.Ref_Access;
      Correlation_Guid : DDS.Guid_T)
      return DDS.ContentFilteredTopic.Ref_Access
   is

   begin
      return null;
   end Create_Correlation_Cft;

   -------------------------
   -- create_reader_topic ---*
   -------------------------
   --@RequesterUntypedImpl.c:131 RTI_Connext_RequesterUntypedImpl_create_reader_topic
   function Create_Reader_Topic
     (Self            : not null access Ref;
      Params          : not null DDS.EntityParams.Ref_Access;
      Reply_Type_Name : String)
      return DDS.TopicDescription.Ref_Access
   is
      Topic            : DDS.Topic.Ref_Access;
      CurrentWriterQos : DDS.DataWriterQos;
      TopicDesc        : DDS.TopicDescription.Ref_Access;
      Reply_Topic_Name : DDS.String;
   begin
      Topic := Self.Participant.Get_Or_Create_Topic (Topic_Name => Reply_Topic_Name,
                                                     Type_Name  =>  Reply_Type_Name);

      raise DDS.ERROR with "Unable to create " & To_Standard_String (Reply_Topic_Name) when Topic = null;
      Self.Writer.Get_Qos (CurrentWriterQos);

      return
      raise Program_Error with "Unimplemented function create_reader_topic";
   end Create_Reader_Topic;

   -------------------------
   -- create_writer_topic --
   -------------------------
   --@RequesterUntypedImpl.c:179
   function Create_Writer_Topic
     (Self              : not null access Ref;
      Params            : not null DDS.EntityParams.Ref_Access;
      Request_Type_Name : String) return DDS.TopicDescription.Ref_Access
   is
   begin
      return
      raise Program_Error with "Unimplemented function create_writer_topic";
   end Create_Writer_Topic;

   ------------
   -- create --
   ------------
   --@RequesterUntypedImpl.c:179
   function Create
     (Params : not null DDS.EntityParams.Ref_Access; Reply_Size : Integer)
      return Ref_Access
   is
   begin
      return raise Program_Error with "Unimplemented function create";
   end Create;

   --@RequesterUntypedImpl.c:342
   function Create_Query_Expression_For_Correlation_Sequence_Number
     (Sequence_Number : DDS.SequenceNumber_T) return Standard.String is
   begin
      return raise Program_Error;
   end;

   --@ .c:361: RTI_Connext_RequesterUntypedImpl_create_correlation_condition
   RequestReplyIndex : constant DDS.String := To_DDS_String ("RequestReplyIndex");
   function Create_Correlation_Condition (Self            : not null access Ref;
                                          State_Kind      : DDS.SampleStateMask;
                                          Sequence_Number : DDS.SequenceNumber_T) return DDS.ReadCondition.Ref_Access
   is
      Sample_Info : DDS.SampleInfo;
   begin
      if Sequence_Number in DDS.AUTO_SEQUENCE_NUMBER | DDS.SEQUENCE_NUMBER_MAX | DDS.SEQUENCE_NUMBER_ZERO | DDS.SEQUENCE_NUMBER_UNKNOWN then
         raise DDS.ERROR with "Invalid correlation sequence number" & Sequence_Number'Img;
      end if;
      Sample_Info.Related_Original_Publication_Virtual_Sequence_Number := Sequence_Number;
      return Condition : DDS.ReadCondition.Ref_Access do

         Condition := ReadCondition.Ref_Access (Self.Reader.Create_Indexcondition
                                                (Sample_State    => State_Kind,
                                                 View_State      => DDS.ANY_VIEW_STATE,
                                                 Instance_State  => DDS.ALIVE_INSTANCE_STATE,
                                                 Index_Name      => RequestReplyIndex,
                                                 Sample_Info     => Sample_Info));

         if Condition = null  then
            raise DDS.ERROR with "Invalid correlation sequence number";
         end if;
      end return;
   end Create_Correlation_Condition;

   function Wait_For_Replies_For_Related_Request
     (Self                 : not null access Ref;
      Min_Reply_Count      : DDS.Long;
      Max_Wait             : DDS.Duration_T;
      Related_Request_Info : DDS.SampleIdentity_T) return ReturnCode_T is
      RetCode : DDS.ReturnCode_T := DDS.RETCODE_OK;
   begin
      if Related_Request_Info /= NULL_SAMPLE_IDENTITY then
         RetCode := Self.Wait_For_Replies (Max_Wait, Min_Reply_Count, Related_Request_Info);
      else
         RetCode := Ref_Access (Self).Wait_For_Any_Sample (Max_Wait, Min_Reply_Count);
      end if;
      if RetCode not in  DDS.RETCODE_OK |  DDS.RETCODE_TIMEOUT then
         raise DDS.ERROR with "wait for samples failed";
      end if;
      return RetCode;
   end;

   function Wait_For_Replies (Self            : not null access Ref;
                              Min_Reply_Count : DDS.Long;
                              Max_Wait        : DDS.Duration_T)
                              return ReturnCode_T is
   begin
      return Self.Wait_For_Replies_For_Related_Request (Min_Reply_Count, Max_Wait, DDS.NULL_SAMPLE_IDENTITY);
   end;

   -- RequesterUntypedImpl.c:394
   function Wait_For_Replies (Self                 : not null access Ref;
                              Max_Wait             : Duration_T;
                              Min_Sample_Count     : Long;
                              Related_Request_Info : DDS.SampleIdentity_T) return DDS.ReturnCode_T is

      Correlation_Condition : DDS.ReadCondition.Ref_Access;
      Initial_Condition     : DDS.ReadCondition.Ref_Access;
      Waitset               : aliased DDS.WaitSet.Ref;
      Retcode               : DDS.ReturnCode_T := DDS.RETCODE_OK;
      procedure Finalize_Self is
      begin
         if Correlation_Condition /= null  then
            Self.Reader.Delete_Readcondition (Correlation_Condition);
         end if;

         if Initial_Condition /= null then
            Self.Reader.Delete_Readcondition (Initial_Condition);
         end if;

         --  if waitset /= NULL) then
         --     self.waitset_pool.returnBuffer (waitset);
         --  end if;

      end Finalize_Self;
   begin
      Correlation_Condition := Self.Create_Correlation_Condition (DDS.NOT_READ_SAMPLE_STATE, Related_Request_Info.Sequence_Number);
      if Correlation_Condition = null then
         raise DDS.ERROR with "Error creating correlation condition";
      end if;


      --
      Initial_Condition := Self.Create_Correlation_Condition (DDS.ANY_SAMPLE_STATE, Related_Request_Info.Sequence_Number);
      if Initial_Condition = null then
         raise DDS.ERROR with "Error creating correlation condition";
      end if;

      Waitset.Attach_Condition (Correlation_Condition);
      Self.Wait_For_Samples (Max_Wait, Min_Sample_Count, Waitset'Unrestricted_Access, Initial_Condition, Correlation_Condition);
      Finalize_Self;
      return Retcode;
   exception
      when others =>
         Finalize_Self;
         raise;
   end Wait_For_Replies;

   ----------------------
   -- get_reply_loaned --
   ----------------------
   --@RequesterUntypedImpl.c:472
   function Get_Reply_Loaned
     (Self                    : not null access Ref; Received_Data : System.Address;
      Data_Count              : out DDS.Integer;
      Is_Loan                 : out Boolean;
      DataSeqContiguousBuffer : System.Address;
      Info_Seq                : in out DDS.SampleInfo_Seq.Sequence;
      Data_Seq_Len            : DDS.Long;
      Data_Seq_Max_Len        : DDS.Long;
      Ownership               : Boolean;
      Max_Samples             : DDS.Long;
      Related_Request_Id      : DDS.SampleIdentity_T;
      Take                    : Boolean)
      return DDS.ReturnCode_T
   is
   begin
      return raise Program_Error with "Unimplemented function get_reply_loaned";
   end Get_Reply_Loaned;


end DDS.Request_Reply.Requester.Impl;
