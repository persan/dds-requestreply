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


--  ----------------------------------------------------------------------------
--  Note this is an implementation package and is subject to change att any time.
--  ----------------------------------------------------------------------------
with DDS.ReadCondition;
with DDS.Request_Reply.Impl;
with DDS.TopicDescription;
with DDS.EntityParams;
with DDS.DomainParticipant;
with DDS.ContentFilteredTopic;
with DDS.Topic;
private package DDS.Request_Reply.Requester.Impl is

   type Ref is abstract limited new DDS.Request_Reply.Impl.Ref and DDS.Request_Reply.Requester.Ref with record
      null;
   end record;
   type Ref_Access is access all Ref'Class;


   --@RequesterUntypedImpl.c:41
   CFTBuilder_GUID_FIELD_NAME       : constant Standard.String := "@related_sample_identity.writer_guid.value";
   CFTBuilder_GUID_SIZE             : constant := 16;
   CFTBuilder_MAX_TOPIC_NAME_LENGTH : constant :=  255-(16 * 4 + 1);




   --@RequesterUntypedImpl.c:131
   function Create_Reader_Topic (Self            : not null access Ref;
                                 Params          : not null DDS.EntityParams.Ref_Access;
                                 Reply_Type_Name : String) return DDS.TopicDescription.Ref_Access;

   --@RequesterUntypedImpl.c:179
   function Create_Writer_Topic (Self              : not null access Ref;
                                 Params            : not null DDS.EntityParams.Ref_Access;
                                 Request_Type_Name : String) return DDS.TopicDescription.Ref_Access;

   --@RequesterUntypedImpl.c:179
   function Create (Params     : not null DDS.EntityParams.Ref_Access;
                    Reply_Size : Integer) return Ref_Access;

   --@RequesterUntypedImpl.c:339
   CORRELATION_SN_FIELD_NAME        : constant Standard.String := "@related_sample_identity.sequence_number";


   --@RequesterUntypedImpl.c:342
   function Create_Query_Expression_For_Correlation_Sequence_Number
     (Sequence_Number : DDS.SequenceNumber_T) return Standard.String;

   --@RequesterUntypedImpl.c:361
   function Create_Correlation_Condition
     (Self            : not null access Ref;
      State_Kind      : DDS.SampleStateMask;
      Sequence_Number : DDS.SequenceNumber_T) return DDS.ReadCondition.Ref_Access;

   --@RequesterUntypedImpl.c:394
   function Wait_For_Replies
     (Self                 : not null access Ref;
      Max_Wait             : DDS.Duration_T;
      Min_Sample_Count     : DDS.Integer;
      Related_Request_Info : DDS.SampleIdentity_T) return DDS.ReturnCode_T;


   --@RequesterUntypedImpl.c:472
   function Get_Reply_Loaned
     (Self                    : not null access Ref;
      Received_Data           : System.Address;
      Data_Count              : out DDS.Integer;
      Is_Loan                 : out Boolean;
      DataSeqContiguousBuffer : System.Address;
      Info_Seq                : in out DDS.SampleInfo_Seq.Sequence;
      Data_Seq_Len            : DDS.Long;
      Data_Seq_Max_Len        : DDS.Long;
      Ownership               : Boolean;
      Max_Samples             : DDS.Long;
      Related_Request_Id      : DDS.SampleIdentity_T;
      Take                    : Boolean) return DDS.ReturnCode_T;

   function Create_Correlation_Cft
     (Participant      : not null DDS.DomainParticipant.Ref_Access;
      Topic            : not null DDS.Topic.Ref_Access;
      Correlation_Guid : DDS.Guid_T)
      return DDS.ContentFilteredTopic.Ref_Access;

   function Wait_For_Replies (Self            : not null access Ref;
                              Min_Reply_Count : DDS.Long;
                              Max_Wait        : DDS.Duration_T)
                              return ReturnCode_T;

end DDS.Request_Reply.Requester.Impl;
