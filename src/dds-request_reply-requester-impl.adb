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

with DDS.WaitSet;

--  ----------------------------------------------------------------------------
--  Note this is an implementation package and is subject to change att any time.
--  ----------------------------------------------------------------------------
package body DDS.Request_Reply.Requester.Impl is
   use DDS.ContentFilteredTopic;
   use DDS.ReadCondition;

   -------------------
   -- Touch_Samples --
   ----------------------------
   -- create_correlation_cft --
   ----------------------------
   function Create_Correlation_Cft
     (Participant      : not null DDS.DomainParticipant.Ref_Access;
      Topic            : not null DDS.Topic.Ref_Access;
      Correlation_Guid : DDS.Guid_T)
      return not null  DDS.ContentFilteredTopic.Ref_Access
   is
   begin
      return Participant.Create_Correlation_ContentFilteredTopic (Topic, Correlation_Guid);
   end Create_Correlation_Cft;


   function Create_Query_Expression_For_Correlation_Sequence_Number
     (Sequence_Number : DDS.SequenceNumber_T) return Standard.String is
   begin
      return CORRELATION_SN_FIELD_NAME &".low = "&Sequence_Number.Low'image & "and "&CORRELATION_SN_FIELD_NAME&".high =" & Sequence_Number.High'image;
   end;


   RequestReplyIndex : constant DDS.String := To_DDS_String ("RequestReplyIndex");

   function Create_Correlation_Condition
     (Self            : not null access Ref;
      State_Kind      : DDS.SampleStateMask;
      Sequence_Number : DDS.SequenceNumber_T) return not null DDS.ReadCondition.Ref_Access
   is
      Sample_Info : DDS.SampleInfo;
      Condition   : DDS.ReadCondition.Ref_Access;
   begin
      if Sequence_Number in DDS.AUTO_SEQUENCE_NUMBER | DDS.SEQUENCE_NUMBER_MAX | DDS.SEQUENCE_NUMBER_ZERO | DDS.SEQUENCE_NUMBER_UNKNOWN then
         raise DDS.ERROR with "Invalid correlation sequence number" & Sequence_Number'Img;
      end if;
      Sample_Info.Related_Original_Publication_Virtual_Sequence_Number := Sequence_Number;

         Condition := ReadCondition.Ref_Access (Self.Reader.Create_Indexcondition
                                                (Sample_State    => State_Kind,
                                                 View_State      => DDS.ANY_VIEW_STATE,
                                                 Instance_State  => DDS.ALIVE_INSTANCE_STATE,
                                                 Index_Name      => RequestReplyIndex,
                                                 Sample_Info     => Sample_Info));

      if Condition = null  then
         return raise DDS.ERROR with "Invalid correlation sequence number";
      else
         return Condition;
      end if;
   end Create_Correlation_Condition;

   function Wait_For_Replies_For_Related_Request
     (Self                 : not null access Ref;
      Min_Reply_Count      : DDS.Long;
      Max_Wait             : DDS.Duration_T;
      Related_Request_Info : DDS.SampleIdentity_T) return ReturnCode_T is
      RetCode : DDS.ReturnCode_T := DDS.RETCODE_OK;
      Self_Classwide : constant not null access ref'class := self;
   begin
      if Related_Request_Info /= NULL_SAMPLE_IDENTITY then
         RetCode := Self.Wait_For_Replies (Max_Wait, Min_Reply_Count, Related_Request_Info);
      else
         RetCode := Self_Classwide.Wait_For_Any_Sample (Max_Wait, Min_Reply_Count);
      end if;

      if RetCode not in  DDS.RETCODE_OK |  DDS.RETCODE_TIMEOUT then
         dds.Ret_Code_To_Exception(RetCode);
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
      Initial_Condition := Self.Create_Correlation_Condition (DDS.ANY_SAMPLE_STATE, Related_Request_Info.Sequence_Number);

      Waitset.Attach_Condition (Correlation_Condition);
      Retcode:= Self.Wait_For_Samples (Max_Wait, Min_Sample_Count, Waitset'Unrestricted_Access, Initial_Condition, Correlation_Condition);
      Finalize_Self;
      RETURN Retcode;
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
      Take                    : Boolean)
      return DDS.ReturnCode_T
   is
   begin
      return raise Program_Error with "Unimplemented function get_reply_loaned";
   end Get_Reply_Loaned;


end DDS.Request_Reply.Requester.Impl;
