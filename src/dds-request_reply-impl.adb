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

with DDS.Condition;
with DDS.ConditionSeq;
with Interfaces.C.Extensions;
package body DDS.Request_Reply.Impl is
   use DDS.DomainParticipant;
   use DDS.Publisher;
   use DDS.Subscriber;

   --------------------------
   -- Create_Request_Topic --
   --------------------------

   function Create_Request_Topic
     (Self       : not null access Ref;
      Topic_Name : DDS.String;
      Type_Name  : DDS.String;
      QoS        : DDS.TopicQos := DDS.DomainParticipant.TOPIC_QOS_DEFAULT) return DDS.Topic.Ref_Access
   is
   begin
      return Self.Participant.Get_Or_Create_Topic (Topic_Name, Type_Name, Qos);
   end Create_Request_Topic;

   ------------------------
   -- Create_Reply_Topic --
   ------------------------

   function Create_Reply_Topic
     (Self       : not null access Ref;
      Topic_Name : DDS.String;
      Type_Name  : DDS.String;
      QoS        : DDS.TopicQos := DDS.DomainParticipant.TOPIC_QOS_DEFAULT) return DDS.Topic.Ref_Access
   is
   begin
      return Self.Participant.Get_Or_Create_Topic (Topic_Name, Type_Name, Qos);
   end Create_Reply_Topic;

   ---------------------------------------
   -- Create_Request_Topic_With_Profile --
   ---------------------------------------

   function Create_Request_Topic_With_Profile
     (Self             : not null access Ref;
      Topic_Name       : DDS.String;
      Type_Name        : DDS.String;
      Library_Name     : DDS.String;
      Profile_Name     : DDS.String) return DDS.Topic.Ref_Access
   is
   begin
      return Self.Participant.Get_Or_Create_Topic_With_Profile (Topic_Name, Type_Name, Library_Name, Profile_Name);
   end Create_Request_Topic_With_Profile;

   -------------------------------------
   -- Create_Reply_Topic_With_Profile --
   -------------------------------------

   function Create_Reply_Topic_With_Profile
     (Self             : not null access Ref;
      Topic_Name       : DDS.String;
      Type_Name        : DDS.String;
      Library_Name     : DDS.String;
      Profile_Name     : DDS.String) return DDS.Topic.Ref_Access
   is
   begin
      return Self.Participant.Get_Or_Create_Topic_With_Profile (Topic_Name, Type_Name, Library_Name, Profile_Name);
   end Create_Reply_Topic_With_Profile;

   --------------
   -- Validate --
   --------------
   function CreateContentFilteredTopicName ( Self             : not null access Ref;
                                             RelatedTopicName : DDS.String;
                                             Guid             : Guid_T) return Standard.String is
   begin
      return raise Program_Error with "CreateContentFilteredTopicName Unimplemented";
   end;

   procedure Validate
     (Self       : not null access Ref;
      Publisher  : DDS.Publisher.Ref_Access;
      Subscriber : DDS.Subscriber.Ref_Access)
   is
   begin
      if (Publisher /= null) then
         if Self.Participant /= Publisher.Get_Participant then
            raise Program_Error with "Publisher dont belong to participant";
         else
            Self.Publisher := Publisher;
         end if;
      else
         Self.Publisher := Self.Participant.Get_Implicit_Publisher;
      end if;

      if (Subscriber /= null) then
         if Self.Participant /= Subscriber.Get_Participant then
            raise Program_Error with "Subscriber dont belong to participant";
         else
            Self.Subscriber := Self.Participant.Get_Implicit_Subscriber;
         end if;
      else
         Self.Subscriber := Subscriber;
      end if;
   end Validate;

   procedure Wait_For_Any_Sample (Self             : not null access Ref;
                                  Max_Wait         : DDS.Duration_T;
                                  Min_Sample_Count : DDS.Natural) is
   begin
      Self.Wait_For_Samples (Max_Wait,
                             Min_Sample_Count,
                             Self.Waitset,
                             Self.Any_Sample_Cond,
                             Self.Not_Read_Sample_Cond);
   end Wait_For_Any_Sample;

   function Get_Sample_Loaned_W_Len
     (Self                    : not null access Ref;
      Received_Data           : System.Address;
      Data_Count              : out Integer;
      Is_Loan                 : out Boolean;
      DataSeqContiguousBuffer : Interfaces.C.Extensions.Void_Ptr;
      Info_Seq                : DDS.SampleInfo_Seq.Sequence;
      Data_Seq_Len            : Long_Integer;
      Data_Seq_Max_Len        : Long_Integer;
      Data_Seq_Has_Ownership  : Boolean;
      Max_Samples             : Natural;
      Read_Condition          : not null DDS.ReadCondition.Ref_Access;
      Take                    : Boolean)return DDS.ReturnCode_T is

   begin
      return Self.Reader.Read_Or_Take_W_Condition_UntypedI
        (Is_Loan                             => Is_Loan,
         Received_Data                       => Received_Data,
         Data_Count                          => Data_Count'Unrestricted_Access,
         Info_Seq                            => Info_Seq,
         Data_Seq_Len                        => Data_Seq_Len,
         Data_Seq_Max_Len                    => Data_Seq_Max_Len,
         Data_Seq_Has_Ownership              => Boolean'Pos (Data_Seq_Has_Ownership),
         Data_Seq_Contiguous_Buffer_For_Copy => DataSeqContiguousBuffer,
         Data_Size                           => Self.Sample_Size,
         Max_Samples                         => Standard.Long_Integer (Max_Samples),
         Condition                           => Read_Condition,
         Take                                => Take);
   end Get_Sample_Loaned_W_Len;

   function Touch_Samples
     (Self          : not null access Ref;
      Max_Count     : DDS.Natural;
      ReadCondition : not null DDS.ReadCondition.Ref_Access) return Integer
   is
      Received_Data : System.Address := System.Null_Address;
      Info_Seq      : aliased DDS.SampleInfo_Seq.Sequence;
      Data_Count    : Integer := -1;
      IsLoan        : Boolean := True;
      RetCode       : DDS.ReturnCode_T;
   begin
      RetCode := Self.Get_Sample_Loaned_W_Len
        (Received_Data           => Received_Data'Address,
         Data_Count              => Data_Count,
         Is_Loan                 => IsLoan,
         DataSeqContiguousBuffer => System.Null_Address,
         Info_Seq                => Info_Seq,
         Data_Seq_Len            => 0,
         Data_Seq_Max_Len        => 0,
         Data_Seq_Has_Ownership  => True,
         Max_Samples             => Max_Count,
         Read_Condition          => ReadCondition,
         Take                    => False);
      if RetCode = DDS.RETCODE_OK  then
         Self.Reader.Return_Loan_UntypedI (Received_Data, Data_Count, Info_Seq);
      elsif RetCode /= DDS.RETCODE_NO_DATA then
         Ret_Code_To_Exception (RetCode, "error with getting sample loan");
      end if;
      return Data_Count;
   end Touch_Samples;


   procedure Wait_For_Samples (Self              : not null access Ref;
                               Max_Wait          : DDS.Duration_T;
                               Min_Sample_Count  : DDS.Integer;
                               WaitSet           : not null DDS.WaitSet.Ref_Access;
                               Initial_Condition : not null DDS.ReadCondition.Ref_Access;
                               Condition         : not null DDS.ReadCondition.Ref_Access) is
      Remaining_Sample_Count : DDS.Natural := (if Min_Sample_Count = DDS.LENGTH_UNLIMITED then DDS.Natural'Last else Min_Sample_Count);
      TimeBefore, TimeAfter  : DDS.Time_T;
      RemainingWait          : aliased DDS.Duration_T;
      ActiveConditions       : aliased DDS.ConditionSeq.Sequence;
      use DDS.ConditionSeq;
      use Dds.Condition;
   begin

      if Condition.Get_Sample_State_Mask /= DDS.NOT_READ_SAMPLE_STATE then
         raise PRECONDITION_NOT_MET;
      end if;

      if Initial_Condition.Get_Sample_State_Mask /= DDS.ANY_SAMPLE_STATE then
         raise PRECONDITION_NOT_MET;
      end if;
      Remaining_Sample_Count := Remaining_Sample_Count - Self.Touch_Samples (Min_Sample_Count, Initial_Condition);

      while Remaining_Sample_Count > 0 loop
         if Remaining_Sample_Count = 1 then
            WaitSet.Wait (ActiveConditions'Access, RemainingWait);
            Remaining_Sample_Count :=  Remaining_Sample_Count - Self.Touch_Samples (Min_Sample_Count, Condition);
         else

            TimeBefore := Self.Participant.Get_Current_Time;
            WaitSet.Wait (ActiveConditions'Access, RemainingWait);
            TimeAfter := Self.Participant.Get_Current_Time;

            RemainingWait := RemainingWait -(TimeAfter - TimeBefore);

            exit when
              Get_Length (ActiveConditions'Access) /= 1 or
              Get (ActiveConditions'Access, 0) /= DDS.Condition.Ref_Access (Condition);
            if Remaining_Sample_Count > 1 then
               Remaining_Sample_Count :=  Remaining_Sample_Count - Self.Touch_Samples (Min_Sample_Count, Condition);
            else
               Remaining_Sample_Count := Remaining_Sample_Count - 1;
            end if;
         end if;
      end loop;
   end Wait_For_Samples;

function Wait_For_Samples (Self              : not null access Ref;
                           Max_Wait          : DDS.Duration_T;
                           Min_Sample_Count  : DDS.Integer;
                           WaitSet           : not null DDS.WaitSet.Ref_Access;
                           Initial_Condition : not null DDS.ReadCondition.Ref_Access;
                           Condition         : not null DDS.ReadCondition.Ref_Access) return DDS.ReturnCode_T is
      Remaining_Sample_Count : DDS.Natural := (if Min_Sample_Count = DDS.LENGTH_UNLIMITED then DDS.Natural'Last else Min_Sample_Count);
      TimeBefore, TimeAfter  : DDS.Time_T;
      RemainingWait          : aliased DDS.Duration_T := Max_Wait;
      ActiveConditions       : aliased DDS.ConditionSeq.Sequence;
      use DDS.ConditionSeq;
      use Dds.Condition;
   begin

      if Condition.Get_Sample_State_Mask /= DDS.NOT_READ_SAMPLE_STATE then
         return DDS.RETCODE_PRECONDITION_NOT_MET;
      end if;

      if Initial_Condition.Get_Sample_State_Mask /= DDS.ANY_SAMPLE_STATE then
         return DDS.RETCODE_PRECONDITION_NOT_MET;
      end if;

      Remaining_Sample_Count := Remaining_Sample_Count - Self.Touch_Samples (Min_Sample_Count, Initial_Condition);

      while Remaining_Sample_Count > 0 loop
         if Remaining_Sample_Count = 1 then
            WaitSet.Wait (ActiveConditions'Access, RemainingWait);
            Remaining_Sample_Count :=  Remaining_Sample_Count - Self.Touch_Samples (Min_Sample_Count, Condition);
         else

            TimeBefore := Self.Participant.Get_Current_Time;
            WaitSet.Wait (ActiveConditions'Access, RemainingWait);
            TimeAfter := Self.Participant.Get_Current_Time;

            RemainingWait := RemainingWait -(TimeAfter - TimeBefore);

            exit when
              Get_Length (ActiveConditions'Access) /= 1 or
              Get (ActiveConditions'Access, 0) /= DDS.Condition.Ref_Access (Condition);
            if Remaining_Sample_Count > 1 then
               Remaining_Sample_Count :=  Remaining_Sample_Count - Self.Touch_Samples (Min_Sample_Count, Condition);
            else
               Remaining_Sample_Count := Remaining_Sample_Count - 1;
            end if;
         end if;
      end loop;
      return DDS.RETCODE_OK;
   end;

end DDS.Request_Reply.Impl;
