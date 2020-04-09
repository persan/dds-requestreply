
with Ada.Unchecked_Deallocation;
with Interfaces.C;
with DDS.TypeSupport;
with RTIDDS.Low_Level.Ndds_Dds_C_Dds_C_Common_H;
with RTIDDS.Low_Level.Ndds_Dds_C_Dds_C_Subscription_Impl_H;
with RTIDDS.Low_Level.Ndds_Dds_C_Dds_C_Subscription_H; use RTIDDS.Low_Level.Ndds_Dds_C_Dds_C_Subscription_H;
with DDS.ReadCondition_Impl;
--  (c) Copyright, Real-Time Innovations, $Date:: 2012-02-16 #$
--  All rights reserved.
--
--  No duplications, whole or partial, manual or electronic, may be made
--  without express written permission.  Any such copies, or
--  revisions thereof, must display this notice unaltered.
--  This code contains trade secrets of Real-Time Innovations, Inc.

package body DDS.Typed_DataReader_Generic is

   use RTIDDS.Low_Level.Ndds_Dds_C_Dds_C_Common_H;
   use RTIDDS.Low_Level.Ndds_Dds_C_Dds_C_Subscription_Impl_H;

   use type Interfaces.C.Unsigned_Char;
   use type Interfaces.C.Unsigned;
   use type System.Address;

   MetpImpl : MetpTypeSupport;

   type Ref_Pointer is access all Ref;
   procedure Free is new Ada.Unchecked_Deallocation (Ref, Ref_Pointer);

   ----------
   -- Read --
   ----------
   function Read
     (Self            : not null access Ref;
      Max_Samples     : in DDS.long := DDS.LENGTH_UNLIMITED;
      Sample_States   : in DDS.SampleStateMask := DDS.ANY_SAMPLE_STATE;
      View_States     : in DDS.ViewStateMask := DDS.ANY_VIEW_STATE;
      Instance_States : in DDS.InstanceStateMask := DDS.ANY_INSTANCE_STATE) return Container'Class is
   begin
      return Ret : Container do
         Ret.Reader := Self.all'Access;
         Ret.Retcode := Self.Read_Or_TakeI (Ret.Received_Data'Access,
                                            Ret.Info_Seq'Access,
                                            Max_Samples,
                                            Sample_States,
                                            View_States,
                                            Instance_States, Take => False);
         if Ret.Retcode not in DDS_RETCODE_OK |  DDS_RETCODE_NO_DATA then
            Ret_Code_To_Exception (Ret.Retcode);
         end if;
      end return;
   end Read;

   procedure Read
     (Self            : not null access constant Ref;
      Received_Data   : not null access Data_Sequences.Sequence;
      Info_Seq        : not null access DDS.SampleInfo_Seq.Sequence;
      Max_Samples     : in DDS.long := DDS.LENGTH_UNLIMITED;
      Sample_States   : in DDS.SampleStateMask := DDS.ANY_SAMPLE_STATE;
      View_States     : in DDS.ViewStateMask := DDS.ANY_VIEW_STATE;
      Instance_States : in DDS.InstanceStateMask := DDS.ANY_INSTANCE_STATE)
   is
   begin
      Read_Or_TakeI (Self,
                     Received_Data,
                     Info_Seq,
                     Max_Samples,
                     Sample_States,
                     View_States,
                     Instance_States,
                     False);
   end Read;

   procedure Read
     (Self            : not null access constant Ref;
      Received_Data   : in out  Data_Sequences.Sequence;
      Info_Seq        : in out  DDS.SampleInfo_Seq.Sequence;
      Max_Samples     : in DDS.long := DDS.LENGTH_UNLIMITED;
      Sample_States   : in DDS.SampleStateMask := DDS.ANY_SAMPLE_STATE;
      View_States     : in DDS.ViewStateMask := DDS.ANY_VIEW_STATE;
      Instance_States : in DDS.InstanceStateMask := DDS.ANY_INSTANCE_STATE)
   is
   begin
      Read_Or_TakeI (Self,
                     Received_Data'Unrestricted_Access,
                     Info_Seq'Unrestricted_Access,
                     Max_Samples,
                     Sample_States,
                     View_States,
                     Instance_States,
                     False);
   end Read;

   ----------
   -- Take --
   ----------

   procedure Take
     (Self            : not null access constant Ref;
      Received_Data   : not null access Data_Sequences.Sequence;
      Info_Seq        : not null access DDS.SampleInfo_Seq.Sequence;
      Max_Samples     : in DDS.long := DDS.LENGTH_UNLIMITED;
      Sample_States   : in DDS.SampleStateMask := DDS.ANY_SAMPLE_STATE;
      View_States     : in DDS.ViewStateMask := DDS.ANY_VIEW_STATE;
      Instance_States : in DDS.InstanceStateMask := DDS.ANY_INSTANCE_STATE)
   is
   begin
      Self.Read_Or_TakeI (Received_Data,
                          Info_Seq,
                          Max_Samples,
                          Sample_States,
                          View_States,
                          Instance_States,
                          True);
   end Take;

   procedure Take
     (Self            : not null access constant Ref;
      Received_Data   : out Data_Sequences.Sequence;
      Info_Seq        : out DDS.SampleInfo_Seq.Sequence;
      Max_Samples     : in DDS.long := DDS.LENGTH_UNLIMITED;
      Sample_States   : in DDS.SampleStateMask := DDS.ANY_SAMPLE_STATE;
      View_States     : in DDS.ViewStateMask := DDS.ANY_VIEW_STATE;
      Instance_States : in DDS.InstanceStateMask := DDS.ANY_INSTANCE_STATE) is
   begin
      Self.Take (Data_Sequences.Sequence_Access'(Received_Data'Unrestricted_Access),
                 DDS.SampleInfo_Seq.Sequence_Access'(Info_Seq'Unrestricted_Access),
                 Max_Samples,
                 Sample_States,
                 View_States,
                 Instance_States);

   end Take;

   ----------------------
   -- Read_W_Condition --
   ----------------------

   procedure Read_W_Condition
     (Self          : not null access constant Ref;
      Received_Data : not null access Data_Sequences.Sequence;
      Info_Seq      : not null access DDS.SampleInfo_Seq.Sequence;
      Max_Samples   : in DDS.long := DDS.LENGTH_UNLIMITED;
      Condition     : access DDS.ReadCondition.Ref'Class)
   is
   begin
      Self.Read_Or_Take_W_ConditionI (Received_Data,
                                      Info_Seq,
                                      Max_Samples,
                                      Condition,
                                      False);
   end Read_W_Condition;

   procedure Read_W_Condition
     (Self          : not null access constant Ref;
      Received_Data : out Data_Sequences.Sequence;
      Info_Seq      : out DDS.SampleInfo_Seq.Sequence;
      Max_Samples   : in DDS.long := DDS.LENGTH_UNLIMITED;
      Condition     : access DDS.ReadCondition.Ref'Class)
   is
   begin
      Self.Read_Or_Take_W_ConditionI (Received_Data'Unrestricted_Access,
                                      Info_Seq'Unrestricted_Access,
                                      Max_Samples,
                                      Condition,
                                      False);
   end Read_W_Condition;
   ----------------------
   -- Take_W_Condition --
   ----------------------

   procedure Take_W_Condition
     (Self          : not null access constant Ref;
      Received_Data : not null access Data_Sequences.Sequence;
      Info_Seq      : not null access DDS.SampleInfo_Seq.Sequence;
      Max_Samples   : in DDS.long := DDS.LENGTH_UNLIMITED;
      Condition     : access DDS.ReadCondition.Ref'Class)
   is
   begin
      Self.Read_Or_Take_W_ConditionI (Received_Data,
                                      Info_Seq,
                                      Max_Samples,
                                      Condition,
                                      True);
   end Take_W_Condition;

   ----------------------
   -- Read_Next_Sample --
   ----------------------

   procedure Read_Next_Sample
     (Self          : not null access constant Ref;
      Received_Data : not null Data_Type_Access;
      Sample_Info   : not null access DDS.SampleInfo)
   is
   begin
      Read_Or_Take_Next_SampleI (Self, Received_Data,
                                 Sample_Info, False);
   end Read_Next_Sample;
   procedure Read_Next_Sample
     (Self          : not null access constant Ref;
      Received_Data : out Data_Type;
      Sample_Info   : not null access DDS.SampleInfo) is

   begin
      Self.Read_Next_Sample (Data_Type_Access'(Received_Data'Unrestricted_Access), Sample_Info);
   end Read_Next_Sample;

   ----------------------
   -- Take_Next_Sample --
   ----------------------

   procedure Take_Next_Sample
     (Self          : not null access constant Ref;
      Received_Data : not null Data_Type_Access;
      Sample_Info   : not null access DDS.SampleInfo)
   is
   begin
      Self.Read_Or_Take_Next_SampleI (Received_Data,
                                      Sample_Info, True);
   end Take_Next_Sample;

   procedure Take_Next_Sample
     (Self          : not null access constant Ref;
      Received_Data : out Data_Type;
      Sample_Info   : not null access DDS.SampleInfo) is
   begin
      Self.Take_Next_Sample (Data_Type_Access'(Received_Data'Unrestricted_Access), Sample_Info);
   end Take_Next_Sample;

   -------------------
   -- Read_Instance --
   -------------------

   procedure Read_Instance
     (Self            : not null access constant Ref;
      Received_Data   : not null access Data_Sequences.Sequence;
      Info_Seq        : not null access DDS.SampleInfo_Seq.Sequence;
      Max_Samples     : in DDS.long := DDS.LENGTH_UNLIMITED;
      A_Handle        : access constant DDS.InstanceHandle_T;
      Sample_States   : in DDS.SampleStateMask := DDS.ANY_SAMPLE_STATE;
      View_States     : in DDS.ViewStateMask := DDS.ANY_VIEW_STATE;
      Instance_States : in DDS.InstanceStateMask := DDS.ANY_INSTANCE_STATE)
   is
   begin
      Self.Read_Or_Take_InstanceI (Received_Data,
                                   Info_Seq,
                                   Max_Samples,
                                   A_Handle,
                                   Sample_States,
                                   View_States,
                                   Instance_States,
                                   False);
   end Read_Instance;

   -------------------
   -- Take_Instance --
   -------------------

   procedure Take_Instance
     (Self            : not null access constant Ref;
      Received_Data   : not null access Data_Sequences.Sequence;
      Info_Seq        : not null access DDS.SampleInfo_Seq.Sequence;
      Max_Samples     : in DDS.long := DDS.LENGTH_UNLIMITED;
      A_Handle        : access constant DDS.InstanceHandle_T;
      Sample_States   : in DDS.SampleStateMask := DDS.ANY_SAMPLE_STATE;
      View_States     : in DDS.ViewStateMask := DDS.ANY_VIEW_STATE;
      Instance_States : in DDS.InstanceStateMask := DDS.ANY_INSTANCE_STATE)
   is
   begin
      Self.Read_Or_Take_InstanceI (Received_Data,
                                   Info_Seq,
                                   Max_Samples,
                                   A_Handle,
                                   Sample_States,
                                   View_States,
                                   Instance_States,
                                   True);
   end Take_Instance;

   ------------------------
   -- Read_Next_Instance --
   ------------------------

   procedure Read_Next_Instance
     (Self            : not null access constant Ref;
      Received_Data   : not null access Data_Sequences.Sequence;
      Info_Seq        : not null access DDS.SampleInfo_Seq.Sequence;
      Max_Samples     : in DDS.long := DDS.LENGTH_UNLIMITED;
      Previous_Handle : access constant DDS.InstanceHandle_T;
      Sample_States   : in DDS.SampleStateMask := DDS.ANY_SAMPLE_STATE;
      View_States     : in DDS.ViewStateMask := DDS.ANY_VIEW_STATE;
      Instance_States : in DDS.InstanceStateMask := DDS.ANY_INSTANCE_STATE)
   is
   begin
      Self.Read_Or_Take_Next_InstanceI (Received_Data,
                                        Info_Seq,
                                        Max_Samples,
                                        Previous_Handle,
                                        Sample_States,
                                        View_States,
                                        Instance_States,
                                        False);
   end Read_Next_Instance;

   ------------------------
   -- Take_Next_Instance --
   ------------------------

   procedure Take_Next_Instance
     (Self            : not null access constant Ref;
      Received_Data   : not null access Data_Sequences.Sequence;
      Info_Seq        : not null access DDS.SampleInfo_Seq.Sequence;
      Max_Samples     : in DDS.long := DDS.LENGTH_UNLIMITED;
      Previous_Handle : access constant DDS.InstanceHandle_T;
      Sample_States   : in DDS.SampleStateMask := DDS.ANY_SAMPLE_STATE;
      View_States     : in DDS.ViewStateMask := DDS.ANY_VIEW_STATE;
      Instance_States : in DDS.InstanceStateMask := DDS.ANY_INSTANCE_STATE)
   is
   begin
      Self.Read_Or_Take_Next_InstanceI (Received_Data,
                                        Info_Seq,
                                        Max_Samples,
                                        Previous_Handle,
                                        Sample_States,
                                        View_States,
                                        Instance_States,
                                        True);
   end Take_Next_Instance;

   ------------------------------------
   -- Read_Next_Instance_W_Condition --
   ------------------------------------

   procedure Read_Next_Instance_W_Condition
     (Self            : not null access constant Ref;
      Received_Data   : not null access Data_Sequences.Sequence;
      Info_Seq        : not null access DDS.SampleInfo_Seq.Sequence;
      Max_Samples     : in DDS.long := DDS.LENGTH_UNLIMITED;
      Previous_Handle : access constant DDS.InstanceHandle_T;
      Condition       : DDS.ReadCondition.Ref_Access)
   is
   begin
      Self.Read_Or_Take_Next_Instance_W_ConditionI (Received_Data,
                                                    Info_Seq,
                                                    Max_Samples,
                                                    Previous_Handle,
                                                    Condition,
                                                    False);
   end Read_Next_Instance_W_Condition;

   -------------------------------
   -- Read_Instance_W_Condition --
   -------------------------------

   procedure Read_Instance_W_Condition
     (Self            : not null access constant Ref;
      Received_Data   : not null access Data_Sequences.Sequence;
      Info_Seq        : not null access DDS.SampleInfo_Seq.Sequence;
      Max_Samples     : in DDS.long := DDS.LENGTH_UNLIMITED;
      A_Handle        : access constant DDS.InstanceHandle_T;
      Condition       : DDS.ReadCondition.Ref_Access) is
   begin
      Self.Read_Or_Take_Instance_W_ConditionI (Received_Data => Received_Data,
                                               Info_Seq      => Info_Seq,
                                               Max_Samples   => Max_Samples,
                                               Handle        => A_Handle,
                                               Condition     => Condition,
                                               Take          => False);
   end Read_Instance_W_Condition;

   -------------------------------
   -- Take_Instance_W_Condition --
   -------------------------------

   procedure Take_Instance_W_Condition
     (Self          : not null access constant Ref;
      Received_Data : not null access Data_Sequences.Sequence;
      Info_Seq      : not null access DDS.SampleInfo_Seq.Sequence;
      Max_Samples   : in DDS.long := DDS.LENGTH_UNLIMITED;
      A_Handle      : access constant DDS.InstanceHandle_T;
      Condition     : DDS.ReadCondition.Ref_Access) is
   begin
      Self.Read_Or_Take_Instance_W_ConditionI (Received_Data => Received_Data,
                                               Info_Seq      => Info_Seq,
                                               Max_Samples   => Max_Samples,
                                               Handle        => A_Handle,
                                               Condition     => Condition,
                                               Take          => True);
   end Take_Instance_W_Condition;

   -------------------------------------------------------------------------------------------------


   ------------------------------------
   -- Take_Next_Instance_W_Condition --
   ------------------------------------

   procedure Take_Next_Instance_W_Condition
     (Self            : not null access constant Ref;
      Received_Data   : not null access Data_Sequences.Sequence;
      Info_Seq        : not null access DDS.SampleInfo_Seq.Sequence;
      Max_Samples     : in DDS.long := DDS.LENGTH_UNLIMITED;
      Previous_Handle : access constant DDS.InstanceHandle_T;
      Condition       : DDS.ReadCondition.Ref_Access)
   is
   begin
      Self.Read_Or_Take_Next_Instance_W_ConditionI (Received_Data,
                                                    Info_Seq,
                                                    Max_Samples,
                                                    Previous_Handle,
                                                    Condition,
                                                    True);
   end Take_Next_Instance_W_Condition;

   -----------------
   -- Return_Loan --
   -----------------

   procedure Return_Loan
     (Self          : not null access constant Ref;
      Received_Data : not null access Data_Sequences.Sequence;
      Info_Seq      : not null access DDS.SampleInfo_Seq.Sequence)
   is
      DataSeqMaxLen              : constant Index_Type := Data_Sequences.Get_Maximum (Received_Data);
      DataSeqHasOwnership        : constant DDS.Boolean := Data_Sequences.Has_Ownership (Received_Data);
      InfoSeqHasOwnership        : constant DDS.Boolean := DDS.SampleInfo_Seq.Has_Ownership (Info_Seq);
      DataSeqDisContiguousBuffer : constant access Data_Sequences.Memory_Element_Pointer  := Data_Sequences.Get_DisContiguous_BufferI (Received_Data);
      Result                     : DDS_ReturnCode_T;
   begin
      --        Self.Return_Loan (Received_Data.all'Address,
      --                          Info_Seq);
      --  Check for loan
      if  DataSeqHasOwnership and InfoSeqHasOwnership then
         --  No loan to return: exit successfully now.
         --  Only do this if *both* sequences have ownership. Otherwise,
         --  we could fail to catch the user error of passing mismatched
         --  sequences.
         goto Done;
      end if;

      --      /* call return loan */
      Result := Self.Return_Loan_UntypedI (DataSeqDisContiguousBuffer.all'Address,
                                           Natural (DataSeqMaxLen),
                                           Info_Seq.all'Unrestricted_Access);
      --
      if Result /= DDS_RETCODE_OK then
         --              DDS_DataReader_return_loan_untypedI() should already have
         --               logged this error.
         goto Done;
      end if;

      --          /* Don't unloan sequence unless DDS_DataReader_return_loan_untypedI()
      --           * returns successfully. Otherwise, we will leave the sequences in
      --           * an inconsistent state.

      begin

         Data_Sequences.Unloan (Received_Data);
      exception
         when others =>
            --  TODO?
            --            DDSLog_exception(METHOD_NAME, &RTI_LOG_ANY_FAILURE_s,
            --                               "unloan sequence");
            Result := DDS_RETCODE_ERROR;
            goto Done;
      end;

      <<Done>>
      Ret_Code_To_Exception (Result);
      return;
   end Return_Loan;

   procedure Return_Loan
     (Self          : not null access constant Ref;
      Received_Data : in out  Data_Sequences.Sequence;
      Info_Seq      : in out  DDS.SampleInfo_Seq.Sequence) is
   begin
      Self.Return_Loan (Received_Data'Unrestricted_Access, Info_Seq'Unrestricted_Access);
   end Return_Loan;

   -------------------
   -- Get_Key_Value --
   -------------------

   procedure Get_Key_Value
     (Self       : not null access constant Ref;
      Key_Holder : not null Data_Type_Access;
      Handle     : access constant DDS.InstanceHandle_T)
   is
   begin
      Get_Key_Value_UntypedI (Self,
                              Key_Holder.all'Address,
                              Handle);
   end Get_Key_Value;

   procedure Get_Key_Value
     (Self       : not null access constant Ref;
      Key_Holder : out Data_Type;
      Handle     : access constant DDS.InstanceHandle_T)
   is
   begin
      Get_Key_Value_UntypedI (Self,
                              Key_Holder'Address,
                              Handle);
   end Get_Key_Value;

   procedure Get_Key_Value
     (Self       : not null access constant Ref;
      Key_Holder : out Data_Type;
      Handle     : DDS.InstanceHandle_T)
   is
   begin
      Get_Key_Value_UntypedI (Self,
                              Key_Holder'Address,
                              Handle'Unrestricted_Access);
   end Get_Key_Value;

   ---------------------
   -- Lookup_Instance --
   ---------------------

   function Lookup_Instance
     (Self       : not null access constant Ref;
      Key_Holder : not null Data_Type_Access)
      return DDS.InstanceHandle_T
   is
   begin
      return Ret : DDS.InstanceHandle_T do
         Ret := Self.Lookup_Instance_UntypedI (Key_Holder.all'Address);
      end return;
   end Lookup_Instance;

   function Lookup_Instance
     (Self       : not null access constant Ref;
      Key_Holder : Data_Type)
      return DDS.InstanceHandle_T
   is
   begin
      return Ret : DDS.InstanceHandle_T do
         Ret := Self.Lookup_Instance_UntypedI (Key_Holder'Address);
      end return;
   end Lookup_Instance;

   procedure Lookup_Instance
     (Self       : not null access constant Ref;
      Key_Holder : Data_Type;
      Instance   : out DDS.InstanceHandle_T) is
   begin
      Instance := Self.Lookup_Instance_UntypedI (Key_Holder'Address);
   end Lookup_Instance;

   ------------------
   -- CreateTypedI --
   ------------------

   function CreateTypedI return DDS.DataReader.Ref_Access is
      S_Access : Ref_Access;
   begin
      S_Access := new Ref;
      return DDS.DataReader.Ref_Access (S_Access);
   end CreateTypedI;

   -------------------
   -- DestroyTypedI --
   -------------------
   procedure DestroyTypedI (Reader : in out DDS.DataReader.Ref_Access) is
      R_Impl : DDS.DataReader_Impl.Ref_Access :=
                 DDS.DataReader_Impl.Ref_Access (Reader);
   begin
      DDS.DataReader_Impl.Free (R_Impl);
      Free (Ref_Pointer (Reader));
   end DestroyTypedI;
   use DDS.SampleInfo_Seq;

   -----------------------------------------------------------------------------
   --  Internal stuff
   -----------------------------------------------------------------------------

   procedure  Read_Or_TakeI
     (Self            : not null access constant Ref;
      Received_Data   : not null access Data_Sequences.Sequence;
      Info_Seq        : not null access DDS.SampleInfo_Seq.Sequence;
      Max_Samples     : DDS.long;
      Sample_States   : DDS.SampleStateMask;
      View_States     : DDS.ViewStateMask;
      Instance_States : DDS.InstanceStateMask;
      Take            : DDS.Boolean)
   is

   begin
      Ret_Code_To_Exception (Self.Read_Or_TakeI (Received_Data, Info_Seq, Max_Samples, Sample_States, View_States, Instance_States, Take));
   end Read_Or_TakeI;

   function  Read_Or_TakeI
     (Self            : not null access constant Ref;
      Received_Data   : not null access Data_Sequences.Sequence;
      Info_Seq        : not null access DDS.SampleInfo_Seq.Sequence;
      Max_Samples     : DDS.long;
      Sample_States   : DDS.SampleStateMask;
      View_States     : DDS.ViewStateMask;
      Instance_States : DDS.InstanceStateMask;
      Take            : DDS.Boolean) return DDS_ReturnCode_T
   is
      DataSeqLen              : constant Index_Type := Data_Sequences.Get_Length (Received_Data);
      DataSeqMaxLen           : constant Index_Type := Data_Sequences.Get_Maximum (Received_Data);
      DataSeqHasOwnership     : constant DDS.Boolean := Data_Sequences.Has_Ownership (Received_Data);
      IsLoan                  : aliased DDS.Boolean := True;
      DataPtrArray            : aliased Data_Type_Access;
      DataCount               : aliased Natural;
      DataSeqContiguousBuffer : constant Data_Sequences.Memory_Element_Array_Pointer  := Data_Sequences.Get_Contiguous_BufferI (Received_Data);
      Result                  : DDS_ReturnCode_T;
   begin
      Result := Self.Read_Or_Take_UntypedI (IsLoan'Access,
                                            DataPtrArray'Address,
                                            DataCount'Access,
                                            Info_Seq.all'Unrestricted_Access,
                                            Standard.Long_Integer (DataSeqLen),
                                            Standard.Long_Integer (DataSeqMaxLen),
                                            Boolean'Pos (DataSeqHasOwnership),
                                            DataSeqContiguousBuffer'Address,
                                            (Data_Type'Object_Size + 1) / 8,
                                            Standard.Long_Integer (Max_Samples),
                                            Sample_States,
                                            View_States,
                                            Instance_States,
                                            Take);

      --/* --- Read/take data --- */
      if Result = DDS_RETCODE_NO_DATA then
         Data_Sequences.Set_Length (Received_Data, 0);
         goto Done;
      end if;
      --
      if Result /= DDS_RETCODE_OK then
         goto Done;
      end if;

      --
      if IsLoan then
         --  /* loan buffer to sequence */
         begin
            Data_Sequences.Loan_Discontiguous (Received_Data,
                                               DataPtrArray'Access,
                                               Index_Type (DataCount),
                                               Index_Type (DataCount));
         exception
            when others =>
               --  since we failed to loan data to data seq, but data is already
               --  taken, we will need to return it still.
               --  Note that data will be lost in this case */
               --                 self.Return_Loan (dataPtrArray'Address, Info_Seq);
               Return_Loan_UntypedI (Self, DataPtrArray'Address, DataCount, Info_Seq.all'Unrestricted_Access);
               raise;
         end;
      else
         --  data is already copied to dataSeqContiguousBuffer
         Data_Sequences.Set_Length (Received_Data, Index_Type (DataCount));
      end if;
      <<Done>>
      return Result;
   end Read_Or_TakeI;

   procedure  Read_Or_Take_W_ConditionI
     (Self            : not null access constant Ref;
      Received_Data   : not null access Data_Sequences.Sequence;
      Info_Seq        : not null access DDS.SampleInfo_Seq.Sequence;
      Max_Samples     : DDS.long;
      Condition       : not null access constant DDS.ReadCondition.Ref'Class;
      Take            : DDS.Boolean)
   is
   begin
      Ret_Code_To_Exception (Self.Read_Or_Take_W_ConditionI (Received_Data, Info_Seq, Max_Samples, Condition, Take));
   end Read_Or_Take_W_ConditionI;

   function  Read_Or_Take_W_ConditionI
     (Self            : not null access constant Ref;
      Received_Data   : not null access Data_Sequences.Sequence;
      Info_Seq        : not null access DDS.SampleInfo_Seq.Sequence;
      Max_Samples     : DDS.long;
      Condition       : not null access constant DDS.ReadCondition.Ref'Class;
      Take            : DDS.Boolean) return DDS_ReturnCode_T
   is
      DataSeqLen              : constant Index_Type := Data_Sequences.Get_Length (Received_Data);
      DataSeqMaxLen           : constant Index_Type := Data_Sequences.Get_Maximum (Received_Data);
      DataSeqHasOwnership     : constant DDS.Boolean := Data_Sequences.Has_Ownership (Received_Data);
      IsLoan                  : aliased DDS.Boolean := True;
      DataPtrArray            : aliased Data_Type_Access;
      DataCount               : aliased Natural := 0;

      DataSeqContiguousBuffer : Data_Sequences.Memory_Element_Array_Pointer;
      Result                  : DDS_ReturnCode_T;
   begin

      --  Read/take data
      Result := Self.Read_Or_Take_W_Condition_UntypedI
        (IsLoan'Access,
         DataPtrArray'Address,
         DataCount'Access,
         Info_Seq.all'Unrestricted_Access,
         Standard.Long_Integer (DataSeqLen),
         Standard.Long_Integer (DataSeqMaxLen),
         Boolean'Pos (DataSeqHasOwnership),
         DataSeqContiguousBuffer'Address,
         (Data_Type'Object_Size + 1) / 8,
         Standard.Long_Integer (Max_Samples),
         Condition,
         Take);


      if Result = DDS_RETCODE_NO_DATA then
         Data_Sequences.Set_Length (Received_Data, 0);
         goto Done;
      end if;

      if Result /= DDS_RETCODE_OK then
         goto Done;
      end if;

      if IsLoan then
         --  /* loan buffer to sequence */
         begin
            Data_Sequences.Loan_Discontiguous (Received_Data,
                                               DataPtrArray'Access,
                                               Index_Type (DataCount),
                                               Index_Type (DataCount));
         exception
               --  this should never happen
            when others =>
               --  since we failed to loan data to data seq, but data is already
               --  taken, we will need to return it still.
               --  Note that data will be lost in this case */
               --                 Self.Return_Loan (DataPtrArray'Address, Info_Seq);
               Return_Loan_UntypedI (Self, DataPtrArray'Address, DataCount, Info_Seq.all'Unrestricted_Access);
               raise;
         end;
      else
         --  data is already copied to dataSeqContiguousBuffer
         Data_Sequences.Set_Length (Received_Data, Index_Type (DataCount));
      end if;

      <<Done>>
      return Result;
   end Read_Or_Take_W_ConditionI;

   procedure  Read_Or_Take_Next_SampleI
     (Self            : not null access constant Ref;
      Received_Data   : not null Data_Type_Access;
      Sample_Info     : not null access DDS.SampleInfo;
      Take            : DDS.Boolean)
   is
      Result : DDS_ReturnCode_T;
   begin
      Result := Self.Read_Or_Take_Next_Sample_UntypedI (Received_Data.all'Address,
                                                        Sample_Info.all'Unrestricted_Access,
                                                        Take);
      Ret_Code_To_Exception (Result);
   end Read_Or_Take_Next_SampleI;

   procedure  Read_Or_Take_InstanceI
     (Self            : not null access constant Ref;
      Received_Data   : not null access Data_Sequences.Sequence;
      Info_Seq        : not null access DDS.SampleInfo_Seq.Sequence;
      Max_Samples     : DDS.long;
      Handle          : not null access constant DDS.InstanceHandle_T;
      Sample_States   : DDS.SampleStateMask;
      View_States     : DDS.ViewStateMask;
      Instance_States : DDS.InstanceStateMask;
      Take            : DDS.Boolean) is
   begin
      Ret_Code_To_Exception (Self.Read_Or_Take_InstanceI (Received_Data, Info_Seq, Max_Samples, Handle, Sample_States, View_States, Instance_States, Take));
   end Read_Or_Take_InstanceI;

   function Read_Or_Take_InstanceI
     (Self            : not null access constant Ref;
      Received_Data   : not null access Data_Sequences.Sequence;
      Info_Seq        : not null access DDS.SampleInfo_Seq.Sequence;
      Max_Samples     : DDS.long;
      Handle          : not null access constant DDS.InstanceHandle_T;
      Sample_States   : DDS.SampleStateMask;
      View_States     : DDS.ViewStateMask;
      Instance_States : DDS.InstanceStateMask;
      Take            : DDS.Boolean) return DDS_ReturnCode_T
   is
      DataSeqLen              : constant Index_Type := Data_Sequences.Get_Length (Received_Data);
      DataSeqMaxLen           : constant Index_Type := Data_Sequences.Get_Maximum (Received_Data);
      DataSeqHasOwnership     : constant DDS.Boolean := Data_Sequences.Has_Ownership (Received_Data);
      IsLoan                  : aliased DDS.Boolean := True;
      DataPtrArray            : aliased Data_Type_Access;
      DataCount               : aliased Natural;
      DataSeqContiguousBuffer : constant Data_Sequences.Memory_Element_Array_Pointer  := Data_Sequences.Get_Contiguous_BufferI (Received_Data);
      Result                  : DDS_ReturnCode_T;
   begin
      Result := Self.Read_Or_Take_Instance_UntypedI
        (IsLoan'Access,
         DataPtrArray'Address,
         DataCount'Access,
         Info_Seq.all'Unrestricted_Access,
         Standard.Long_Integer (DataSeqLen),
         Standard.Long_Integer (DataSeqMaxLen),
         Boolean'Pos (DataSeqHasOwnership),
         DataSeqContiguousBuffer'Address,
         (Data_Type'Object_Size + 1) / 8,
         Standard.Long_Integer (Max_Samples),
         Handle.all'Unrestricted_Access,
         Sample_States,
         View_States,
         Instance_States,
         Take);


      if Result = DDS_RETCODE_NO_DATA then
         Data_Sequences.Set_Length (Received_Data, 0);
         goto Done;
      end if;

      if Result /= DDS_RETCODE_OK then
         goto Done;
      end if;

      if IsLoan then
         --  /* loan buffer to sequence */
         begin
            Data_Sequences.Loan_Discontiguous (Received_Data,
                                               DataPtrArray'Unrestricted_Access,
                                               Index_Type (DataCount),
                                               Index_Type (DataCount));
         exception
               --  this should never happen
            when others =>
               --  since we failed to loan data to data seq, but data is already
               --  taken, we will need to return it still.
               --  Note that data will be lost in this case */
               --                 Self.Return_Loan (DataPtrArray'Address, Info_Seq);
               Return_Loan_UntypedI (Self, DataPtrArray'Address, DataCount, Info_Seq.all'Unrestricted_Access);
               raise;
         end;
      else
         --  data is already copied to dataSeqContiguousBuffer
         Data_Sequences.Set_Length (Received_Data, Index_Type (DataCount));
      end if;

      <<Done>>
      return Result;
   end Read_Or_Take_InstanceI;

   procedure Read_Or_Take_Instance_W_ConditionI
     (Self            : not null access constant Ref;
      Received_Data   : not null access Data_Sequences.Sequence;
      Info_Seq        : not null access DDS.SampleInfo_Seq.Sequence;
      Max_Samples     : DDS.long;
      Handle          : not null access constant DDS.InstanceHandle_T;
      Condition       : not null access constant DDS.ReadCondition.Ref'Class;
      Take            : DDS.Boolean)
   is
   begin
      Ret_Code_To_Exception (Self.Read_Or_Take_Instance_W_ConditionI (Received_Data, Info_Seq, Max_Samples, Handle, Condition, Take));
   end Read_Or_Take_Instance_W_ConditionI;

   function  Read_Or_Take_Instance_W_ConditionI
     (Self            : not null access constant Ref;
      Received_Data   : not null access Data_Sequences.Sequence;
      Info_Seq        : not null access DDS.SampleInfo_Seq.Sequence;
      Max_Samples     : DDS.long;
      Handle          : not null access constant DDS.InstanceHandle_T;
      Condition       : not null access constant DDS.ReadCondition.Ref'Class;
      Take            : DDS.Boolean) return DDS_ReturnCode_T
   is
   --              L_Is_Loan    : aliased DDS_Boolean;
   --        L_Data_Count : aliased Interfaces.C.int;
      DataSeqLen              : constant Index_Type := Data_Sequences.Get_Length (Received_Data);
      DataSeqMaxLen           : constant Index_Type := Data_Sequences.Get_Maximum (Received_Data);
      DataSeqHasOwnership     : constant DDS.Boolean := Data_Sequences.Has_Ownership (Received_Data);
      IsLoan                  : aliased DDS.Boolean := True;
      DataPtrArray            : aliased Data_Type_Access;
      DataCount               : aliased Natural;
      DataSeqContiguousBuffer : constant Data_Sequences.Memory_Element_Array_Pointer  := Data_Sequences.Get_Contiguous_BufferI (Received_Data);
      Result                  : DDS_ReturnCode_T;
   begin
      Result := Self.Read_Or_Take_Instance_W_Condition_UntypedI
        (IsLoan'Access,
         DataPtrArray'Address,
         DataCount'Access,
         Info_Seq.all'Unrestricted_Access,
         Standard.Long_Integer (DataSeqLen),
         Standard.Long_Integer (DataSeqMaxLen),
         Boolean'Pos (DataSeqHasOwnership),
         DataSeqContiguousBuffer'Address,
         (Data_Type'Object_Size + 1) / 8,
         Standard.Long_Integer (Max_Samples),
         Handle.all'Unrestricted_Access,
         Condition,
         Take);


      if Result = DDS_RETCODE_NO_DATA then
         Data_Sequences.Set_Length (Received_Data, 0);
         goto Done;
      end if;

      if Result /= DDS_RETCODE_OK then
         goto Done;
      end if;

      if IsLoan then
         --  /* loan buffer to sequence */
         begin
            Data_Sequences.Loan_Discontiguous (Received_Data,
                                               DataPtrArray'Unrestricted_Access,
                                               Index_Type (DataCount),
                                               Index_Type (DataCount));
         exception
               --  this should never happen
            when others =>
               --  since we failed to loan data to data seq, but data is already
               --  taken, we will need to return it still.
               --  Note that data will be lost in this case */
               --                 Self.Return_Loan (DataPtrArray'Address, Info_Seq);
               Return_Loan_UntypedI (Self, DataPtrArray'Address, DataCount, Info_Seq.all'Unrestricted_Access);
               raise;
         end;
      else
         --  data is already copied to dataSeqContiguousBuffer
         Data_Sequences.Set_Length (Received_Data, Index_Type (DataCount));
      end if;

      <<Done>>
      return Result;
   end Read_Or_Take_Instance_W_ConditionI;

   procedure  Read_Or_Take_Next_InstanceI
     (Self            : not null access constant Ref;
      Received_Data   : not null access Data_Sequences.Sequence;
      Info_Seq        : not null access DDS.SampleInfo_Seq.Sequence;
      Max_Samples     : DDS.long;
      Previous_Handle : not null access constant DDS.InstanceHandle_T;
      Sample_States   : DDS.SampleStateMask;
      View_States     : DDS.ViewStateMask;
      Instance_States : DDS.InstanceStateMask;
      Take            : DDS.Boolean) is
   begin
      Ret_Code_To_Exception (Self.Read_Or_Take_Next_InstanceI (Received_Data, Info_Seq, Max_Samples, Previous_Handle, Sample_States, View_States, Instance_States, Take));
   end Read_Or_Take_Next_InstanceI;

   function  Read_Or_Take_Next_InstanceI
     (Self            : not null access constant Ref;
      Received_Data   : not null access Data_Sequences.Sequence;
      Info_Seq        : not null access DDS.SampleInfo_Seq.Sequence;
      Max_Samples     : DDS.long;
      Previous_Handle : not null access constant DDS.InstanceHandle_T;
      Sample_States   : DDS.SampleStateMask;
      View_States     : DDS.ViewStateMask;
      Instance_States : DDS.InstanceStateMask;
      Take            : DDS.Boolean) return DDS_ReturnCode_T
   is
   --        L_Is_Loan    : aliased DDS_Boolean;
   --        L_Data_Count : aliased Interfaces.C.int;
      DataSeqLen              : constant Index_Type := Data_Sequences.Get_Length (Received_Data);
      DataSeqMaxLen           : constant Index_Type := Data_Sequences.Get_Maximum (Received_Data);
      DataSeqHasOwnership     : constant DDS.Boolean := Data_Sequences.Has_Ownership (Received_Data);
      IsLoan                  : aliased DDS.Boolean := True;
      DataPtrArray            : aliased Data_Type_Access;
      DataCount               : aliased Natural;
      DataSeqContiguousBuffer : constant Data_Sequences.Memory_Element_Array_Pointer  := Data_Sequences.Get_Contiguous_BufferI (Received_Data);
      Result                  : DDS_ReturnCode_T;
   begin
      Result := Self.Read_Or_Take_Next_Instance_UntypedI
        (IsLoan'Access,
         DataPtrArray'Address,
         DataCount'Access,
         Info_Seq.all'Unrestricted_Access,
         Standard.Long_Integer (DataSeqLen),
         Standard.Long_Integer (DataSeqMaxLen),
         Boolean'Pos (DataSeqHasOwnership),
         DataSeqContiguousBuffer'Address,
         (Data_Type'Object_Size + 1) / 8,
         Standard.Long_Integer (Max_Samples),
         Previous_Handle.all'Unrestricted_Access,
         Sample_States,
         View_States,
         Instance_States,
         Take);


      if Result = DDS_RETCODE_NO_DATA then
         Data_Sequences.Set_Length (Received_Data, 0);
         goto Done;
      end if;

      if Result /= DDS_RETCODE_OK then
         goto Done;
      end if;

      if IsLoan then
         --  /* loan buffer to sequence */
         begin
            Data_Sequences.Loan_Discontiguous (Received_Data,
                                               DataPtrArray'Unrestricted_Access,
                                               Index_Type (DataCount),
                                               Index_Type (DataCount));
         exception
               --  this should never happen
            when others =>
               --  since we failed to loan data to data seq, but data is already
               --  taken, we will need to return it still.
               --  Note that data will be lost in this case */
               --  self.Return_Loan (DataPtrArray'Address, Info_Seq);
               Return_Loan_UntypedI (Self, DataPtrArray'Address, DataCount, Info_Seq.all'Unrestricted_Access);
               raise;
         end;
      else
         --  data is already copied to dataSeqContiguousBuffer
         Data_Sequences.Set_Length (Received_Data, Index_Type (DataCount));
      end if;

      <<Done>>
      return Result;
   end Read_Or_Take_Next_InstanceI;

   procedure Read_Or_Take_Next_Instance_W_ConditionI
     (Self            : not null access constant Ref;
      Received_Data   : not null access Data_Sequences.Sequence;
      Info_Seq        : not null access DDS.SampleInfo_Seq.Sequence;
      Max_Samples     : DDS.long;
      Previous_Handle : not null access constant DDS.InstanceHandle_T;
      Condition       : not null access constant DDS.ReadCondition.Ref'Class;
      Take            : DDS.Boolean) is
   begin
      Ret_Code_To_Exception (Self.Read_Or_Take_Next_Instance_W_ConditionI (Received_Data, Info_Seq, Max_Samples, Previous_Handle, Condition, Take));
   end Read_Or_Take_Next_Instance_W_ConditionI;

   function Read_Or_Take_Next_Instance_W_ConditionI
     (Self            : not null access constant Ref;
      Received_Data   : not null access Data_Sequences.Sequence;
      Info_Seq        : not null access DDS.SampleInfo_Seq.Sequence;
      Max_Samples     : DDS.long;
      Previous_Handle : not null access constant DDS.InstanceHandle_T;
      Condition       : not null access constant DDS.ReadCondition.Ref'Class;
      Take            : DDS.Boolean) return DDS_ReturnCode_T
   is
   --        L_Is_Loan    : aliased DDS_Boolean;
   --        L_Data_Count : aliased Interfaces.C.int;
      DataSeqLen              : constant Index_Type := Data_Sequences.Get_Length (Received_Data);
      DataSeqMaxLen           : constant Index_Type := Data_Sequences.Get_Maximum (Received_Data);
      DataSeqHasOwnership     : constant DDS.Boolean := Data_Sequences.Has_Ownership (Received_Data);
      IsLoan                  : aliased DDS.Boolean := True;
      DataPtrArray            : aliased Data_Type_Access;
      DataCount               : aliased Natural;
      DataSeqContiguousBuffer : constant Data_Sequences.Memory_Element_Array_Pointer  := Data_Sequences.Get_Contiguous_BufferI (Received_Data);
      Result                  : DDS_ReturnCode_T;

   begin
      Result := Self.Read_Or_Take_Next_Instance_W_Condition_UntypedI
        (IsLoan'Access,
         DataPtrArray'Address,
         DataCount'Access,
         Info_Seq.all'Unrestricted_Access,
         Standard.Long_Integer (DataSeqLen),
         Standard.Long_Integer (DataSeqMaxLen),
         Boolean'Pos (DataSeqHasOwnership),
         DataSeqContiguousBuffer'Address,
         (Data_Type'Object_Size + 1) / 8,
         Standard.Long_Integer (Max_Samples),
         Previous_Handle.all'Unrestricted_Access,
         Condition,
         Take);

      if Result = DDS_RETCODE_NO_DATA then
         Data_Sequences.Set_Length (Received_Data, 0);
         goto Done;
      end if;

      if Result /= DDS_RETCODE_OK then
         goto Done;
      end if;

      if IsLoan then
         --  /* loan buffer to sequence */
         begin
            Data_Sequences.Loan_Discontiguous (Received_Data,
                                               DataPtrArray'Access,
                                               Index_Type (DataCount),
                                               Index_Type (DataCount));
         exception
               --  this should never happen
            when others =>
               --  since we failed to loan data to data seq, but data is already
               --  taken, we will need to return it still.
               --  Note that data will be lost in this case */
               Return_Loan_UntypedI (Self, DataPtrArray'Address, DataCount, Info_Seq.all'Unrestricted_Access);
               raise;
         end;
      else
         --  data is already copied to dataSeqContiguousBuffer
         Data_Sequences.Set_Length (Received_Data, Index_Type (DataCount));
      end if;

      <<Done>>
      return Result;
   end Read_Or_Take_Next_Instance_W_ConditionI;



   ----------------------------------------
   -- Calls to low level DDS functions   --
   -- To be moved to dds-datareader_impl --
   ----------------------------------------


   function Return_Loan_UntypedI
     (Self          : not null access constant Ref;
      Received_Data : System.Address;
      Data_Count    : Natural;
      Info_Seq      : not null Standard.DDS.SampleInfo_Seq.Sequence_Access)  return DDS_ReturnCode_T
   is
      type DDS_SampleInfoSeq_Access is access all DDS_SampleInfoSeq;
      function Convert is new Ada.Unchecked_Conversion (SampleInfo_Seq.Sequence_Access, DDS_SampleInfoSeq_Access);
   begin
      return Ret : DDS_ReturnCode_T do
         Ret := DDS_DataReader_Return_Loan_UntypedI
           (Self          => Self.GetInterface,
            Received_Data => Received_Data,
            DataCount     => Interfaces.C.int (Data_Count),
            Info_Seq      => Convert (Info_Seq));
         if  Ret /= DDS_RETCODE_OK then
            Debug_Trap;
         end if;
      end return;
   end Return_Loan_UntypedI;

   procedure Return_Loan_UntypedI
     (Self          : not null access constant Ref;
      Received_Data : System.Address;
      Data_Count    : Natural;
      Info_Seq      : not null Standard.DDS.SampleInfo_Seq.Sequence_Access)
   is
      type DDS_SampleInfoSeq_Access is access all DDS_SampleInfoSeq;
      function Convert is new Ada.Unchecked_Conversion (SampleInfo_Seq.Sequence_Access, DDS_SampleInfoSeq_Access);
   begin
      Ret_Code_To_Exception (DDS_DataReader_Return_Loan_UntypedI
                             (Self          => Self.GetInterface,
                              Received_Data => Received_Data,
                              DataCount     => Interfaces.C.int (Data_Count),
                              Info_Seq      => Convert (Info_Seq)));
   end Return_Loan_UntypedI;

   procedure Get_Key_Value_UntypedI (Self          : not null access constant Ref;
                                     Key_Holder    : System.Address;
                                     Handle        : access constant DDS.InstanceHandle_T)
   is
      type DDS_InstanceHandle_T_Access is access all DDS_InstanceHandle_T;
      function ConvertHandle is new Ada.Unchecked_Conversion (System.Address, DDS_InstanceHandle_T_Access);
   begin
      Ret_Code_To_Exception (DDS_DataReader_Get_Key_Value_UntypedI
                             (Self       => Self.GetInterface,
                              Key_Holder => Key_Holder,
                              Handle     => ConvertHandle (Handle.all'Address)));
   end Get_Key_Value_UntypedI;

   function Lookup_Instance_UntypedI (Self          : not null access constant Ref;
                                      Key_Holder    : System.Address) return DDS.InstanceHandle_T
   is
      function ConvertHandle is new Ada.Unchecked_Conversion (DDS_InstanceHandle_T, DDS.InstanceHandle_T);
   begin
      return Ret :  DDS.InstanceHandle_T do
         Ret := ConvertHandle (DDS_DataReader_Lookup_Instance_UntypedI (Self.GetInterface,
                               Key_Holder));

      end return;
   end Lookup_Instance_UntypedI;

   function Read_Or_Take_W_Condition_UntypedI
     (Self                                : not null access constant Ref;
      Is_Loan                             : access DDS.Boolean;
      Received_Data                       : System.Address;
      Data_Count                          : access Natural;
      Info_Seq                            : SampleInfo_Seq.Sequence_Access;
      Data_Seq_Len                        : Long_Integer;
      Data_Seq_Max_Len                    : Long_Integer;
      Data_Seq_Has_Ownership              : Long_Integer;
      Data_Seq_Contiguous_Buffer_For_Copy : System.Address;
      Data_Size                           : Integer;
      Max_Samples                         : Long_Integer;
      Condition                           : not null access DDS.ReadCondition.Ref'Class;
      Take                                : DDS.Boolean) return DDS_ReturnCode_T
   is
      L_Is_Loan    : aliased DDS_Boolean := 0;
      L_Data_Count : aliased Interfaces.C.int := 0;
      type DDS_SampleInfoSeq_Access is access all DDS_SampleInfoSeq;
      function Convert is new Ada.Unchecked_Conversion (SampleInfo_Seq.Sequence_Access, DDS_SampleInfoSeq_Access);
      C            : constant Standard.DDS.ReadCondition_Impl.Ref_Access :=
                       Standard.DDS.ReadCondition_Impl.Ref_Access (Condition);
   begin
      return Ret : DDS_ReturnCode_T do
         Ret := DDS_DataReader_Read_Or_Take_W_Condition_UntypedI
           (Self                                => Self.GetInterface,
            Is_Loan                             => L_Is_Loan'Access,
            Received_Data                       => Received_Data,
            Data_Count                          => L_Data_Count'Access,
            Info_Seq                            => Convert (Info_Seq),
            Data_Seq_Len                        => Interfaces.C.int (Data_Seq_Len),
            Data_Seq_Max_Len                    => Interfaces.C.int (Data_Seq_Max_Len),
            Data_Seq_Has_Ownership              => Interfaces.C.Unsigned_Char (Data_Seq_Has_Ownership),
            Data_Seq_Contiguous_Buffer_For_Copy => Data_Seq_Contiguous_Buffer_For_Copy,
            Data_Size                           => Interfaces.C.int (Data_Size),
            Max_Samples                         => Interfaces.C.int (Max_Samples),
            Condition                           => C.GetInterface,
            Take                                => Boolean'Pos (Take));
         Is_Loan.all := L_Is_Loan /= 0;
         Data_Count.all := Natural (L_Data_Count);
      end return;
   end Read_Or_Take_W_Condition_UntypedI;

   function Read_Or_Take_Instance_UntypedI
     (Self                                : not null access constant Ref;
      Is_Loan                             : access DDS.Boolean;
      Received_Data                       : System.Address;
      Data_Count                          : access Natural;
      Info_Seq                            : SampleInfo_Seq.Sequence_Access;
      Data_Seq_Len                        : Long_Integer;
      Data_Seq_Max_Len                    : Long_Integer;
      Data_Seq_Has_Ownership              : Long_Integer;
      Data_Seq_Contiguous_Buffer_For_Copy : System.Address;
      Data_Size                           : Integer;
      Max_Samples                         : Long_Integer;
      Handle                              : DDS.InstanceHandle_T_Access;
      Sample_States                       : DDS.SampleStateMask;
      View_States                         : DDS.ViewStateMask;
      Instance_States                     : DDS.InstanceStateMask;
      Take                                : DDS.Boolean) return DDS_ReturnCode_T
   is
      L_Is_Loan    : aliased DDS_Boolean;
      L_Data_Count : aliased Interfaces.C.int;
      type DDS_InstanceHandle_T_Access is access all DDS_InstanceHandle_T;
      function ConvertHandle is new Ada.Unchecked_Conversion (DDS.InstanceHandle_T_Access, DDS_InstanceHandle_T_Access);
      type DDS_SampleInfoSeq_Access is access all DDS_SampleInfoSeq;
      function Convert is new Ada.Unchecked_Conversion (SampleInfo_Seq.Sequence_Access, DDS_SampleInfoSeq_Access);

   begin
      return Ret : DDS_ReturnCode_T do
         Ret := DDS_DataReader_Read_Or_Take_Instance_UntypedI
           (Self                                => Self.GetInterface,
            Is_Loan                             => L_Is_Loan'Access,
            Received_Data                       => Received_Data,
            Data_Count                          => L_Data_Count'Access,
            Info_Seq                            => Convert (Info_Seq),
            Data_Seq_Len                        => Interfaces.C.int (Data_Seq_Len),
            Data_Seq_Max_Len                    => Interfaces.C.int (Data_Seq_Max_Len),
            Data_Seq_Has_Ownership              => Interfaces.C.Unsigned_Char (Data_Seq_Has_Ownership),
            Data_Seq_Contiguous_Buffer_For_Copy => Data_Seq_Contiguous_Buffer_For_Copy,
            Data_Size                           => Interfaces.C.int (Data_Size),
            Max_Samples                         => Interfaces.C.int (Max_Samples),
            A_Handle                            => ConvertHandle (Handle),
            Topic_Query_Guid                    => null,
            Sample_States                       => Interfaces.C.Unsigned (Sample_States),
            View_States                         => Interfaces.C.Unsigned (View_States),
            Instance_States                     => Interfaces.C.Unsigned (Instance_States),
            Take                                => Boolean'Pos (Take));
         Is_Loan.all := Boolean'Val (L_Is_Loan);
         Data_Count.all := Natural (L_Data_Count);
      end return;
   end Read_Or_Take_Instance_UntypedI;

   function Read_Or_Take_Instance_W_Condition_UntypedI
     (Self                                : not null access constant Ref;
      Is_Loan                             : access DDS.Boolean;
      Received_Data                       : System.Address;
      Data_Count                          : access Natural;
      Info_Seq                            : SampleInfo_Seq.Sequence_Access;
      Data_Seq_Len                        : Long_Integer;
      Data_Seq_Max_Len                    : Long_Integer;
      Data_Seq_Has_Ownership              : Long_Integer;
      Data_Seq_Contiguous_Buffer_For_Copy : System.Address;
      Data_Size                           : Integer;
      Max_Samples                         : Long_Integer;
      Handle                              : DDS.InstanceHandle_T_Access;
      Condition                           : not null access DDS.ReadCondition.Ref'Class;
      Take                                : DDS.Boolean) return DDS_ReturnCode_T
   is
      L_Is_Loan    : aliased DDS_Boolean;
      L_Data_Count : aliased Interfaces.C.int;
      type DDS_InstanceHandle_T_Access is access all DDS_InstanceHandle_T;
      function ConvertHandle is new Ada.Unchecked_Conversion (DDS.InstanceHandle_T_Access, DDS_InstanceHandle_T_Access);
      type DDS_SampleInfoSeq_Access is access all DDS_SampleInfoSeq;
      function Convert is new Ada.Unchecked_Conversion (SampleInfo_Seq.Sequence_Access, DDS_SampleInfoSeq_Access);
      C            : constant Standard.DDS.ReadCondition_Impl.Ref_Access := Standard.DDS.ReadCondition_Impl.Ref_Access (Condition);
   begin
      return Ret : DDS_ReturnCode_T do
         Ret := DDS_DataReader_Read_Or_Take_Instance_W_Condition_UntypedI
           (Self                                       => Self.GetInterface,
            Is_Loan                                    => L_Is_Loan'Access,
            Received_Data                              => Received_Data,
            Data_Count                                 => L_Data_Count'Access,
            Info_Seq                                   => Convert (Info_Seq),
            DataSeqLen                                 => Interfaces.C.int (Data_Seq_Len),
            DataSeqMaxLen                              => Interfaces.C.int (Data_Seq_Max_Len),
            DataSeqHasOwnership                        => Interfaces.C.Unsigned_Char (Data_Seq_Has_Ownership),
            Data_Seq_Contiguous_Buffer_For_Copy        => Data_Seq_Contiguous_Buffer_For_Copy,
            Data_Size                                  => Interfaces.C.int (Data_Size),
            Max_Samples                                => Interfaces.C.int (Max_Samples),
            Previous_Handle                            => ConvertHandle (Handle),
            Condition                                  => C.GetInterface,
            Take                                       => Boolean'Pos (Take));
         Is_Loan.all := Boolean'Val (L_Is_Loan);
         Data_Count.all := Natural (L_Data_Count);
      end return;
   end Read_Or_Take_Instance_W_Condition_UntypedI;

   function Read_Or_Take_Next_Instance_UntypedI
     (Self                                : not null access constant Ref;
      Is_Loan                             : access DDS.Boolean;
      Received_Data                       : System.Address;
      Data_Count                          : access Natural;
      Info_Seq                            : SampleInfo_Seq.Sequence_Access;
      Data_Seq_Len                        : Long_Integer;
      Data_Seq_Max_Len                    : Long_Integer;
      Data_Seq_Has_Ownership              : Long_Integer;
      Data_Seq_Contiguous_Buffer_For_Copy : System.Address;
      Data_Size                           : Integer;
      Max_Samples                         : Long_Integer;
      Previous_Handle                     : DDS.InstanceHandle_T_Access;
      Sample_States                       : DDS.SampleStateMask;
      View_States                         : DDS.ViewStateMask;
      Instance_States                     : DDS.InstanceStateMask;
      Take                                : DDS.Boolean) return DDS_ReturnCode_T
   is
      L_Is_Loan    : aliased DDS_Boolean;
      L_Data_Count : aliased Interfaces.C.int;
      type DDS_InstanceHandle_T_Access is access all DDS_InstanceHandle_T;
      function ConvertHandle is new Ada.Unchecked_Conversion (DDS.InstanceHandle_T_Access, DDS_InstanceHandle_T_Access);
      type DDS_SampleInfoSeq_Access is access all DDS_SampleInfoSeq;
      function Convert is new Ada.Unchecked_Conversion (SampleInfo_Seq.Sequence_Access, DDS_SampleInfoSeq_Access);
   begin
      return Ret : DDS_ReturnCode_T do
         Ret := DDS_DataReader_Read_Or_Take_Next_Instance_UntypedI
           (Self                                => Self.GetInterface,
            Is_Loan                             => L_Is_Loan'Access,
            Received_Data                       => Received_Data,
            Data_Count                          => L_Data_Count'Access,
            Info_Seq                            => Convert (Info_Seq),
            Data_Seq_Len                        => Interfaces.C.int (Data_Seq_Len),
            Data_Seq_Max_Len                    => Interfaces.C.int (Data_Seq_Max_Len),
            Data_Seq_Has_Ownership              => Interfaces.C.Unsigned_Char (Data_Seq_Has_Ownership),
            Data_Seq_Contiguous_Buffer_For_Copy => Data_Seq_Contiguous_Buffer_For_Copy,
            Data_Size                           => Interfaces.C.int (Data_Size),
            Max_Samples                         => Interfaces.C.int (Max_Samples),
            Previous_Handle                     => ConvertHandle (Previous_Handle),
            Sample_States                       => Interfaces.C.Unsigned (Sample_States),
            View_States                         => Interfaces.C.Unsigned (View_States),
            Instance_States                     => Interfaces.C.Unsigned (Instance_States),
            Take                                => Boolean'Pos (Take));
         Is_Loan.all := Boolean'Val (L_Is_Loan);
         Data_Count.all := Natural (L_Data_Count);
      end return;
   end Read_Or_Take_Next_Instance_UntypedI;

   function Read_Or_Take_Next_Instance_W_Condition_UntypedI
     (Self                                : not null access constant Ref;
      Is_Loan                             : access DDS.Boolean;
      Received_Data                       : System.Address;
      Data_Count                          : access Natural;
      Info_Seq                            : SampleInfo_Seq.Sequence_Access;
      Data_Seq_Len                        : Long_Integer;
      Data_Seq_Max_Len                    : Long_Integer;
      Data_Seq_Has_Ownership              : Long_Integer;
      Data_Seq_Contiguous_Buffer_For_Copy : System.Address;
      Data_Size                           : Integer;
      Max_Samples                         : Long_Integer;
      Previous_Handle                     : DDS.InstanceHandle_T_Access;
      Condition                           : not null access DDS.ReadCondition.Ref'Class;
      Take                                : DDS.Boolean) return DDS_ReturnCode_T
   is
      L_Is_Loan    : aliased DDS_Boolean;
      L_Data_Count : aliased Interfaces.C.int;
      type DDS_InstanceHandle_T_Access is access all DDS_InstanceHandle_T;
      function ConvertHandle is new Ada.Unchecked_Conversion (DDS.InstanceHandle_T_Access, DDS_InstanceHandle_T_Access);
      type DDS_SampleInfoSeq_Access is access all DDS_SampleInfoSeq;
      function Convert is new Ada.Unchecked_Conversion (SampleInfo_Seq.Sequence_Access, DDS_SampleInfoSeq_Access);

      C                       : constant Standard.DDS.ReadCondition_Impl.Ref_Access :=
                                  Standard.DDS.ReadCondition_Impl.Ref_Access (Condition);

   begin
      return Ret : DDS_ReturnCode_T do
         Ret := DDS_DataReader_Read_Or_Take_Next_Instance_W_Condition_UntypedI
           (Self                                => Self.GetInterface,
            Is_Loan                             => L_Is_Loan'Access,
            Received_Data                       => Received_Data,
            Data_Count                          => L_Data_Count'Access,
            Info_Seq                            => Convert (Info_Seq),
            DataSeqLen                          => Interfaces.C.int (Data_Seq_Len),
            DataSeqMaxLen                       => Interfaces.C.int (Data_Seq_Max_Len),
            DataSeqHasOwnership                 => Interfaces.C.Unsigned_Char (Data_Seq_Has_Ownership),
            Data_Seq_Contiguous_Buffer_For_Copy => Data_Seq_Contiguous_Buffer_For_Copy,
            Data_Size                           => Interfaces.C.int (Data_Size),
            Max_Samples                         => Interfaces.C.int (Max_Samples),
            Previous_Handle                     => ConvertHandle (Previous_Handle),
            Condition                           => C.GetInterface,
            Take                                => Boolean'Pos (Take));
         Is_Loan.all := Boolean'Val (L_Is_Loan);
         Data_Count.all := Natural (L_Data_Count);
      end return;
   end Read_Or_Take_Next_Instance_W_Condition_UntypedI;

   function Read_Or_Take_Next_Sample_UntypedI
     (Self                                : not null access constant Ref;
      Received_Data                       : System.Address;
      Sample_Info                         : DDS.SampleInfo_Access;
      Take                                : DDS.Boolean) return DDS_ReturnCode_T
   is
      type DDS_SampleInfo_Access is access all DDS_SampleInfo;
      function Convert is new Ada.Unchecked_Conversion (DDS.SampleInfo_Access, DDS_SampleInfo_Access);
   begin
      return Ret : DDS_ReturnCode_T do
         Ret := DDS_DataReader_Read_Or_Take_Next_Sample_UntypedI
           (Self                                => Self.GetInterface,
            Received_Data                       => Received_Data,
            Info_Seq                            => Convert (Sample_Info),
            Take                                => Boolean'Pos (Take));
      end return;
   end Read_Or_Take_Next_Sample_UntypedI;

   -----------------------------------------------------------------------------
   --- End of lowlevel stuff
   -----------------------------------------------------------------------------

   function I_First_Element (Self : Container) return Cursor is
      pragma Unreferenced (Self);
   begin
      return Cursor (First_Element);
   end I_First_Element;

   function I_Advance (Self : Container; C : Cursor) return Cursor is
      pragma Unreferenced (Self);
   begin
      return C + 1;
   end I_Advance;

   function I_Has_Element (Self : Container; C : Cursor) return Standard.Boolean is
   begin
      return C <= Cursor (Data_Sequences.Get_Length (Self.Received_Data'Access));
   end I_Has_Element;

   function I_Get_Element (Self : Container; C : Cursor) return Element_Type is
   begin
      return Ret : Element_Type do
         Ret.Data := Data_Sequences.Get_Reference (Self  => Self.Received_Data'Access,
                                                   Index => Index_Type (C));
         Ret.Sample_Info := SampleInfo_Seq.Get_Reference (Self  => Self.Info_Seq'Access,
                                                          Index => Natural (C));
      end return;
   end I_Get_Element;


   function Take
     (Self            : not null access Ref;
      Max_Samples     : in DDS.long := DDS.LENGTH_UNLIMITED;
      Sample_States   : in DDS.SampleStateMask := DDS.ANY_SAMPLE_STATE;
      View_States     : in DDS.ViewStateMask := DDS.ANY_VIEW_STATE;
      Instance_States : in DDS.InstanceStateMask := DDS.ANY_INSTANCE_STATE) return Container'Class is
   begin
      return Ret : Container do
         Ret.Reader := Self.all'Access;
         Ret.Retcode := Self.Read_Or_TakeI (Ret.Received_Data'Access,
                                            Ret.Info_Seq'Access,
                                            Max_Samples,
                                            Sample_States,
                                            View_States,
                                            Instance_States, Take => True);
         if Ret.Retcode not in DDS_RETCODE_OK |  DDS_RETCODE_NO_DATA then
            Ret_Code_To_Exception (Ret.Retcode);
         end if;
      end return;
   end Take;

   function Read_W_Condition
     (Self          : not null access Ref;
      Max_Samples   : in DDS.long := DDS.LENGTH_UNLIMITED;
      Condition     : access DDS.ReadCondition.Ref'Class) return Container'Class is
   begin
      return Ret : Container do
         Ret.Reader := Self.all'Access;
         Ret.Retcode := Self.Read_W_Condition (Ret.Received_Data'Address,
                                               Ret.Info_Seq'Access,
                                               Max_Samples,
                                               Condition);
         if Ret.Retcode not in DDS_RETCODE_OK |  DDS_RETCODE_NO_DATA then
            Ret_Code_To_Exception (Ret.Retcode);
         end if;
      end return;
   end Read_W_Condition;

   function Take_W_Condition
     (Self          : not null access Ref;
      Max_Samples   : in DDS.long := DDS.LENGTH_UNLIMITED;
      Condition     : access DDS.ReadCondition.Ref'Class) return Container'Class is
   begin
      return Ret : Container do
         Ret.Reader := Self.all'Access;
         Ret.Retcode := Self.Take_W_Condition (Ret.Received_Data'Address,
                                               Ret.Info_Seq'Access,
                                               Max_Samples,
                                               Condition);
         if Ret.Retcode not in DDS_RETCODE_OK |  DDS_RETCODE_NO_DATA then
            Ret_Code_To_Exception (Ret.Retcode);
         end if;
      end return;
   end Take_W_Condition;

   function Read_Instance
     (Self            : not null access Ref;
      Max_Samples     : in DDS.long := DDS.LENGTH_UNLIMITED;
      A_Handle        : access constant DDS.InstanceHandle_T;
      Sample_States   : in DDS.SampleStateMask := DDS.ANY_SAMPLE_STATE;
      View_States     : in DDS.ViewStateMask := DDS.ANY_VIEW_STATE;
      Instance_States : in DDS.InstanceStateMask := DDS.ANY_INSTANCE_STATE) return Container'Class is
   begin
      return Ret : Container do
         Ret.Reader := Self.all'Access;
         Ret.Retcode := Self.Read_Or_Take_InstanceI (Ret.Received_Data'Access,
                                                     Ret.Info_Seq'Access,
                                                     Max_Samples,
                                                     A_Handle,
                                                     Sample_States,
                                                     View_States,
                                                     Instance_States,
                                                     False);

         if Ret.Retcode not in DDS_RETCODE_OK |  DDS_RETCODE_NO_DATA then
            Ret_Code_To_Exception (Ret.Retcode);
         end if;
      end return;
   end Read_Instance;

   function Take_Instance
     (Self            : not null access Ref;
      Max_Samples     : in DDS.long := DDS.LENGTH_UNLIMITED;
      A_Handle        : access constant DDS.InstanceHandle_T;
      Sample_States   : in DDS.SampleStateMask := DDS.ANY_SAMPLE_STATE;
      View_States     : in DDS.ViewStateMask := DDS.ANY_VIEW_STATE;
      Instance_States : in DDS.InstanceStateMask := DDS.ANY_INSTANCE_STATE) return Container'Class is
   begin
      return Ret : Container do
         Ret.Reader := Self.all'Access;
         Ret.Retcode := Self.Read_Or_Take_InstanceI (Ret.Received_Data'Access,
                                                     Ret.Info_Seq'Access,
                                                     Max_Samples,
                                                     A_Handle,
                                                     Sample_States,
                                                     View_States,
                                                     Instance_States,
                                                     True);
         if Ret.Retcode not in DDS_RETCODE_OK |  DDS_RETCODE_NO_DATA then
            Ret_Code_To_Exception (Ret.Retcode);
         end if;
      end return;
   end Take_Instance;

   function Read_Next_Instance
     (Self            : not null access Ref;
      Max_Samples     : in DDS.long := DDS.LENGTH_UNLIMITED;
      Previous_Handle : access constant DDS.InstanceHandle_T;
      Sample_States   : in DDS.SampleStateMask := DDS.ANY_SAMPLE_STATE;
      View_States     : in DDS.ViewStateMask := DDS.ANY_VIEW_STATE;
      Instance_States : in DDS.InstanceStateMask := DDS.ANY_INSTANCE_STATE) return Container'Class is
   begin
      return Ret : Container do
         Ret.Reader := Self.all'Access;

         Ret.Retcode := Self.Read_Or_Take_Next_InstanceI (Ret.Received_Data'Access,
                                                          Ret.Info_Seq'Access,
                                                          Max_Samples,
                                                          Previous_Handle,
                                                          Sample_States,
                                                          View_States,
                                                          Instance_States,
                                                          Take => False);
         if Ret.Retcode not in DDS_RETCODE_OK |  DDS_RETCODE_NO_DATA then
            Ret_Code_To_Exception (Ret.Retcode);
         end if;
      end return;
   end Read_Next_Instance;

   function Take_Next_Instance
     (Self            : not null access Ref;
      Max_Samples     : in DDS.long := DDS.LENGTH_UNLIMITED;
      Previous_Handle : access constant DDS.InstanceHandle_T;
      Sample_States   : in DDS.SampleStateMask := DDS.ANY_SAMPLE_STATE;
      View_States     : in DDS.ViewStateMask := DDS.ANY_VIEW_STATE;
      Instance_States : in DDS.InstanceStateMask := DDS.ANY_INSTANCE_STATE) return Container'Class is
   begin
      return Ret : Container do
         Ret.Reader := Self.all'Access;
         Ret.Retcode := Self.Read_Or_Take_Next_InstanceI (Ret.Received_Data'Access,
                                                          Ret.Info_Seq'Access,
                                                          Max_Samples,
                                                          Previous_Handle,
                                                          Sample_States,
                                                          View_States,
                                                          Instance_States,
                                                          Take => True);
         if Ret.Retcode not in DDS_RETCODE_OK |  DDS_RETCODE_NO_DATA then
            Ret_Code_To_Exception (Ret.Retcode);
         end if;
      end return;
   end Take_Next_Instance;

   function Read_Next_Instance_W_Condition
     (Self            : not null access Ref;
      Max_Samples     : in DDS.long := DDS.LENGTH_UNLIMITED;
      Previous_Handle : access constant DDS.InstanceHandle_T;
      Condition       : DDS.ReadCondition.Ref_Access)  return Container'Class is
   begin
      return Ret : Container do
         Ret.Reader := Self.all'Access;
         Ret.Retcode := Self.Read_Next_Instance_W_Condition (Ret.Received_Data'Address,
                                                             Ret.Info_Seq'Access,
                                                             Max_Samples,
                                                             Previous_Handle,
                                                             Condition);
         if Ret.Retcode not in DDS_RETCODE_OK |  DDS_RETCODE_NO_DATA then
            Ret_Code_To_Exception (Ret.Retcode);
         end if;
      end return;
   end Read_Next_Instance_W_Condition;

   procedure Take_Next_Instance_W_Condition
     (Self            : not null access constant Ref;
      Received_Data   : in out Data_Sequences.Sequence;
      Info_Seq        : in out DDS.SampleInfo_Seq.Sequence;
      Max_Samples     : in DDS.long := DDS.LENGTH_UNLIMITED;
      Previous_Handle : access constant DDS.InstanceHandle_T;
      Condition       : DDS.ReadCondition.Ref_Access) is
   begin
      Self.Take_Next_Instance_W_Condition (Received_Data'Unrestricted_Access,
                                           Info_Seq'Unrestricted_Access,
                                           Max_Samples,
                                           Previous_Handle,
                                           Condition);

   end Take_Next_Instance_W_Condition;

   function Take_Next_Instance_W_Condition
     (Self            : not null access Ref;
      Max_Samples     : in DDS.long := DDS.LENGTH_UNLIMITED;
      Previous_Handle : access constant DDS.InstanceHandle_T;
      Condition       : DDS.ReadCondition.Ref_Access)  return Container'Class is
   begin
      return Ret : Container do
         Ret.Reader := Self.all'Access;
         Ret.Retcode := Self.Take_Next_Instance_W_Condition (Ret.Received_Data'Address,
                                                             Ret.Info_Seq'Access,
                                                             Max_Samples,
                                                             Previous_Handle,
                                                             Condition);

         if Ret.Retcode not in DDS_RETCODE_OK |  DDS_RETCODE_NO_DATA then
            Ret_Code_To_Exception (Ret.Retcode);
         end if;
      end return;
   end Take_Next_Instance_W_Condition;


   function Is_Data_Consistent
     (Self          : not null access constant Ref;
      Sample        : in not null Data_Type_Access;
      Sample_Info   : in not null DDS.SampleInfo_Access) return Standard.Boolean is
   begin
      return Self.Is_Data_ConsistentI (Sample.all'Address, Sample_Info, MetpImpl);
   end Is_Data_Consistent;


   procedure Finalize (Self : in out Container) is
   begin
      if Self.Retcode = DDS_RETCODE_OK then
         Self.Reader.Return_Loan (Self.Received_Data'Access, Self.Info_Seq'Access);
      end if;
   exception
         --  Safeguard to guarantie that no exceptions are thrown back
         --  at calling the C-Thread.
      when others =>
         null;
   end Finalize;

end DDS.Typed_DataReader_Generic;
