package DDS.Request_Reply.connext_c_untyped_impl is

   function RTI_Connext_EntityUntypedImpl_Wait_For_Samples
     (Self              : not null access RTI_Connext_EntityUntypedImpl;
      Max_Wait          : DDS.Duration_T;
      Min_Sample_Count  : DDS.Natural;
      Waitset           : not null DDS.WaitSet.Ref_Access;
      Initial_Condition : DDS.ReadCondition.Ref_Access;
      Condition         : DDS.ReadCondition.Ref_Access) return Dds.ReturnCode_T;

 
   function RTI_Connext_EntityUntypedImpl_Get_Sample_Loaned
     (Self                    : not null access  RTI_Connext_EntityUntypedImpl;
      Received_Data           : System.Address;
      Data_Count              : out DDS.Integer;
      Is_Loan                 : DDS.Boolean;
      DataSeqContiguousBuffer : System.Address;
      Info_Seq                : not null access DDS.SampleInfo_Seq.Sequence;
      Data_Seq_Len            : DDS.long;
      Data_Seq_Max_Len        : DDS.long;
      Ownership               : DDS.Boolean;
      Max_Samples             : DDS.long;
      Read_Condition          : DDS.ReadCondition.Ref_Access;
      Take                    : DDS.Boolean);
 
   
   
   function RTI_Connext_EntityUntypedImpl_Send_Sample
     (Self : not null access RTI_Connext_EntityUntypedImpl;
      Data : System.Address;
      Info : not null access DDS.WriteParams_T) return DDS.ReturnCode_T;

   --  extern XMQCDllExport
   --  DDS_ReturnCode_t RTI_Connext_EntityUntypedImpl_wait_for_any_sample(
   --      struct RTI_Connext_EntityUntypedImpl* self,
   --      const struct DDS_Duration_t* max_wait,
   --      int min_sample_count);
   --  
   --  extern XMQCDllExport
   --  DDS_ReturnCode_t RTI_Connext_EntityUntypedImpl_return_loan(
   --      struct RTI_Connext_EntityUntypedImpl* self,
   --      void ** dataArray,
   --      struct DDS_SampleInfoSeq* info_seq);
   --  
   --  extern XMQCDllExport
   --  DDS_DataWriter* RTI_Connext_EntityUntypedImpl_get_datawriter(
   --      struct RTI_Connext_EntityUntypedImpl* self);
   --  
   --  extern XMQCDllExport
   --  DDS_DataReader* RTI_Connext_EntityUntypedImpl_get_datareader(
   --      struct RTI_Connext_EntityUntypedImpl* self);
   --  
   --  extern XMQCDllExport
   --  DDS_ReturnCode_t RTI_Connext_EntityUntypedImpl_delete(
   --      struct RTI_Connext_EntityUntypedImpl * self);
   --  
   --  extern XMQCDllExport
   --  RTIBool RTI_Connext_EntityUntypedImpl_validate_receive_params(
   --      const struct RTI_Connext_EntityUntypedImpl * self,
   --      const char * METHOD_NAME,
   --      DDS_Long min_count,
   --      DDS_Long max_count,
   --      const struct DDS_Duration_t * max_wait);
   --  
   --  #endif /* connext_c_untyped_impl_h */
end DDS.Request_Reply.connext_c_untyped_impl;
