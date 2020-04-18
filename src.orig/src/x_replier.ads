with Interfaces.C.Extensions;
with DDS.DataReader;
with Dds;
package Replier is
   type RTI_Connext_ReplierParams is new Integer;
   type RTI_Connext_EntityParams is new Integer;

   type RTI_Connext_Replier is abstract tagged record
      null;
   end record;

   procedure RTI_Connext_Replier_On_Data_Available (Listener_Data : Interfaces.C.Extensions.Void_Ptr;
                                                    Reader        : DDS.DataReader.Ref_Access);

   procedure RTI_Connext_ReplierParams_ToEntityParams (Self     : in RTI_Connext_ReplierParams;
                                                       ToParams : out RTI_Connext_EntityParams);

   procedure RTI_Connext_Replier_Wait_For_Requests (Self        : not null access RTI_Connext_Replier;
                                                    Min_Count   : DDS.Natural;
                                                    Max_Wait    : out DDS.Duration_T);

   procedure RTI_Connext_EntityUntypedImpl_Wait_For_Any_Sample (Self        : not null access RTI_Connext_Replier;
                                                                Min_Count   : DDS.Natural;
                                                                Max_Wait    : out DDS.Duration_T) is abstract;

end Replier;
