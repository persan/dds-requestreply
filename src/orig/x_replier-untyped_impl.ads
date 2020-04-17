
package Replier.Untyped_Impl is
   type Ref is new Replier.RTI_Connext_Replier with record
      null;
   end record;

   procedure RTI_Connext_EntityUntypedImpl_Wait_For_Any_Sample (Self        : not null access Ref;
                                                                Min_Count   : DDS.Natural;
                                                                Max_Wait    : out DDS.Duration_T);

   function RTI_Connext_ReplierUntypedImpl_Create_Writer_Topic (Self            : RTI_Connext_EntityUntypedImpl;
                                                                Params          : RTI_Connext_EntityParams;
                                                                Reply_Type_Name : DDS.String) return DDS.Topic.Ref_Access;

   function RTI_Connext_ReplierUntypedImpl_Create_Reader_Topic (Self              : RTI_Connext_EntityUntypedImpl;
                                                                Params            : RTI_Connext_EntityParams;
                                                                Request_Type_Name : DDS.String) return DDS.Topic.Ref_Access;

end Replier.Untyped_Impl;
