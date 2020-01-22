with DDS.DataReader;
with DDS.Request_Reply.Connext_C_Replier;
with DDS.Request_Reply.Connext_C_Entity_Params;
package DDS.Request_Reply.Replier is
   use Connext_C_Replier;
   use Connext_C_Entity_Params;
   procedure RTI_Connext_Replier_On_Data_Available
     (Self   : RTI_Connext_Replier_Access;
      Reader : DDS.DataReader.Ref_Access);


   procedure RTI_Connext_ReplierParams_ToEntityParams
     (Self     : RTI_Connext_ReplierParams;
      ToParams : out RTI_Connext_EntityParams);

   procedure RTI_Connext_Replier_Delete
     (Self : in out RTI_Connext_Replier_Access);

   procedure RTI_Connext_Replier_Wait_For_Requests
     (Self      : not null access RTI_Connext_Replier;
      Min_Count : DDS.Integer;
      Max_Wait  : DDS.Duration_T);
end DDS.Request_Reply.Replier;
