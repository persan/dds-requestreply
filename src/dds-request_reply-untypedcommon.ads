with DDS.DataReader_Impl;
with DDS.DataReaderListener;
with DDS.DataWriter_Impl;
with DDS.DomainParticipant;
with DDS.Publisher;
with DDS.ReadCondition;
with DDS.Subscriber;
with DDS.WaitSet;
with Dds.Topic;
with DDS.TopicDescription;
with Interfaces.C.Extensions;
with RTIDDS.Low_Level.Ndds_Reda_Reda_FastBuffer_H;
with Ada.Finalization;
with System;
with DDS.Request_Reply.Connext_C_Entity_Params; use DDS.Request_Reply.Connext_C_Entity_Params;

package DDS.Request_Reply.Untypedcommon is

   use Dds;
   
   type RTI_Connext_EntityUntypedImpl is abstract new Ada.Finalization.Limited_Controlled with  record
      Participant  : DDS.DomainParticipant.Ref_Access;
      Publisher    : DDS.Publisher.Ref_Access;
      Subscriber   : DDS.Subscriber.Ref_Access;
      Writer_Topic : DDS.Topic.Ref_Access;
      Reader_Topic : DDS.TopicDescription.Ref_Access;
   
      Writer       : DDS.DataWriter_Impl.Ref_Access;
      Reader       : DDS.DataReader_Impl.Ref_Access;
   
      Waitset      : DDS.WaitSet.Ref_Access;
      Not_Read_Sample_Cond : DDS.ReadCondition.Ref_Access;
      Any_Sample_Cond : DDS.ReadCondition.Ref_Access;
   
      Sample_Size  : DDS.long := -1;
      -- waitset_pool : RTIDDS.Low_Level.ndds_reda_reda_fastBuffer_h.REDAFastBufferPool;
      Waitset_Pool : Interfaces.C.Extensions.Void_Ptr;
   
      Max_Samples_Per_Read : DDS.long;
      
   end record;
   
   function RTI_Connext_CreateWriterTopicFunc
     (Self   : access RTI_Connext_EntityUntypedImpl;
      Params : access RTI_Connext_EntityParams;
      Name   : DDS.String) return DDS.TopicDescription.Ref_Access is abstract;
   function Create_Writer_Topic
     (Self   : access RTI_Connext_EntityUntypedImpl;
      Params : access RTI_Connext_EntityParams;
      Name   : DDS.String) return DDS.TopicDescription.Ref_Access is  abstract;
   

   function RTI_Connext_Get_Or_Create_Topic (Participant : DDS.DomainParticipant.Ref_Access;
                                             Name        : DDS.String;
                                             Type_Name   : DDS.String;
                                             Allow_Cft   : DDS.Boolean) return DDS.TopicDescription.Ref_Access;

   
  
   function RTI_Connext_Create_Request_Topic_Name_From_Service_Name (Service_Name : DDS.String) return DDS.String is
     (DDS.To_DDS_String (DDS.To_Standard_String (Service_Name) & "Request"));
                                                                      

   function RTI_Connext_Create_Reply_Topic_Name_From_Service_Name (Service_Name : DDS.String) return DDS.String is
     (DDS.To_DDS_String (DDS.To_Standard_String (Service_Name) & "Reply"));

   

   --  
   function RTI_Connext_EntityUntypedImpl_Initialize (Self             : in out RTI_Connext_EntityUntypedImpl;
                                                      Params           : RTI_Connext_EntityParams;                                                      
                                                      Writer_Type_Name : DDS.String;
                                                      Reader_Type_Name : DDS.String;
                                                      Sample_Size      : DDS.long;
                                                      Reader_Listener  : DDS.DataReaderListener.Ref_Access;
                                                      Role_Name        : DDS.String) return DDS.ReturnCode_T;
   
                                                      
end DDS.Request_Reply.Untypedcommon;
