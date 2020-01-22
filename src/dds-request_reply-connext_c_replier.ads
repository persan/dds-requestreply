with DDS.DataReader;
with DDS.DataWriter;
with DDS.Publisher;
with DDS.Subscriber;
with Interfaces.C.Extensions;
with DDS.DomainParticipant;
with DDS.Treats_Generic;
with DDS.Request_Reply.Connext_C_Entity_Params; 
with DDS;
with DDS.DataReaderListener;
with Interfaces.C.Extensions;
with Ada.Finalization;
with DDS.Request_Reply.Untypedcommon;
package DDS.Request_Reply.Connext_C_Replier is
   use Connext_C_Entity_Params;


   type RTI_Connext_ReplierUntypedImpl is abstract new Untypedcommon.RTI_Connext_EntityUntypedImpl with null Record;
   
   type RTI_Connext_Replier;
   type RTI_Connext_ReplierListener;


   type RTI_Connext_ReplierListener_OnRequestAvailableCallback is access 
     procedure (Self : RTI_Connext_ReplierListener; Replier :  not null access RTI_Connext_Replier);

   
   type RTI_Connext_ReplierListener is interface;
   procedure On_Request_Available (Self    : not null access RTI_Connext_ReplierListener; 
                                   Replier :  not null access RTI_Connext_Replier'Class) is abstract;
   type RTI_Connext_ReplierListener_Access is access all RTI_Connext_ReplierListener'Class;
   type RTI_Connext_SimpleReplierListener;

   
   
   type RTI_Connext_SimpleReplierListener_OnRequestAvailableCallback is access 
     procedure (Self    : RTI_Connext_SimpleReplierListener; 
                Request : Interfaces.C.Extensions.Void_Ptr;
                Replier :  not null access RTI_Connext_Replier);
   

   type RTI_Connext_SimpleReplierListener_OnReturnLoanCallback is access 
     procedure (Self         : RTI_Connext_SimpleReplierListener; 
                Requreplyest : Interfaces.C.Extensions.Void_Ptr);
   
   

   type RTI_Connext_SimpleReplierListener is record
      On_Request_Available : RTI_Connext_SimpleReplierListener_OnRequestAvailableCallback;
      Return_Loan          : RTI_Connext_SimpleReplierListener_OnReturnLoanCallback;
      User_Data            : Interfaces.C.Extensions.Void_Ptr;
   end record;
   
   

   type RTI_Connext_Replier is tagged record
      Impl           : access RTI_Connext_ReplierUntypedImpl;      
      Listener       : RTI_Connext_ReplierListener_Access;
      SimpleListener : RTI_Connext_SimpleReplierListener;
   end record;
   
   

   type RTI_Connext_ReplierParams is new Ada.Finalization. Limited_Controlled with record 
      Participant        : DDS.DomainParticipant.Ref_Access;
      Service_Name       : DDS.String;
      Request_Topic_Name : DDS.String;
      Reply_Topic_Name   : DDS.String;
      Qos_Library_Name   : DDS.String;
      Qos_Profile_Name   : DDS.String;
      Datawriter_Qos     : DDS.DataWriterQos;
      Datareader_Qos     : DDS.DataReaderQos;
      Publisher          : DDS.Publisher.Ref_Access;
      Subscriber         : DDS.Subscriber.Ref_Access;
      Listener           : RTI_Connext_ReplierListener_Access;
   end record;
   procedure Initialize (Object : in out RTI_Connext_ReplierParams) is null;
   procedure Finalize   (Object : in out RTI_Connext_ReplierParams) is null;

   type RTI_Connext_Replier_Access is access all RTI_Connext_Replier;   
   

   function RTI_Connext_Replier_Delete (Self : RTI_Connext_Replier_Access) return DDS.ReturnCode_T;

   
   function RTI_Connext_Replier_Wait_For_Requests (Self      : access RTI_Connext_Replier;
                                                   Min_Count : DDS.Integer; 
                                                   Max_Wait  : DDS.Duration_T) return DDS.ReturnCode_T;
   
   
   
   function RTI_Connext_ReplierUntypedImpl_create return RTI_Connext_ReplierUntypedImpl'Class;
   
private
   


   
   function RTI_Connext_ReplierUntypedImpl_Initialize
     (Self              : RTI_Connext_ReplierUntypedImpl;
      Params            : RTI_Connext_EntityParams;
      Request_Type_Name : DDS.String;
      Reply_Type_Name   : DDS.String;
      Request_Size      : DDS.Integer;
      Reader_Listener   : DDS.DataReaderListener.Ref_Access)
   return DDS.ReturnCode_T;
  
   function RTI_Connext_ReplierUntypedImpl_send_sample
     (Self              : RTI_Connext_ReplierUntypedImpl;
      Data              : Interfaces.C.Extensions.Void_Ptr;
      Related_Request_Info : DDS.SampleIdentity_T;
      WriteParams          : DDS.WriteParams_T) return DDS.ReturnCode_T;
  
   function RTI_Connext_ReplierParams_toEntityParams
     (Self              : RTI_Connext_ReplierParams;
      ToParams          : out RTI_Connext_EntityParams) return DDS.ReturnCode_T;
   
   
end DDS.Request_Reply.Connext_C_Replier;
