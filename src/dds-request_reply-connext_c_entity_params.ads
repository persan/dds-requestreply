with DDS;
with Dds.DomainParticipant;
with DDS.Publisher;
with DDS.Subscriber;

with Ada.Finalization;
package DDS.Request_Reply.Connext_C_Entity_Params is
   type RTI_Connext_EntityParams is new Ada.Finalization.Limited_Controlled with record 
      Participant        : DDS.DomainParticipant.Ref_Access;
      Service_Name       : DDS.String;
      Request_Topic_Name : DDS.String;
      Reply_Topic_Name   : DDS.String;
      Qos_Library_Name   : DDS.String;
      Qos_Profile_Name   : DDS.String;
      Datawriter_Qos     : access DDS.DataWriterQos;
      Datareader_Qos     : access DDS.DataReaderQos;
      Publisher          : DDS.Publisher.Ref_Access;
      Subscriber         : DDS.Subscriber.Ref_Access;
   end record;
   function RTI_Connext_EntityParams_Validate (Self : RTI_Connext_EntityParams) return DDS.Boolean is (True);
   procedure Initialize (Object : in out RTI_Connext_EntityParams);
   procedure Finalize   (Object : in out RTI_Connext_EntityParams);
   
end DDS.Request_Reply.Connext_C_Entity_Params;
