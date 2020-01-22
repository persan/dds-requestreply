with DDS.DomainParticipant;
with DDS.Publisher;
with DDS.Subscriber;
with dds.Request_Reply;
package body DDS.Entity_Params is
   use Dds.Request_Reply;
   use DDS.DomainParticipant;
   use DDS.Publisher;
   use DDS.Subscriber;
   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Object : in out RTI_Connext_EntityParams) is
   begin
      null;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out RTI_Connext_EntityParams) is
   begin
      Dds.Finalize (Object.Service_Name);
      Dds.Finalize (Object.Request_Topic_Name);
      Dds.Finalize (Object.Reply_Topic_Name);
      Dds.Finalize (Object.Qos_Library_Name);
      Dds.Finalize (Object.Qos_Profile_Name);
   end Finalize;


   function Validate (Self : RTI_Connext_EntityParams) return Boolean is
      OK : Boolean := True;
   begin
      if Self.Participant = null then
         DDSLog_Exception ("A participant is required");
         OK := FALSE;
      end if;
      if Self.Service_Name = NULL_STRING then
         if (Self.Request_Topic_Name = Null_String) or (Self.Reply_Topic_Name = NULL_STring) then
            DDSLog_Exception ("Either service name or request and reply topics are required.");
            OK := FALSE;
         end if;
      else
         if (Self.Request_Topic_Name /= NULL_STRING) or ( Self.Reply_Topic_Name /= NULL_STRING) then
            DDSLog_Exception ("Service name and topics are mutually exclusive.");
            Ok := FALSE;
         end if;
      end if;

      if (Self.Publisher /= null) and then (Self.Publisher.Get_Participant /=  Self.Participant) then
         DDSLog_Exception ("The publisher belongs to a different participant");
         Ok :=  FALSE;
      end if;


      if (Self.Subscriber /= null) and then (Self.Subscriber.Get_Participant /=  Self.Participant) then
         DDSLog_Exception ("The subscriber belongs to a different participant");
         Ok :=  FALSE;
      end if;



      if (Self.Qos_Library_Name /= NULL_STRING ) and ( Self.Qos_Profile_Name = NULL_STRING) then
         DDSLog_Exception ("qos_library_name is set but qos_profile_name is not");
         Ok :=  FALSE;
      end if;

      if (Self.Qos_Library_Name = NULL_STRING ) and ( Self.Qos_Profile_Name /= NULL_STRING) then
         DDSLog_Exception ("qos_profile_name is set but qos_profile_name is not");
         Ok :=  FALSE;
      end if;
      return OK;
   end Validate;
end DDS.Entity_Params;
