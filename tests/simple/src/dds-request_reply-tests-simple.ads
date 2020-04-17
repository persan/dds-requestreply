with DDS.DomainParticipantFactory;
with DDS.DomainParticipant;
package DDS.Request_Reply.Tests.Simple is
   pragma Elaborate_Body;
   Domain_Id          : DDS.DomainId_T := 0;
   Request_Topic_Name : String := To_DDS_String ("Request_Topic_Name");
   Reply_Topic_Name   : String := To_DDS_String ("Reply_Topic_Name");
   Qos_Library        : String := To_DDS_String ("library");
   Qos_Profile        : String := To_DDS_String ("profile");
   DONE               : String := To_DDS_String ("<DONE>");

   Factory            : constant DDS.DomainParticipantFactory.Ref_Access :=
                          DDS.DomainParticipantFactory.Get_Instance;

   Participant        : DDS.DomainParticipant.Ref_Access := Factory.Create_Participant_With_Profile
     (Domain_Id    => Domain_Id,
      Library_Name => Qos_Library,
      Profile_Name => Qos_Profile);

end DDS.Request_Reply.Tests.Simple;
