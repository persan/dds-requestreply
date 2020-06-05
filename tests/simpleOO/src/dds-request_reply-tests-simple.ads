with DDS.DomainParticipantFactory;
with DDS.DomainParticipant;
package DDS.Request_Reply.Tests.Simple is
   pragma Elaborate_Body;

   Domain_Id           : DDS.DomainId_T := 0;
   Service_Name        : DDS.String := To_DDS_String ("myService");
   Service_Name_Octets : DDS.String := To_DDS_String ("myOctets");
   Qos_Library         : DDS.String := To_DDS_String ("library");
   Qos_Profile         : DDS.String := To_DDS_String ("profile");
   DONE                : DDS.String := To_DDS_String ("<DONE>");

   Factory             : constant DDS.DomainParticipantFactory.Ref_Access :=
                           DDS.DomainParticipantFactory.Get_Instance;

   Participant        : DDS.DomainParticipant.Ref_Access := Factory.Create_Participant_With_Profile
     (Domain_Id    => Domain_Id,
      Library_Name => Qos_Library,
      Profile_Name => Qos_Profile);

end DDS.Request_Reply.Tests.Simple;
