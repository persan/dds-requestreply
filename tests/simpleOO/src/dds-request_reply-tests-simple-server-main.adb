with DDS.Request_Reply.Tests.Simple.String_Replier;
with DDS.Request_Reply.Tests.Simple.Octets_Replier;

procedure DDS.Request_Reply.Tests.Simple.Server.Main is

   Listner        : aliased Ref;

   Octets_Replier : Simple.Octets_Replier.Ref_Access := Simple.Octets_Replier.Create
     (Participant  => Participant,
      Service_Name => Service_Name_Octets,
      Library_Name => Qos_Library,
      Profile_Name => Qos_Profile,
      A_Listner    => Listner'Unchecked_Access,
      Mask         => DDS.STATUS_MASK_ALL);

   String_Replier : Simple.String_Replier.Ref_Access := Simple.String_Replier.Create
     (Participant  => Participant,
      Service_Name => Service_Name,
      Library_Name => Qos_Library,
      Profile_Name => Qos_Profile,
      A_Listner    => Listner'Unchecked_Access,
      Mask         => DDS.STATUS_MASK_ALL);

   Service_Time   : constant Duration := 60 * 60.0; -- one hour
begin
   delay Service_Time;
   Simple.String_Replier.Delete (String_Replier);
   Simple.Octets_Replier.Delete (Octets_Replier);
end DDS.Request_Reply.Tests.Simple.Server.Main;
