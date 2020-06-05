package body DDS.Request_Reply.Impl is
   use DDS.DomainParticipant;
   use DDS.Publisher;
   use DDS.Subscriber;

   --------------------------
   -- Create_Request_Topic --
   --------------------------

   function Create_Request_Topic
     (Self       : not null access Ref;
      Topic_Name : DDS.String;
      Type_Name  : DDS.String;
      QoS        : DDS.TopicQos := DDS.DomainParticipant.TOPIC_QOS_DEFAULT) return DDS.Topic.Ref_Access
   is
   begin
      return Self.Participant.Get_Or_Create_Topic (Topic_Name, Type_Name, Qos);
   end Create_Request_Topic;

   ------------------------
   -- Create_Reply_Topic --
   ------------------------

   function Create_Reply_Topic
     (Self       : not null access Ref;
      Topic_Name : DDS.String;
      Type_Name  : DDS.String;
      QoS        : DDS.TopicQos := DDS.DomainParticipant.TOPIC_QOS_DEFAULT) return DDS.Topic.Ref_Access
   is
   begin
      return Self.Participant.Get_Or_Create_Topic (Topic_Name, Type_Name, Qos);
   end Create_Reply_Topic;

   ---------------------------------------
   -- Create_Request_Topic_With_Profile --
   ---------------------------------------

   function Create_Request_Topic_With_Profile
     (Self             : not null access Ref;
      Topic_Name       : DDS.String;
      Type_Name        : DDS.String;
      Library_Name     : DDS.String;
      Profile_Name     : DDS.String) return DDS.Topic.Ref_Access
   is
   begin
      return Self.Participant.Get_Or_Create_Topic_With_Profile (Topic_Name, Type_Name, Library_Name, Profile_Name);
   end Create_Request_Topic_With_Profile;

   -------------------------------------
   -- Create_Reply_Topic_With_Profile --
   -------------------------------------

   function Create_Reply_Topic_With_Profile
     (Self             : not null access Ref;
      Topic_Name       : DDS.String;
      Type_Name        : DDS.String;
      Library_Name     : DDS.String;
      Profile_Name     : DDS.String) return DDS.Topic.Ref_Access
   is
   begin
      return Self.Participant.Get_Or_Create_Topic_With_Profile (Topic_Name, Type_Name, Library_Name, Profile_Name);
   end Create_Reply_Topic_With_Profile;

   --------------
   -- Validate --
   --------------

   procedure Validate
     (Self       : not null access Ref;
      Publisher  : DDS.Publisher.Ref_Access;
      Subscriber : DDS.Subscriber.Ref_Access)
   is
   begin
      if (Publisher /= null) then
         if Self.Participant /= Publisher.Get_Participant then
            raise Program_Error with "Publisher dont belong to participant";
         else
            Self.Publisher := Publisher;
         end if;
      else
         Self.Publisher := Self.Participant.Get_Implicit_Publisher;
      end if;

      if (Subscriber /= null) then
         if Self.Participant /= Subscriber.Get_Participant then
            raise Program_Error with "Subscriber dont belong to participant";
         else
            Self.Subscriber := Self.Participant.Get_Implicit_Subscriber;
         end if;
      else
         Self.Subscriber := Subscriber;
      end if;
   end Validate;


end DDS.Request_Reply.Impl;
