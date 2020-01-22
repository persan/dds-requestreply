pragma Ada_2012;
with DDS.TopicDescription;
package body DDS.Request_Reply is
   use DDS.DomainParticipant;

   -----------
   -- Image --
   -----------

   function Image (Item : Guid_T) return DDS.String is
   begin
      return Ret : DDS.String do
         Copy (Ret, String'(Image (Item)));
      end return;
   end Image;
   subtype Octet_Image_String is Standard.String (1 .. 2);
   function Value (Item : Octet_Image_String) return Octet is
      Map : constant array (Character) of Octet := ('0'    => 0,
                                                    '1'    => 1,
                                                    '2'    => 2,
                                                    '3'    => 3,
                                                    '4'    => 4,
                                                    '5'    => 5,
                                                    '6'    => 6,
                                                    '7'    => 7,
                                                    '8'    => 8,
                                                    '9'    => 9,
                                                    'A'    => 10,
                                                    'B'    => 11,
                                                    'C'    => 12,
                                                    'D'    => 13,
                                                    'E'    => 14,
                                                    'F'    => 15,
                                                    others => 0);
      use type Interfaces.Unsigned_8;
   begin
      return (Map (item(Item'First)) * 16 + Map (Item(Item'First + 1)));
   end;
   -----------
   -- Value --
   -----------

   function Value (Item : Standard.String) return Guid_T is
      Cursor : Standard.Natural := Item'First;
   begin
      return ret : Guid_T do
         if Item'Length /= Ret.Value'Length * 2 then
            raise Constraint_Error with "invalid length";
         end if;
         for I in Ret.Value'First .. Ret.Value'Last loop
            Ret.Value (I) := Value (Item (Cursor .. Cursor + 1));
            Cursor := Cursor + 2;
         end loop;
      end return;
   end;

   -----------
   -- Image --
   -----------
   function Image (Item : Octet) return Standard.String is
      use Interfaces;
      Map : constant array (Unsigned_8'(0) .. Unsigned_8'(15)) of Character := "0123456789ABCDEF";
   begin
      return Map (Item / 16) & Map (Item mod 16);
   end;

   function Image (Item : Guid_T) return Standard.String is
      Cursor : Standard.Natural := 1;
   begin
      return Ret : Standard.String (1 .. (Item.Value'Length * 2)) do
         for V of Item.Value loop
            Ret (Cursor .. Cursor + 1) := Image (V);
            Cursor := Cursor + 2;
         end loop;
      end return;
   end Image;



   function  Get_Or_Create_Topic  (Self       : not null DDS.DomainParticipant.Ref_Access;
                                   Topic_Name : in DDS.String;
                                   Type_Name  : in DDS.String;
                                   Qos        : in DDS.TopicQos := DDS.DomainParticipant.TOPIC_QOS_DEFAULT;
                                   A_Listener : in DDS.TopicListener.Ref_Access := null;
                                   Mask       : in DDS.StatusMask := STATUS_MASK_NONE)return  DDS.Topic.Ref_Access is
      use type DDS.TopicDescription.Ref_Access;
      use type DDS.Topic.Ref_Access;
      Temp : DDS.TopicDescription.Ref_Access;

   begin
      return Ret : DDS.Topic.Ref_Access do
         Temp := self.Lookup_Topicdescription (Topic_Name);
         if Temp = null then
            Ret := Self.Create_Topic (Topic_Name, Topic_Name, Qos, A_Listener, Mask);
         else
            Ret := Topic.Narrow (Temp);
         end if;
      end return;
   end;

   function  Get_Or_Create_Topic_With_Profile  (Self         : not null DDS.DomainParticipant.Ref_Access;
                                                Topic_Name   : in DDS.String;
                                                Type_Name    : in DDS.String;
                                                Library_Name : in DDS.String;
                                                Profile_Name : in DDS.String;
                                                A_Listener   : in DDS.TopicListener.Ref_Access := null;
                                                Mask         : in DDS.StatusMask := STATUS_MASK_NONE)return  DDS.Topic.Ref_Access is
      use type DDS.TopicDescription.Ref_Access;
      use type DDS.Topic.Ref_Access;
      Temp : DDS.TopicDescription.Ref_Access;

   begin
      return Ret : DDS.Topic.Ref_Access do
         Temp := self.Lookup_Topicdescription (Topic_Name);
         if Temp = null then
            Ret := Self.Create_Topic_With_Profile (Topic_Name, Topic_Name, Library_Name, Profile_Name, A_Listener, Mask);
         else
            Ret := Topic.Narrow (Temp);
         end if;
      end return;
   end;

   procedure Append (To : in out DDS.String; Data : DDS.String) is
      Temp : constant Standard.String := To_Standard_String (Data);
   begin
      Append (To, Temp);
   end;

   procedure Append (To : in out DDS.String; Data : Standard.String) is
      Temp : constant Standard.String := To_Standard_String (To);
   begin
      Copy (To, Temp & Data);
   end;

   procedure Prepend (To : in out DDS.String; Data : DDS.String) is
      Temp : constant Standard.String := To_Standard_String (Data);
   begin
      Prepend (To, Temp);
   end;

   procedure Prepend (To : in out DDS.String; Data : Standard.String) is
      Temp : constant Standard.String := To_Standard_String (To);
   begin
      Copy (To,  Data & Temp);
   end;

   procedure Copy (To : out DataReaderQoS; From : DataReaderQoS) is
   begin
      null;
   end;
   procedure Copy (To : out DataWriterQos; From : DataWriterQos) is
   begin
      null;
   end;


   --  =========================================================================
   --  package DDS.DomainParticipantFactory
   function  Get_Or_Create_Participant
     (Self       : not null DDS.DomainParticipantFactory.Ref_Access;
      Domain_Id  : in DDS.DomainId_T := Default_Domain;
      Qos        : in DDS.DomainParticipantQos := DDS.DomainParticipantFactory.PARTICIPANT_QOS_DEFAULT;
      A_Listener : in DDS.DomainParticipantListener.Ref_Access := null;
      Mask       : in DDS.StatusMask := STATUS_MASK_NONE)return  DDS.DomainParticipant.Ref_Access is
      Ret : DDS.DomainParticipant.Ref_Access;
   begin
      Ret := Self.Lookup_Participant (Domain_Id);
      if Ret = null then
         Ret := Self.Create_Participant (Domain_Id => Domain_Id , Qos => Qos , A_Listener => A_Listener , Mask => Mask);
      end if;
      return Ret;
   end;

   function  Get_Or_Create_Participant_With_Profile
     (Self         : not null DDS.DomainParticipantFactory.Ref_Access;
      Domain_Id    : in DDS.DomainId_T := Default_Domain;
      Library_Name : in DDS.String;
      Profile_Name : in DDS.String;
      A_Listener   : in DDS.DomainParticipantListener.Ref_Access := null;
      Mask         : in DDS.StatusMask := STATUS_MASK_NONE) return  DDS.DomainParticipant.Ref_Access is
      Ret : DDS.DomainParticipant.Ref_Access;
   begin
      Ret := Self.Lookup_Participant (Domain_Id);
      if Ret = null then
         ret := Self.Create_Participant_With_Profile
           (Domain_Id    => Domain_Id ,
            Library_Name => Library_Name,
            Profile_Name => Profile_Name ,
            A_Listener   => A_Listener,
            Mask         => Mask);
      end if;
      return Ret;
   end;


end DDS.Request_Reply;
