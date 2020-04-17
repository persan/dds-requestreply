pragma Ada_2012;
package body DDS.Request_Reply.Impl is

   --------------------------
   -- Create_Request_Topic --
   --------------------------

   function Create_Request_Topic
     (Self      : not null access Ref; Topic_Name : DDS.String;
      Type_Name : DDS.String) return DDS.Topic.Ref_Access
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Create_Request_Topic unimplemented");
      return
        raise Program_Error with "Unimplemented function Create_Request_Topic";
   end Create_Request_Topic;

   ------------------------
   -- Create_Reply_Topic --
   ------------------------

   function Create_Reply_Topic
     (Self      : not null access Ref; Topic_Name : DDS.String;
      Type_Name : DDS.String) return DDS.Topic.Ref_Access
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Create_Reply_Topic unimplemented");
      return
        raise Program_Error with "Unimplemented function Create_Reply_Topic";
   end Create_Reply_Topic;

   ---------------------------------------
   -- Create_Request_Topic_With_Profile --
   ---------------------------------------

   function Create_Request_Topic_With_Profile
     (Self             : not null access Ref; Topic_Name : DDS.String;
      Type_Name        : DDS.String; Qos_Library_Name : DDS.String;
      Qos_Profile_Name : DDS.String) return DDS.Topic.Ref_Access
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Create_Request_Topic_With_Profile unimplemented");
      return
        raise Program_Error
          with "Unimplemented function Create_Request_Topic_With_Profile";
   end Create_Request_Topic_With_Profile;

   -------------------------------------
   -- Create_Reply_Topic_With_Profile --
   -------------------------------------

   function Create_Reply_Topic_With_Profile
     (Self             : not null access Ref; Topic_Name : DDS.String;
      Type_Name        : DDS.String; Qos_Library_Name : DDS.String;
      Qos_Profile_Name : DDS.String) return DDS.Topic.Ref_Access
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Create_Reply_Topic_With_Profile unimplemented");
      return
        raise Program_Error
          with "Unimplemented function Create_Reply_Topic_With_Profile";
   end Create_Reply_Topic_With_Profile;

   --------------
   -- Validate --
   --------------

   procedure Validate
     (Self       : not null access Ref; Publisher : DDS.Publisher.Ref_Access;
      Subscriber : DDS.Subscriber.Ref_Access)
   is
   begin
      pragma Compile_Time_Warning (Standard.True, "Validate unimplemented");
      raise Program_Error with "Unimplemented procedure Validate";
   end Validate;

end DDS.Request_Reply.Impl;
