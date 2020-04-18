pragma Ada_2012;
package body DDS.Request_Reply.Replier.Impl is

   -------------------------
   -- Create_Writer_Topic --
   -------------------------

   function Create_Writer_Topic
     (Self : not null access Ref; Params : DDS.Entity_Params.EntityParams;
      Name : DDS.String) return DDS.Topic.Ref_Access
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Create_Writer_Topic unimplemented");
      return
      raise Program_Error with "Unimplemented function Create_Writer_Topic";
   end Create_Writer_Topic;

   -------------------------
   -- Create_Reader_Topic --
   -------------------------

   function Create_Reader_Topic
     (Self : not null access Ref; Params : DDS.Entity_Params.EntityParams;
      Name : DDS.String) return DDS.Topic.Ref_Access
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Create_Reader_Topic unimplemented");
      return
      raise Program_Error with "Unimplemented function Create_Reader_Topic";
   end Create_Reader_Topic;

   -----------------------
   -- Wait_For_Requests --
   -----------------------

   procedure Wait_For_Requests
     (Self     : not null access Ref; Min_Count : DDS.Integer;
      Max_Wait : DDS.Duration_T)
   is
   begin
      pragma Compile_Time_Warning
        (Standard.True, "Wait_For_Requests unimplemented");
      raise Program_Error with "Unimplemented procedure Wait_For_Requests";
   end Wait_For_Requests;

   -----------------------------
   -- Get_Request_Data_Reader --
   -----------------------------

   function Get_Request_Data_Reader
     (Self : not null access Ref) return DDS.DataReader.Ref_Access
   is (DDS.DataReader.Ref_Access (Self.Reply_DataWriter));

   ---------------------------
   -- Get_Reply_Data_Writer --
   ---------------------------

   function Get_Reply_Data_Writer
     (Self : not null access Ref) return DDS.DataWriter.Ref_Access
   is (DDS.DataWriter.Ref_Access (Self.Reply_DataWriter));

end DDS.Request_Reply.Replier.Impl;
