pragma Ada_2012;
package body DDS.Request_Reply.Replier.Impl is


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

   --------------------------------
   -- Configure_Params_For_Reply --
   --------------------------------

   procedure Configure_Params_For_Reply
     (Self                 : not null access Ref;
      Params               : in out WriteParams_T;
      Related_Request_Info : DDS.SampleIdentity_T)
   is
   begin
      if Related_Request_Info =  AUTO_SAMPLE_IDENTITY then
         raise BAD_PARAMETER with "AUTO_SAMPLE_IDENTITY not allowed";
      end if;
      Params.Related_Sample_Identity := Related_Request_Info;
   end Configure_Params_For_Reply;

end DDS.Request_Reply.Replier.Impl;
