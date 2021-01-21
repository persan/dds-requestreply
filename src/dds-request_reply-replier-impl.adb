pragma Ada_2012;
package body DDS.Request_Reply.Replier.Impl is


   -----------------------
   -- Wait_For_Requests --
   -----------------------

   procedure Wait_For_Requests
     (Self      : not null access Ref;
      Min_Count : DDS.Integer;
      Max_Wait  : DDS.Duration_T)
   is
   begin
      Self.Wait_For_Any_Sample (Max_Wait => Max_Wait , Min_Sample_Count => Min_Count);
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

   procedure  send_sample
     (Self                 : not null access Ref;
      data                 : System.Address;
      Params               : in out WriteParams_T;
      Related_Request_Info : DDS.SampleIdentity_T) is
   begin
      self.configure_params_for_reply (Params, related_request_info);
      --  self.Writer.Write_W_Params (data, params);
   end;

end DDS.Request_Reply.Replier.Impl;
