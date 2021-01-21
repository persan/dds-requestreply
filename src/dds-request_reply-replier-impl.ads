with DDS.Request_Reply.Impl;
private package  DDS.Request_Reply.Replier.Impl is

   type Ref is limited new DDS.Request_Reply.Impl.Ref and DDS.Request_Reply.Replier.Ref with record
      null;
   end record;
   type Ref_Access is access all Ref;


   procedure Wait_For_Requests
     (Self      : not null access Ref;
      Min_Count : DDS.Integer;
      Max_Wait  : DDS.Duration_T);

   procedure Configure_Params_For_Reply
     (Self                 : not null access Ref;
      Params               : in out WriteParams_T;
      Related_Request_Info : DDS.SampleIdentity_T);

   procedure  send_sample
     (Self                 : not null access Ref;
      data                 : System.Address;
      Params               : in out WriteParams_T;
      Related_Request_Info : DDS.SampleIdentity_T);
end DDS.Request_Reply.Replier.Impl;
