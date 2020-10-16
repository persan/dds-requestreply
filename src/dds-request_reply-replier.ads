package DDS.Request_Reply.Replier is

   type Ref is limited interface and DDS.Request_Reply.Ref;
   type Ref_Access is access all Ref'class;


   procedure Wait_For_Requests
     (Self      : not null access Ref;
      Min_Count : DDS.Integer;
      Max_Wait  : DDS.Duration_T) is abstract;

end DDS.Request_Reply.Replier;
