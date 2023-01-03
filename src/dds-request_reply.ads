package DDS.Request_Reply is

   type Ref is limited interface;
   type Ref_Access  is access all Ref'Class with Storage_Size => 0;
   procedure Log_Exception (Self : not null access Ref; Log : Standard.String) is null;


end DDS.Request_Reply;
