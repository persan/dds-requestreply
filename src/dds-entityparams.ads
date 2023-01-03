package DDS.EntityParams is

   type ref is limited interface;
   type Ref_Access is access all Ref'Class with Storage_Size => 0;
   function Valiate (Self : not null access Ref) return Boolean is abstract;
  
   
end DDS.EntityParams;
