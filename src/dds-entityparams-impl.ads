with RTIDDS.Low_Level.ndds_connext_c_connext_c_entity_params_h;
with Interfaces.c;
package DDS.EntityParams.Impl is
   use RTIDDS.Low_Level.ndds_connext_c_connext_c_entity_params_h;
   use Interfaces.c;

   type ref is new ada.Finalization.Limited_Controlled with Record
      Impl : aliased RTI_Connext_EntityParams;
   end record;

   type Ref_Access is access all Ref'Class;
   function Valiate (Self : not null access Ref) return Boolean is
     (RTI_Connext_EntityParams_validate (Self.Impl'Access) = 0);


end DDS.EntityParams.Impl;
