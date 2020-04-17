pragma Ada_2012;
package body DDS.Entity_Params is

   --------------
   -- Validate --
   --------------

   function Validate (Self : RTI_Connext_EntityParams) return DDS.Boolean is
   begin
      pragma Compile_Time_Warning (Standard.True, "Validate unimplemented");
      return raise Program_Error with "Unimplemented function Validate";
   end Validate;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Object : in out RTI_Connext_EntityParams) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Initialize unimplemented");
      raise Program_Error with "Unimplemented procedure Initialize";
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Object : in out RTI_Connext_EntityParams) is
   begin
      pragma Compile_Time_Warning (Standard.True, "Finalize unimplemented");
      raise Program_Error with "Unimplemented procedure Finalize";
   end Finalize;

end DDS.Entity_Params;
