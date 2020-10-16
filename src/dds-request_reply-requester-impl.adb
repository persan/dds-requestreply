--  ----------------------------------------------------------------------------
--  Note this is an implementation package and is subject to change att any time.
--  ----------------------------------------------------------------------------
package body DDS.Request_Reply.Requester.Impl is


   -------------------
   -- Touch_Samples --
   -------------------

   function Touch_Samples
     (Self           : not null access Ref; Max_Count : DDS.Integer;
      Read_Condition : DDS.ReadCondition.Ref_Access) return Integer
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
                                   "Touch_Samples unimplemented");
      return raise Program_Error with "Unimplemented function Touch_Samples";
   end Touch_Samples;

   -------------------------
   -- Wait_For_Any_Sample --
   -------------------------

   function Wait_For_Any_Sample
     (Self             : not null access Ref;
      Max_Wait         : DDS.Duration_T;
      Min_Sample_Count : DDS.Integer) return DDS.ReturnCode_T
   is
   begin
      pragma Compile_Time_Warning (Standard.True,
                                   "Wait_For_Any_Sample unimplemented");
      return raise Program_Error
        with "Unimplemented function Wait_For_Any_Sample";
   end Wait_For_Any_Sample;

end DDS.Request_Reply.Requester.Impl;
