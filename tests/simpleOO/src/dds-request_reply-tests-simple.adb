with GNAT.Exception_Traces;
with GNAT.Traceback.Symbolic;
package body DDS.Request_Reply.Tests.Simple is
begin
   GNAT.Exception_Traces.Trace_On (GNAT.Exception_Traces.Every_Raise);
   GNAT.Exception_Traces.Set_Trace_Decorator (GNAT.Traceback.Symbolic.Symbolic_Traceback_No_Hex'Access);
end DDS.Request_Reply.Tests.Simple;
