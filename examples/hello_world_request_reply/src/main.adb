with Primes.Replier_Main;
with Primes.Requester_Main;
with GNAT.Traceback.Symbolic;
with GNAT.Exception_Traces;

procedure Main is
   task Requester is 
      entry start;
   end Requester;
   task Replier is 
      entry start;
   end Replier;

   task body Requester is 
   begin
      accept start;      
      Primes.Requester_Main;
   end Requester;
   
   task body Replier is 
   begin
      accept start;
      Primes.Replier_Main;
   end Replier;

begin
   GNAT.Exception_Traces.Trace_On(GNAT.Exception_Traces.Every_Raise);
   GNAT.Exception_Traces.Set_Trace_Decorator(GNAT.Traceback.Symbolic.Symbolic_Traceback_No_Hex'access);
   
   Replier.start;
   delay 0.5;
   
   Requester.start;   
   
end Main;
