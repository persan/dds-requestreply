with AUnit.Test_Suites;
with AUnit.Test_Cases;
with AUnit.Reporter.Text;
with AUnit.Run;
with Ada.Command_Line;
use AUnit;
procedure DDS.Request_Reply.Unittest.Test_Case_Runner_generic is
   S : constant AUnit.Test_Suites.Access_Test_Suite := new AUnit.Test_Suites.Test_Suite;
   Tc : constant AUnit.Test_Cases.Test_Case_Access := new Test_Case;
   function Suite return AUnit.Test_Suites.Access_Test_Suite is
   begin
      S.Add_Test (Tc);
      return S;
   end Suite;
   function Run is new AUnit.Run.Test_Runner_With_Status (Suite);
   Reporter       : aliased AUnit.Reporter.Text.Text_Reporter;
begin
   if Run (Reporter) = AUnit.Success then
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
   else
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end DDS.Request_Reply.Unittest.Test_Case_Runner_generic;
