with GNAT.OS_Lib; use GNAT.OS_Lib;
with Ada.Text_IO; use Ada.Text_IO;
procedure DDS.Request_Reply.Tests.Simple.Driver is
   procedure Spawn (Name : Standard.String) is
      Full_Name  : constant Standard.String := "bin/dds-request_reply-tests-simple-" & Name;
      Pid        : Process_Id;
      Args       : Argument_List (1 .. 0);
   begin
      Put ("Spawning:" & Full_Name);
      Pid := Non_Blocking_Spawn (Full_Name, Args);
      Put_Line ("  | Pid ->" & Pid_To_Integer (Pid)'Img);
      delay 0.1;
   end;
begin
   Spawn ("replier_main");
   Spawn ("requester_main");
end;
