-------------------------------------------------------------------------------
--
--  This file is only for conviniece to be able to build and run with
--  one click
--
-------------------------------------------------------------------------------

with GNAT.OS_Lib;
with Ada.Text_IO; use Ada.Text_IO;
procedure DDS.Request_Reply.Tests.Simple.Driver is
   Args       : GNAT.OS_Lib.Argument_List (1 .. 0);
   procedure Non_Blocking_Spawn (Name : Standard.String) is
      Full_Name  : constant Standard.String := "bin/dds-request_reply-tests-simple-" & Name;
      Pid        : GNAT.OS_Lib.Process_Id;
   begin
      Put ("Spawning:" & Full_Name);
      Pid := GNAT.OS_Lib.Non_Blocking_Spawn (Full_Name, Args);
      Put_Line ("  | Pid ->" & GNAT.OS_Lib.Pid_To_Integer (Pid)'Img);
      delay 0.1;
   end;
   Dummy_Status     : Standard.Integer;
   Gprbuild   : constant GNAT.OS_Lib.String_Access := GNAT.OS_Lib.Locate_Exec_On_Path ("gprbuild");
begin
   Dummy_Status := GNAT.OS_Lib.Spawn (Gprbuild.all, Args); -- always build prior to lauch
   Non_Blocking_Spawn ("replier_main");
   Non_Blocking_Spawn ("requester_main");
end;
