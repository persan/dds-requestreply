-------------------------------------------------------------------------------
--
--  This file is only for conviniece to be able to build and run with
--  one click
--
-------------------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO;
with GNAT.OS_Lib;

procedure DDS.Request_Reply.Tests.Simple.Driver is
   use Ada.Text_IO;

   Args       : constant GNAT.OS_Lib.Argument_List :=
                  (new Standard.String'("-j0"),
                   new Standard.String'("-k"),
                   new Standard.String'("-s"));

   procedure Non_Blocking_Spawn (Name : Standard.String) is
      Full_Name  : constant Standard.String := "bin/dds-request_reply-tests-simple-" & Name;
      Pid        : GNAT.OS_Lib.Process_Id;
   begin
      if Ada.Directories.Exists (Full_Name) then
         Put ("Spawning:" & Full_Name);
         Pid := GNAT.OS_Lib.Non_Blocking_Spawn (Full_Name, Args);
         Put_Line ("  | Pid ->" & GNAT.OS_Lib.Pid_To_Integer (Pid)'Img);
         delay 0.4;
      else
         Put_Line (Full_Name & ": does not exists.");
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      end if;
   end;
   Gprbuild   : constant GNAT.OS_Lib.String_Access := GNAT.OS_Lib.Locate_Exec_On_Path ("gprbuild");
begin
   if GNAT.OS_Lib.Spawn (Gprbuild.all, Args) = 0 then -- Dont try to run if the build fails
      Non_Blocking_Spawn ("replier_main");
      Non_Blocking_Spawn ("requester_main");
   end if;
end;
