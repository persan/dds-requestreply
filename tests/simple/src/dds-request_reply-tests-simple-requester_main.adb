with Ada.Text_IO;
with DDS.Request_Reply.Tests.Simple.Requester;
procedure DDS.Request_Reply.Tests.Simple.Requester_Main is

   Requester    : DDS.Request_Reply.Tests.Simple.Requester.Ref_Access :=
                    DDS.Request_Reply.Tests.Simple.Requester.Create
                      (Participant        => Participant,
                       Request_Topic_Name => Request_Topic_Name,
                       Reply_Topic_Name   => Reply_Topic_Name,
                       Qos_Library_Name   => Qos_Library,
                       Qos_Profile_Name   => Qos_Profile);

   Request_Data : aliased String;

begin
   for I in 1 .. 10 loop -- Do 10 requests.
      Copy (Request_Data, "Test Data " & I'Img);
      for I of Requester.Send_Request (Request => Request_Data) loop
         if I.Sample_Info.Valid_Data then
            Ada.Text_IO.Put_Line (To_Standard_String (I.Data.all));
         end if;
      end loop;
      delay 1.0;
   end loop;

   --  Tell replier to finalize and get the last reply
   Copy (Request_Data, DONE);
   for I of Requester.Send_Request (Request => Request_Data) loop
      if I.Sample_Info.Valid_Data then
         Ada.Text_IO.Put_Line (To_Standard_String (I.Data.all));
      end if;
   end loop;

   --  Clean up.
   DDS.Request_Reply.Tests.Simple.Requester.Delete (Requester);
   Factory.Delete_Participant (Participant);
end DDS.Request_Reply.Tests.Simple.Requester_Main;
