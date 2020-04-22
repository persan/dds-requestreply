with DDS.Request_Reply.Tests.Simple.Replier;

procedure DDS.Request_Reply.Tests.Simple.Replier_Main is

   Replier    : Simple.Replier.Ref_Access :=
                  DDS.Request_Reply.Tests.Simple.Replier.Create
                    (Participant    => Participant,
                     Service_Name   => Service_Name,
                     Library_Name   => Qos_Library,
                     Profile_Name   => Qos_Profile);

   Reply      : String;
   Count      : Natural := 0;

begin
   Main_Loop : while True loop

      for I of Replier.Receive_Request loop

         if I.Data.all = DONE then
            Append (Reply, "FINAL " & Count'Img);
         end if;

         Append (Reply, "reply to<");
         Append (Reply, I.Data.all);
         Append (Reply, "> Count" & Count'Img);

         if I.Data.all = DONE then
            Append (Reply, "> Count" & Count'Img);
         end if;

         Count := Count + 1;
         Replier.Send_Reply
           (Reply => Reply,
            Id    => Get_Sample_Identity (I.Sample_Info.all));
         Finalize (Reply);

         if I.Data.all = DONE then
            exit Main_Loop;
         end if;

      end loop;
   end loop Main_Loop;
   Simple.Replier.Delete (Replier);
end DDS.Request_Reply.Tests.Simple.Replier_Main;
