-- ---------------------------------------------------------------------
--                                                                    --
--               Copyright (c) per.sandberg@bahnhof.se                --
--                                                                    --
--  Permission is hereby granted, free of charge, to any person       --
--  obtaining a copy of this software and associated documentation    --
--  files (the "Software"), to deal in the Software without           --
--  restriction, including without limitation the rights to use,      --
--  copy, modify, merge, publish, distribute, sublicense, and/or sell --
--  copies of the Software, and to permit persons to whom the Software--
--  is furnished to do so, subject to the following conditions:       --
--                                                                    --
--  The above copyright notice and this permission notice             --
--  (including the next paragraph) shall be included in all copies or --
--  substantial portions of the Software.                             --
--                                                                    --
--  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,   --
--  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF--
--  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND             --
--  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT       --
--  HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,      --
--  WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,--
--  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER     --
--  DEALINGS IN THE SOFTWARE.                                         --
--                                                                    --
--  <spdx: MIT>
--                                                                    --
-- ---------------------------------------------------------------------
with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with DDS.DomainParticipant;
with DDS.DomainParticipantFactory;
with Primes.PrimeNumberRequester;
with DDS.Logger.LoggerDevice.Errors_And_Warnings_As_Exceptions; pragma Warnings (Off, DDS.Logger.LoggerDevice.Errors_And_Warnings_As_Exceptions);
with GNAT.Time_Stamp;
with Ada.Directories;
procedure Primes.Requester_Main is
   use DDS.DomainParticipant;
   use all type DDS.ReturnCode_T;
   command_name : constant String := Ada.Directories.Simple_Name(Ada.Command_Line.command_name);
   Factory : DDS.DomainParticipantFactory.Ref_Access := DDS.DomainParticipantFactory.Get_Instance;

   procedure Requester_Shutdown (Participant : in out DDS.DomainParticipant.Ref_Access;
                                 Requester   : in out PrimeNumberRequester.Ref_Access;
                                 Request     : in out PrimeNumberRequest) is
   begin


      Request.finalize;
      PrimeNumberRequester.Delete (Requester);

      if Participant /= null then
         Participant.Delete_Contained_Entities;
      end if;

      Factory.Delete_Participant (Participant);
      Factory.Finalize_Instance;
   end;


   procedure Requester_Main (N                : DDS.Long ;
                             Primes_Per_Reply : DDS.Long ;
                             Domain_Id        : DDS.DomainId_T)
   is

      Retcode : DDS.ReturnCode_T;

      Replies          : aliased PrimeNumberReply_Seq.Sequence;
      Info_Seq         : aliased DDS.SampleInfo_Seq.Sequence;
      Participant      : DDS.DomainParticipant.Ref_Access;
      Requester        : PrimeNumberRequester.Ref_Access;
      Request          : PrimeNumberRequest;
      MAX_WAIT         : constant DDS.Duration_T := DDS.To_Duration_T (2.0);
      In_Progress      : Boolean := False;
      Publication_Matched_Status : DDS.PublicationMatchedStatus;

      use type PrimeNumberRequester.Ref_Access;
      use PrimeNumberReply_Seq;
      use DDS.SampleInfo_Seq;
      use type DDS.Long;
   begin
      Request.initialize;
      DDS.Logger.Get_Instance.Set_Verbosity (DDS.VERBOSITY_ALL);

      --  Create the participant
      Participant := Factory.Create_Participant (Domain_Id);

      --  Create the requester with that participant, and a QoS profile
      --  defined in USER_QOS_PROFILES.xml
      --
      Requester := PrimeNumberRequester.Create (Participant  => Participant,
                                                Service_Name => Service_Name,
                                                Library_Name => Qos_Library_Name,
                                                Profile_Name => Qos_Profile_Name);



      loop
         Requester.Get_Request_Data_Writer.Get_Publication_Matched_Status(Publication_Matched_Status);
         exit when Publication_Matched_Status.total_count > 0;
         Put_Line(GNAT.Time_Stamp.Current_Time & ":" & Command_Name & " Waiting for server");
         Requester.Get_Request_Data_Writer.wait(DDS.PUBLICATION_MATCHED_STATUS,DDS.To_Duration_T(60.0));
      end loop;


      Requester.Send_Request ((N  => N, Primes_Per_Reply => Primes_Per_Reply));

      Retcode := Requester.Receive_Replies
        (Replies         => Replies'Unrestricted_Access,
         Sample_Info     => Info_Seq'Unrestricted_Access,
         Min_Reply_Count => 1,
         Max_Reply_Count => DDS.LENGTH_UNLIMITED,
         Timeout         => MAX_WAIT);


      In_Progress := True;

      while In_Progress and then (Retcode = DDS.RETCODE_OK)loop
         Put ("(");
         for I in 1 .. Get_Length (Replies'Unrestricted_Access) loop
            declare
               Reply        : constant PrimeNumberReply_Access := Get_Reference (Replies'Unrestricted_Access, I);
               Info         : constant DDS.SampleInfo_Access := Get_Reference (Info_Seq'Unrestricted_Access, I);
            begin
               if (Info.Valid_Data) then
                  for Prime_Number of Reply.Primes loop
                     Put (Prime_Number.all'Img);
                  end loop;
               end if;
               if Reply.Status /= REPLY_IN_PROGRESS then
                  In_Progress := False;
                  if Reply.Status = REPLY_ERROR then
                     Put_Line (Standard_Error, "Error in Replier");
                  elsif Reply.Status = REPLY_COMPLETED then
                     null;
                  end if;
               end if;
               Put_Line (")");
            end;
         end loop;

         --   Return the loan to the middleware
         Requester.Return_Loan (Replies, Info_Seq);

         if In_Progress then
            Retcode := Requester.Receive_Replies
              (Replies         => Replies'Unrestricted_Access,
               Sample_Info     => Info_Seq'Unrestricted_Access,
               Min_Reply_Count => 1,
               Max_Reply_Count => DDS.LENGTH_UNLIMITED,
               Timeout         => MAX_WAIT);
         end if;
      end loop;

      if Retcode /= DDS.RETCODE_OK then
         if (Retcode = DDS.RETCODE_TIMEOUT) then
            Put_Line (Standard_Error, "Timed out waiting for prime numbers");
         else
            Put_Line (Standard_Error, "Error receiving replies" &  Retcode'Img);
         end if;
      end if;
      Requester_Shutdown (Participant, Requester, Request);
   exception
      when others =>
         Requester_Shutdown (Participant, Requester, Request);
         raise;
   end;


   Domain_Id        : DDS.DomainId_T :=  0;
   N                : DDS.Long := 40;
   Primes_Per_Reply : DDS.Long := 5;
begin

   if Ada.Command_Line.Argument_Count < 1 then
      Put_Line ("PrimeNumberRequester:");
      Put_Line ("Sends a request to calculate the prime numbers <= n");
      Put_Line ("Parameters: <n> [<primes_per_reply> = 5] [<domain_id> = 0]");
      Put_Line ("Using default (5)");
   else
      N := DDS.Integer'Value (Ada.Command_Line.Argument (1));
   end if;

   if Ada.Command_Line.Argument_Count > 1 then
      Primes_Per_Reply := DDS.Integer'Value (Ada.Command_Line.Argument (2));
   end if;

   if Ada.Command_Line.Argument_Count > 2 then
      Domain_Id := DDS.DomainId_T'Value (Ada.Command_Line.Argument (3));
   end if;

   DDS.Logger.Get_Instance.Set_Verbosity (DDS.VERBOSITY_WARNING);
   -- Uncomment this to turn on additional logging
   -- RTIDDS.Config.Logger.Get_Instance.Set_Verbosity (RTIDDS.Config.VERBOSITY_ERROR);

   Put_Line ("PrimeNumberRequester: Sending a request to calculate the " &
               "prime numbers " & N'Img & " in sequences of " & Primes_Per_Reply'Img & " or less elements." &
               "(on domain " & Domain_Id'Img & ")" );


   Requester_Main (N, Primes_Per_Reply, Domain_Id);
end;
