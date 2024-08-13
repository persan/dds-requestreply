with AUnit.Assertions;
with GNAT.Source_Info;
with AUnit; use AUnit;
package body DDS.Request_Reply.Requester.Impl.Unittest is
   use Aunit.Assertions;
   ----------
   -- Name --
   ----------
   Test_Name : constant Standard.String := GNAT.Source_Info.Enclosing_Entity;

   function Name (Test : Test_Case) return AUnit.Message_String is
   begin
      return Format (Test_Name);
   end Name;

   procedure Test_Setup (Test : in out Aunit.Test_Cases.Test_Case'Class) is
      t : Test_Case renames Test_Case(Test);
   begin
      T.Participant := T.Factory.Create_Participant;
      T.Replier := DDS.Request_Reply.Unittest.Replier.Create(T.Participant,Service_Name,Library_Name,Profile_Name);
      T.Requester := DDS.Request_Reply.Unittest.Requester.Create(T.Participant,Service_Name,Library_Name,Profile_Name);
   end;


   procedure Test_Create_Correlation_Cft (Test : in out Aunit.Test_Cases.Test_Case'Class) is
   begin
      null;
   end;


   procedure Test_Create_Reader_Topic (Test : in out Aunit.Test_Cases.Test_Case'Class) is
   begin
      null;
   end;
   procedure Test_Create_Writer_Topic (Test : in out Aunit.Test_Cases.Test_Case'Class) is
   begin
      null;
   end;
   procedure Test_Create (Test : in out Aunit.Test_Cases.Test_Case'Class) is
   begin
      null;
   end;
   procedure Test_Create_Query_Expression_For_Correlation_Sequence_Number (Test : in out Aunit.Test_Cases.Test_Case'Class) is
   begin
      null;
   end;
   procedure Test_Create_Correlation_Condition (Test : in out Aunit.Test_Cases.Test_Case'Class) is
   begin
      null;
   end;
   procedure Test_Wait_For_Replies_For_Related_Request (Test : in out Aunit.Test_Cases.Test_Case'Class) is
   begin
      null;
   end;

   procedure Test_Wait_For_Replies (Test : in out Aunit.Test_Cases.Test_Case'Class) is
      T    : Test_Case renames Test_Case(Test);
       Data : DDS.String;
       Info : DDS.SampleInfo;
      task sender is
      end sender;
      task body sender is
      begin
         delay 1.0;
         T.Requester.Send_Request(Test_Data);
      end sender;
      use type Ada.calendar.time;
      T0 : constant Ada.calendar.time := Ada.calendar.Clock;
      T1 : Ada.calendar.time := Ada.calendar.Clock;
      delta_t : Duration;
   begin

      T.Replier.Wait_for_requests(1,DDS.To_Duration_t(10.0));
      T1 := Ada.calendar.Clock;
      delta_t := T1 - T0;
      Assert (delta_t > 0.9 and delta_t < 1.1, "Dit not wait for request");
      T.Replier.Receive_Request(Data,Info);

      -- T.Requester.Read_Reply(
   end;

   procedure Test_Wait_For_Replies2 (Test : in out Aunit.Test_Cases.Test_Case'Class) is
   begin
      null;
   end;

   procedure Test_Get_Reply_Loaned (Test : in out Aunit.Test_Cases.Test_Case'Class) is
   begin
      null;
   end;

   procedure Test_Tear_Down (Test : in out Aunit.Test_Cases.Test_Case'Class) is
      T : Test_Case renames Test_Case(Test);
   begin
      DDS.Request_Reply.Unittest.Replier.Delete(T.Replier);
      DDS.Request_Reply.Unittest.Requester.Delete(T.Requester);
      T.Participant.Delete_Contained_Entities;
      T.Factory.Delete_Participant(T.Participant);
   end Test_Tear_Down;

   procedure Register_Tests (Test : in out Test_Case) is separate;
   -----------------
   -- Set_Up_Case --
   -----------------

   procedure Set_Up_Case (Test : in out Test_Case) is
   begin
      null;
   end Set_Up_Case;

   --------------------
   -- Tear_Down_Case --
   --------------------

   procedure Tear_Down_Case (Test : in out Test_Case) is
   begin
      null;
   end Tear_Down_Case;

end DDS.Request_Reply.Requester.Impl.Unittest;
