with AUnit.Test_Cases;
with AUnit;
with DDS.DomainParticipantFactory;
with DDS.DomainParticipant;
with DDS.Request_Reply.Unittest.Requester;
with DDS.Request_Reply.Unittest.Replier;

package DDS.Request_Reply.Requester.Impl.Unittest is
   
   type Test_Case is new AUnit.Test_Cases.Test_Case with record
      Factory     : DDS.DomainParticipantFactory.Ref_Access := DDS.DomainParticipantFactory.Get_Instance;
      Participant : DDS.DomainParticipant.Ref_Access;
      Requester   : DDS.Request_Reply.Unittest.Requester.Ref_Access;
      Replier     : DDS.Request_Reply.Unittest.Replier.Ref_Access;
   end record;
   
   function Name (Test : Test_Case) return AUnit.Message_String;
   
   procedure Register_Tests (Test : in out Test_Case);
   --  Register test methods with test suite

   procedure Set_Up_Case (Test : in out Test_Case);
   procedure Tear_Down_Case (Test : in out Test_Case);

   Service_Name  : DDS.String := DDS.To_DDS_String("Tweet");
   Library_Name  : DDS.String := DDS.To_DDS_String("BuiltinQosLib");
   Profile_Name  : DDS.String := DDS.To_DDS_String("Baseline.7.3.0");
   Test_Data     : DDS.String := DDS.To_DDS_String("<<<   Test Data >>>");
   
end DDS.Request_Reply.Requester.Impl.Unittest;
