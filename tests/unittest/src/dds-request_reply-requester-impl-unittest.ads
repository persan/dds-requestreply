with AUnit.Test_Cases;
with AUnit;
with DDS.DomainParticipantFactory;
with DDS.DomainParticipant;
package DDS.Request_Reply.Requester.Impl.Unittest is
   
   type Test_Case is new AUnit.Test_Cases.Test_Case with record
      Factory     : DDS.DomainParticipantFactory.Ref_Access := DDS.DomainParticipantFactory.Get_Instance;
      Participant : DDS.DomainParticipant.Ref_Access;
   end record;
   
   function Name (Test : Test_Case) return AUnit.Message_String;
   
   procedure Register_Tests (Test : in out Test_Case);
   --  Register test methods with test suite

   procedure Set_Up_Case (Test : in out Test_Case);
   procedure Tear_Down_Case (Test : in out Test_Case);

end DDS.Request_Reply.Requester.Impl.Unittest;
